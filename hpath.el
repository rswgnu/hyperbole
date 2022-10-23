;;; hpath.el --- GNU Hyperbole support routines for handling POSIX and MSWindows paths  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     1-Nov-91 at 00:44:23
;; Last-Mod:     13-Oct-22 at 21:54:38 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'cl-lib)
(require 'hact)
(require 'seq) ;; For seq-find
(require 'subr-x) ;; For string-trim
(require 'hversion) ;; for (hyperb:window-system) definition
(require 'hui-select) ;; for `hui-select-markup-modes'
(require 'tramp)

(unless (fboundp 'br-in-browser)
    ;; Then the OO-Browser is not loaded, so we can never be within the
    ;; browser.  Define this as a dummy function that always returns nil
    ;; until the OO-Browser is ever loaded.
    (defun br-in-browser ()
      "Always returns nil since the OO-Browser is not loaded."
      nil))

;;; ************************************************************************
;;; Public Variables
;;; ************************************************************************

(defcustom hpath:auto-completing-read-modes '(helm-mode ivy-mode selectrum-mode)
  "*List of boolean mode variables whose modes automatically list completions.
These are modes where completions are listed without the need for
pressing the ? key."
  :type '(repeat variable)
  :group 'hyperbole-commands)

(defvar hpath:auto-variable-alist
  '(("\\.el[cn]?\\'" . load-path)
    ("\\.org\\'" . org-directory)
    ("\\.py\\'" . "PYTHONPATH"))
  "Alist of filename patterns and related variables to prepend to resolve them.
Each element looks like FILENAME-REGEXP . LISP-VARIABLE-OR-ENV-VARIABLE-STR.

The VARIABLE value may be: a directory path, a list of directory paths
or a colon or semicolon delimited string of directory paths.")

(defcustom hpath:find-file-urls-mode nil
  "Set to t to enable use of ftp and http urls in file finding commands.
Requires that a remote file access library is available.
Default is nil since this can slow down normal file finding."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (lambda (_symbol _value) (call-interactively #'hpath:find-file-urls-mode))
  :group 'hyperbole-buttons)

(defconst hpath:line-and-column-regexp
  ":\\([-+]?[0-9]+\\)\\(:\\([-+]?[0-9]+\\)\\)?\\s-*\\'"
  "Regexp matching a trailing line number with an optional column number.
Path, line number and column are colon separated.
Group 1 is the line number.  Group 3 is the column number.")

(defconst hpath:markup-link-anchor-regexp
  "\\`\\(#?[^#]*[^#.]\\)?\\(#\\)\\([^\]\[#^{}<>\"`'\\\n\t\f\r]*\\)"
  "Regexp matching a filename followed by a hash (#) and an optional anchor name.
The anchor is an in-file reference.
# is group 2.  Group 3 is the anchor name.")

(defvar hpath:path-variable-regexp "\\`\\$?[{(]?\\([-_A-Z]*path[-_A-Z]*\\)[)}]?\\'"
  "Regexp that matches exactly to a standalone path variable name reference.
Group 1 is the variable name.")

(defvar hpath:path-variable-value-regexp
  (let* ((posix-separators ":;")
	 (windows-separators ";")
	 (reserved-chars "<>:\"|?*")
	 (exclude-space  "\t\n\r\f")
	 (control-chars  "[:cntrl:]")
	 ;; Posix char set to exclude from paths
	 (nope (concat "\[^" posix-separators reserved-chars exclude-space control-chars "\]+"))
	 ;; Windows char set to exclude from paths
	 (nowin (concat "\[^" windows-separators reserved-chars exclude-space control-chars "\]+")))
    (concat "\\`\\.?:" nope ":" nope ":\\|:\\.?:" nope ":\\|:" nope ":" nope ":\\|:" nope ":" nope ":\\.?\\'"
	    "\\|"
	    "\\`\\.?;" nowin ";" nowin ";\\|;\\.?;\\|;" nowin ";" nowin ";\\|;" nowin ";" nowin ";\\|;" nowin ";" nowin ";\\.?\\'"))
  ;; A zero-length (null) directory name in the value of PATH indicates the current directory.
  ;; A null directory name may appear as two adjacent colons, or as an initial or trailing colon.
  "Regexp that heuristically matches to Posix or Windows path variable values.
Posix has colon-separated and Windows has semicolon-separated
path variable values.")

(defconst hpath:section-line-and-column-regexp
  "\\([^ \t\n\r\f:][^\t\n\r\f:]+\\(:[^0-9\t\n\r\f]*\\)*\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?$"
  "Regexp that matches to a path with optional #section and :line-num:col-num.
Grouping 1 is path, grouping 3 is line number, grouping 4 is
column number.  Allow for \\='c:' single letter drive prefixes on
MSWindows and Elisp vars with colons in them.")

(defconst hpath:variable-regexp "\\$@?\{\\([^\}]+\\)@?\}"
  "Regexp matching variable names that Hyperbole resolves within pathnames.
The format is ${variable}.  Match grouping 1 is the name of the variable.")

;;; ************************************************************************
;;; Public Declarations
;;; ************************************************************************
(declare-function br-quit "ext:br")
(declare-function br-in-browser "ext:br")
(declare-function br-to-view-window "ext:br")

;;; ************************************************************************
;;; MS WINDOWS PATH CONVERSIONS
;;; ************************************************************************

;; This section adds automatic recognition of MSWindows implicit path
;; links and converts disk drive and path separators to whatever
;; format is needed by the underlying OS upon which Emacs is one,
;; notably either for POSIX or MSWindows (with no POSIX layer).

;; Especially useful when running Emacs under Windows Subsystem for
;; Linux (WSL) where the system-type variable is gnu/linux but
;; MSWindows is underneath so the user likely has many Windows
;; formatted links.

;; See "https://docs.microsoft.com/en-us/dotnet/standard/io/file-path-formats"
;; and "https://docs.microsoft.com/en-us/windows/wsl/interop" for
;; Windows path specifications and use under WSL.

(defvar hpath:posix-mount-points-regexp
  "^\\(Filesystem\\|rootfs\\|none\\) "
  "Regexp of \\='mount' output lines that are not mount points of MSWindows paths.")

(defvar hpath:mswindows-mount-prefix
  (cond ((eq system-type 'cygwin)
	 "/cygdrive/")
	(hyperb:microsoft-os-p
	 "")
	(t ;; POSIX
	"/mnt/"))
  "Path prefix to add when converting MSWindows drive paths to POSIX-style.
Must include a trailing directory separator or be nil.")

(defconst hpath:mswindows-drive-regexp (format "\\`\\(%s\\)?[\\/]?\\([a-zA-Z]\\)[:\\/]"
					       hpath:mswindows-mount-prefix)
  "Regular expression matching an MSWindows drive letter.
Matches at the beginning of a path string.  Grouping 2 is the
actual letter of the drive.  If the value of
`hpath:mswindows-mount-prefix' changes, then re-initialize this
constant.")

(defconst hpath:mswindows-path-regexp "\\`.*\\.*[a-zA-Z0-9_.]"
  "Regexp matching the start of an MSWindows path without a drive letter.
Path contains directory separators.")

;;;###autoload
(defvar hpath:posix-mount-point-to-mswindows-alist nil
  "Automatically set alist of (posix-mount-point . window-path-prefix) elements.
Used to expand posix mount points to Windows UNC paths during
posix-to-mswindows conversion.")

;;;###autoload
(defun hpath:mswindows-to-posix (path)
  "Convert a MSWindows PATH to a Posix-style path or return the path unchanged.
If path begins with an MSWindows drive letter, prefix the
converted path with the value of `hpath:mswindows-mount-prefix'."
  (interactive "sMSWindows path to convert to POSIX: ")
  (when (and (stringp path) (not (equal path "\\\\")))
    (setq path (hpath:mswindows-to-posix-separators path))
    (when (string-match hpath:mswindows-drive-regexp path)
      (when (string-match hpath:mswindows-drive-regexp path)
	(let* ((drive-prefix (downcase (match-string 2 path)))
	       (rest-of-path (substring path (match-end 0)))
	       (absolute-p (and (not (string-empty-p rest-of-path))
				(= (aref rest-of-path 0) ?/))))
	  ;; Convert MSWindows disk drive paths to POSIX-style with a mount prefix.
	  (setq path (concat hpath:mswindows-mount-prefix drive-prefix
			     (cond (hyperb:microsoft-os-p ":")
				   (absolute-p "")
				   (t "/"))
			     rest-of-path))))))
  path)

(defun hpath:mswindows-to-posix-separators (path)
  "Replace backslashes with forward slashes and abbreviate the PATH if possible.
Path must be a string or an error will be triggered.  See
`abbreviate-file-name' for how path abbreviation is handled."
    (setq path (replace-regexp-in-string "\\\\" "/" path)
          ;; Downcase any host and domain for mount-point matching
          path (if (string-match "\\`//[^/:]+" path)
                   (concat (downcase (match-string 0 path))
                           (substring path (match-end 0)))
                 path)
          path (hpath:abbreviate-file-name path)
          path (replace-regexp-in-string (regexp-quote "\\`") "" path)
          path (replace-regexp-in-string (regexp-quote "\\>") "" path)))

;;;###autoload
(defun hpath:posix-to-mswindows (path)
  "Convert a Posix-style PATH to an MSWindows path or return the path unchanged.
If path begins with an optional mount prefix,
`hpath:mswindows-mount-prefix', followed by an MSWindows drive
letter, remove the mount prefix."
  (interactive "sPOSIX path to convert to MSWindows: ")
  (when (stringp path)
    (setq path (hpath:posix-to-mswindows-separators path))
    ;; Remove any POSIX mount prefix preceding an MSWindows path.
    (if (eq 0 (string-match hpath:mswindows-mount-prefix path))
	(setq path (substring path (match-end 0))))
    (when (string-match hpath:mswindows-drive-regexp path)
      (when (string-match hpath:mswindows-drive-regexp path)
	(let* ((drive-prefix (downcase (match-string 2 path)))
	       (rest-of-path (substring path (match-end 0)))
	       (absolute-p (= (aref path (1- (match-end 0))) ?\\)))
	  ;; Convert formerly Posix-style Windows disk drive paths to MSWindows-style.
	  (setq path (concat drive-prefix ":"
			     (if (or (not absolute-p)
				     (string-match "\\`[~/]" rest-of-path))
				 ""
			       "\\")
			     rest-of-path))))))
  path)

(defun hpath:posix-to-mswindows-separators (path)
  "Replace forward slashes with backslashes and abbreviate the PATH if possible.
Path must be a string or an error will be triggered.  See
`abbreviate-file-name' for how path abbreviation is handled."
  (let ((directory-abbrev-alist hpath:posix-mount-point-to-mswindows-alist))
    (replace-regexp-in-string "/" "\\\\" (hpath:abbreviate-file-name path))))

(defun hpath:posix-path-p (path)
  "Return non-nil if PATH looks like a Posix path."
  (and (stringp path) (string-match  "/" path)))

;;;###autoload
(defun hpath:substitute-posix-or-mswindows-at-point ()
  "If point is in a Posix or MSWindows path, change the path to the other type."
  (interactive "*")
  (barf-if-buffer-read-only)
  (let* ((opoint (point))
	 (str-and-positions (hpath:delimited-possible-path t t))
	 (path (car str-and-positions))
	 (start (nth 1 str-and-positions))
	 (end (nth 2 str-and-positions)))
      (when path
	(if (hpath:posix-path-p path)
	    (setq path (hpath:posix-to-mswindows path))
	  (setq path (hpath:mswindows-to-posix path)))
	(delete-region start end)
	(insert path)
	(goto-char (min opoint (point-max))))))

;;;###autoload
(defun hpath:substitute-posix-or-mswindows (path)
  "Change a recognizable Posix or MSWindows PATH to the other type of path."
  (when (stringp path)
    (if (hpath:posix-path-p path)
	(hpath:posix-to-mswindows path)
      (hpath:mswindows-to-posix path))))

;;;###autoload
(defun hpath:cache-mswindows-mount-points ()
  "Cache valid MSWindows mounts when under a non-MSWindows OS, e.g. WSL.
Mount points are cached in `directory-abbrev-alist'.
Call this function manually if mount points change after Hyperbole is loaded."
  (interactive)
  (when (not hyperb:microsoft-os-p)
    (let (mount-points-to-add)
      ;; Convert plist to alist for sorting.
      (hypb:map-plist (lambda (path mount-point)
			(when (string-match "\\`\\([a-zA-Z]\\):\\'" path)
			  (setq path (concat "/" (match-string 1 path))))
			;; Drive letter must be downcased
			;; in order to work when converted back to Posix.
			;; Assume all mounted Windows paths are
			;; lowercase for now.
                        (setq path (downcase path))
                        (push (cons (downcase path) mount-point)
                              mount-points-to-add)
                        ;; If a network share with a domain
                        ;; name, also add an entry WITHOUT the
                        ;; domain name to the mount points
                        ;; table since Windows paths often omit
                        ;; the domain.
                        (when (string-match "\\`\\([\\/][\\/][^.\\/]+\\)\\([^\\/]+\\)" path)
                          (push (cons (concat (match-string 1 path)
                                              (substring path (match-end 0)))
                                      mount-point)
                                mount-points-to-add)))
		      ;; Return a plist of MSWindows path-mounted mount-point pairs.
		      (split-string (shell-command-to-string (format "df -a -t drvfs 2> /dev/null | sort | uniq | grep -v '%s' | sed -e 's+ .*[-%%] /+ /+g'" hpath:posix-mount-points-regexp))))
      ;; Sort alist of (path-mounted . mount-point) elements from shortest
      ;; to longest path so that the longest path is selected first within
      ;; 'directory-abbrev-alist' (elements are added in reverse order).
      (setq mount-points-to-add
            (sort mount-points-to-add
                  (lambda (cons1 cons2) (<= (length (car cons1)) (length (car cons2))))))
      (let (path
            mount-point)
        (mapc (lambda (path-and-mount-point)
		(setq path (car path-and-mount-point)
		      mount-point (cdr path-and-mount-point))
                ;; Don't abbreviate /mnt or /cygdrive to /, skip this entry.
                (unless (equal mount-point "/")
		  (add-to-list 'directory-abbrev-alist (cons (format "\\`%s\\>" (regexp-quote path))
							     mount-point))))
	      mount-points-to-add))
      ;; Save the reverse of each mount-points-to-add so
      ;; can expand paths when going from posix-to-mswindows.
      (setq hpath:posix-mount-point-to-mswindows-alist
	    (mapcar (lambda (elt) (cons (concat "\\`" (cdr elt) "\\>")
                                        (car elt))) mount-points-to-add))
      mount-points-to-add)))


;;; ************************************************************************
;;; FILE VIEWER COMMAND SETTINGS
;;; ************************************************************************

(defcustom hpath:external-open-office-suffixes "doc[mx]?\\|od[st]\\|ppsx?\\|ppt[mx]?\\|v[dst][s]?[tx]\\|vsd[x]?\\|xls[mx]?"
  "*Regexp of Open Office document suffix alternatives.
These are to be display externally with the Action Key
Do not include an initial period or enclosing grouping parentheses;
these will be added automatically.

See http://www.openwith.org/programs/openoffice for a full list of
possible suffixes."
  :type 'string
  :group 'hyperbole-commands)

(defcustom hpath:external-file-suffixes "e?ps\\|dvi\\|pdf\\|ps\\.g?[zZ]\\|gif\\|tiff?\\|xpm\\|xbm\\|xwd\\|pm\\|pbm\\|jpe?g\\|ra?s\\|xcf"
  "*Non-operating system dependent regexp of file suffixes to open outside Emacs.
These are opened with the Action Key when not handled by
`hpath:native-image-suffixes'.  Do not include an initial period
or enclosing grouping parentheses; these will be added
automatically."
  :type 'string
  :group 'hyperbole-commands)

(defcustom hpath:external-display-alist-macos (list (cons (format "\\.\\(%s\\|%s\\|app\\|adaptor\\|bshlf\\|clr\\|concur\\|create\\|diagram\\|dp\\|frame\\|locus\\|Mesa\\|nib\\|project\\|rtf\\|sense\\|tree\\)$"
								  hpath:external-open-office-suffixes
								  hpath:external-file-suffixes)
							  "open"))
  "*An alist of (FILENAME-REGEXP . DISPLAY-PROGRAM-STRING-OR-LIST) for the macOS.
See the function `hpath:get-external-display-alist' for detailed
format documentation."
  :type '(alist :key-type regexp :value-type string)
  :group 'hyperbole-commands)

(defcustom hpath:external-display-alist-mswindows (list (cons (format "\\.\\(%s\\)$" hpath:external-open-office-suffixes)
							      "openoffice.exe")
							(cons (format "\\.\\(%s\\|vba\\)$" hpath:external-file-suffixes)
							      "/c/Windows/System32/cmd.exe //c start \"${@//&/^&}\""))
  "*An alist of (FILENAME-REGEXP . DISPLAY-PROGRAM-STRING-OR-LIST) for MS Windows.
See the function `hpath:get-external-display-alist' for detailed
format documentation."
  :type '(alist :key-type regexp :value-type string)
  :group 'hyperbole-commands)


;; Old X Window System configuration prior to use of 'xdg-open'.
;; (defvar hpath:external-display-alist-x (list '("\\.e?ps$" . "ghostview")
;; 					     '("\\.dvi$"  . "xdvi")
;; 					     (cons (format "\\.\\(%s\\)$" hpath:external-open-office-suffixes) "openoffice")
;; 					     '("\\.pdf$"  . ("xpdf" "acroread"))
;; 					     '("\\.ps\\.g?[zZ]$" . "zcat %s | ghostview -")
;; 					     '("\\.\\(gif\\|tiff?\\|xpm\\|xbm\\|xwd\\|pm\\|pbm\\|jpe?g\\)"  . "xv")
;; 					     '("\\.ra?s$" . "snapshot -l"))

(defcustom hpath:external-display-alist-x (list (cons (format "\\.\\(%s\\|%s\\)$"
							      hpath:external-open-office-suffixes
							      hpath:external-file-suffixes)
						      "setsid -w xdg-open"))
  "*An alist of (FILENAME-REGEXP . DISPLAY-PROGRAM-STRING-OR-LIST) for X.
See the function `hpath:get-external-display-alist' for detailed
format documentation."
  :type '(alist :key-type regexp :value-type string)
  :group 'hyperbole-commands)

(defvar hpath:info-suffix "\\.info\\(-[0-9]+\\)?\\(\\.gz\\|\\.Z\\|-z\\)?\\'"
  "Regexp matching to the end of Info manual file names.")

(defcustom hpath:internal-display-alist
  (delq
   nil
   (list

    ;; Support internal sound when available.
    (if (fboundp 'play-sound-file)
	'("\\.\\(au\\|mp3\\|ogg\\|wav\\)$" . play-sound-file))

    ;; Run the OO-Browser on OOBR or OOBR-FTR Environment files.
    '("\\(\\`\\|/\\)\\(OOBR\\|oobr\\).*\\(-FTR\\|-ftr\\)?\\'" . br-env-browse)

    ;; Display the top node from Info online manuals.
    (cons
     (concat hpath:info-suffix
	     "\\|/\\(info\\|INFO\\)/[^.]+$\\|/\\(info-local\\|INFO-LOCAL\\)/[^.]+$")
     (lambda (file)
       (when (and (string-match hpath:info-suffix file)
		  (match-beginning 1))
	 ;; Removed numbered trailer to get basic filename.
	 (setq file (concat (substring-no-properties file 0 (match-beginning 1))
			    (substring-no-properties file (match-end 1)))))
       ;; Ensure that Info files with non-absolute directories outside of the
       ;; `Info-directory-list' are resolved properly, e.g. "man/hyperbole.info".
       (unless (file-name-absolute-p file)
	 (setq file (expand-file-name "man/hyperbole.info")))
       (require 'info)
       ;; Ensure that *info* buffer is displayed in the right place.
       (hpath:display-buffer (current-buffer))
       (condition-case ()
	   (Info-find-node file "Top")
	 (error (if (and file (file-exists-p file))
		    (progn
		      (if (get-buffer "*info*")
			  (kill-buffer "*info*"))
		      (Info-find-node file "*" nil t))
		  (error "Invalid file"))))))

    '("\\.rdb\\'" . rdb:initialize)))
  "*Alist of (FILENAME-REGEXP . EDIT-FUNCTION) elements for calling special
functions to display particular file types within Emacs.  See
also the function (hpath:get-external-display-alist) for external
display program settings."
  :type '(alist :key-type regexp :value-type sexp)
  :group 'hyperbole-commands)

(defvar hpath:display-buffer-alist
  (list
   (list 'this-window   #'switch-to-buffer)
   (list 'other-window  (lambda (b)
			  (if (br-in-browser)
			      (progn (br-to-view-window)
				     (switch-to-buffer b))
			    (switch-to-buffer-other-window b))))
   (list 'one-window    (lambda (b)
			  (when (br-in-browser)
			    (br-quit))
			  (delete-other-windows)
			  (switch-to-buffer b)))
   (list 'new-frame     (lambda (b)
			  ;; Give temporary modes such as isearch a chance to turn off.
			  (run-hooks 'mouse-leave-buffer-hook)
			  (select-frame (make-frame (frame-parameters)))
			  (switch-to-buffer b)))
   (list 'other-frame   #'hpath:display-buffer-other-frame)
   (list 'other-frame-one-window   (lambda (b)
				     (hpath:display-buffer-other-frame b)
				     (delete-other-windows))))
  "*Alist of (DISPLAY-WHERE-SYMBOL  DISPLAY-BUFFER-FUNCTION) elements.
This permits fine-grained control of where Hyperbole displays linked to buffers.

The default value of DISPLAY-WHERE-SYMBOL is given by `hpath:display-where'.
Valid DISPLAY-WHERE-SYMBOLs are:
    this-window             - display in the current window
    other-window            - display in another window in the current frame
    one-window              - display in the current window, deleting other
                              windows
    new-frame               - display in a new frame
    other-frame             - display in another, possibly existing, frame
    other-frame-one-window  - display in another frame, deleting other windows.")

(defvar hpath:display-where 'other-window
  "Symbol specifying the default method to use to display Hyperbole link referents.
See documentation of `hpath:display-where-alist' for valid values.

See Info node `(elisp)Choosing Window Options' for where Emacs displays buffers.")

(defvar hpath:display-where-alist
  (list
   (list 'this-window  #'find-file)
   (list 'other-window (lambda (f)
			 (if (br-in-browser)
			     (progn (br-to-view-window)
				    (find-file f))
			   (find-file-other-window f))))
   (list 'one-window   (lambda (f)
			 (if (br-in-browser) (br-quit))
			 (delete-other-windows)
			 (find-file f)))
   (list 'new-frame    (lambda (f)
			 (if (fboundp 'find-file-new-frame)
			     (find-file-new-frame f)
			   (hpath:find-other-frame f))))
   (list 'other-frame  #'hpath:find-other-frame)
   (list 'other-frame-one-window (lambda (f)
				   (hpath:find-other-frame f)
				   (delete-other-windows))))
  "*Alist of (DISPLAY-WHERE-SYMBOL DISPLAY-FILE-FUNCTION) elements.
This permits fine-grained control of where Hyperbole displays
linked to files.  The default value of DISPLAY-WHERE-SYMBOL is
given by `hpath:display-where'.
Valid DISPLAY-WHERE-SYMBOLs are:
    this-window             - display in the current window
    other-window            - display in another window in the current frame
    one-window              - display in current window, deleting other windows
    new-frame               - display in a new frame
    other-frame             - display in another, possibly existing, frame
    other-frame-one-window  - display in another frame, deleting other windows.")

(defcustom hpath:native-image-suffixes "\\.\\(xbm\\|xpm\\|xwd\\|png\\|gif\\|jpe?g\\)\\'"
  "Regular expression matching file name suffixes of images to display in Emacs.
Used only if the function `image-mode' is defined."
  :type 'regexp
  :group 'hyperbole-commands)

;;; ************************************************************************
;;; LINK PATH VARIABLE SUBSTITUTION SETTINGS
;;; ************************************************************************

;; The following variable permits sharing of links over wide areas, where
;; links may contain variable references whose values may differ between
;; link creator and link activator.
;;
;; When a link is created, if its path contains a match for any of the
;; variable values in hpath:variables, then the variable's symbol
;; surrounded by ${ } delimiters is substituted for the literal value.
;; Hyperbole then replaces the variable with a matching value when the
;; link is later resolved.
;;
(defcustom hpath:variables
  '(hyperb:dir Info-directory Info-directory-list sm-directory load-path exec-path)
  "*List of Emacs Lisp variable symbols to substitute within matching link paths.
Each variable value, if bound, must be either a pathname or a list of pathnames.
When embedded within a path, the format is ${variable}."
  :type '(repeat variable)
  :group 'hyperbole-commands)

;;; ************************************************************************
;;; Other public variables
;;; ************************************************************************

(defvar hpath:rfc "https://www.ietf.org/rfc/rfc%s.txt"
  "*Url pattern for (hpath:rfc rfc-num) to get the RFC document for `rfc-num'.")

(defcustom hpath:suffixes '(".gz" ".Z")
  "*List of filename suffixes to add or remove within hpath calls.
Used by `hpath:exists-p' and `hpath:substitute-dir'."
  :type '(repeat string)
  :group 'hyperbole-commands)

(defvar hpath:tmp-prefix "/tmp/remote-"
  "*Pathname prefix for use with external viewers.
The prefix is attached to remote files when copied locally for viewing.")

;; WWW URL format:  [URL[:=]]<protocol>:/[<user>@]<domain>[:<port>][/<path>]
;;             or   [URL[:=]]<protocol>://[<user>@]<domain>[:<port>][<path>]
;;             or   URL[:=][<user>@]<domain>[:<port>][<path>]  (no protocol specified)
;; Avoid [a-z]:/path patterns since these may be disk paths on OS/2, DOS or
;; Windows.
(defvar hpath:url-regexp "<?\\(URL[:=]\\)?\\(\\([a-zA-Z][a-zA-Z]+\\)://?/?\\([^/:@ \t\n\r\"`'|]+@\\)?\\([^/:@ \t\n\r\"`'|]+\\)\\(\\)\\(:[0-9]+\\)?\\([/~]\\([^\]\[ \t\n\r\"`'|(){}<>]+[^\]\[ \t\n\r\"`'|(){}<>.,?#!*]\\)*\\)?\\)>?"
  "Regular expression which matches a Url in a string or buffer.
Its match groupings and their names are:
  1 = hpath:url-keyword-grpn = optional `URL:' or `URL=' literal
  2 = hpath:url-grpn         = the whole URL
  3 = hpath:protocol-grpn    = access protocol
  4 = hpath:username-grpn    = optional username
  5 = hpath:sitename-grpn    = URL site to connect to
  6 = unused                 = for compatibility with hpath:url-regexp2
  7 = hpath:portnumber-grpn  = optional port number to use
  8 = hpath:pathname-grpn    = optional pathname to access.")

(defvar hpath:url-hostnames-regexp  "\\(www\\|s?ftp\\|telnet\\|news\\|nntp\\)"
  "Regexp group of hostnames that contains the Url access protocol to use.")

(defvar hpath:url-regexp2
  (concat
   "<?\\(URL[:=]\\|[^/@]\\|\\)\\(\\(\\)\\(\\)\\("
   hpath:url-hostnames-regexp
   "\\.[^/:@ \t\n\r\"`'|]+\\):?\\([0-9]+\\)?\\([/~]\\([^\]\[ \t\n\r\"`'|(){}<>]+[^\]\[ \t\n\r\"`'|(){}<>.,?#!*]\\)*\\)?\\)>?")
  "Regular expression which matches a Url in a string or buffer.
Its match groupings and their names are:
  1 = hpath:url-keyword-grpn = optional `URL:' or `URL=' literal
  2 = hpath:url-grpn         = the whole URL
  3 = unused                 = for compatibility with hpath:url-regexp
  4 = unused                 = for compatibility with hpath:url-regexp
  5 = hpath:sitename-grpn    = URL site to connect to
  6 = hpath:hostname-grpn = hostname used to determine the access
      protocol, e.g. ftp.domain.com
  7 = hpath:portnumber-grpn  = optional port number to use
  8 = hpath:pathname-grpn    = optional pathname to access.")

(defvar hpath:url-regexp3
  (concat
   "<?\\(URL[:=]\\)\\(\\(\\)\\(\\)"
   "\\([a-zA-Z0-9][^/:@ \t\n\r\"`'|]*\\.[^/:@ \t\n\r\"`'|]+\\)"
   ":?\\([0-9]+\\)?\\([/~]\\([^\]\[ \t\n\r\"`'|(){}<>]+[^\]\[ \t\n\r\"`'|(){}<>.,?#!*]\\)*\\)?\\)>?")
  "Regular expression which matches a Url in a string or buffer.
Its match groupings and their names are:
  1 = hpath:url-keyword-grpn = required `URL:' or `URL=' literal
  2 = hpath:url-grpn         = the whole URL
  3 = unused                 = for compatibility with hpath:url-regexp
  4 = unused                 = for compatibility with hpath:url-regexp
  5 = hpath:sitename-grpn    = URL site to connect to
  6 = hpath:hostname-grpn    = hostname used to determine the access
      protocol, e.g. ftp.domain.com
  7 = hpath:portnumber-grpn  = optional port number to use
  8 = hpath:pathname-grpn    = optional pathname to access.")

(defconst hpath:url-keyword-grpn 1
  "Optional `URL:' or `URL=' literal.
See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")
(defconst hpath:url-grpn 2
  "The whole URL.  See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")
(defconst hpath:protocol-grpn 3
  "Access protocol.  See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")
(defconst hpath:username-grpn 4
  "Optional username.  See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")
(defconst hpath:sitename-grpn 5
  "URL site to connect to.
See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")
(defconst hpath:hostname-grpn 6
  "Hostname used to determine the access protocol, e.g. sftp.domain.com.
See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")
(defconst hpath:portnumber-grpn 7
  "Optional port number to use.
See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")
(defconst hpath:pathname-grpn 8
  "Optional pathname to access.
See doc for `hpath:url-regexp' and `hpath:url-regexp[2,3]'.")

(defvar hpath:string-url-regexp (concat "\\`" hpath:url-regexp "\\'")
  "Regexp that matches to a possibly delimited Url and nothing else.
See the documentation for `hpath:url-regexp' for match groupings to
use with `string-match'.")

(defvar hpath:string-url-regexp2 (concat "\\`" hpath:url-regexp2 "\\'")
  "Regexp that matches to a possibly delimited terse Url and nothing else.
See the documentation for `hpath:url-regexp' for match groupings to
use with `string-match'.")

(defvar hpath:string-url-regexp3 (concat "\\`" hpath:url-regexp3 "\\'")
  "Regexp that matches to a possibly delimited terse Url and nothing else.
See the documentation for `hpath:url-regexp' for match groupings to
use with `string-match'.")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hpath:html-anchor-id-pattern "\\(id\\|name\\)=['\"]%s['\"]?"
  "Regexp matching an html anchor id definition.
Contains a %s for replacement of a specific anchor id.")

(defconst hpath:markdown-anchor-id-pattern "^[ ]*%s: "
  "Regexp matching a Markdown anchor id definition.
Contains a %s for replacement of a specific anchor id.")

(defconst hpath:markdown-section-pattern "^[ \t]*\\(#+\\|\\*+\\)[ \t]+%s\\([ \t[:punct:]]*\\)$"
  "Regexp matching a Markdown section header.
Contains a %s for replacement of a specific section name.")

(defconst hpath:markdown-suffix-regexp "\\.[mM][dD]"
  "Regexp that matches to a Markdown file suffix.")

(defconst hpath:outline-section-pattern "^\\*+[ \t]+%s[ \t]*\\([:punct:]+\\|$\\)"
  "Bol-anchored, no leading spaces regexp matching an Emacs outline section header.
Contains a %s for replacement of a specific section name.")

(defvar hpath:prefix-regexp "\\`[-!&][ ]*"
  "Regexp matching command characters which may precede a pathname.
These are used to indicate how to display or execute the pathname.
  - means evaluate it as Emacs Lisp;
  ! means execute it as a shell script
  & means run it under the current window system.")

(defvar hpath:remote-regexp
  "\\`/[^/:]+:\\|\\`s?ftp[:.]\\|\\`www\\.\\|\\`https?:"
  "Regexp matching remote pathnames and urls which invoke remote file handlers.")

(defconst hpath:shell-modes '(sh-mode csh-mode shell-script:mode)
  "List of modes for editing shell scripts where # is a comment character.")

(defconst hpath:texinfo-section-pattern "^@node+[ \t]+%s[ \t]*\\(,\\|$\\)"
  "Regexp matching a Texinfo section header.
Contains a %s for replacement of a specific section name.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hpath:abbreviate-file-name (path)
  "Same as `abbreviate-file-name' but disables tramp-mode.
This prevents improper processing of hargs with colons in them,
e.g. `actypes::link-to-file'."
  (let (tramp-mode)
    (abbreviate-file-name path)))

(defun hpath:absolute-arguments (actype arg-list &optional default-dirs)
  "Return any paths in ACTYPE's ARG-LIST made absolute.
Uses optional DEFAULT-DIRS (a list of dirs or a single dir) or
`default-directory'.  Other arguments are returned unchanged."
  (let ((param-list (delq nil (mapcar (lambda (param)
					(when param
					  (setq param (symbol-name param))
					  (unless (= ?& (aref param 0))
					    param)))
				      (action:params (actype:action actype))))))
    ;; Extend param-list to length of arg-list in case of any &rest param.
    (setq param-list
	  (nconc param-list
		 (make-list (max 0 (- (length arg-list) (length param-list)))
			    (last param-list))))
    (cl-mapcar (lambda (param arg)
		 (if (and arg
			  (or (string-match-p "file" param)
			      (string-match-p "dir" param)
			      (string-match-p "path" param)))
		     (hpath:absolute-to arg default-dirs)
		   arg))
	       param-list arg-list)))

(defun hpath:absolute-to (path &optional default-dirs)
  "Return PATH as an absolute path relative to one directory.
Search optional DEFAULT-DIRS or `default-directory'.
Return PATH unchanged when it is absolute, a buffer name, not a valid path,
or when DEFAULT-DIRS is invalid.  DEFAULT-DIRS when non-nil may be a single
directory or a list of directories.  The first one in which PATH is found is
used."
  (if (not (stringp path))
      path
    (hpath:call
     (lambda (path non-exist)
       (when (stringp path)
	 (setq path (hpath:trim path)))
       (cond ((not (and (stringp path)
			(not (hypb:object-p path))
			(setq path (hpath:expand path))
			(not (get-buffer path))
			(not (file-name-absolute-p path))
			(hpath:is-p path nil non-exist)))
              path)
             ((not (cond ((null default-dirs)
			  (setq default-dirs (cons default-directory nil)))
			 ((stringp default-dirs)
			  (setq default-dirs (cons default-dirs nil)))
			 ((listp default-dirs))
			 (t nil)))
              path)
             (t
              (let ((rtn) dir)
		(while (and default-dirs (null rtn))
		  (setq dir (expand-file-name
                             (file-name-as-directory (car default-dirs)))
			rtn (expand-file-name path dir)
			default-dirs (cdr default-dirs))
		  (unless (file-exists-p rtn)
		    (setq rtn nil)))
		(or rtn path)))))
     path 'allow-spaces)))

(defun hpath:tramp-file-name-regexp ()
  "Return a regexp for matching to the beginning of a remote file name.
Modifies `tramp-file-name-regexp' by removing bol anchor and
match to empty string if present."
  (let* ((tramp-localname-regexp "[^[:cntrl:]]*\\'")
	 (tramp-regexp (car (if (fboundp 'tramp-file-name-structure)
				(tramp-file-name-structure)
			      tramp-file-name-structure))))
    (replace-regexp-in-string
     "\\\\'" ""
     (cond ((string-match-p "\\\\(\\?:^/\\\\)" tramp-regexp)
            (replace-regexp-in-string  "\\\\(\\?:\\^/\\\\)" "\\(?:/\\)" tramp-regexp nil t))
           (t (substring tramp-regexp 1))))))

(defun hpath:remote-at-p ()
  "Return a remote pathname that point is within or nil.
See the `(emacs)Remote Files' info documentation for pathname format details.
Always returns nil if (hpath:remote-available-p) returns nil."
  (let ((remote-package (hpath:remote-available-p))
	(user (hpath:remote-default-user))
	path
	path-no-site)
    (when remote-package
      (setq path
	    (save-excursion
	      (skip-chars-backward "^[ \t\n\r\f\"`'|\(\{<")
	      (cond
	       ((and (eq remote-package 'tramp)
		     (looking-at (hpath:tramp-file-name-regexp)))
		(match-string-no-properties 0))
	       ((looking-at hpath:url-regexp)
		(if (string-match-p "\\`s?ftp\\'" (match-string-no-properties hpath:protocol-grpn))
		    (concat
		     (format "/%s:" (match-string-no-properties hpath:protocol-grpn))
		     ;; user
		     (if (match-beginning hpath:username-grpn)
			 (match-string-no-properties hpath:username-grpn)
		       (concat user "@"))
		     ;; sitename
		     (hpath:delete-trailer
		      (match-string-no-properties hpath:sitename-grpn))
		     ":"
		     ;; path
		     (if (match-beginning hpath:pathname-grpn)
			 (match-string-no-properties hpath:pathname-grpn)))
		  ;; else ignore this other type of WWW path
		  ))
	       ((or (looking-at hpath:url-regexp2)
		    (looking-at hpath:url-regexp3))
		(if (string-match-p "\\`s?ftp\\'" (match-string-no-properties hpath:hostname-grpn))
		    (concat
		     (format "/%s:" (match-string-no-properties hpath:hostname-grpn))
		     user "@"
		     ;; site
		     (hpath:delete-trailer
		      (match-string-no-properties hpath:sitename-grpn))
		     ":"
		     ;; path
		     (if (match-beginning hpath:pathname-grpn)
			 (match-string-no-properties hpath:pathname-grpn)))
		  ;; else ignore this other type of WWW path
		  ))
	       ;; user, site and path
	       ((looking-at "/?[^/:@ \t\n\r\"`'|]+@[^/:@ \t\n\r\"`'|]+:[^]@ \t\n\r\"`'|\)\}]+")
		(match-string-no-properties 0))
	       ;; @site and path
	       ((looking-at "@[^/:@ \t\n\r\"`'|]+:[^]@:, \t\n\r\"`'|\)\}]+")
		(concat "/" user (match-string-no-properties 0)))
	       ;; site and path
	       ((and (looking-at
		      "/?\\(\\([^/:@ \t\n\r\"`'|]+\\):\\([^]@:, \t\n\r\"`'|\)\}]+\\)\\)[] \t\n\r,.\"`'|\)\}]")
		     (setq path (match-string-no-properties 1) ;; includes site
			   path-no-site (match-string-no-properties 3))
		     (string-match "[^.]\\.[^.]" (match-string-no-properties 2))
		     (not (string-match "\\(\\`\\|:\\)[:0-9]+\\'" path-no-site))) ;; prevent matching to line:col suffixes
		(concat "/" user "@" path))
	       ;; host and path
	       ((and (looking-at "/\\([^/:@ \t\n\r\"`'|]+:[^]@:, \t\n\r\"`'|\)\}]+\\)")
		     (setq path (match-string-no-properties 1)))
		(concat "/" user "@" path)))))
      (hpath:delete-trailer path))))

(defun hpath:remote-p (path)
  "Return a normalized form of PATH if a remote, non-local, pathname, else nil.
See the `(Tramp)Top' Emacs Lisp package manual for remote
pathname format details.  Always returns nil
if (hpath:remote-available-p) returns nil."
  (and (stringp path)
       (let ((remote-package (hpath:remote-available-p))
	     (user (hpath:remote-default-user))
	     result)
	 (setq result
	       (cond
		((null remote-package) nil)
		((eq remote-package 'tramp)
		 (if (tramp-tramp-file-p path) path))
		((string-match hpath:string-url-regexp path)
		 (if (string-match-p "\\`s?ftp\\'" (match-string-no-properties hpath:protocol-grpn path))
		     (concat
		      "/"
		      ;; user
		      (if (match-beginning hpath:username-grpn)
			  (match-string-no-properties hpath:username-grpn path)
			(concat user "@"))
		      ;; site
		      (hpath:delete-trailer
		       (match-string-no-properties hpath:sitename-grpn path))
		      ":"
		      ;; path
		      (if (match-beginning hpath:pathname-grpn)
			  (match-string-no-properties hpath:pathname-grpn path)))
		   ;; else ignore this other type of WWW path
		   ))
		((or (string-match hpath:string-url-regexp2 path)
		     (string-match hpath:string-url-regexp3 path))
		 (if (string-match-p "\\`s?ftp\\'"
				     (match-string-no-properties hpath:hostname-grpn path))
		     
		     (concat
		      "/" user "@"
		      ;; site
		      (hpath:delete-trailer
		       (match-string-no-properties hpath:sitename-grpn path))
		      ":"
		      ;; path
		      (if (match-beginning hpath:pathname-grpn)
			  (match-string-no-properties hpath:pathname-grpn path)))
		   ;; else ignore this other type of WWW path
		   ))
		;; user, site and path
		((string-match "/?[^/:@ \t\n\r\"`'|]+@[^/:@ \t\n\r\"`'|]+:[^]@ \t\n\r\"`'|\)\}]*"
			       path)
		 (match-string-no-properties 0 path))
		;; @site and path
		((string-match "@[^/:@ \t\n\r\"`'|]+:[^]@ \t\n\r\"`'|\)\}]*"
			       path)
		 (concat "/" user (match-string-no-properties 0 path)))
		;; site and path
		((and (string-match
		       "/?\\(\\([^/:@ \t\n\r\"`'|]+\\):[^]@:, \t\n\r\"`'|\)\}]*\\)"
		       path)
		      (setq result (match-string-no-properties 1 path))
		      (string-match "[^.]\\.[^.]"
				    (match-string-no-properties 2 path)))
		 (concat "/" user "@" result))
		;; host and path
		((and (string-match
		       "/\\([^/:@ \t\n\r\"`'|]+:[^]@:, \t\n\r\"`'|\)\}]*\\)"
		       path)
		      (setq result (match-string-no-properties 1 path)))
		 (concat "/" user "@" result))))
	 (hpath:delete-trailer result))))

(defun hpath:at-p (&optional type non-exist)
  "Return delimited path or non-delimited remote path at point, if any.
Path is expanded and normalized.  World-Wide Web urls are ignored
and therefore dealt with by other code.  Delimiters may be:
double quotes, open and close single quote, whitespace, or
Texinfo file references.  If optional TYPE is the symbol \\='file or
\\='directory, then only that path type is accepted as a match.
Only locally reachable paths are checked for existence.  With
optional NON-EXIST, nonexistent local paths are allowed.
Absolute pathnames must begin with a `/' or `~'."
  (let ((path (hpath:delimited-possible-path non-exist))
	subpath)
    (when (and path (not non-exist) (string-match-p hpath:prefix-regexp path))
      (setq non-exist t))
    (cond ((and path (file-readable-p path))
	   path)
	  ((and path
		;; Don't allow more than one set of grouping chars
		(not (string-match-p "\)\\s-*\(\\|\\]\\s-*\\[\\|\}\\s-*\{" path))
		;; With point inside a path variable, return the path that point is on or to the right of.
		(setq subpath (or (and (setq subpath (hargs:delimited "[:\"\']\\|^\\s-*" "[:\"\']\\|\\s-*$" t t nil "[\t\n\r\f]\\|[;:] \\| [;:]"))
				       (not (string-match-p "[:;\t\n\r\f]" subpath))
				       subpath)
				  (and (setq subpath (hargs:delimited "[;\"\']\\|^\\s-*" "[;\"\']\\|\\s-*$"  t t nil "[\t\n\r\f]\\|[;:] \\| [;:]"))
				       (not (string-match-p "[;\t\n\r\f]\\|:[^:]*:" subpath))
				       subpath)))
		;; Handle anchored or action prefix char paths in the
		;; following clause; otherwise, might just be looking
		;; at part of the path
		(and subpath (not (or (string-match-p "#" subpath)
				      (string-match-p hpath:prefix-regexp subpath))))
		(setq subpath
		      (if subpath
			  (cond ((and (string-match "\\`\\s-*\\([^; \t]+\\)" subpath)
				      (executable-find (match-string 1 subpath)))
				 ;; Could be a shell command from a semicolon separated
				 ;; list; ignore if so
				 nil)
				(t (expand-file-name subpath)))
			;; Only default to current path if know are within a PATH value
			(when (string-match-p hpath:path-variable-value-regexp path)
			  ".")))
		(hpath:is-p subpath type non-exist))
	   subpath)
	  ((hpath:is-p path type non-exist))
	  ;; Local file URLs
	  ;; ((hpath:is-p (hargs:delimited "file://" "[ \t\n\r\"\'\}]" nil t)))
	  ((hpath:remote-at-p))
	  ((hpath:www-at-p) nil))))

(defun hpath:call (func path &optional non-exist)
  "Call FUNC with a PATH and optional NON-EXIST flag.
Path is stripped of any prefix operator and suffix location,
NON-EXIST may be either t (path cannot contain whitespace) or
\\='allow-spaces to allow for whitespace.

Return the result of calling FUNC, which must be either nil or the
possibly modified path, but with the prefix and suffix reattached.
Make any existing path within a file buffer absolute before returning."
  (unless (or (functionp func) (subrp func))
    (error "(hpath:call): Invalid function: %s" func))
  (unless (stringp path)
    (error "(%s): '%s' must be a string" func path))
  ;; Convert tabs and newlines to space.
  (setq path (hbut:key-to-label (hbut:label-to-key path)))
  (let* ((expanded-path)
	 (prefix (when (stringp path)
		   (car (delq nil (list (when (string-match hpath:prefix-regexp path)
					  (prog1 (match-string 0 path)
					    (setq path (substring path (match-end 0)))))
					(when (string-match "\\`file://" path)
					  (setq path (substring path (match-end 0)))
					  nil)
					(when (string-match hpath:prefix-regexp path)
					  (prog1 (match-string 0 path)
					    (setq path (substring path (match-end 0))))))))))
	 (suffix (when (stringp path)
		   (apply #'concat (nreverse (list (when (string-match hpath:line-and-column-regexp path)
						     (prog1 (match-string 0 path)
						       (setq path (substring path 0 (match-beginning 0)))))
						   (if (string-match-p hpath:variable-regexp path)
						       ;; Path may be a link reference with a suffix component
						       ;; following a comma or # symbol, so temporarily strip
						       ;; these, if any, before expanding any embedded variables.
						       (when (string-match "[ \t\n\r]*[#,]" path)
							 (prog1 (substring path (1- (match-end 0)))
							   (setq path (substring path 0 (match-beginning 0)))))
						     (when (string-match hpath:markup-link-anchor-regexp path)
						       (prog1 (concat "#" (match-string 3 path))
							 (setq path (substring path 0 (match-beginning 2))))))))))))
    (if (or (null path) (string-empty-p path))
	(setq expanded-path ""
	      path "")
      ;; Never expand paths with a prefix character, e.g. program
      ;; names which need to use exec-directory expansion.
      (setq expanded-path (if prefix (hpath:resolve path) (hpath:expand path))
	    path (funcall func expanded-path non-exist)))
    ;;
    ;; If path is just a local reference that begins with #,
    ;; in a file buffer, prepend the file name to it.  If an HTML
    ;; file, prepend file:// to it.
    (let ((mode-prefix (if (memq major-mode '(js2-mode js-mode js3-mode javascript-mode html-mode web-mode))
			   "file://"
			 "")))
      (if (and path
	       (not (string-empty-p path))
	       ;; If just a numeric suffix like ":40" by itself, ignore
	       ;; it, but if a markdown type suffix alone, like
	       ;; "#section", use it.
	       (and suffix (not (string-empty-p suffix))
		    (= ?# (aref suffix 0))))
	  (progn
	    (setq path (concat prefix path suffix))
	    (cond ((and buffer-file-name
			;; ignore HTML color strings
			(not (string-match "\\`#[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\'" path))
			;; match to in-file #anchor references
			(string-match "\\`#[^\'\"<>#]+\\'" path))
		   (setq path (concat mode-prefix buffer-file-name path)))
		  ((string-match "\\`\\([^#]+\\)\\(#[^#]*\\)\\'" path)
		   ;; file and #anchor reference
		   (setq suffix (match-string 2 path)
			 path (match-string 1 path))
		   (unless (file-name-absolute-p path)
		     ;; make absolute
		     (setq path (hpath:expand path))
		     (unless (string-match-p hpath:variable-regexp path)
		       (setq path (expand-file-name path))))
		   (when (or non-exist (file-exists-p path))
		     (setq path (concat mode-prefix path suffix))))
		  (t
		   (when (or non-exist (file-exists-p path))
		     path))))

	(when (or (and (stringp suffix) (not (string-empty-p suffix))
		       (= ?# (aref suffix 0)))
		  (and (stringp expanded-path)
		       (or non-exist
			   (file-name-absolute-p expanded-path) ;; absolute path
			   (string-match-p hpath:variable-regexp expanded-path) ;; path with var
			   (string-match-p "\\`([^\):]+)" expanded-path)))) ;; Info node
	  (when (or non-exist (file-exists-p expanded-path))
	    (concat prefix mode-prefix expanded-path suffix)))))))

(defun hpath:is-path-variable-p (path-var)
  "Return a colon or semicolon-delimited set in PATH-VAR or nil if not a match."
  (when (stringp path-var)
    (or (getenv path-var)
        (let ((sym (intern-soft path-var)))
	  (and sym (boundp sym) (stringp (symbol-value sym)) (symbol-value sym))))))

(defun hpath:choose-from-path-variable (path-var &optional action-str)
  "Interactively choose and return a path from the set in PATH-VAR.
PATH-VAR is a colon or semicolon-delimited set.  Optional
ACTION-STR is used at the start of a prompt to choose the path."
  (let (path-value
	paths)
    (when (setq path-value (hpath:is-path-variable-p path-var)
		paths (and path-value
			   (split-string path-value (if (string-match ";" path-value) ";" ":") nil "\\s-")))
      (setq paths (sort (mapcar (lambda (path) (if (string-empty-p path) "." path)) paths) #'string-lessp))
      (unless (memq t (mapcar (lambda (mode) (and (fboundp mode) (symbol-value mode))) hpath:auto-completing-read-modes))
	(kbd-key:key-series-to-events "?"))
      (completing-read (format "%s path from ${%s}: "
			       (if (stringp action-str) action-str "Choose")
			       path-var)
		       paths nil t))))

(defun hpath:delimited-possible-path (&optional non-exist include-positions)
  "Return delimited possible path or non-delimited remote path at point, if any.
No validity checking is done on the possible path.  Delimiters
may be: double quotes, open and close single quote, whitespace,
or Texinfo file references.

With optional NON-EXIST, nonexistent local paths are allowed.
Absolute pathnames must begin with a `/' or `~'.

With optional INCLUDE-POSITIONS, return a triplet list of (path start-pos
end-pos) or nil."
  ;; Prevents MSWindows to Posix path substitution
  (let ((hyperb:microsoft-os-p t))
    (or (hargs:delimited "file://" "\\s-" nil t include-positions)
	(hargs:delimited "\"" "\"" nil nil include-positions "[`'’]")
	;; Filenames in Info docs or Python files
	(hargs:delimited "[`'‘]" "[`'’]" t t include-positions "\"")
	;; Filenames in TexInfo docs
	(hargs:delimited "@file{" "}" nil nil include-positions)
	;; Any existing whitespace delimited filename at point.
	;; If match consists of only punctuation, like
	;; . or .., don't treat it as a pathname.  Only look for
	;; whitespace delimited filenames if non-exist is nil.
	(unless non-exist
	  (let* ((triplet (hargs:delimited "^\\|\\(\\s-\\|[\]\[()<>\;&,@]\\)+"
					   "\\([\]\[()<>\;&,@]\\|:*\\s-\\)+\\|$"
					   t t t))
		 (p (car triplet))
		 (punc (char-syntax ?.)))
	    ;; May have matched to a string with an embedded double
	    ;; quote or surrounded by braces; if so, don't consider it a path.
            ;; Also ignore whitespace delimited root dirs, e.g. " / ".
	    (when (and (stringp p) (not (string-match-p "\\`{.*}\\'\\|\"\\|\\`[/\\]+\\'" p))
		       (delq nil (mapcar (lambda (c) (/= punc (char-syntax c))) p)))
	      ;; Prepend proper directory from cd, ls *, recursive ls or dir file
	      ;; listings when needed.
	      (setq p (or (hpath:prepend-shell-directory p) p))
	      (setcar triplet p)
	      (if include-positions
		  triplet
		p)))))))

;;;###autoload
(defun hpath:display-buffer (buffer &optional display-where)
  "Display and select BUFFER at optional DISPLAY-WHERE or at `hpath:display-where'.
BUFFER must be a buffer or a buffer name.

See the documentation of `hpath:display-buffer-alist' for valid
values of DISPLAY-WHERE.  Return the window in which the buffer
is displayed or nil if not displayed because BUFFER is invalid."
  (interactive "bDisplay buffer: ")
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (when buffer
    ;; RSW 4/30/2016 - Commented out in case interferes with Smart Key
    ;; selection and yanking of the region via drags.
    ;; (hpath:push-tag-mark)
    (funcall (hpath:display-buffer-function display-where) buffer)
    (selected-window)))

(defun hpath:display-buffer-other-frame (buffer)
  "Display and select BUFFER, in another frame.
BUFFER must be a buffer or a buffer name.

May create a new frame, or reuse an existing one.  See the
documentation of `hpath:display-buffer' for details.  Return the
window in which the buffer is displayed."
  (interactive "bDisplay buffer in other frame: ")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  ;; BW 4/30/2016 - Commented out in case interferes with Smart Key
  ;; selection and yanking or the region via drags.
  ;; (hpath:push-tag-mark)
  (if (= (length (frame-list)) 1)
      (select-frame (make-frame))
    (other-frame 1))
  (if (br-in-browser)
      (br-to-view-window))
  (switch-to-buffer buffer)
  (selected-window))

(defun hpath:display-buffer-function (&optional display-where)
  "Return the function to display a Hyperbole buffer.
Optionally use symbol DISPLAY-WHERE or `hpath:display-where'."
  (hpath:display-where-function display-where hpath:display-buffer-alist))

(defun hpath:display-path-function (&optional display-where)
  "Return the function to display a Hyperbole path.
Optionally use symbol DISPLAY-WHERE or `hpath:display-where'."
  (hpath:display-where-function display-where hpath:display-where-alist))

(defun hpath:resolve (path)
  "Resolve variables in PATH or prepend path from `hpath:auto-variable-alist'.
Path variable are prepended from the first PATH matching regexp
in `hpath:auto-variable-alist'.  Return any absolute or invalid
PATH unchanged."
  (when (stringp path)
    (unless (string-match-p hpath:variable-regexp path)
      (setq path (substitute-in-file-name path)))
    (let (variable-path
	  substituted-path)
      (setq variable-path (hpath:expand-with-variable path)
	    substituted-path (hpath:substitute-value variable-path))
      (cond ((or (null substituted-path) (string-empty-p substituted-path))
	     path)
	    ((and (string-match-p hpath:variable-regexp variable-path)
		  (string-match-p hpath:variable-regexp substituted-path))
	     ;; If a path is invalid, then a variable may have been prepended but
	     ;; it will remain unresolved in `substituted-path', in which case we
	     ;; want to return `path' without any further changes.
	     path)
	    (t substituted-path)))))

(defun hpath:expand (path)
  "Expand relative PATH using match in `hpath:auto-variable-alist'.
The path variable is expanded from the first file matching regexp
in `hpath:auto-variable-alist'.  Return any absolute or invalid
PATH unchanged."
  (when (stringp path)
    (unless (string-match-p hpath:variable-regexp path)
      (setq path (substitute-in-file-name path)))
    (let (variable-path
	  substituted-path)
      (setq variable-path (hpath:expand-with-variable path)
	    substituted-path (hpath:substitute-value variable-path))
      (cond ((or (null substituted-path) (string-empty-p substituted-path))
	     path)
	    ((and (string-match-p hpath:variable-regexp variable-path)
		  (string-match-p hpath:variable-regexp substituted-path))
	     ;; If a path is invalid, then a variable may have been prepended but
	     ;; it will remain unresolved in `substituted-path', in which case we
	     ;; want to return `path' without any further changes.
	     path)
	    ;; For compressed Elisp libraries, add any found compressed suffix to the path.
	    ((string-match-p "\\.el\\(\\.\\|\\'\\)" substituted-path)
	     (or (locate-library substituted-path t) path))
	    ((or (string-match-p "\\`\\(#\\|([^\)\\/]+)\\|[^.\\/].*\\.[^.\\/]\\)" substituted-path)
		 (string-match-p "[\\/~]" substituted-path))
	     ;; Don't expand if an Info path, URL, #anchor or has a directory prefix
	     substituted-path)
	    (t (expand-file-name substituted-path))))))

(defun hpath:prepend-shell-directory (&optional filename)
  "Prepend subdir to a filename in an \\='ls'-file listing.
When in a shell buffer and on a filename result of an \\='ls *' or
recursive \\='ls -R' or \\='dir' command, prepend the subdir to the
filename at point, or optional FILENAME, when needed and return
it, else return nil."
  (when (derived-mode-p #'shell-mode)
    (let ((prior-prompt-pos (save-excursion (comint-previous-prompt 1) (1- (point))))
	  dir)
      (unless (stringp filename)
	(setq filename (thing-at-point 'filename t)))
      (save-excursion
	(when (and filename
		   (if (memq system-type '(windows-nt cygwin ms-dos))
		       ;; Windows Cmd or PowerShell dir cmds
		       (and (re-search-backward "^\\s-*\\(cd \\|pushd \\|Directory: \\|Directory of \\)\\(.+\\)$" prior-prompt-pos t)
			    (setq dir (match-string-no-properties 2)))
		     ;; POSIX
		     (or (and (re-search-backward "^$\\|\\`\\|^\\(.+\\):$" prior-prompt-pos t)
			      (setq dir (match-string-no-properties 1)))
			 (and (re-search-backward "\\(^\\| \\)\\(cd\\|pushd\\|ls\\)\\(\\s-+-[-a-zA-Z0-9]*\\)*\\s-+[\'\"]?\\([^&!;,\'\"\t\n\r\f]+[^&!;,\'\" \t\n\r\f]\\)[\'\"]?" prior-prompt-pos t)
			      (setq dir (match-string-no-properties 4)))))
		   (and dir (not (string-empty-p dir))))
	  (unless (file-name-absolute-p filename)
	    ;; If dir ends with a glob expression, then the dir is
	    ;; already prepended to each file listing, the file name should simply be
	    ;; expanded; otherwise, prepend the dir.
	    (if (and dir (string-match-p "\\*[^\\/\n\r]*$" dir))
		(expand-file-name filename)
	      (when (file-directory-p dir)
		(setq dir (file-name-as-directory dir)))
	      (when (and dir (not (string-empty-p dir)) (file-exists-p dir))
		(expand-file-name filename dir)))))))))

(defvar hpath:compressed-suffix-regexp (concat (regexp-opt '(".gz" ".Z" ".zip" ".bz2" ".xz" ".zst")) "\\'")
   "Regexp of compressed file name suffixes.")

(defun hpath:expand-with-variable (path)
  "Prepend to relative PATH the ${load var name} from `hpath:auto-variable-alist'.
When PATH is relative, prepend to it the first file matching regexp in
`hpath:auto-variable-alist' sans any compression suffix in
`hpath:compressed-suffix-regexp'.
If PATH is absolute, return it unchanged."
  (when (stringp path)
    (let ((auto-variable-alist hpath:auto-variable-alist)
	  (compression-suffix (when (string-match hpath:compressed-suffix-regexp path)
				(prog1 (match-string 0 path)
				  (setq path (substring path 0 (match-beginning 0))))))
	  regexp
	  variable
	  variable-name)
      (unless (or (file-name-absolute-p path)
		  (hpath:url-p path)
		  (string-match-p hpath:variable-regexp path))
	(while auto-variable-alist
	  (setq regexp (caar auto-variable-alist)
		variable (cdar auto-variable-alist)
		auto-variable-alist (cdr auto-variable-alist)
		variable-name (if (and variable (symbolp variable))
				  (symbol-name variable)
				variable))
	  (when (and path variable (string-match regexp path))
	    (when (and (not (string-match (regexp-quote variable-name) path))
		       (or (and (stringp variable) (getenv variable))
			   (and (symbolp variable) (boundp variable))))
	      (when (string-match "\\`\\.[\\/]" path)
		(setq path (substring path (match-end 0))))
	      (setq path (format "${%s}/%s" variable path)))
	    (setq auto-variable-alist nil))))
      (concat path compression-suffix))))

(defun hpath:file-line-and-column (path-line-and-col)
  "Return list of parts from PATH-LINE-AND-COL string of format path:line:col.
Parse out the parts and return a list, else nil."
  (when (and (stringp path-line-and-col)
	     (string-match hpath:section-line-and-column-regexp path-line-and-col))
    ;; Ensure any variables and heading suffixes following [#,] are removed before returning file.
    (let ((file (save-match-data (hpath:expand (match-string-no-properties 1 path-line-and-col))))
	  (line-num (string-to-number (match-string-no-properties 3 path-line-and-col)))
	  (col-num (when (match-end 4)
		     (string-to-number (match-string-no-properties 5 path-line-and-col)))))
      (when (and (save-match-data (setq file (hpath:is-p file)))
		 file)
	(if line-num
	    (if col-num
		(list file line-num col-num)
	      (list file line-num))
	  (list file))))))

(defun hpath:find-noselect (filename)
  "Find but don't display FILENAME.
Use user customizable settings of display program and location.
Return the current buffer iff FILENAME is displayed within a
buffer (not with an external program), else nil.

See `hpath:find' documentation for acceptable formats of FILENAME."
  (hpath:find filename nil t))

(defun hpath:find (pathname &optional display-where noselect)
  "Edit PATHNAME using user customizable settings of display program and location.
Return the current buffer iff file is read into a buffer (not displayed with
an external program), else nil.

PATHNAME may contain references to Emacs Lisp variables or shell
environment variables using the syntax, \"${variable-name}\".

PATHNAME may start with a special prefix character that is handled as follows:
  !pathname  - execute as a non-windowed program within a shell;
  &pathname  - execute as a windowed program;
  -pathname  - load as an Emacs Lisp program.

If PATHNAME does not start with a prefix character:

  it may be followed by a hash-style link reference to HTML, XML,
  SGML, shell script comment, Markdown or Emacs outline headings
  of the form, <file>#<anchor-name>, e.g. \"~/.bashrc#Alias
  Section\";

  it may end with a line number and optional column number to which to go,
  of the form, :<line-number>[:<column-number>], e.g. \"~/.bashrc:20:5\";
  normally, this is an absolute line number (disabling buffer restriction),
  but if preceded by a hash-style link reference, it is relative to the
  location of the link anchor;

  if it matches a regular expression in the alist returned by
  (hpath:get-external-display-alist), invoke the associated external
  display program

  if not, consult `hpath:internal-display-alist' for a specialized internal
  display function to use;

  if no matches are found there, consult `hpath:display-where-alist'
  using the optional second argument, DISPLAY-WHERE (a symbol);

  if that is nil, consult the value of `hpath:display-where', and use the
  matching display function.

Optional third argument, NOSELECT, means simply find the file and return its
buffer but don't display it."
  (interactive "FFind file: ")
  (unless (stringp pathname)
    (error "(hpath:find): pathname arg must be a string, not, %S" pathname))
  (let ((case-fold-search t)
	(default-directory default-directory)
	modifier loc anchor hash path line-num col-num)
    (setq loc (hattr:get 'hbut:current 'loc)
	  default-directory (or (hattr:get 'hbut:current 'dir)
				;; Loc may be a buffer without a file
				(if (stringp loc)
				    (file-name-directory loc)
				  default-directory)))
    (when (string-match hpath:prefix-regexp pathname)
      (setq modifier (aref pathname 0)
	    pathname (substring pathname (match-end 0))))
    (setq path pathname) ;; default
    (when (string-match hpath:line-and-column-regexp path)
      (setq line-num (string-to-number (match-string 1 path))
	    col-num (when (match-string 3 path)
		      (string-to-number (match-string 3 path)))
	    path (substring path 0 (match-beginning 0))))
    (when (string-match hpath:markup-link-anchor-regexp path)
      (setq hash t
	    anchor (match-string 3 path)
	    path (if (match-end 1)
		     (substring path 0 (match-end 1))
		   (or buffer-file-name "")))
      ;; 'anchor' may improproperly include trailing punctuation;
      ;; remove it if so.
      (when (string-match "\\s.+\\'" anchor)
	(setq anchor (substring anchor 0 (match-beginning 0)))))
    (if (string-empty-p path)
	(setq path ""
	      pathname "")
      ;; Never expand pathnames with modifier prepended.
      (if modifier
	  (setq path (hpath:resolve path))
	(setq path (hpath:expand path)
	      pathname (hpath:absolute-to path default-directory))))
    (let ((remote-pathname (hpath:remote-p path)))
      (or modifier remote-pathname
	  (file-exists-p pathname)
	  (error "(hpath:find): \"%s\" does not exist"
		 (concat modifier pathname (when hash "#") anchor)))
      (or modifier remote-pathname
	  (file-readable-p pathname)
	  (error "(hpath:find): \"%s\" is not readable"
		 (concat modifier pathname (when hash "#") anchor)))
      (if noselect
	  (let ((buf (find-file-noselect pathname)))
	    (with-current-buffer buf
	      (when (or hash anchor) (hpath:to-markup-anchor hash anchor))
	      buf))
	;; If pathname is a remote file (not a directory), we have to copy it to
	;; a temporary local file and then display that.
	(when (and remote-pathname (not (file-directory-p remote-pathname)))
	  (copy-file remote-pathname
		     (setq path (concat hpath:tmp-prefix
					(file-name-nondirectory remote-pathname)))
		     t t)
	  (setq pathname (cond (anchor (concat remote-pathname "#" anchor))
			       (hash   (concat remote-pathname "#"))
			       (t path))))))
    (cond (modifier (cond ((= modifier ?!)
			   (hact 'exec-shell-cmd pathname))
			  ((= modifier ?&)
			   (hact 'exec-window-cmd pathname))
			  ((= modifier ?-)
			   (hact 'load pathname)))
		    nil)

	  ;; If no path, e.g. just an anchor link in a non-file buffer,
	  ;; then must display within Emacs, ignoring any external programs.
	  ((string-empty-p path)
	   (hpath:display-buffer (current-buffer) display-where)
	   (when (or hash anchor)
	     (hpath:to-markup-anchor hash anchor))
	   (when line-num
	     ;; With an anchor, goto line relative to anchor
	     ;; location, otherwise use absolute line number
	     ;; within the visible buffer portion.
	     (if (or hash anchor)
		 (forward-line line-num)
	       (hpath:to-line line-num)))
	   (when col-num (move-to-column col-num))
	   (current-buffer))

	  ;; Display paths either internally or externally.
	  (t (let ((display-executables (hpath:find-program path))
		   executable)
	       (cond ((stringp display-executables)
		      (hact 'exec-window-cmd
			    (hpath:command-string display-executables pathname))
		      nil)
		     ((functionp display-executables)
		      (funcall display-executables pathname)
		      (current-buffer))
		     ((and (listp display-executables) display-executables)
		      (setq executable (hpath:find-executable display-executables))
		      (if executable
			  (hact 'exec-window-cmd
				(hpath:command-string executable pathname))
			(error "(hpath:find): No available executable from: %s"
			       display-executables)))
		     (t (setq path (hpath:validate path))
			(funcall (hpath:display-path-function display-where) path)
			;; Perform a loose test that the current buffer
			;; file name matches the path file name since exact
			;; matching of path is likely to be wrong in
			;; certain cases, e.g. with mount point or os path
			;; alterations.
			(when (and buffer-file-name
				   (equal (file-name-nondirectory path)
					  (file-name-nondirectory buffer-file-name)))
			  (when (or hash anchor)
			    (hpath:to-markup-anchor hash anchor))
			  (when line-num
			    ;; With an anchor, goto line relative to anchor
			    ;; location, otherwise use absolute line number
			    ;; within the visible buffer portion.
			    (if (or hash anchor)
				(forward-line line-num)
			      (hpath:to-line line-num)))
			  (when col-num (move-to-column col-num))
			  (current-buffer)))))))))

(defun hpath:to-markup-anchor (hash anchor)
  "Move point to ANCHOR if found or, if null, to the beginning of the buffer."
  (cond ((and (stringp anchor) (not (equal anchor "")))
	 (cond ((memq major-mode hui-select-markup-modes)
		;; In HTML-like mode where link ids are case-sensitive.
		(let ((opoint (point))
		      (case-fold-search))
		  (goto-char (point-min))
		  (if (re-search-forward (format hpath:html-anchor-id-pattern (regexp-quote anchor)) nil t)
		      (progn (forward-line 0)
			     (when (eq (current-buffer) (window-buffer))
			       (recenter 0)))
		    (goto-char opoint)
		    (error "(hpath:to-markup-anchor): %s - Anchor `%s' not found in the visible buffer portion"
			   (buffer-name)
			   anchor))))
	       (t
		(let* ((opoint (point))
		       (prog-mode (derived-mode-p 'prog-mode))
		       ;; Markdown or outline link ids are case
		       ;; insensitive and - characters are converted to
		       ;; spaces at the point of definition unless
		       ;; anchor contains both - and space characters,
		       ;; then no conversion occurs.
		       (case-fold-search (not prog-mode))
		       (anchor-name (if (or prog-mode
					    (string-match-p "-.* \\| .*-" anchor))
					anchor
				      (subst-char-in-string ?- ?\  anchor)))
		       (referent-regexp (format
					 (cond ((or (derived-mode-p 'outline-mode) ;; Includes Org mode
						    ;; Treat all caps filenames without suffix like outlines, e.g. README, INSTALL.
						    (and buffer-file-name
							 (string-match-p "\\`[A-Z][A-Z0-9]+\\'" buffer-file-name)))
						hpath:outline-section-pattern)
					       (prog-mode
						"%s")
					       ((or (and buffer-file-name
							 (string-match-p hpath:markdown-suffix-regexp buffer-file-name))
						    (memq major-mode hpath:shell-modes))
						hpath:markdown-section-pattern)
					       ((derived-mode-p 'texinfo-mode)
						hpath:texinfo-section-pattern)
					       ((derived-mode-p 'text-mode)
						"%s")
					       (t hpath:outline-section-pattern))
					 (regexp-quote anchor-name)))
		       (referent-leading-spaces-regexp
			(when (and (not (string-empty-p referent-regexp))
				   (= (aref referent-regexp 0) ?^))
			  (concat "^[ \t]+" (substring referent-regexp 1)))))
		  (goto-char (point-min))
		  (if (or (re-search-forward referent-regexp nil t)
			  (and referent-leading-spaces-regexp
			       (re-search-forward referent-leading-spaces-regexp nil t)))
		      (progn (forward-line 0)
			     (when (eq (current-buffer) (window-buffer))
			       (recenter 0)))
		    (goto-char opoint)
		    (error "(hpath:to-markup-anchor): %s - Section `%s' not found in the visible buffer portion"
			   (buffer-name)
			   anchor-name))))))
	(hash (goto-char (point-min)))))

(defun hpath:find-executable (executable-list)
  "Return first executable string from EXECUTABLE-LIST found in `exec-path'.
Return nil if none are found."
  (catch 'found
    (mapc
     (lambda (executable)
       (if (stringp executable)
	   ;; Match only to files with execute permission.
	   (if (locate-file executable exec-path nil #'file-executable-p)
	       (throw 'found executable))
	 (error "(hpath:find-executable): Non-string entry, %s"
		executable-list)))
     executable-list)
    nil))

(defun hpath:find-line (filename line-num &optional display-where)
  "Edit file FILENAME with point placed at LINE-NUM.

`hpath:display-where-alist' is consulted using the optional
argument, DISPLAY-WHERE (a symbol) or if that is nil, the value
of `hpath:display-where', and the matching display function is
used to determine where to display the file, e.g. in another
frame.  Always return t."
  (interactive "FFind file: ")
  ;; Just delete any special Hyperbole command characters preceding
  ;; the filename, ignoring them.
  (if (string-match hpath:prefix-regexp filename)
      (setq filename (substring filename (match-end 0))))
  (hpath:find
   (if (integerp line-num)
       (concat filename ":" (int-to-string line-num))
     filename)
   display-where)
  t)

(defun hpath:find-other-frame (filename)
  "Edit file FILENAME in another frame.
May create a new frame or reuse an existing one.
See documentation of `hpath:find' for details.
Return the buffer of displayed file."
  (interactive "FFind file in other frame: ")
  (if (= (length (frame-list)) 1)
      (if (fboundp 'id-create-frame)
	  (id-create-frame)
	(select-frame (make-frame)))
    (other-frame 1))
  (if (br-in-browser)
      (br-to-view-window))
  (find-file filename))

(defun hpath:find-other-window (filename)
  "Edit file FILENAME, in another window or using an external program.
May create a new window, or reuse an existing one; see the
function `display-buffer'.  See documentation of `hpath:find' for
details.  Returns non-nil iff file is displayed within a buffer."
  (interactive "FFind file in other window: ")
  (hpath:find filename 'other-window))

;; (hyperb:window-system) function from "hversion.el" must be defined
;; prior to use of this function.
(defun hpath:get-external-display-alist ()
  "Return an alist of elements for the current window system.
The alist is (FILENAME-REGEXP . DISPLAY-PROGRAM-STRING-OR-LIST).
These are used to display matching filenames with external window system
programs, such as a pdf reader.  The cdr of each element may be:
  a string which must represent either an executable name or a shell command
  with an embedded %s for substitution of the current context filename;
  a list of executable names \(the first valid one is used);
  or a function of one filename argument.
See also `hpath:internal-display-alist' for internal,
`window-system' independent display settings."
  (cond ((memq window-system '(dps ns))
	 hpath:external-display-alist-macos)
	(hyperb:microsoft-os-p
	 hpath:external-display-alist-mswindows)
	(t (cdr (assoc (hyperb:window-system)
		       (list (cons "emacs" hpath:external-display-alist-x) ; GNU Emacs under X
			     (cons "next" hpath:external-display-alist-macos)))))))

(defun hpath:is-p (path &optional type non-exist)
  "Return normalized PATH if PATH is a Posix or MSWindows path, else nil.
If optional TYPE is the symbol \\='file or \\='directory, then only that path type
is accepted as a match.  The existence of the path is checked only for
locally reachable paths (Info paths are not checked).

Single spaces are permitted in the middle of existing pathnames, but not at
the start or end.  With optional NON-EXIST equal to t, nonexistent local
paths without spaces are allowed.  Set NON-EXIST to \\='allow-spaces to allow
spaces in non-existent paths.

Before the pathname is checked for existence, sequences of tabs and newlines
are converted to a single space, `hpath:prefix-regexp' matches at the start
are temporarily stripped, \"file://\" prefixes are stripped, link anchors at
the end following a # or , character are temporarily stripped, and path
variables are expanded with `hpath:substitute-value'.  This normalized path
form is what is returned for PATH."
  (when (and (stringp path) (not (string-match-p hpath:path-variable-value-regexp path))
	     ;; If a single character in length, must be a word or symbol character
	     (or (/= (length path) 1) (and (string-match-p "\\sw\\|\\s_" path)
					   (not (string-match-p "[@#&!*]" path)))))
    (setq path (hpath:mswindows-to-posix path))
    (unless (or (string-match-p "\\`[.~/]\\'" path)
		(file-readable-p path))
      (setq path (hpath:call
		  (lambda (path non-exist)
		    (let ((modifier nil)
			  suffix)
		      (and (not (or (string-equal path "")
				    (string-match-p "\\`\\s-\\|\\s-\\'" path)))
			   (or (not (string-match-p "[()]" path))
			       (string-match-p "\\`([^ \t\n\r\)]+)[ *A-Za-z0-9]" path))
			   ;; Allow for @{ and @} in texinfo-mode
			   (or (when (string-match-p hpath:variable-regexp path)
				 ;; Path may be a link reference with embedded
				 ;; variables that must be expanded.
				 ;; A variable may be a PATH-type variable
				 ;; where the first matching expansion
				 ;; that matches the full path must be utilized.
				 (setq path (hpath:substitute-value path)
				       non-exist t)) ;; Ensure non-existent path links handled as pathnames.
			       t)
			   (not (string-match-p "[\t\n\r\"`'|{}\\]" path))
			   (let ((rtn-path (concat path "%s")))
			     (and (or (not (hpath:www-p path))
				      (string-match-p "\\`s?ftp[:.]" path))
				  (let ((remote-path (string-match-p "\\(@.+:\\|^/.+:\\|..+:/\\).*[^:0-9/]" path)))
				    (when (cond (remote-path
						 (cond ((eq type 'file)
							(not (string-equal "/" (substring path -1))))
						       ((eq type 'directory)
							(string-equal "/" (substring path -1)))
						       (t)))
						((or (and non-exist
							  (or
							   ;; Info or remote path, so don't check for.
							   (string-match-p "[()]" path)
							   (hpath:remote-p path)
							   (setq suffix (hpath:exists-p path t))
							   ;; Don't allow spaces in non-existent pathnames
							   ;; unless 'non-exist' equals 'allow-spaces.
							   (eq non-exist 'allow-spaces)
							   (not (string-match-p "\\s-" path))))
						     (setq suffix (hpath:exists-p path t)))
						 (cond ((eq type 'file)
							(not (file-directory-p path)))
						       ((eq type 'directory)
							(file-directory-p path))
						       (t))))
				      ;; Might be an encoded URL with % characters, so
				      ;; decode it before calling format below.
				      (when (string-match-p "%" rtn-path)
					(let (decoded-path)
					  (while (not (equal rtn-path (setq decoded-path (hypb:decode-url rtn-path))))
					    (setq rtn-path decoded-path))))
				      ;; Quote any % except for one %s at the end of the
				      ;; path part of rtn-path (immediately preceding a #
				      ;; or , character or the end of string).
				      (setq rtn-path (replace-regexp-in-string "%" "%%" rtn-path t nil)
					    rtn-path (replace-regexp-in-string "%%s\\([#,]\\|\\'\\)" "%s\\1" rtn-path t nil))
				      ;; Return path if non-nil return value.
				      (if (stringp suffix) ;; suffix could = t, which we ignore
					  (if (string-match (concat (regexp-quote suffix) "%s") rtn-path)
					      ;; remove suffix
					      (concat (substring rtn-path 0 (match-beginning 0))
						      (substring rtn-path (match-end 0)))
					    ;; add suffix
					    (concat modifier (format rtn-path suffix)))
					(concat modifier (format rtn-path ""))))))))))
		  path non-exist)))
     (unless (or (null path)
		 (string-empty-p path)
		 (string-match-p "#['`\"]" path)
		 ;; If a single character in length, must be a word or
		 ;; symbol character other than [.~ /].
		 (and (= (length path) 1)
		      (not (string-match-p "\\`[.~/]\\'" path))
		      (or (not (string-match-p "\\sw\\|\\s_" path))
			  (string-match-p "[@#&!*]" path))))
       path)))

(defun hpath:push-tag-mark ()
  "Add a tag return marker at point if within a programming language file buffer.
Is a no-op if the function `push-tag-mark' is not available."
  (and buffer-file-name
       comment-start
       (not (memq last-command
		  '(xref-find-definitions find-tag find-tag-other-window tags-loop-continue)))
       (or (and (fboundp 'xref-push-marker-stack)
		;; push old position
		(xref-push-marker-stack))
	   (and (fboundp 'push-tag-mark)
		;; push old position
		(push-tag-mark)))))

(defun hpath:relative-arguments (arg-list)
  "Return any paths in ARG-LIST below button source loc directory made relative.
Other paths are simply expanded.  Non-path arguments are returned unchanged."
  (let ((loc (hattr:get 'hbut:current 'loc)))
    (mapcar (lambda (arg)
	      (hpath:relative-to arg (if (stringp loc)
					 (file-name-directory loc)
				       (buffer-local-value 'default-directory loc))))
	    arg-list)))

(defun hpath:relative-to (path &optional default-dir)
  "Return PATH relative to optional DEFAULT-DIR or `default-directory'.
Expand any other valid path.  Return PATH unchanged when it is not a
valid path."
  (cond ((not (and (stringp path)
		   (not (hypb:object-p path))))
	 path)
	((and (setq path (hpath:trim path))
	      (not (hpath:is-p path)))
	 path)
	(t
	 (setq default-dir
	       (expand-file-name
		(file-name-as-directory (or default-dir default-directory)))
	       path (expand-file-name path))
	 (and path default-dir
	      (if (string-equal "/" default-dir)
		  path
		(let ((end-dir (min (length path) (length default-dir))))
		  (cond
		   ((string-equal (substring path 0 end-dir) default-dir)
		    (concat "./" (substring path end-dir)))
		   ((progn (setq default-dir (file-name-directory (directory-file-name default-dir))
				 end-dir (min (length path) (length default-dir)))
			   (string-equal (substring path 0 end-dir) default-dir))
		    (concat "../" (substring path end-dir)))
		   ((progn (setq default-dir (file-name-directory (directory-file-name default-dir))
				 end-dir (min (length path) (length default-dir)))
			   (string-equal (substring path 0 end-dir) default-dir))
		    (concat "../../" (substring path end-dir)))
		   (t path))))))))

(defun hpath:rfc (rfc-num)
  "Return pathname to textual rfc document indexed by RFC-NUM.
See the documentation of the `hpath:rfc' variable."
  (format hpath:rfc rfc-num))

(defun hpath:start-end (path)
  "Return a list of start and end positions in PATH (sans delimiters), else nil.
Point must be within the first line of PATH.
NOTE: This will return nil if the path has been normalized in any way
\(adjusted for mount points or variables replaced) since the
in-buffer path will not match."
  ;; Create a regexp from path by regexp-quoting it and then matching spaces
  ;; to any whitespace.
  (when (stringp path)
    (let ((path-regexp (replace-regexp-in-string "[ \t\n\r\f]+" "[ \t\n\r\f]" (regexp-quote path) t t))
	  (opoint (point))
	  found
	  search-end-point
	  start
	  end)
      ;; Save point, move to bol and search for regexp match to a max of 5 lines
      ;; just to limit searches in large buffers.
      (save-excursion
	(forward-line 0)
	(setq search-end-point (save-excursion (forward-line 5) (point)))
	(while (and (not found)
		    (re-search-forward path-regexp search-end-point t))
		 ;; If match found, ensure that start pos is <= orig point and end pos >
		 ;; orig point and return (start . end).
		 (setq start (match-beginning 0) end (match-end 0)
		       found (and (<= start opoint) (>= end opoint)))))
      (when found
	(list start end)))))

(defun hpath:return-one-value (path &optional return-path-flag)
  "Return the value of one variable substitution in PATH.
With optional RETURN-PATH-FLAG non-nil, return the whole path,
expanded and with the variable value substituted.

The caller must have run `string-match' over `path' immediately
prior to calling this function."
  (unless (match-data)
    (error "(hpath:return-one-value): Caller failed to run (string-match hpath:variable-regexp path) before calling this"))
  (let* ((var-group (match-string 0 path))
	 (var-ext (match-string 1 path))
	 (path-prefix (substring path 0 (match-beginning 0)))
	 (rest-of-path (substring path (match-end 0)))
	 (var-name (if (= ?@ (aref var-ext (1- (length var-ext))))
		       (substring var-ext 0 -1)
		     var-ext))
	 (trailing-dir-sep-flag (and (not (string-empty-p rest-of-path))
				     (car (member (char-to-string (aref rest-of-path 0))
						  '("/" "\\")))))
	 (sym (intern-soft var-name)))
    (when (file-name-absolute-p rest-of-path)
      (setq rest-of-path (substring rest-of-path 1)))
    (if (or (and sym (boundp sym)) (getenv var-name))
	;; directory-file-name or hpath:substitute-dir may trigger
	;; an error but this may be called when testing for
	;; implicit button matches where no error should occur, so
	;; catch the error and ignore variable expansion in such a
	;; case.
	;; -- RSW, 08-26-2019
	;; Removed errors on non-existent paths.
	;; -- RSW, 04-19-2021
	(condition-case nil
	    (funcall (if trailing-dir-sep-flag #'directory-file-name #'identity)
		     (hpath:substitute-dir path-prefix var-name rest-of-path
					   trailing-dir-sep-flag return-path-flag))
	  (error ""))
      var-group)))

(defun hpath:substitute-value (path)
  "Substitute values for Emacs Lisp variables and environment variables in PATH.
Return the resulting PATH.
Format of path-type variables must be \"${variable-name}\"; other,
single-valued variables may be given as \"$variable-name\"."

;; Algorithm
;;  1. Extract all ${} and $ vars into a reverse order list and save start and end points.
;;  2. Get value of each var.
;;  3. For each string var not containing [:;], replace its var-name with var value.
;;  4. Run hpath:return-one-value on this reversed list.
  (let ((start 0)
	(new-path (copy-sequence path))
	multi-dir-vars
	var-name
	var-start
	var-end
	var-value
	result)
    (while (and (< start (length new-path))
		(string-match hpath:variable-regexp new-path start))
      (setq var-name (match-string 1 new-path)
	    var-start (match-beginning 0)
	    var-end (match-end 0)
	    var-value (save-match-data (hpath:get-single-string-variable-value var-name)))
      (if var-value
	  (setq new-path (hpath:return-one-value new-path t)
		start (+ var-start (length var-value)))
	(setq multi-dir-vars (cons var-name multi-dir-vars)
	      start var-end)))
    (while multi-dir-vars
      (setq result nil
	    var-name (car multi-dir-vars)
	    multi-dir-vars (cdr multi-dir-vars))
      (while (and (not result)
		  (string-match hpath:variable-regexp new-path))
	;; Match multi-dir-vars in reverse order so can
	;; match each var to a path without other variables.
	(when (string-equal (match-string 1 new-path)
			    var-name)
	  (setq result (hpath:return-one-value new-path t)
		new-path (if (string-empty-p result) new-path result)))))
    new-path))

(defun hpath:substitute-var (path)
  "Replace up to one match in PATH with the first variable from `hpath:variables'.
The variable whose value contains a string match to PATH is
replaced.  After any match, the resulting path contains a
variable reference like ${variable}."
  (if (not (and (stringp path) (string-match "/" path) (hpath:is-p path)))
      path
    (setq path (hpath:symlink-referent path))
    (let ((new-path)
	  (vars hpath:variables)
	  result var val)
      (while (and vars (null new-path))
	(setq var (car vars) vars (cdr vars))
	(when (boundp var)
	  (setq val (symbol-value var))
	  (cond ((stringp val)
		 (if (setq result
			   (hpath:substitute-var-name var val path))
		     (setq new-path result)))
		((null val))
		((listp val)
		 (while (and val (null new-path))
		   (when (setq result
			       (hpath:substitute-var-name var (car val) path))
		     (setq new-path result))
		   (setq val (cdr val))))
		(t (error "(hpath:substitute-var): `%s' has invalid value for hpath:variables" var)))))
      (or new-path path))))

;;
;; The following function recursively resolves all POSIX links to their
;; final referents.
;; Works with variable-based and other strange links like:
;; /usr/local -> $(SERVER_LOCAL)/usr/local, /usr/bin ->
;; ../$(SYSTYPE)/usr/bin and /tmp -> `node_data/tmp.  It also handles
;; relative links properly as in /usr/local/emacs -> gnu/emacs which must
;; be resolved relative to the `/usr/local' directory.
;;
(defun hpath:symlink-referent (linkname)
  "Return expanded file or directory referent of LINKNAME.
LINKNAME should not end with a directory delimiter.
Return nil if LINKNAME is not a string.
Return LINKNAME unchanged if it is not a symbolic link but is a pathname."
  (when (stringp linkname)
    (let ((referent (file-symlink-p linkname)))
      (cond ((null referent)
	     linkname)
	    ((file-name-absolute-p referent)
	     referent)
	    (t (expand-file-name referent
				 (directory-file-name (file-name-directory
						       (directory-file-name (expand-file-name linkname))))))))))

(defun hpath:symlink-expand (referent dirname)
  "Return expanded file or directory REFERENT relative to DIRNAME."
  (let ((var-link)
	(dir dirname))
    (while (string-match "\\$(\\([^\)]*\\))" referent)
      (setq var-link (getenv (substring referent (match-beginning 1)
					(match-end 1)))
	    referent (concat (substring referent 0 (match-beginning 0))
			     var-link
			     (substring referent (match-end 0)))))
    ;; If referent is not an absolute path
    (let ((nd-abbrev (string-match "`node_data" referent)))
      (if (and nd-abbrev (= nd-abbrev 0))
	  (setq referent (concat
			   ;; Prepend node name given in dirname, if any
			   (and (string-match "^//[^/]+" dirname)
				(substring dirname 0 (match-end 0)))
			   "/sys/" (substring referent 1)))))
    (while (string-match "\\(^\\|/\\)\\.\\.\\(/\\|$\\)" referent)
      ;; Match to "//.." or "/.." at the start of link referent
      (while (string-match "^\\(//\\.\\.\\|/\\.\\.\\)\\(/\\|$\\)" referent)
	(setq referent (substring referent (match-end 1))))
      ;; Match to "../" or ".." at the start of link referent
      (while (string-match "^\\.\\.\\(/\\|$\\)" referent)
	(setq dir (file-name-directory (directory-file-name dir))
	      referent (concat dir (substring referent (match-end 0)))))
      ;; Match to rest of "../" in link referent
      (while (string-match "[^/]+/\\.\\./" referent)
	(setq referent (concat (substring referent 0 (match-beginning 0))
			       (substring referent (match-end 0))))))
    (and (not (eq (aref referent 0) ?~))
	 (not (eq (aref referent 0) ?/))
	 (setq referent (expand-file-name referent dirname))))
  referent)

(defun hpath:to-line (line-num)
  "Move point to the start of an absolute LINE-NUM or the last line."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (if (eq selective-display t)
	(re-search-forward "[\n\r]" nil 'end (1- line-num))
      (forward-line (1- line-num)))))

(defun hpath:trim (path)
  "Return PATH with any [\" \t\n\r] characters trimmed from its start and end."
  ;; Trim only matching starting and ending quoted double quotes (must
  ;; be a single line string).
  (setq path (string-trim path))
  (when (string-match "\\`\".*\"\\'" path)
    (setq path (string-trim path "\"" "\"")))
  path)

(defun hpath:normalize (filename)
  "Normalize and return PATH if PATH is a valid, readable path, else signal error."
  (hpath:validate (hpath:substitute-value
		   (buffer-file-name (hpath:find-noselect filename)))))

(defun hpath:validate (path)
  "Validate PATH is readable and return it in Posix format.
Signal an error if not validated.
Info and remote pathnames are considered readable without any
validation checks.

Default-directory should be equal to the current Hyperbole button
source directory when called, so that PATH is expanded relative
to it."
  (unless (stringp path)
    (error "(hpath:validate): \"%s\" is not a pathname." path))
  (setq path (hpath:mswindows-to-posix path))
  (cond ((or (string-match "[()]" path) (hpath:remote-p path))
	 ;; info or remote path, so don't validate
	 path)
	((if (not (hpath:www-p path))
	     ;; Otherwise, must not be a WWW link ref and must be a readable path.
	     (let ((return-path (hpath:exists-p path)))
	       (and return-path (file-readable-p return-path)
		    return-path))))
	(t (error "(hpath:validate): \"%s\" is not readable" path))))

;;; URL Handling
(defun hpath:find-file-urls-p ()
  (and (boundp 'file-name-handler-alist) (hpath:remote-available-p) t))

;; Partial setup which makes file finding commands recognize full and
;; abbreviated ftp and www URLs when a remote file access library is
;; available.
(defun hpath:handle-urls ()
  (let ((remote-fs-package (hpath:remote-available-p)))
    ;; www-url functions are defined in "hsys-www.el".
    (put 'expand-file-name   remote-fs-package   'www-url-expand-file-name)
    (put 'find-file-noselect remote-fs-package   'www-url-find-file-noselect)
    ;; Necessary since Dired overrides other file-name-handler-alist
    ;; settings.
    (put 'expand-file-name   'dired 'www-url-expand-file-name)
    (put 'find-file-noselect 'dired 'www-url-find-file-noselect)

    (unless (fboundp 'hyperb:substitute-in-file-name)
      (progn

;; Overload `substitute-in-file-name' to eliminate truncation of URL prefixes
;; such as http://.
(eval-and-compile
(unless (fboundp 'hyperb:substitute-in-file-name)
(defalias 'hyperb:substitute-in-file-name
  (symbol-function 'substitute-in-file-name))))

(defun substitute-in-file-name (filename)
  "Substitute environment variables referred to in FILENAME (skip Urls).
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.
If `/~' appears, all of FILENAME through that `/' is discarded."
  (if (string-match
       "\\(/\\|[^a-zA-Z0-9]\\)?\\(https?\\|s?ftp\\|telnet\\|news\\|nntp\\):[/~]"
       filename)
      (substring filename (match-beginning 2))
    (hyperb:substitute-in-file-name filename)))))))

(defun hpath:enable-find-file-urls ()
  "Enable the use of ftp and www Urls as arguments to `find-file' commands."
  (interactive)
  (cond ((hpath:find-file-urls-p)
	 ;; Setup for remote pathname support whenever an ftp or www
	 ;; URL is given to a find-file command.
	 (and (boundp 'allow-remote-paths) (setq allow-remote-paths t))
	 (require 'tramp)
	 (add-hook 'file-name-handler-alist
		   (cons hpath:remote-regexp 'tramp-file-name-handler))

	 (hpath:handle-urls)

	 (setq hpath:find-file-urls-mode t)

	 (or (assq 'hpath:find-file-urls-mode minor-mode-alist)
	     (if (fboundp 'add-minor-mode)
		 (add-minor-mode 'hpath:find-file-urls-mode
				 " URLs" nil 'abbrev-mode)
	       (setq minor-mode-alist
		     (cons '(hpath:find-file-urls-mode " URLs")
			   minor-mode-alist))))
	 (force-mode-line-update)

	 (if (called-interactively-p 'interactive)
	     (message
	      "(hpath:enable-find-file-urls): Find file commands will accept URLs.")))
	((called-interactively-p 'interactive)
	 (message "(hpath:enable-find-file-urls): A remote file library access like Tramp is needed to utilize find-file URLs.")
	 (beep))))

(defun hpath:disable-find-file-urls ()
  "Disable the use of ftp and www Urls as arguments to `find-file' commands."
  (interactive)
  (when (hpath:find-file-urls-p)
    (remove-hook 'file-name-handler-alist
		 (cons hpath:remote-regexp 'tramp-file-name-handler))

    ;; www-url functions are defined in "hsys-www.el".
    (put 'expand-file-name   'tramp nil)
    (put 'find-file-noselect 'tramp nil)
    ;; Necessary since Dired overrides other file-name-handler-alist
    ;; settings.
    (put 'expand-file-name   'dired nil)
    (put 'find-file-noselect 'dired nil)

    ;; Remove overloaded functions.
    (if (fboundp 'hyperb:substitute-in-file-name)
	(progn (defalias 'substitute-in-file-name
		 (symbol-function 'hyperb:substitute-in-file-name))
	       (fmakunbound 'hyperb:substitute-in-file-name)))

    (setq hpath:find-file-urls-mode nil)
    (force-mode-line-update)

    (if (called-interactively-p 'interactive)
	(message
	 "(hpath:disable-find-file-urls): Find file commands will not accept URLs."))))

;;;###autoload
(defun hpath:find-file-urls-mode (&optional arg)
  "Toggle use of ftp and www Urls as arguments to `find-file' commands.
With optional prefix ARG, enable this feature if ARG is positive or turn it
off otherwise."
  (interactive "P")
  (funcall (if (called-interactively-p 'interactive) #'call-interactively #'funcall)
	   (if (if arg
		   (> (prefix-numeric-value arg) 0)
		 (not hpath:find-file-urls-mode))
	       #'hpath:enable-find-file-urls
	     #'hpath:disable-find-file-urls)))

(defun hpath:url-at-p ()
  "Return an url that point immediately precedes or nil.
Url is a world-wide-web universal resource locator.
See the documentation for `hpath:url-regexp' for `match-string' groupings."
  (if (or (looking-at hpath:url-regexp) (looking-at hpath:url-regexp2)
	  (looking-at hpath:url-regexp3))
      (save-excursion
	(goto-char (match-end hpath:url-grpn))
	(skip-chars-backward ".,?#!*()")
	(buffer-substring-no-properties (match-beginning hpath:url-grpn) (point)))))

(defun hpath:url-p (obj)
  "Return t if OBJ is an url, else nil.
Url is a world-wide-web universal resource locator.
See the documentation for `hpath:url-regexp' for match groupings to
use with `string-match'."
  (and (stringp obj)
       (or (string-match hpath:string-url-regexp obj)
	   (string-match hpath:string-url-regexp2 obj)
	   (string-match hpath:string-url-regexp3 obj))
       t))

(defun hpath:www-at-p (&optional include-start-and-end-p)
  "Return a world-wide-web link reference that point is within or nil.
With optional INCLUDE-START-AND-END-P non-nil, returns list of:
  (link-string begin-position end-position)."
  (save-excursion
    (skip-chars-backward "^\[ \t\n\r\f\"`'|\(\{<")
    (cond ((not include-start-and-end-p)
	   (hpath:url-at-p))
	  ((or (looking-at hpath:url-regexp) (looking-at hpath:url-regexp2)
	       (looking-at hpath:url-regexp3))
	   (goto-char (match-end hpath:url-grpn))
	   (skip-chars-backward ".,?#!*()")
	   (list (buffer-substring-no-properties (match-beginning hpath:url-grpn) (point))
		 (match-beginning hpath:url-grpn)
		 (point))))))

(defun hpath:www-p (path)
  "Return PATH iff PATH is a world-wide-web link reference, else nil."
  (and (stringp path) (hpath:url-p path) path))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hpath:command-string (cmd filename)
  "Return a single string that runs a shell CMD over FILENAME.
CMD may contain a single `%s' indicating where FILENAME is to
be integrated, otherwise the filename is appended as a double-quoted argument."
  ;; Permit %s substitution of filename within program.
  (if (string-match "[^%]%s" cmd)
      (format cmd filename)
    (format "%s \"%s\"" cmd filename)))

(defun hpath:display-where-function (display-where display-where-alist)
  "Return the function to display a Hyperbole buffer or path.
Uses the symbol DISPLAY-WHERE or if null, `hpath:display-where'.
DISPLAY-WHERE-ALIST is a lookup table mapping from DISPLAY-WHERE
values to associated functions."
  (unless display-where
    (setq display-where hpath:display-where))
  (cadr (or (assq display-where display-where-alist)
	    (assq 'other-window display-where-alist))))

(defun hpath:remote-available-p ()
  "Return non-nil if a remote file access package is available, nil otherwise.
Return the symbol for the appropriate package to require/use.

Either the package must have been loaded already or under versions of Emacs
19 and above, it must be set for autoloading via `file-name-handler-alist',
in which case, it is loaded here."
  (cond ((featurep 'tramp) 'tramp)
	((featurep 'efs) 'efs)
	((featurep 'ange-ftp) 'ange-ftp)
	((boundp 'file-name-handler-alist) ; GNU Emacs
	 (cond ((or (rassq 'tramp-file-name-handler file-name-handler-alist)
		    (rassq 'tramp-autoload-file-name-handler file-name-handler-alist))
		(require 'tramp))
	       ((rassq 'efs-file-handler-function file-name-handler-alist)
		(require 'efs))
	       ((rassq 'ange-ftp-hook-function file-name-handler-alist)
		(require 'ange-ftp))))))

(defun hpath:remote-default-user ()
  "Return default user account for remote file access.
Returns \"anonymous\" if no default user is set."
  (cond ((and (boundp 'tramp-default-user)
	      (stringp tramp-default-user))
	 tramp-default-user)
	((and (boundp 'efs-default-user)
	      (stringp efs-default-user))
	 efs-default-user)
	((and (boundp 'ange-ftp-default-user)
	      (stringp ange-ftp-default-user))
	 ange-ftp-default-user)
	(t "anonymous")))

(defun hpath:delete-trailer (string)
  "Return STRING minus any trailing .?#!*() or quoting characters."
  (save-match-data
    (if (and (stringp string) (> (length string) 0)
	     (string-match "[.?#!*()`'\"]+\\'" string))
	(substring string 0 (match-beginning 0))
      string)))

(defun hpath:exists-p (path &optional suffix-flag)
  "Return PATH if it exists.  (This does not mean you can read it).
If PATH exists with or without a suffix from hpath:suffixes, then that
pathname is returned.

With optional SUFFIX-FLAG and PATH exists, return suffix added or removed
from path or t."
  (setq path (hpath:expand path))
  (let ((return-path)
	(suffix) suffixes)
    (if (file-exists-p path)
	(setq return-path path)
      (setq suffixes hpath:suffixes)
      (while suffixes
	(setq suffix (car suffixes))
	(if (string-match (concat (regexp-quote suffix) "\\'") path)
	    ;; Remove suffix
	    (setq return-path (substring path 0 (match-beginning 0)))
	  ;; Add suffix
	  (setq return-path (concat path suffix)))
	(if (file-exists-p return-path)
	    (setq suffixes nil) ;; found a match
	  (setq suffix nil
		suffixes (cdr suffixes)
		return-path nil))))
    (if return-path
	(if suffix-flag
	    (or suffix t)
	  return-path))))

;; Next function from: 2006-11-02  Mats Lidell
(defun hpath:find-file-mailcap (file-name)
  "Find command to view FILE-NAME according to the mailcap file."
  (when (featurep 'mailcap)
    (mailcap-parse-mailcaps)
    (let (mime-type method)
      (when (and (string-match "\\.[^\\.]+$" file-name)
		 (setq mime-type
		       (mailcap-extension-to-mime
			(match-string-no-properties 0 file-name)))
		 (stringp
		  (setq method
			(cdr (assoc 'viewer
				    (car (mailcap-mime-info mime-type
							    'all)))))))
	(mm-mailcap-command method file-name nil)))))

(defun hpath:find-program (filename)
  "Return one or a list of shell or Lisp commands to execute to display FILENAME.
Return nil if FILENAME is a directory name or an image file that
Emacs can display.  See also documentation for the
function (hpath:get-external-display-alist) and the variable
`hpath:internal-display-alist'."
  (cond ((and (fboundp 'image-mode)
	      (string-match hpath:native-image-suffixes filename))
	 nil)
	((let ((case-fold-search nil))
	   (hpath:match filename hpath:internal-display-alist)))
	((let ((case-fold-search t))
	   (hpath:match filename (hpath:get-external-display-alist))))
	((and (stringp filename) (file-directory-p filename))
	 nil)
	;; 01/21/2019 - RSW commented this next line out since it can
	;; trigger external viewers on many file types that Emacs
	;; displays natively.
	;; (t (hpath:find-file-mailcap filename))
	))

(defun hpath:match (filename regexp-alist)
  "If FILENAME matches the car of any element in REGEXP-ALIST, return its cdr.
REGEXP-ALIST elements must be of the form (<filename-regexp>
. <command-to-display-file>).  <command-to-display-file> may be a string
representing an external `window-system' command to run or it may be a Lisp
function to call with FILENAME as its single argument."
  (let ((cmd)
	elt)
    (while (and (not cmd) regexp-alist)
      (if (string-match (car (setq elt (car regexp-alist))) filename)
	  (setq cmd (cdr elt)))
      (setq regexp-alist (cdr regexp-alist)))
    cmd))

(defun hpath:get-single-string-variable-value (var-name)
  "Return VAR-NAME's value if is a string without any colon or semicolon.
Otherwise return nil."
  (let (sym
	val)
    (cond ((not (stringp var-name))
	   ;; (error "(hpath:get-single-string-variable-value): var-name, `%s', must be a string" var-name)
	   nil)
	  ((not (or (and (setq sym (intern-soft var-name))
			 (boundp sym))
		    (setq val (getenv var-name))))
	   ;; (error "(hpath:get-single-string-variable-value): var-name, \"%s\", is not a bound variable nor a set environment variable"
	   ;;   var-name)
	   nil)
	  ((let ((case-fold-search t))
	     (or (stringp (setq val (cond ((and sym (boundp sym))
					   (symbol-value sym))
					  ((and (string-match-p "path" var-name)
						(seq-find (lambda (c) (memq c '(?: ?\;)))
							  (or (getenv var-name) "")))
					   nil)
					  (t (getenv var-name)))))
		 (setq val nil))))
	  ((listp val)
	   (setq val nil))
	  (t
	   ;; (error "(hpath:get-single-string-variable-value): Value of var-name, \"%s\", must be a string or list" var-name)
	   (setq val nil)))
    val))

(defun hpath:substitute-dir (path-prefix var-name rest-of-path trailing-dir-sep-flag &optional return-path-flag)
  "Return directory after substitutions.
Return the concatenation of PATH-PREFIX, dir for VAR-NAME,
TRAILING-DIR-SEP-FLAG and REST-OF-PATH when optional
RETURN-PATH-FLAG is non-nil.  Otherwise, return just the dir for
VAR-NAME.  Trigger an error when no match.  With RETURN-PATH-FLAG
non-nil, return path expanded and with first variable value
substituted.

VAR-NAME's value may be a directory or a list of directories.  If
it is a list, return the first directory prepended to
REST-OF-PATH which produces a valid local pathname."
  (unless (stringp rest-of-path)
    (setq rest-of-path ""))
  (unless (string-match-p hpath:variable-regexp path-prefix)
    (setq path-prefix (substitute-in-file-name path-prefix)))
  (unless (string-match-p hpath:variable-regexp rest-of-path)
    (setq rest-of-path (substitute-in-file-name rest-of-path)))
  (let (path sym val)
    (cond ((not (stringp var-name))
	   (error "(hpath:substitute-dir): var-name, `%s', must be a string" var-name))
	  ((not (or (and (setq sym (intern-soft var-name))
			 (boundp sym))
		    (setq val (getenv var-name))))
	   (error "(hpath:substitute-dir): var-name, \"%s\", is not a bound variable nor a set environment variable"
		  var-name))
	  ((let ((case-fold-search t))
	     (stringp (setq val (cond ((and (boundp sym) sym)
				       (symbol-value sym))
				      ((and (string-match "path" var-name)
					    (seq-find (lambda (c) (memq c '(?: ?\;))) (or (getenv var-name) "")))
				       (split-string (getenv var-name) "[:;]"))
				      (t (getenv var-name)))))))
	  ((listp val)
	   (unless (and (setq path (locate-file rest-of-path val (cons "" hpath:suffixes)
						(lambda (f) (when (file-readable-p f)
							      'dir-ok))))
			return-path-flag)
	     (let* ((suffix-added (car (delq nil (mapcar (lambda (suffix)
							   (when (string-suffix-p suffix path)
							     suffix))
							 hpath:suffixes))))
		    (path-with-dots rest-of-path)
		    (len-dot-paths 0))
	       (while (string-match "\\(\\`\\|[\\/]\\)\\(\\.\\.?[\\/]\\)" path-with-dots)
		 (setq len-dot-paths (+ len-dot-paths (length (match-string 2 path-with-dots)))
		       path-with-dots (substring path-with-dots (match-end 0))))
	       (when path
 		 (setq val (substring path 0 (- (+ (length rest-of-path)
						   (- len-dot-paths)
						   (length suffix-added))))))
	       (or val
		   (error "(hpath:substitute-dir): Can't find match for \"%s\""
			  (concat "$\{" var-name "\}/" rest-of-path))))))
	  (t (error "(hpath:substitute-dir): Value of var-name, \"%s\", must be a string or list" var-name)))
    (when (stringp val)
      (setq val (directory-file-name val)))
    (cond ((and return-path-flag path)
	   (concat path-prefix path))
	  ((and return-path-flag rest-of-path)
	   (if (stringp val)
	       (concat path-prefix val trailing-dir-sep-flag rest-of-path)
	     (error "(hpath:substitute-dir): Can't find match for \"%s\""
		    (concat "$\{" var-name "\}/" rest-of-path))))
	  (t val))))

(defun hpath:substitute-match-value (regexp str new &optional literal fixedcase)
  "Replace all matches for REGEXP in STR with NEW string and return the result.

Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEW string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\.

If optional fifth arg FIXEDCASE is non-nil, do not alter the case of
the replacement text.  Otherwise, maybe capitalize the whole text, or
maybe just word initials, based on the replaced text.  If the replaced
text has only capital letters and has at least one multiletter word,
convert NEW to all caps.  Otherwise if all words are capitalized
in the replaced text, capitalize each word in NEW.

NEW may instead be a function of one argument (the string to replace in)
that returns a replacement string."
  (unless (stringp str)
    (error "(hpath:substitute-match-value): 2nd arg must be a string: %s" str))
  (unless (or (stringp new) (functionp new))
    (error "(hpath:substitute-match-value): 3rd arg must be a string or function: %s"
	   new))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	     rtn-str
	     (substring str prev-start match)
	     (cond ((functionp new)
		    (replace-regexp-in-string
		     regexp (funcall new str)
		     (substring str match start) fixedcase literal))
		   (literal new)
		   (t (mapconcat
		       (lambda (c)
			 (cond (special
				(setq special nil)
				(cond ((eq c ?\\) "\\")
				      ((eq c ?&)
				       (match-string 0 str))
				      ((and (>= c ?0) (<= c ?9))
				       (if (> c (+ ?0 (length (match-data))))
					   ;; Invalid match num
					   (error "(hpath:substitute-match-value): Invalid match num: %c" c)
					 (setq c (- c ?0))
					 (match-string c str)))
				      (t (char-to-string c))))
			       ((eq c ?\\)
				(setq special t)
				nil)
			       (t (char-to-string c))))
		       new ""))))))
    (if (or (null rtn-str) (string-empty-p rtn-str))
	str
      (concat rtn-str (substring str start)))))

(defun hpath:substitute-var-name (var-symbol var-dir-val path)
  "Replace with VAR-SYMBOL any occurrences of VAR-DIR-VAL in PATH.
Replacement is done iff VAR-DIR-VAL is an absolute path.
If PATH is modified, return PATH, otherwise return nil."
  (when (and (stringp var-dir-val) (file-name-absolute-p var-dir-val))
    (let ((new-path (replace-regexp-in-string
		     (regexp-quote (file-name-as-directory
				    (or var-dir-val default-directory)))
		     (concat "$\{" (symbol-name var-symbol) "\}/") path
		     t t)))
      (if (equal new-path path) nil new-path))))


(provide 'hpath)

;;; hpath.el ends here
