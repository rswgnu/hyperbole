;;; hypb.el --- Miscellaneous GNU Hyperbole support features  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     6-Oct-91 at 03:42:38
;; Last-Mod:      1-May-22 at 10:25:55 by Bob Weiner
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

(eval-and-compile (mapc #'require '(compile hversion hact locate)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hypb:help-buf-prefix "*Help: Hyperbole "
  "Prefix attached to all native Hyperbole help buffer names.
This should end with a space.")

(defcustom hypb:rgrep-command
  ;; Only the FreeBSD version of zgrep supports all of the grep
  ;; options that Hyperbole needs: -r, --include, and --exclude
  (format "%sgrep -insIHr" (if (and (executable-find "zgrep")
                                    (string-match-p "bsd" (shell-command-to-string "zgrep --version | head -1")))
                               "z" ""))
  "*Grep command string and initial arguments to send to `hypb:rgrep' command.
It must end with a space."
  :type 'string
  :group 'hyperbole-commands)


;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar mh-e-RCS-id)
(defvar pm-version)
(defvar vm-version)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defmacro hypb:assert-same-start-and-end-buffer (&rest body)
  "Trigger an error with traceback if the buffer is not live or its name differs at the start and end of BODY."
  (declare (indent 0) (debug t))
  `(let ((debug-on-error t)
	 (start-buffer (current-buffer)))
     (unless (buffer-live-p start-buffer)
       (error "Start buffer, '%s', is not live" (current-buffer)))
     ;; `kill-buffer' can change current-buffer in some odd cases.
     (unwind-protect
	 (progn ,@body)
       (unless  (eq start-buffer (current-buffer))
	 (error "Start buffer, '%s', differs from end buffer, '%s'" start-buffer (current-buffer)))
       (unless (buffer-live-p start-buffer)
	 (error "End buffer, '%s', is not live" (current-buffer))))))

(defun hypb:call-process-p (program &optional infile predicate &rest args)
  "Call an external PROGRAM with INFILE for input.
If PREDICATE is given, it is evaluated in a buffer with the PROGRAM's
output and the result returned.  If PREDICATE is nil, returns t iff
program has no output or just a 0-valued output.
Rest of ARGS are passed as arguments to PROGRAM."
  (let ((buf (get-buffer-create "*test-output*"))
	(found))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (apply 'call-process program infile buf nil args)
      (setq found
	    (if predicate
		(eval predicate)
	      (or (= (point-max) 1) ;; No output, consider cmd a success.
		  (and (< (point-max) 4)
		       (string= (buffer-substring 1 2) "0")))))
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    found))

(defun hypb:char-count (char array)
  "Return count of occurrences of CHAR in ARRAY."
  (let ((i 0) (c 0) (l (length array)))
    (while (< i l)
      (when (= char (aref array i)) (setq c (1+ c)))
      (setq i (1+ i)))
    c))

(defun hypb:chmod (op octal-permissions file)
  "Use OP and OCTAL-PERMISSIONS integer to set FILE permissions.
OP may be +, -, xor, or default =."
  (let ((func (cond ((eq op '+)   #'logior)
		    ((eq op '-)   (lambda (p1 p2) (logand (lognot p1) p2)))
		    ((eq op 'xor) #'logxor)
		    (t            (lambda (p1 p2) p2 p1)))))
    (set-file-modes file (funcall func (hypb:oct-to-int octal-permissions)
				  (file-modes file)))))

(defun hypb:cmd-key-series (cmd-sym &optional keymap)
  "Return a single, brace-delimited, human readable key sequence string bound to CMD-SYM.
Global keymap is used unless optional KEYMAP is given.

Trigger an error if CMD-SYM is not bound."
  (if (and cmd-sym (symbolp cmd-sym) (fboundp cmd-sym))
      (let* ((get-keys (lambda (cmd-sym keymap)
		         (key-description (where-is-internal
				           cmd-sym keymap 'first))))
	     (keys (funcall get-keys cmd-sym keymap)))
        (concat "{"
	        (if (string= keys "")
		    (concat (funcall get-keys 'execute-extended-command nil)
			    " " (symbol-name cmd-sym) " RET")
	          keys)
	        "}"))
    (error "(hypb:cmd-key-series): Invalid cmd-sym arg: %s" cmd-sym)))

(defun hypb:cmd-key-vector (cmd-sym &optional keymap)
  "Return as a vector the first key sequence bound to CMD-SYM from global keymap or optional KEYMAP.
Return nil if no valid key binding is found.

The returned value may be compared with `equal' to `this-single-command-keys'.
Use `key-description' to make it human readable."
  (where-is-internal cmd-sym keymap t))

(defun hypb:installation-type ()
  "Return a list of (hyperbole-installation-type-string hyperbole-install-version-number-string).
If no matching installation type is found, return a list of (\"unknown\" hyperb:dir)."
  (let ((hypb-dir-name (file-name-nondirectory (directory-file-name hyperb:dir)))
	(dir-sep-string (substring (file-name-as-directory ".") -1)))
    (cond
     ;; straight.el package install -- hyperbole gnu-elpa-mirror master 56cd3d8 2022-02-05
     ((string-match (concat dir-sep-string "straight" dir-sep-string
			    "build" dir-sep-string "hyperbole") hyperb:dir)
      (let* ((plist (hypb:straight-package-plist "hyperbole"))
	     (pkg-version (plist-get plist :version))
	     (git-commit (when (string-match " \\([a-f0-9]+\\) " pkg-version)
			   (match-string 1 pkg-version))))
	(list "straight" git-commit)))
     ;; elpa-devel package install -- hyperbole-7.0.0pre0.20220126.1138
     ((string-match "hyperbole-\\([.[:digit:]]+pre[.[:digit:]]+\\)" hypb-dir-name)
      (list "elpa-devel" (match-string 1 hypb-dir-name)))
     ;; melpa/quelpa package install -- hyperbole-20220205.1429
     ((string-match "hyperbole-\\([1-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]\\.[0-9]+\\)"
		    hypb-dir-name)
      (list "melpa" (match-string 1 hypb-dir-name)))
     ;; git install -- hyperbole d27f4c5197
     ((file-exists-p (expand-file-name ".git" hyperb:dir))
      (ignore-errors
        (let ((default-directory hyperb:dir))
          (list
           "git"
           (substring (shell-command-to-string "git rev-parse HEAD") 0 10)))))
     ;; elpa package install -- /elpa/hyperbole-8.0.0"
     ((and (string-match-p (concat dir-sep-string "elpa" dir-sep-string) hyperb:dir)
	   (string-match "hyperbole-\\([.[:digit:]]+\\)" hypb-dir-name))
      (list "elpa" (match-string 1 hypb-dir-name)))
     ;; tarball archive install -- hyperbole-8.0.0
     ((string-match "hyperbole-\\([.[:digit:]]+\\)" hypb-dir-name)
      (list "archive" (match-string 1 hypb-dir-name)))
     ;; unknown -- hyperbole
     (t (list "unknown" hyperb:dir)))))

;;;###autoload
(defun hypb:configuration (&optional out-buf)
  "Insert Emacs configuration information at the end of optional OUT-BUF or the current buffer."
  (save-excursion
    (and out-buf (set-buffer out-buf))
    (goto-char (point-min))
    (if (re-search-forward mail-header-separator nil t)
	(forward-line 1)
      (goto-char (point-max)))
    (delete-blank-lines) (delete-blank-lines)
    (let ((start (point)))
      (insert (format "I use:\tEditor:      GNU Emacs %s\n\tHyperbole:   %s\n"
		      emacs-version hyperb:version))
      (when (and (boundp 'br-version) (stringp br-version))
	(insert (format "\tOO-Browser:  %s\n" br-version)))
      (when (and (boundp 'system-configuration) (stringp system-configuration))
	(insert (format "\tSys Type:    %s\n" system-configuration)))
      (insert (format "\tOS Type:     %s\n\tWindow Sys:  %s\n"
                      system-type (or window-system (hyperb:window-system)
				      "None")))
      (when (and (boundp 'hmail:reader) hmail:reader)
        (insert (format "\tMail Reader: %s\n"
                        (cond ((eq hmail:reader 'rmail-mode) "RMAIL")
                              ((eq hmail:reader 'vm-mode)
                               (concat "VM " vm-version))
                              ((and (eq hmail:reader 'mh-show-mode)
                                    (string-match "v ?\\([0-9]+.[0-9]+\\)"
                                                  mh-e-RCS-id))
                               (concat "MH-e "
                                       (substring mh-e-RCS-id
                                                  (match-beginning 1)
                                                  (match-end 1))))
                              ((eq hmail:reader 'pm-fdr-mode)
                               (concat "PIEmail " pm-version))))))
      (when (and (boundp 'hnews:reader) (boundp 'gnus-version) hnews:reader)
        (insert (format "\tNews Reader: %s\n" gnus-version)))
      (let ((install-type (hypb:installation-type)))
        (when install-type
          (insert (format "\tInstall:     %s, %s" (car install-type) (cadr install-type)))))
      (insert "\n")
      ;; Insert recent Hyperbole debugging messages if any.
      (when (get-buffer "*Messages*")
	(let ((opoint (point)))
	  (insert-buffer-substring "*Messages*")
	  (keep-lines "^(HyDebug)" opoint (point))))
      (untabify start (point)))))

(defun hypb:debug ()
  "Load Hyperbole hbut.el source file and set debugging traceback flag."
  (interactive)
  (or (featurep 'hinit) (load "hyperbole"))
  (or (and (featurep 'hbut)
	   (let ((func (hypb:indirect-function 'ebut:create)))
	     (not (or (hypb:emacs-byte-code-p func)
		      (eq 'byte-code
			  (car (car (nthcdr 3 (hypb:indirect-function
					       'ebut:create)))))))))
      (load "hbut.el"))
  (setq debug-on-error t))

;; Copied from eww.el so as to not require that package.
(defun hypb:decode-url (string)
  (let* ((binary (url-unhex-string string))
         (decoded
          (decode-coding-string
           binary
           ;; Possibly set by `universal-coding-system-argument'.
           (or coding-system-for-read
               ;; RFC 3986 says that %AB stuff is utf-8.
               (if (equal (decode-coding-string binary 'utf-8)
                          '(unicode))
                   'utf-8
                 ;; But perhaps not.
                 (car (detect-coding-string binary))))))
         (encodes (find-coding-systems-string decoded)))
    (if (or (equal encodes '(undecided))
            (memq (coding-system-base (or file-name-coding-system
                                          default-file-name-coding-system))
                  encodes))
        decoded
      ;; If we can't encode the decoded file name (due to language
      ;; environment settings), then we return the original, hexified
      ;; string.
      string)))

;; Similar keyboard macro to next function, but less flexible: {C-x 1 M-o F M-o a C-x b *scratch* RET M-< M-o s C-@ C-M-h M-o t a C-u C-@ C-u C-@ M-o a C-M-p}

;;;###autoload
(defun hypb:def-to-buffer (&optional arg buffer)
  "Copy next optional ARG (default = 1) code definitions to the start of BUFFER (default = *scratch*) and leave point at the start of the inserted text."
  (interactive "p\nbDef insertion buffer (default *scratch*): ")
  (let ((def (save-excursion
	       (mark-defun arg)
	       (deactivate-mark)
	       (buffer-substring (region-beginning) (region-end)))))
    (pop-to-buffer (or buffer "*scratch*"))
    (goto-char (point-min))
    (insert def)
    (goto-char (point-min))
    (forward-line 1)))

(defun hypb:domain-name ()
  "Return current Internet domain name with '@' prepended or nil if none."
  (let* ((dname-cmd (or (file-exists-p "/usr/bin/domainname")
			(file-exists-p "/bin/domainname")))
	 (dname (or (and (boundp 'message-user-fqdn) (stringp message-user-fqdn)
			 (string-match "\\." message-user-fqdn)
			 message-user-fqdn)
		    (getenv "DOMAINNAME")
		    (when dname-cmd
		      (hypb:call-process-p
		       "domainname" nil
		       '(substring (buffer-string) 0 -1)))))
	 host-and-domain)
    (when (or (and dname (string-match "\\." dname))
	      (and (setq host-and-domain (hypb:call-process-p
					  "hostname" nil '(substring (buffer-string) 0 -1) "-f"))
		   (setq dname (when (string-match "\\`[^.]+\\." host-and-domain)
				 (substring host-and-domain (match-end 0)))))
	      (let* ((src "/etc/resolv.conf")
		     (src-buf-exists-p (get-file-buffer src)))
	        (and (file-exists-p src) (file-readable-p src)
		     (with-temp-buffer
		       (insert-file-contents-literally src)
		       (goto-char (point-min))
		       (when (re-search-forward  "^domain[ \t]+\\([^ \t\n\r]+\\)" nil t)
			 (setq dname (match-string 1)))
		       (or src-buf-exists-p (kill-buffer nil))
		       dname))))
      (concat "@" dname))))

(defun hypb:emacs-byte-code-p (obj)
  "Return non-nil iff OBJ is an Emacs byte compiled object."
  (or (and (fboundp 'byte-code-function-p) (byte-code-function-p obj))
      (and (fboundp 'compiled-function-p) (compiled-function-p obj))))

(defun hypb:error (&rest args)
  "Signal an error typically to be caught by `hyperbole'."
  (let ((msg (if (< (length args) 2)
		 (car args)
	       (apply 'format (cons (car args)
				    (mapcar #'hypb:format-quote (cdr args)))))))
    (put 'error 'error-message msg)
    (error msg)))

(defun hypb:fgrep-git-log (string)
  "Asynchronously list git log entries whose changesets include `string' for selection and display.
A press of RET, the Action Key or the Assist Key on any log line will display its committed changes."
  (interactive "sFgrep git commits containing: ")
  (compile (format "git log -S'%s' --line-prefix='commit ' --oneline" string)
	   #'hypb:fgrep-git-log-mode))

(defun hypb:fgrep-git-log-activate (_ignore1 &optional _ignore2)
  "Display git commit for the current line when `compile-goto-error' {RET} is used.
Does not support use of next and previous error; simply displays the current one."
  (interactive '(nil))
  (hkey-either nil))

(define-derived-mode hypb:fgrep-git-log-mode compilation-mode "Fgrep-Git-Log"
  "Major mode (derived from `compilation-mode' for listing a matching set of git commits for selection and display.
Turning on Fgrep-Git-Log mode runs the normal hook `compilation-mode-hook'."
  (setq-local next-error-function #'hypb:fgrep-git-log-activate))

(defun hypb:file-major-mode (file)
  "Return the major mode used by FILE.
FILE is temporarily read into a buffer to determine the major mode if necessary."
  (let ((existing-flag (get-file-buffer file))
	(buf (find-file-noselect file)))
    (prog1 (when buf (save-excursion (with-current-buffer buf
				       major-mode)))
      (unless (or existing-flag (null buf))
	(kill-buffer buf)))))

(defun hypb:format-quote (arg)
  "Replace all single % with %% in any string ARG so that a call to `format' or `message' ignores them.
Return either the modified string or the original ARG when not modified."
  (if (stringp arg)
      (replace-regexp-in-string
       "@@@" "%%" (replace-regexp-in-string
		   "%" "%%" (replace-regexp-in-string "%%" "@@@" arg nil t)
		   nil t)
       nil t)
    arg))

(defun hypb:function-copy (func-symbol)
  "Copy FUNC-SYMBOL's body for overloading.  Return a copy of the body."
  (if (fboundp func-symbol)
      (let ((func (hypb:indirect-function func-symbol)))
	(cond ((listp func) (copy-sequence func))
	      ((subrp func) (error "(hypb:function-copy): `%s' is a primitive; can't copy body"
				   func-symbol))
	      ((and (hypb:emacs-byte-code-p func) (fboundp 'make-byte-code))
	       (let ((new-code (append func nil))) ; turn it into a list
		 (apply 'make-byte-code new-code)))
	      (t (error "(hypb:function-copy): Can't copy function body: %s" func))))
    (error "(hypb:function-copy): `%s' symbol is not bound to a function"
	   func-symbol)))

(defun hypb:function-overload (func-sym prepend &rest new-forms)
  "Redefine function named FUNC-SYM by either PREPENDing (or appending if nil) rest of quoted NEW-FORMS."
  (let ((old-func-sym (intern
			(concat "hypb--old-"
				(symbol-name func-sym)))))
    (or (fboundp old-func-sym)
	(defalias old-func-sym (hypb:function-copy func-sym)))
    (let* ((old-func (hypb:indirect-function old-func-sym))
	   (old-param-list (action:params old-func))
	   (param-list (action:param-list old-func))
	   (old-func-call
	     (list (if (memq '&rest old-param-list)
		       ;; Have to account for extra list wrapper from &rest.
		       (cons 'apply
			     (cons (list 'quote old-func-sym) param-list))
		     (cons old-func-sym param-list)))))
      (eval (append
	      (list 'defun func-sym old-param-list)
	      (delq nil
		    (list
		      (documentation old-func-sym)
		      (action:commandp old-func-sym)))
	      (if prepend
		  (append new-forms old-func-call)
		(append old-func-call new-forms)))))))

(defun hypb:function-symbol-replace (func-sym sym-to-replace replace-with-sym)
  "Replace in body of FUNC-SYM SYM-TO-REPLACE with REPLACE-WITH-SYM.
FUNC-SYM may be a function symbol or its body.  All occurrences within lists
are replaced.  Returns body of modified FUNC-SYM."
  (let ((body (hypb:indirect-function func-sym))
	(constant-vector) (constant))
    (if (subrp body)
        ;; Non-Lisp code, can't do any replacement
        body
      (if (listp body)
	  ;; assume V18 byte compiler
	  (setq constant-vector
	        (car (delq nil (mapcar
			        (lambda (elt)
				  (and (listp elt)
				       (vectorp (setq constant-vector (nth 2 elt)))
				       constant-vector))
			        body))))
        ;; assume EMACS byte compiler   (eq (compiled-function-p body) t)
        (setq constant (if (fboundp 'compiled-function-constants)
			   (compiled-function-constants body)
		         (aref body 2))
	      constant-vector (when (vectorp constant) constant)))
      (if constant-vector
	  ;; Code is byte-compiled.
	  (hypb:constant-vector-symbol-replace
	   constant-vector sym-to-replace replace-with-sym)
        ;;
        ;; Code is not byte-compiled.
        ;; Replaces occurrence of symbol within lists only.
        (hypb:map-sublists
         (lambda (atom list)
	   ;; The ' in the next line *is* required for proper substitution.
	   (when (eq atom 'sym-to-replace)
	     (let ((again t))
	       (while (and again list)
	         (if (eq (car list) atom)
		     (progn (setcar list replace-with-sym)
			    (setq again nil))
		   (setq list (cdr list)))))))
         body))
      body)))

;; Extracted from part of `choose-completion' in "simple.el"
(defun hypb:get-completion (&optional event)
  "Return the completion at point.
If EVENT, use EVENT's position to determine the starting position."
  (interactive (list last-nonmenu-event))
  ;; In case this is run via the mouse, give temporary modes such as
  ;; isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (save-excursion
      (goto-char (posn-point (event-start event)))
      (let (beg end)
        (cond
         ((and (not (eobp)) (get-text-property (point) 'mouse-face))
          (setq end (point) beg (1+ (point))))
         ((and (not (bobp))
               (get-text-property (1- (point)) 'mouse-face))
          (setq end (1- (point)) beg (point)))
         (t (error "No completion here")))
        (setq beg (previous-single-property-change beg 'mouse-face))
        (setq end (or (next-single-property-change end 'mouse-face)
                      (point-max)))
        (buffer-substring-no-properties beg end)))))

(defun hypb:get-raw-syntax-descriptor (char &optional syntax-table)
  "Return the raw syntax descriptor for CHAR using the current syntax table or optional SYNTAX-TABLE."
  (aref (or syntax-table (syntax-table)) char))

;; Derived from pop-global-mark of "simple.el" in GNU Emacs.
(defun hypb:goto-marker (marker)
  "Make MARKER's buffer and position current.
If MARKER is invalid signal an error."
  (cond ((not (markerp marker))
	 (error "Invalid marker: %s" marker))
	((not (marker-buffer marker))
	 (error "Invalid marker buffer: %s" marker))
	(t (let* ((buffer (marker-buffer marker))
		  (position (marker-position marker)))
	     (set-buffer buffer)
	     (unless (and (>= position (point-min))
			  (<= position (point-max)))
	       (if widen-automatically
		   (widen)
		 (error "Marker position is outside accessible part of buffer: %s" marker)))
	     (goto-char position)
	     (switch-to-buffer buffer)))))

(defun hypb:grep-git-log (regexp)
  "Asynchronously list git log entries whose changesets include `regexp' for selection and display.
A press of RET, the Action Key or the Assist Key on any log line will display its committed changes."
  (interactive "sGrep git commits containing: ")
  (compile (format "git log -G'%s' --line-prefix='commit ' --oneline" regexp)))

(defun hypb:help-buf-name (&optional suffix)
  "Return a Hyperbole help buffer name for current buffer.
With optional SUFFIX string, uses it rather than buffer name."
  (let ((bn (or suffix (buffer-name))))
    (if (string-match (regexp-quote hypb:help-buf-prefix) bn)
	(buffer-name (generate-new-buffer bn))
      (concat hypb:help-buf-prefix bn "*"))))

(defun hypb:hkey-help-file ()
  "Return the full path to the Hyperbole mouse key help file."
  (cond ((and (fboundp 'locate-data-file)
	      (locate-data-file "hkey-help.txt")))
	(t (let* ((hypb-man (expand-file-name "man/" hyperb:dir))
		  (help-file (expand-file-name "hkey-help.txt" hypb-man)))
	     (if (or (file-exists-p help-file)
		     (file-exists-p
		      (setq help-file (expand-file-name
				       "hkey-help.txt" data-directory))))
		 help-file
	       (error "(hypb:hkey-help-file): Non-existent file: \"%s\""
		      help-file))))))

(defun hypb:indirect-function (obj)
  "Return the function at the end of OBJ's function chain.
Resolves autoloadable function symbols properly."
  (let ((func
	 (if (fboundp 'indirect-function)
	     (indirect-function obj)
	   (while (symbolp obj)
	     (setq obj (symbol-function obj)))
	   obj)))
    ;; Handle functions with autoload bodies.
    (if (and (symbolp obj) (listp func) (eq (car func) 'autoload))
	(let ((load-file (car (cdr func))))
	  (load load-file)
	  ;; Prevent infinite recursion
	  (if (equal func (symbol-function obj))
	      (error "(hypb:indirect-function): Autoload of '%s' failed" obj)
	    (hypb:indirect-function obj)))
      func)))

(defun hypb:insert-region (buffer start end invisible-flag)
  "Insert into BUFFER the contents of the region from START to END within the current buffer.
INVISIBLE-FLAG, if non-nil, means invisible text in an outline region is
copied, otherwise, it is omitted."
  (if invisible-flag
      ;; Skip hidden blank lines between cells but include hidden outline text.
      (while (< start end)
	(if (not (get-text-property start 'invisible))
	    (append-to-buffer buffer start (1+ start)))
	(setq start (1+ start)))
    ;; Skip both hidden blank lines between cells and hidden outline text.
    (while (< start end)
      (or (kview:char-invisible-p start) (append-to-buffer buffer start (1+ start)))
      (setq start (1+ start)))))

;;;###autoload
(defun hypb:locate (search-string &optional filter arg)
  "Find file name match anywhere, calling the value of `locate-command', and putting results in the `*Locate*' buffer.
Pass it SEARCH-STRING as argument.  Interactively, prompt for SEARCH-STRING.
With prefix arg ARG, prompt for the exact shell command to run instead.

This program searches for those file names in a database that match
SEARCH-STRING and normally outputs all matching absolute file names,
one per line.  The database normally consists of all files on your
system, or of all files that you have access to.  Consult the
documentation of the program for the details about how it determines
which file names match SEARCH-STRING.  (Those details vary highly with
the version.)

You can specify another program for this command to run by customizing
the variables `locate-command' or `locate-make-command-line'.

The main use of FILTER is to implement `locate-with-filter'.  See
the docstring of that function for its meaning.

After preparing the results buffer, this runs `dired-mode-hook' and
then `locate-post-command-hook'."
  (interactive (list (let ((default (symbol-at-point)))
		       (read-string (format "Locate files anywhere with names that match%s: "
					    (if default
						(format " (default %s)" default)
					      ""))
				    nil nil default))
		     nil
		     current-prefix-arg))
  (locate search-string filter arg))

;;;###autoload
(defun hypb:map-plist (func plist)
  "Return result of applying FUNC of two args, key and value, to key-value pairs in PLIST, a property list."
  (cl-loop for (k v) on plist by #'cddr
	   collect (funcall func k v) into result
	   finally return result))

(defun hypb:map-sublists (func list)
  "Apply FUNC to every atom found at any level of LIST.
FUNC must take two arguments, an atom and a list in which the atom is found.
Returns values from applications of FUNC as a list with the same
structure as LIST.  FUNC is therefore normally used just for its side-effects."
  (mapcar (lambda (elt)
	    (if (atom elt)
		(funcall func elt list)
	      (hypb:map-sublists func elt)))
	  list))

(defun hypb:map-vector (func object)
  "Return list of results of application of FUNC to each element of OBJECT.
OBJECT should be a vector or `byte-code' object."
  (unless (or (vectorp object) (hypb:emacs-byte-code-p object))
    (error "(hypb:map-vector): Second argument must be a vector or byte-code object"))
  (let ((end (length object))
	(i 0)
	(result))
    (while (< i end)
      (setq result (cons (funcall func (aref object i)) result)
	    i (1+ i)))
    (nreverse result)))

(defun hypb:mark-object (object)
  "Mark OBJECT as a Hyperbole object if possible to prevent generic functions from changing it.
OBJECT must be a non-empty string or a symbol or this has no effect."
  (cond ((and (stringp object) (not (string-empty-p object)))
	 (put-text-property 0 1 'hyperbole t object))
	((symbolp object)
	 (put object 'hyperbole t))))

;; Derived from "window.el".
(defun hypb:maximize-window-height (&optional window)
  "Maximize WINDOW.
Make WINDOW as large as possible without deleting any windows.
WINDOW must be a valid window and defaults to the selected one.

If the option `window-resize-pixelwise' is non-nil maximize
WINDOW pixelwise."
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window (window-max-delta window nil nil nil nil nil window-resize-pixelwise)
   nil nil window-resize-pixelwise))

(defun hypb:object-p (object)
  "Return t if OBJECT is marked as a Hyperbole object, else nil."
  (cond ((and (stringp object) (not (string-empty-p object)))
	 (get-text-property 0 'hyperbole object))
	((symbolp object)
	 (get object 'hyperbole))))

(defun hypb:replace-match-string (regexp str new &optional literal fixedcase)
  "Replace all matches for REGEXP in STR with NEW string and return the result.
If NEW is nil, return STR unchanged.

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
  (if (null new)
      str
    (unless (stringp str)
      (error "(hypb:replace-match-string): 2nd arg must be a string: %s" str))
    (unless (or (stringp new) (functionp new))
      (error "(hypb:replace-match-string): 3rd arg must be a string or function: %s"
	     new))
    (replace-regexp-in-string regexp new str fixedcase literal)))

(defun hypb:return-process-output (program &optional infile &rest args)
  "Return as a string the output from external PROGRAM with INFILE for input.
Rest of ARGS are passed as arguments to PROGRAM.
Removes any trailing newline at the end of the output."
  (let ((buf (get-buffer-create "*test-output*"))
	(output))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (apply 'call-process program infile buf nil args)
      (setq output (buffer-string))
      ;; Remove trailing newline from output.
      (when (> (length output) 0)
        (setq output (substring output 0 -1)))
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    output))

(defun hypb:remove-lines (regexp)
 "Remove lines containing match for REGEXP within the active region or to the end of buffer."
    (interactive "sRemove lines with match for regexp: ")
    (flush-lines regexp nil nil t))

;;;###autoload
(defun hypb:rgrep (pattern &optional prefx-arg)
  "Recursively grep with symbol at point or PATTERN over all non-backup and non-autosave files in the current directory tree.
If in an Emacs Lisp mode buffer and no PREFX-ARG is given, limit search to only .el and .el.gz files."
  (interactive (list (if (and (not current-prefix-arg) (equal (buffer-name) "*Locate*"))
			 (read-string "Grep files listed here for: ")
		       (let ((default (symbol-at-point)))
			 (when default (setq default (symbol-name default)))
			 (read-string (format "Rgrep below current dir for%s: "
					      (if default
						  (format " (default %s)" default)
						""))
				      nil nil default)))
		     current-prefix-arg))
  (let* ((delim (cond ((not (string-match "\'" pattern)) ?\')
			      ((not (string-match "\"" pattern)) ?\")
			      ((not (string-match "=" pattern)) ?=)
			      (t ?@)))
	 (grep-cmd
	  (if (and (not current-prefix-arg) (equal (buffer-name) "*Locate*"))
	      (format "%s -e \%c%s\%c %s" hypb:rgrep-command delim pattern delim (hypb:locate-pathnames))
	    (format "%s %s -e \%c%s\%c ."
		    hypb:rgrep-command
		    (if (and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
			     (not prefx-arg))
			(if (string-match "\\`rg " hypb:rgrep-command)
			    "-g \"*.el\" -g \"*.el.gz\""
			  "--include=\"*.el\" --include=\"*.el.gz\"")
		      (if (string-match "\\`rg " hypb:rgrep-command)
			  "-g \"!*~\" -g \"!#*\" -g \"!TAGS\""
			"--exclude=\".git\" --exclude=\"CVS\" --exclude=\"*~\" --exclude=\"#*\" --exclude=\"TAGS\""))
		    delim pattern delim))))
    (setq this-command `(grep ,grep-cmd))
    (push this-command command-history)
    (grep grep-cmd)))

(defun hypb:save-lines (regexp)
 "Save only lines containing match for REGEXP within the active region or to the end of buffer."
    (interactive "sSave lines with match for regexp: ")
    (keep-lines regexp nil nil t))

(defmacro hypb:save-selected-window-and-input-focus (&rest body)
  "Execute BODY, then restore the selected window in each frame and the previously selected frame with input focus.
The value returned is the value of the last form in BODY."
  `(let ((frame (selected-frame)))
     (prog1 (save-selected-window ,@body)
       (select-frame-set-input-focus frame))))

(defun hypb:select-window-frame (window)
  "Select WINDOW and its frame (set input focus there)."
  (if (window-live-p window)
      (progn (select-window window)
	     (select-frame-set-input-focus (window-frame window)))
    (error "(hypb:select-window-frame): Argument must be a live window, not '%s'" window)))

(defun hypb:set-raw-syntax-descriptor (char raw-descriptor &optional syntax-table)
  "Set the syntax of CHAR to RAW-DESCRIPTOR (syntax table value) in the current syntax table or optional SYNTAX-TABLE.
Return the RAW-DESCRIPTOR.  Use the `syntax-after' function to
retrieve the raw descriptor for a buffer position.

Similar to modify-syntax-entry but uses a raw descriptor
previously extracted from a syntax table to set the value rather
than a string.

Syntax tables are char-tables whose values are encoded as raw
descriptors."
  (aset (or syntax-table (syntax-table)) char raw-descriptor))

(defun hypb:straight-package-plist (pkg-string)
  "Return a property list of package-name, package-download-source and pckage-version for PKG-STRING, else return nil.
This is for the straight.el package manager."
  (when (fboundp #'straight-bug-report-package-info)
    (car (delq nil (mapcar (lambda (pkg-plist)
			     (when (equal (plist-get pkg-plist :package) pkg-string) pkg-plist))
			   (straight-bug-report-package-info))))))

(defun hypb:string-count-matches (regexp str &optional start end)
  "Count occurrences of REGEXP in STR, limited to optional START and END positions.

START is inclusive and indexed from 0; END is exclusive.

This function starts looking for the next match from the end of the
previous match.  Hence, it ignores matches that overlap a previously
found match."
  (let ((str-len (length str))
	(count 0)
	substr)
    (when (and start (or (>= start str-len) (< start 0)))
      (error "(hypb:string-count-matches): 'start' (%d) must be >= 0 and < str length (%d)"
	     start str-len))
    (when (and end (or (> end str-len) (< end 0)))
      (error "(hypb:string-count-matches): 'end' (%d) must be >= 0 and <= str length (%d)"
	     end str-len))
    (setq start (or start 0)
	  end (or end str-len)
	  substr (substring str start end)
	  end (- end start)
	  start 0)
    (while (and (< start str-len)
		(string-match regexp substr start))
      (setq count (1+ count)
	    start (match-end 0)))
    count))

(defun hypb:supercite-p ()
  "Return non-nil iff the Emacs add-on supercite package is in use."
  (let (hook-val)
    (when (memq t (mapcar
		   (lambda (hook-var)
		     (and (boundp hook-var)
			  (progn (setq hook-val (symbol-value hook-var))
			         (cond ((listp hook-val)
				        (when (memq 'sc-cite-original hook-val)
					  t))
				       ((eq hook-val 'sc-cite-original))))))
		   '(mail-citation-hook mail-yank-hooks)))
      t)))

(defun hypb:toggle-isearch-invisible (&optional arg)
  "Toggle interactive invisible searching on or off.
This determines whether to search inside invisible text or not.
Toggles the variable ‘isearch-invisible’ between values
nil and a non-nil value of the option ‘search-invisible’
\(or ‘open’ if ‘search-invisible’ is nil).

With optional prefix ARG > 0, turn on searching invisible text.
If ARG <= 0, turn search only visible text."
  (interactive "P")
  (if (not (boundp 'isearch-invisible))
      (error "(hypb:toggle-isearch-invisible): Feature not supported by the version of Emacs")
    (setq isearch-invisible (if (if (null arg)
				    (not isearch-invisible)
				  (> (prefix-numeric-value arg) 0))
				(or search-invisible 'open)))
    (message "I-search will %ssearch invisible text"
	     (if isearch-invisible "" "not "))))

(defun hypb:user-name ()
  "Return the current user's email or login name (sans any domain name)."
  (if (string-match "@" hyperb:user-email)
      (substring hyperb:user-email 0 (match-beginning 0))
    (user-login-name)))

(defun hypb:window-list (&optional minibuffer-flag)
  "Return a list of Lisp window objects for all Emacs windows in selected frame.
Optional first arg MINIBUFFER-FLAG t means include the minibuffer window
in the list, even if it is not active.  If MINIBUFFER-FLAG is neither t
nor nil it means to not count the minibuffer window even if it is active."
  (window-list nil minibuffer-flag))

(defmacro hypb:with-suppressed-warnings (warnings &rest body)
  "Like `progn', but prevents compiler WARNINGS in BODY.

Defined here for elpa build compatibility which uses Emacs 26 and
does not include `with-suppressed-warnings'.

WARNINGS is an associative list where the first element of each
item is a warning type, and the rest of the elements in each item
are symbols they apply to.  For instance, if you want to suppress
byte compilation warnings about the two obsolete functions `foo'
and `bar', as well as the function `zot' being called with the
wrong number of parameters, say

\(with-suppressed-warnings ((obsolete foo bar)
                           (callargs zot))
  (foo (bar))
  (zot 1 2))

The warnings that can be suppressed are a subset of the warnings
in `byte-compile-warning-types'; see the variable
`byte-compile-warnings' for a fuller explanation of the warning
types.  The types that can be suppressed with this macro are
`free-vars', `callargs', `redefine', `obsolete',
`interactive-only', `lexical', `mapcar', `constants' and
`suspicious'.

For the `mapcar' case, only the `mapcar' function can be used in
the symbol list.  For `suspicious', only `set-buffer' can be used."

  (declare (debug (sexp &optional body)) (indent 1))
  (if (fboundp 'with-suppressed-warnings)
      `(with-suppressed-warnings ,warnings ,@body)
    `(with-no-warnings ,@body)))

;;; ************************************************************************
;;; About Hyperbole Setup
;;; ************************************************************************

(defvar hypb:home-page "https://www.gnu.org/software/hyperbole/"
  "The web home page for Hyperbole.")

(defvar hypb:hyperbole-banner-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]  'hypb:browse-home-page)
    (define-key map [mouse-2]  'hypb:browse-home-page)
    (define-key map "\C-m"     'hypb:browse-home-page)
    map)
  "Keymap used when on the Hyperbole banner glyph.")

;;;###autoload
(defun hypb:display-file-with-logo (file)
  "Display a text FILE in help mode with the Hyperbole banner prepended.
If FILE is not an absolute path, expand it relative to `hyperb:dir'."
  (unless (stringp file)
    (error "(hypb:display-file-with-logo): 'file' must be a string, not '%s'" file))
  (unless (file-name-absolute-p file)
    (setq file (expand-file-name file hyperb:dir)))
  (let ((existing-buf (when (stringp file) (get-file-buffer file))))
    ;; A stub for this function is defined in hversion.el when not running in InfoDock.
    (id-browse-file file)
    (unless existing-buf
      (hypb:insert-hyperbole-banner)
      (goto-char (point-min))
      (skip-syntax-forward "-")
      (set-window-start (selected-window) 1)
      (set-buffer-modified-p nil)
      (help-mode))))

(defun hypb:browse-home-page ()
  "Visit the web home page for Hyperbole."
  (interactive)
  (require 'hsys-www)
  (hact 'www-url hypb:home-page))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hypb:constant-vector-symbol-replace
  (constant-vector sym-to-replace replace-with-sym)
  "Replace symbols within a byte-compiled constant vector."
  (let ((i (length constant-vector))
	constant)
    (while (>= (setq i (1- i)) 0)
      (setq constant (aref constant-vector i))
      (cond ((eq constant sym-to-replace)
	     (aset constant-vector i replace-with-sym))
	    ((and (fboundp 'compiled-function-p)
		  (compiled-function-p constant))
	     (hypb:function-symbol-replace
	      constant sym-to-replace replace-with-sym))))))

(defun hypb:insert-hyperbole-banner ()
  "Display an optional text FILE with the Hyperbole banner prepended.
Without file, the banner is prepended to the current buffer."
  (let ((hyperbole-banner-path (expand-file-name "hyperbole-banner.png" hyperb:dir)))
    (unless (file-readable-p hyperbole-banner-path)
      (setq hyperbole-banner-path (if (fboundp 'locate-data-file)
				      (locate-data-file "hyperbole-banner.png")
				    (expand-file-name "hyperbole-banner.png"
						      data-directory))))
    (if (or (not (fboundp 'create-image))
	    (not (display-graphic-p))
	    (let ((button (next-button (point-min))))
	      (and button (button-has-type-p button 'hyperbole-banner)))
	    (not hyperbole-banner-path)
	    (not (file-readable-p hyperbole-banner-path)))
	;; Either image support is unavailable, the file cannot be read
	;; or the image has already been inserted, so don't reinsert it.
	nil
      (let ((hyperbole-banner (create-image hyperbole-banner-path))
	     (buffer-read-only)
	     button)
	(goto-char (point-min))
	(insert "\n")
	(insert-image hyperbole-banner)
	(insert "\n\n")
	(setq button (make-button (- (point) 3) (- (point) 2) :type 'hyperbole-banner))
	(button-put button 'help-echo (concat "Click to visit " hypb:home-page))
	(button-put button 'action #'hypb:browse-home-page)
	(button-put button 'face 'default)
	(button-put button 'keymap hypb:hyperbole-banner-keymap)))))

(defun hypb:locate-pathnames ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n" nil t 3)
    (replace-regexp-in-string " *\\([^\n]+\\)\n" "\\1 "
			      (buffer-substring-no-properties (point) (point-max)))))

(defun hypb:oct-to-int (oct-num)
  "Return octal integer OCT-NUM converted to a decimal integer."
  (let ((oct-str (int-to-string oct-num))
	(dec-num 0))
    (and (string-match "[^0-7]" oct-str)
	 (error "(hypb:oct-to-int): Bad octal number: %s" oct-str))
    (mapconcat (lambda (o)
		 (setq dec-num (+ (* dec-num 8)
				  (when (and (>= o ?0) (<= o ?7))
				    (- o ?0)))))
	       oct-str "")
    dec-num))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(define-button-type 'hyperbole-banner)

(provide 'hypb)

;;; hypb.el ends here
