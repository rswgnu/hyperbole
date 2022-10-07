;;; hyrolo.el --- Hierarchical, multi-file, easy-to-use contact management system  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     7-Jun-89 at 22:08:29
;; Last-Mod:      7-Oct-22 at 22:03:04 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This is Hyperbole's advanced rolo system, HyRolo, for convenient
;;  management of hierarchical, record-oriented information.  Most
;;  often this is used for contact management but it can quickly be
;;  adapted to most any record-oriented lookup task, for fast retrieval.
;;
;;  See all the autoloaded functions herein for interactive commands.
;;  See the Info manual entry "(hyperbole)HyRolo" for usage information.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'custom) ;; For defface.
(require 'hversion)
(require 'hmail)
(require 'set)
(require 'sort)
(require 'xml)

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (unless (require 'bbdb nil t)
    (defvar bbdb-file nil))
  (unless (require 'google-contacts nil t)
    (defvar google-contacts-buffer-name nil))
  (defvar next-entry-exists nil))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defgroup hyperbole-rolo nil
  "Hyperbole Rolo hierarchical contact manager customizations."
  :group 'hyperbole)

(defcustom hyrolo-date-format "%m/%d/%Y"
  "*Format of date string used in Rolo automatic date stamps.
Default is American style.  See documentation of the function
`format-time-string' for format options."
  :type 'string
  :group 'hyperbole-rolo)

(defvar hyrolo-display-format-function
  (lambda (entry)
    (concat (replace-regexp-in-string "[ \t\n\r]+\\'" "" entry nil t) "\n"))
  "*Function of one argument which modifies the string for display.
The argument is a rolo entry string.")

(defcustom hyrolo-email-format "%s\t\t<%s>"
  "*Format string to use when adding an entry with e-mail addr from a mail msg.
It must contain a %s indicating where to put the entry name and a second
%s indicating where to put the e-mail address."
  :type 'string
  :group 'hyperbole-rolo)

(defvar hyrolo-entry-name-regexp "[-_a-zA-Z0-9@.]+\\( ?, ?[-_a-zA-Z0-9@.]+\\)?"
  "*Regexp matching a hyrolo entry name after matching to `hyrolo-entry-regexp'.")

(defcustom hyrolo-file-suffix-regexp "\\.\\(kotl\\|md\\|org\\|otl\\)$"
  "File suffix regexp used to select files to search with HyRolo."
  :type 'string
  :group 'hyperbole-rolo)

(defcustom hyrolo-find-file-function #'find-file
  "*Function to interactively display a `hyrolo-file-list' file for editing.
Use the `hyrolo-edit' function instead to edit a new or existing entry."
  :type 'function
  :group 'hyperbole-rolo)

(defcustom hyrolo-find-file-noselect-function #'find-file-noselect
  "*Function used by HyRolo to read `hyrolo-file-list' files into Emacs."
  :type 'function
  :group 'hyperbole-rolo)

(defcustom hyrolo-google-contacts-flag t
  "*Non-nil means search Google Contacts on each hyrolo query.
The google-contact package must be loaded and a gpg encryption
executable must be found as well (for Oauth security)."
  :type 'boolean
  :group 'hyperbole-rolo)

(defvar hyrolo-next-match-function #'hyrolo-next-regexp-match
  "Value is the function to find next match within a HyRolo file.
Must take two arguments, `match-pattern' and `headline-only-flag'.
Must leave point within any matched entry or return nil when no
match is found.")


;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function google-contacts  "ext:google-contacts")
(declare-function google-contacts-add-margin-to-text "ext:google-contacts")
(declare-function google-contacts-build-node-list "ext:google-contacts")
(declare-function google-contacts-data  "ext:google-contacts")
(declare-function google-contacts-make-buffer "ext:google-contacts")
(declare-function google-contacts-margin-element "ext:google-contacts")
(declare-function google-contacts-oauth-token "ext:google-contacts")
(declare-function xml-node-child-string "ext:google-contacts")
(declare-function xml-node-get-attribute-type "ext:google-contacts")
(defvar google-contacts-history)
(defvar google-contacts-expire-time)
(defvar google-contacts-query-string)

(declare-function hyrolo-fgrep-logical "hyrolo-logic")

(defvar hproperty:highlight-face)

(defun hyrolo-google-contacts-p ()
  "Non-nil means google contacts package is available and feature is enabled.
Requires `hyrolo-google-contacts-flag' set as non-nil and
google-contacts package and gpg executables to be available for
use."
  (and hyrolo-google-contacts-flag
       (featurep 'google-contacts)
       (boundp 'google-contacts-buffer-name)
       ;; If no gpg encryption executable, Oauth login to Google will fail.
       (or (executable-find "gpg2") (executable-find "gpg"))))

;; '("~/.rolo.otl" "~/.rolo.org")

;;;###autoload
(defun hyrolo-initialize-file-list ()
  "Initialize the list of files used for HyRolo search."
  (interactive)
  (let* ((gcontacts (when (hyrolo-google-contacts-p) google-contacts-buffer-name))
	 (ms "~/.rolo.otl")
	 (posix "~/.rolo.otl")
	 (list (delq nil (if (and (boundp 'bbdb-file) (stringp bbdb-file))
			     (if hyperb:microsoft-os-p
				 (list ms bbdb-file gcontacts)
			       (list  "~/.rolo.otl" bbdb-file gcontacts))
			   (if hyperb:microsoft-os-p (list ms gcontacts) (list posix gcontacts))))))
    (setq hyrolo-file-list list)
    (when (called-interactively-p 'interactive)
      (message "HyRolo Search List: %S" list))
    list))

(define-obsolete-variable-alias 'rolo-file-list 'hyrolo-file-list "06.00")
(defcustom hyrolo-file-list (hyrolo-initialize-file-list)
  "*List of files containing rolo entries.
The first file should be a user-specific rolo file, typically in the home
directory.

A hyrolo-file consists of:
   (1) an optional header beginning with and ending with a line which matches
       hyrolo-hdr-regexp;
   (2) one or more rolo entries which each begin with
       hyrolo-entry-regexp and may be nested."
  :group 'hyperbole-rolo
  :type '(repeat file))

(defcustom hyrolo-highlight-face 'match
  "*Face used to highlight rolo search matches."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-rolo)

(defcustom hyrolo-kill-buffers-after-use nil
  "*Non-nil means kill rolo file buffers after searching them for entries.
Only unmodified buffers are killed."
  :type 'boolean
  :group 'hyperbole-rolo)

(defcustom hyrolo-save-buffers-after-use t
  "*Non-nil means save rolo file after an entry is killed."
  :type 'boolean
  :group 'hyperbole-rolo)

;; Insert or update the entry date each time an entry is added or edited.
(add-hook 'hyrolo-add-hook  #'hyrolo-set-date)
(add-hook 'hyrolo-edit-hook #'hyrolo-set-date)

(defvar hyrolo-yank-reformat-function nil
  "*A function of two arguments, START and END, invoked after a hyrolo-yank.
It should reformat the region given by the arguments to some preferred style.
Default value is nil, meaning no reformmating is done.")

;;; ************************************************************************
;;; Commands
;;; ************************************************************************

;;;###autoload
(defun hyrolo-add (name &optional file)
  "Add a new entry in personal rolo for NAME.
Last name first is best, e.g. \"Smith, John\".
With prefix argument, prompts for optional FILE to add entry within.
NAME may be of the form: parent/child to insert child below a parent
entry which begins with the parent string."
  (interactive
   (progn
     (unless (fboundp 'mail-fetch-field)
       (require 'mail-utils))
     (let* ((lst (hyrolo-name-and-email))
	    (name (car lst))
	    (email (car (cdr lst)))
	    (entry (read-string "Name to add to rolo: "
				(or name email))))
       (list (if (and email name
		      (string-match (concat "\\`" (regexp-quote entry)) name))
		 (format hyrolo-email-format entry email) entry)
	     current-prefix-arg))))
  (when (or (not (stringp name)) (string-equal name ""))
    (error "(hyrolo-add): Invalid name: `%s'" name))
  (when (and (called-interactively-p 'interactive) file)
    (setq file (completing-read "File to add to: "
				(mapcar #'list hyrolo-file-list))))
  (unless file
    (setq file (car hyrolo-file-list)))
  (cond ((and file (or (not (stringp file)) (string-equal file "")))
	 (error "(hyrolo-add): Invalid file: `%s'" file))
	((and (file-exists-p file) (not (file-readable-p file)))
	 (error "(hyrolo-add): File not readable: `%s'" file))
	((not (file-writable-p file))
	 (error "(hyrolo-add): File not writable: `%s'" file)))
  (set-buffer (or (get-file-buffer file)
		  (hyrolo-find-file-noselect file)))
  (when (called-interactively-p 'interactive)
    (message "Locating insertion point for `%s'..." name))
  (let ((parent "")
	(level "")
	(entry-regexp (default-value 'hyrolo-entry-regexp))
	end)
    (hyrolo-widen)
    (goto-char (point-min))
    ;; If name includes slash level separator character, walk down
    ;; existing matching tree of entries to find insertion point.
    (while (string-match "\\`[^\]\[/<>{}\"]*/" name)
      (setq end (1- (match-end 0))
	    parent (substring name 0 end)
	    name (substring name (min (1+ end) (length name))))
      (if (re-search-forward
	   (concat entry-regexp (regexp-quote parent) "\\s-") nil t)
	  (setq level (match-string-no-properties hyrolo-entry-group-number))
	(error "(hyrolo-add): Insertion failed, `%s' parent entry not found in \"%s\""
	       parent file)))
    (narrow-to-region (point) (progn (hyrolo-to-entry-end t (length level)) (point)))
    (let* ((name-level (concat level "*"))
	   (level-len (length name-level))
	   (first-char (aref name 0))
	   (entry "")
	   (entry-spc "")
	   (entry-level-len)
	   (match)
	   (again t))
      ;; Speed up entry insertion point location if this is a first-level
      ;; entry by moving to an entry with the same (or nearest) first character
      ;; to that of `name'.
      (if (and (= level-len 1)
	       (equal entry-regexp "^\\(\\*+\\)\\([ \t]+\\)"))
	  (let ((case-fold-search))
	    (goto-char (point-min))
	    (if (re-search-forward (concat entry-regexp
					   (regexp-quote (char-to-string first-char)))
				   nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max))
	      (when (and (> first-char ?0)
			 (re-search-backward
			  (concat "^\\*[ \t]+["
				  (substring
				   "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz"
				   0 (min (- first-char ?0) 62))
				  "])")
			  nil t))
		(goto-char (match-end 0))
		(hyrolo-to-entry-end t level-len)
		;; Now at the insertion point, immediately after
		;; the last existing entry whose first character
		;; is less than that of `name'.  Setting `again'
		;; to nil prevents further searching for an
		;; insertion point.
		(setq again nil))))
	(goto-char (point-min)))

      (while (and again (re-search-forward entry-regexp nil 'end))
	(setq entry-level-len (length (match-string-no-properties hyrolo-entry-group-number)))
	(if (/= entry-level-len level-len)
	    (hyrolo-to-entry-end t entry-level-len)
	  (setq entry-spc (match-string-no-properties hyrolo-entry-trailing-space-group-number)
		entry (buffer-substring-no-properties (point)
						      (save-excursion
							(re-search-forward hyrolo-entry-name-regexp nil t)
							(point))))
	  (when (and (eq major-mode #'markdown-mode)
		     (string-match "\\`.*#+" entry-spc))
	    (setq entry-spc (substring entry-spc (length (match-string 0 entry-spc)))))
	  (cond ((string-lessp entry name)
		 (hyrolo-to-entry-end t entry-level-len))
		((string-lessp name entry)
		 (setq again nil) (beginning-of-line))
		(t ;; found existing entry matching name
		 (setq again nil match t)))))
      (setq buffer-read-only nil)
      (unless match
	(unless (zerop (current-column))
	  (insert "\n"))
	(insert (concat level "*")
		(if (string-equal entry-spc "") "   " entry-spc)
		name "\n")
	(backward-char 1))
      ;; hyrolo-to-buffer may move point from its desired location, so
      ;; restore it.
      (let ((opoint (point)))
	(hyrolo-widen)
	(hyrolo-to-buffer (current-buffer))
	(goto-char opoint))
      (when (derived-mode-p 'kotl-mode)
	(kotl-mode:to-valid-position))
      (run-hooks 'hyrolo-add-hook)
      (when (called-interactively-p 'interactive)
	(message "Edit entry at point.")))))

;;;###autoload
(defun hyrolo-display-matches (&optional display-buf return-to-buffer)
  "Display optional DISPLAY-BUF buffer of previously found rolo matches.
If DISPLAY-BUF is nil, use the value in `hyrolo-display-buffer'.
Second arg RETURN-TO-BUFFER is the buffer to leave point within
after the display."
  (interactive)
  (unless display-buf
    (setq display-buf (get-buffer hyrolo-display-buffer)))
  (unless display-buf
    (error "(hyrolo-display-matches): Search the rolo first"))
  ;; Save current window configuration if rolo match buffer is not
  ;; displayed in one of the windows already.
  (or
   ;; Handle both Emacs V18 and V19 versions of get-buffer-window.
   (condition-case ()
       (get-buffer-window display-buf (selected-frame))
     (error (get-buffer-window display-buf)))
   (setq hyrolo--wconfig (current-window-configuration)))
  (hyrolo-to-buffer display-buf)
  (when (fboundp 'hproperty:but-create)
    (hproperty:but-create))
  (hyrolo-shrink-window)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (run-hooks 'hyrolo-display-hook)
  ;; Leave point in match buffer unless a specific RETURN-TO-BUFFER has
  ;; been specified.  Use {q} to quit and restore display.
  (when return-to-buffer
    (hyrolo-to-buffer return-to-buffer)))

;;;###autoload
(defun hyrolo-edit (&optional name file)
  "Edit a rolo entry given by optional NAME within `hyrolo-file-list'.
With prefix argument, prompt for optional FILE within which to
locate entry.  With no NAME arg, simply displays FILE or first
entry in `hyrolo-file-list' in an editable mode.  NAME may be of
the form: parent/child to edit child below a parent entry which
begins with the parent string."
  (interactive "sEdit rolo entry named: \nP")
  (when (string-equal name "")
    (setq name nil))
  (and name (not (stringp name))
       (error "(hyrolo-edit): Invalid name: `%s'" name))
  (when (and (called-interactively-p 'interactive) current-prefix-arg)
    (if (= (length hyrolo-file-list) 1)
	(setq file (car hyrolo-file-list))
      (setq file (completing-read "Entry's File: "
				  (mapcar #'list hyrolo-file-list)))))
  (let ((found-point) (file-list (if file (list file) hyrolo-file-list)))
    (or file (setq file (car file-list)))
    (if (null name)
	(progn (if (not (file-writable-p file))
		   (error "(hyrolo-edit): File not writable: `%s'" file))
	       (find-file-other-window file) (setq buffer-read-only nil))
      (if (setq found-point (hyrolo-to name file-list))
	  (progn
	    (setq file buffer-file-name)
	    (if (file-writable-p file)
		(setq buffer-read-only nil)
	      (message
	       "(hyrolo-edit): Entry found but file not writable: `%s'" file)
	      (beep))
	    (hyrolo-to-buffer (current-buffer)))
	(message "(hyrolo-edit): `%s' not found." name)
	(beep)
	(hyrolo-to-buffer (or (get-file-buffer (car file-list))
			      (hyrolo-find-file-noselect (car file-list))))
	(setq buffer-read-only nil))
      (hyrolo-widen)
      ;; hyrolo-to-buffer may have moved point from its desired location, so
      ;; restore it.
      (when found-point
	(goto-char found-point))
      (when (derived-mode-p 'kotl-mode)
	(kotl-mode:to-valid-position))
      (run-hooks 'hyrolo-edit-hook))))

(defun hyrolo-edit-entry ()
  "Edit the source entry of the rolo match buffer entry at point.
Return entry name if found, else nil."
  (interactive)
  (let ((name (hyrolo-name-at))
	src)
    (if name
	(progn (setq src (hbut:to-key-src))
	       (cond ((and (boundp 'bbdb-file) (stringp bbdb-file) (equal src (expand-file-name bbdb-file)))
		      ;; For now, can't edit an entry from the bbdb database, signal an error.
		      (error "(hyrolo-edit-entry): BBDB entries are not editable"))
		     ((and (hyrolo-google-contacts-p) (equal src (get-buffer google-contacts-buffer-name)))
		      ;; For now, can't edit an entry from Google Contacts, signal an error.
		      (error "(hyrolo-edit-entry): Google Contacts entries are not editable"))
		     (t (hyrolo-edit name src)
			name)))
      (error "(hyrolo-edit-entry): Move to an entry to edit it"))))

;;;###autoload
(defun hyrolo-fgrep (string &optional max-matches hyrolo-file count-only headline-only no-display)
  "Display rolo entries matching STRING or a logical match expression.
Return count of matches.

To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or `hyrolo-file-list'.  Default is to find all matching entries.
Each entry is displayed with all of its sub-entries.  Optional COUNT-ONLY
non-nil skips retrieval of matching entries.  Optional HEADLINE-ONLY searches
only the first line of entries, not the full text.  Optional NO-DISPLAY non-nil
retrieves entries but does not display them.

Nil value of MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to the inverse of
that number of entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
`hyrolo-file-list' and the function `hyrolo-fgrep-logical' for documentation on
the logical sexpression matching."
  (interactive "sFind rolo string (or logical sexpression): \nP")
  (let ((total-matches 0))
    (if (string-match "\(\\(and\\|or\\|xor\\|not\\)\\>" string)
	(progn
	  ;; Search string contains embedded logic operators.
	  ;; First try to match logical sexpression within a single
	  ;; subentry to minimize entries displayed.  If no match,
	  ;; then match across ancestors and descendants.
	  (when (zerop (setq total-matches (hyrolo-fgrep-logical string count-only nil t)))
	    (hyrolo-fgrep-logical string count-only t t)))
      (setq total-matches (hyrolo-grep (regexp-quote string) max-matches
				       hyrolo-file count-only headline-only no-display)))
    (if (called-interactively-p 'interactive)
	(message "%s matching entr%s found in rolo."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-find-file (&optional file find-function &rest args)
  "Select and edit a FILE in `hyrolo-file-list' with FIND-FUNCTION.
Default to the first listed file when not given a prefix arg."
  (interactive "P")
  (when (or (called-interactively-p 'interactive)
	    (null file))
    (if (or (= (length hyrolo-file-list) 1)
	    (not current-prefix-arg))
	(setq file (car hyrolo-file-list))
      (setq file (completing-read "Edit HyRolo file: "
				  (mapcar #'list hyrolo-file-list)))))
  (when (stringp file)
    (prog1 (apply (or find-function hyrolo-find-file-function) file args)
      (setq buffer-read-only nil))))

;;;###autoload
(defun hyrolo-find-file-noselect (&optional file)
  "HyRolo function to read a FILE in literally.
It uses the setting of `hyrolo-find-file-noselect-function'."
  (let (enable-local-variables)
    (if (string-match "\\.org$" file)
	(let ((find-file-literally t))
	  (hyrolo-find-file file hyrolo-find-file-noselect-function nil t))
      (hyrolo-find-file file hyrolo-find-file-noselect-function))))

(defun hyrolo-forward-visible-line (&optional arg)
  "Move forward by optional ARG lines (default = 1).
Ignore currently invisible newlines only.
If ARG is negative, move backward -ARG lines.
If ARG is zero, move to the beginning of the current line."
  (unless arg
    (setq arg 1))
  (forward-visible-line arg))

;;;###autoload
(defun hyrolo-grep (regexp &optional max-matches hyrolo-file-or-bufs count-only headline-only no-display)
  "Display rolo entries matching REGEXP and return count of matches.
To a maximum of prefix arg MAX-MATCHES, in buffer(s) from
optional HYROLO-FILE-OR-BUFS or hyrolo-file-list.  Default is to
find all matching entries.  Each entry is displayed with all of
its sub-entries.  Optional COUNT-ONLY non-nil means don't
retrieve and don't display matching entries.  Optional
HEADLINE-ONLY searches only the first line of entries, not the
full text.  Optional NO-DISPLAY non-nil retrieves entries but
does not display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
\`hyrolo-file-list'."
  (interactive "sFind rolo regular expression: \nP")
  (unless (or (integerp max-matches) (memq max-matches '(nil t)))
    (setq max-matches (prefix-numeric-value max-matches)))
  (let ((hyrolo-file-list
	 (cond ((null hyrolo-file-or-bufs) hyrolo-file-list)
	       ((listp hyrolo-file-or-bufs) hyrolo-file-or-bufs)
	       ((list hyrolo-file-or-bufs))))
	(case-fold-search t)
	(display-buf (unless count-only
		       (hyrolo-set-display-buffer)))
	(total-matches 0)
	(num-matched 0)
	(inserting (or (eq max-matches t)
		       (and (integerp max-matches) (< max-matches 0))))
	(hyrolo-entry-regexps (set:create))
	(outline-regexps (set:create))
	(file)
	hyrolo-buf)
    (unless count-only
      (setq buffer-read-only nil)
      (unless inserting
	(erase-buffer)))
    (while (and (setq file (car hyrolo-file-list))
		(or (not (integerp max-matches))
		    (< total-matches (max max-matches (- max-matches)))))
      (setq hyrolo-buf (hyrolo-find-file-noselect file)
	    hyrolo-entry-regexps (set:add (buffer-local-value 'hyrolo-entry-regexp hyrolo-buf)
					  hyrolo-entry-regexps)
	    outline-regexps (set:add (buffer-local-value 'outline-regexp hyrolo-buf)
				     outline-regexps)
	    hyrolo-file-list (cdr hyrolo-file-list)
	    num-matched (cond ((and (featurep 'bbdb) (equal file bbdb-file))
			       (hyrolo-bbdb-grep-file file regexp max-matches count-only))
			      ((and (hyrolo-google-contacts-p) (equal file google-contacts-buffer-name))
			       (hyrolo-retrieve-google-contacts (regexp-quote regexp))
			       (hyrolo-google-contacts-grep-file file regexp max-matches count-only))
			      (t (hyrolo-grep-file file regexp max-matches count-only headline-only)))
	    total-matches (+ total-matches num-matched))
      (when (integerp max-matches)
	(setq max-matches
	      (if (>= max-matches 0)
		  (- max-matches num-matched)
		(+ max-matches num-matched)))))
    (unless (or count-only no-display inserting (= total-matches 0))
      (set-buffer display-buf)
      (when hyrolo-entry-regexps
	(setq hyrolo-entry-regexp (string-join hyrolo-entry-regexps "\\|"))
	(unless (string-prefix-p hyrolo-hdr-regexp hyrolo-entry-regexp)
	  (setq hyrolo-entry-regexp (concat hyrolo-hdr-regexp "\\|" hyrolo-entry-regexp))))
      (when outline-regexps
	(setq outline-regexp (string-join outline-regexps "\\|"))
	(unless (string-prefix-p hyrolo-hdr-regexp outline-regexp)
	  (setq outline-regexp (concat hyrolo-hdr-regexp "\\|" outline-regexp))))
      (hyrolo-display-matches display-buf))
    (when (called-interactively-p 'interactive)
      (message "%s matching entr%s found in rolo."
	       (if (= total-matches 0) "No" total-matches)
	       (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-grep-or-fgrep (&optional arg)
  "Grep over `hyrolo-file-list' and display the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match."
  (interactive "P")
  (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep)))

(defun hyrolo-hide-subtree ()
  "Move back to the start of current subtree and hide everything after the heading.

Necessary, since with reveal-mode active, outline-hide-subtree works
only if on the heading line of the subtree."
  (interactive)
  (let ((opoint (point)))
    (forward-line 0)
    (unless (looking-at outline-regexp)
      (outline-previous-visible-heading 1))
    (if (looking-at outline-regexp)
	(outline-hide-subtree)
      (goto-char opoint))))

(defun hyrolo-isearch (&optional arg)
  "Interactively search forward for the next occurrence of current match string.
Then add characters to further narrow the search.  With optional
prefix ARG non-nil, search for the current match regular
expression rather than string."
  (interactive "P")
  (if arg
      (hyrolo-isearch-regexp)
    (hyrolo-verify)
    (if hyrolo-match-regexp
	(progn (setq unread-command-events
		     (append unread-command-events (string-to-list (regexp-quote hyrolo-match-regexp))))
	       (let ((case-fold-search t))
		 (isearch-forward)))
      (error (substitute-command-keys "(hyrolo-isearch): Use {\\[hyrolo-grep-or-fgrep]} to do an initial search")))))

(defun hyrolo-isearch-regexp (&optional arg)
  "Interactively search forward for the next occurrence of current match string.
Then add characters to further narrow the search.  With optional
prefix ARG non-nil, search for the current match regular
expression rather than string."
  (interactive "P")
  (if arg
      (hyrolo-isearch)
    (hyrolo-isearch-for-regexp hyrolo-match-regexp t)))

(defun hyrolo-verify ()
  "Verify point is in a HyRolo or HyNote match buffer."
  (when (not (member (buffer-name) (list hyrolo-display-buffer
					 (and (car hyrolo-file-list)
					      (file-name-nondirectory (car hyrolo-file-list)))
					 (when (boundp 'hynote-display-buffer)
					   hynote-display-buffer)
					 (when (boundp 'hynote-file-list)
					   (and (car hynote-file-list)
						(file-name-nondirectory (car hynote-file-list)))))))
    (error "(HyRolo): Use this command in HyRolo/HyNote match buffers or primary file buffers")))

;;;###autoload
(defun hyrolo-kill (name &optional file)
  "Kill a rolo entry given by NAME within `hyrolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
NAME may be of the form: parent/child to kill child below a parent entry
which begins with the parent string.
Return t if entry is killed, nil otherwise."
  (interactive "sKill rolo entry named: \nP")
  (if (or (not (stringp name)) (string-equal name "") (string-match "\\*" name))
      (error "(hyrolo-kill): Invalid name: `%s'" name))
  (if (and (called-interactively-p 'interactive) current-prefix-arg)
      (setq file (completing-read "Entry's File: "
				  (mapcar #'list hyrolo-file-list))))
  (let ((file-list (if file (list file) hyrolo-file-list))
	(killed))
    (or file (setq file (car file-list)))
    (if (hyrolo-to name file-list)
	(progn
	  (setq file buffer-file-name)
	  (if (file-writable-p file)
	      (let ((kill-op
		     (lambda (start level-len)
		       (kill-region
			start (hyrolo-to-entry-end t level-len))
		       (setq killed t)
		       (hyrolo-save-buffer)
		       (hyrolo-kill-buffer)))
		    (case-fold-search)
		    start end level-len)
		(setq buffer-read-only nil)
		(re-search-backward hyrolo-entry-regexp nil t)
		(setq end (match-end 0))
		(setq start (line-beginning-position)
		      level-len (length (buffer-substring-no-properties start end)))
		(goto-char end)
		(skip-chars-forward " \t")
		(if (called-interactively-p 'interactive)
		    (let ((entry-line (buffer-substring-no-properties
				       (point)
				       (min (+ (point) 60)
					    (progn (end-of-line) (point))))))
		      (if (y-or-n-p (format "Kill `%s...'? " entry-line))
			  (progn
			    (funcall kill-op start level-len)
			    (message "Killed"))
			(message "Aborted")))
		  (funcall kill-op start level-len)))
	    (message
	     "(hyrolo-kill): Entry found but file not writable: `%s'" file)
	    (beep)))
      (message "(hyrolo-kill): `%s' not found." name)
      (beep))
    killed))

(defun hyrolo-locate ()
  "Interactively search for an entry beginning with a set of search characters."
  (interactive)
  (hyrolo-isearch-for-regexp hyrolo-entry-regexp nil))

(defun hyrolo-mail-to ()
  "Start composing mail addressed to the first e-mail address at or after point."
  (interactive)
  (let ((opoint (point)) button)
    (skip-chars-backward "^ \t\n\r<>")
    (if (and (re-search-forward mail-address-regexp nil t)
	     (goto-char (match-beginning 1))
	     (setq button (ibut:at-p)))
	(hui:hbut-act button)
      (goto-char opoint)
      (beep)
      (message "(hyrolo-mail-to): Invalid buffer or no e-mail address found"))))

(defun hyrolo-next-match ()
  "Move point forward to the start of the next rolo search match.
Raise an error if a match is not found."
  (interactive)
  (hyrolo-verify)
  (let ((start (point))
	(case-fold-search t)
	(prior-regexp-search (stringp hyrolo-match-regexp)))
    (when (and prior-regexp-search (looking-at hyrolo-match-regexp))
      (goto-char (match-end 0)))
    (if (and prior-regexp-search (re-search-forward hyrolo-match-regexp nil t))
	(goto-char (match-beginning 0))
      (goto-char start)
      (if prior-regexp-search
	  (error
	   "(hyrolo-next-match): No following matches for \"%s\"" hyrolo-match-regexp)
	(error (substitute-command-keys "(hyrolo-next-match): Use {\\[hyrolo-grep-or-fgrep]} to do a search first"))))))

(defun hyrolo-overview (levels-to-show)
  "Show the first line of all levels of rolo matches.
With a prefix argument of LEVELS-TO-SHOW > 0, show the first
lines of entries only to that depth."
  (interactive "P")
  (hyrolo-verify)
  ;; Use {t} to display top-level cells only.
  (if (or (null levels-to-show)
	  (if (called-interactively-p 'interactive)
	      (progn (setq levels-to-show (prefix-numeric-value current-prefix-arg))
		     (<= levels-to-show 0))
	    (not (integerp levels-to-show))))
      (setq levels-to-show 100))
  (hyrolo-hide-subtree) ;; Ensure reveal-mode does not expand current entry.
  (hyrolo-show-levels levels-to-show))

(defun hyrolo-previous-match ()
  "Move point back to the start of the previous rolo search match.
This could be the current match if point is past its `hyrolo-match-regexp'.
Raise an error if a match is not found."
  (interactive)
  (hyrolo-verify)
  (if hyrolo-match-regexp
      (let ((case-fold-search t))
	(or (re-search-backward hyrolo-match-regexp nil t)
	    (error
	     "(hyrolo-previous-match): No prior matches for \"%s\"" hyrolo-match-regexp)))
    (error (substitute-command-keys "(hyrolo-previous-match): Use {\\[hyrolo-grep-or-fgrep]} to do an initial search"))))

(defun hyrolo-prompt (keyboard-function prompt)
  "Use KEYBOARD-FUNCTION to PROMPT for a yes/no answer."
  (funcall keyboard-function prompt))

(defun hyrolo-quit ()
  "Quit from the rolo match buffer and restore the prior frame display."
  (interactive)
  (hyrolo-verify)
  (bury-buffer)
  (and hyrolo--wconfig (window-configuration-p hyrolo--wconfig)
       (set-window-configuration hyrolo--wconfig)))

(defun hyrolo-rename (old-file new-file)
  "Prompt user to rename OLD-FILE to NEW-FILE."
  (interactive (if hyperb:microsoft-os-p
		   '("c:/_rolo.otl" "~/.rolo.otl")
		 '("~/.rolodex.otl" "~/.rolo.otl")))
  (if (and (equal (car hyrolo-file-list) new-file)
	   (file-readable-p old-file)
	   (progn (beep)
		  (or (hyrolo-prompt
		       'y-or-n-p
		       (format "(hyrolo-rename): Rename \"%s\" to the new standard \"%s\"? "
			       old-file new-file))
		      ;; Setup to get rolo matches from OLD-FILE.
		      (progn (setq hyrolo-file-list
				   (cons old-file (cdr hyrolo-file-list)))
			     nil))))
      (progn (rename-file old-file new-file 1)
	     ;; Also rename backup file if it exists.
	     (when (file-readable-p (concat old-file "~"))
	       (rename-file (concat old-file "~") (concat new-file "~") 1))
	     (when (get-file-buffer old-file)
	       (with-current-buffer (get-file-buffer old-file)
		 (rename-buffer (file-name-nondirectory new-file))
		 (setq buffer-file-name (expand-file-name new-file))))
	     (message "(HyRolo): Your personal rolo file is now: \"%s\"."
		      new-file))))

(defun hyrolo-set-display-buffer ()
  (prog1 (set-buffer (get-buffer-create hyrolo-display-buffer))
    (unless (eq major-mode 'hyrolo-mode)
      (hyrolo-mode))
    (setq buffer-read-only nil)))

;;;###autoload
(defun hyrolo-sort (&optional hyrolo-file)
  "Sort up to 14 levels of entries in HYROLO-FILE (default is personal rolo).
Assume entries are delimited by one or more `*' characters.
Return list of number of groupings at each entry level."
  (interactive
   (list (let ((default "")
	       (file))
	   (setq file
		 (completing-read
		  (format "Sort rolo file (default %s): "
			  (file-name-nondirectory
			   (setq default
				 (if (and buffer-file-name
					  (memq
					   t (mapcar
					      (lambda (file)
						(equal buffer-file-name
						       (expand-file-name file)))
					      hyrolo-file-list)))
				     buffer-file-name
				   (car hyrolo-file-list)))))
		  (mapcar #'list hyrolo-file-list)))
	   (if (string-equal file "") default file))))
  (when (or (not hyrolo-file) (equal hyrolo-file ""))
    (setq hyrolo-file (car hyrolo-file-list)))
  (unless (and (stringp hyrolo-file) (file-readable-p hyrolo-file))
    (error "(hyrolo-sort): Invalid or unreadable file: %s" hyrolo-file))
  (let ((level-regexp (regexp-quote "**************"))
	(entries-per-level-list)
	(n))
    (while (not (string-empty-p level-regexp))
      (setq n (hyrolo-sort-level hyrolo-file level-regexp))
      (when (or (/= n 0) entries-per-level-list)
	(setq entries-per-level-list (cons (list (/ (length level-regexp) 2) n)
					   entries-per-level-list)))
      ;; Subtract 2 here because there are two chars per star when
      ;; regexp-quoted: \\*
      (setq level-regexp (substring level-regexp 0 (- (length level-regexp) 2))))
    (goto-char (point-min))
    (hyrolo-kill-buffer (current-buffer))
    entries-per-level-list))

(defun hyrolo-sort-level (hyrolo-file level-regexp &optional max-groupings)
  "Sort groupings of entries in HYROLO-FILE at hierarchy level LEVEL-REGEXP.
To a maximum of optional MAX-GROUPINGS.  Nil value of MAX-GROUPINGS means all
groupings at the given level.  LEVEL-REGEXP should simply match the text of
any rolo entry of the given level, not the beginning of a line (^); an
example, might be (regexp-quote \"**\") to match level two.  Return number
of groupings sorted."
  (interactive "sSort rolo file: \nRegexp for level's entries: \nP")
  (outline-hide-sublevels (/ (length level-regexp) 2))
  (let ((sort-fold-case t))
    (hyrolo-map-level
     (lambda (start end) (hyrolo-sort-lines nil start end))
     hyrolo-file
     level-regexp
     max-groupings)))

;; This wraps forward-visible-line, making its ARG optional, making
;; its calling convention match that of forward-line.

;; Derived from `sort-lines' in "sort.el" since through at least Emacs 25.0
;; invisible lines are not grouped with the prior visible line, making
;; rolo entry (or any record) sorts fail.  This next function fixes that.
;; Only the last line changes from the original `sort-lines' function.
(defun hyrolo-sort-lines (reverse beg end)
  "Sort lines in region alphabetically; REVERSE non-nil means descending order.
Interactively, REVERSE is the prefix argument, and BEG and END are the region.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; To make `end-of-line', etc. ignore fields
      (let ((inhibit-field-text-motion t))
	(sort-subr reverse #'hyrolo-forward-visible-line #'end-of-visible-line)))))

;;;###autoload
(defun hyrolo-toggle-datestamps (&optional arg)
  "Toggle whether datestamps are updated when rolo entries are modified.
With optional ARG, turn them on iff ARG is positive."
  (interactive "P")
  (if (or (and arg (<= (prefix-numeric-value arg) 0))
	  (and (not (and arg (> (prefix-numeric-value arg) 0)))
	       (boundp 'hyrolo-add-hook) (listp hyrolo-add-hook)
	       (memq 'hyrolo-set-date hyrolo-add-hook)))
      (progn (remove-hook 'hyrolo-add-hook 'hyrolo-set-date)
	     (remove-hook 'hyrolo-edit-hook 'hyrolo-set-date)
	     (message "Rolo date stamps are now off."))
    (add-hook 'hyrolo-add-hook  #'hyrolo-set-date)
    (add-hook 'hyrolo-edit-hook #'hyrolo-set-date)
    (message "Rolo date stamps are now on.")))

(defun hyrolo-toggle-narrow-to-entry ()
  "Toggle between display of current entry and display of all matched entries.
Useful when bound to a mouse key."
  (interactive)
  (if (buffer-narrowed-p)
      (hyrolo-widen)
    (let (case-fold-search)
      (when (or (looking-at hyrolo-entry-regexp)
		(re-search-backward hyrolo-entry-regexp nil t))
	(forward-char)
	(narrow-to-region (1- (point)) (hyrolo-display-to-entry-end)))))
  (hyrolo-shrink-window)
  (goto-char (point-min)))

(defun hyrolo-top-level ()
  "Show only the first line of all top-level hyrolo matches.
Top-level matches are those with the lowest outline level among the
matched entries."
  (interactive)
  (hyrolo-verify)
  (hyrolo-hide-subtree)
  (hyrolo-show-levels 1))

(defun hyrolo-widen ()
  "Widen non-special HyRolo buffers mainly for adding entries or editing them."
  (unless (eq (get major-mode 'mode-class) 'special)
    (widen)))

;;;###autoload
(defun hyrolo-word (string &optional max-matches hyrolo-file count-only headline-only no-display)
  "Display rolo entries with whole word match for STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or hyrolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY
non-nil skips retrieval of matching entries.  Optional HEADLINE-ONLY searches
only the first line of entries, not the full text.  Optional NO-DISPLAY non-nil
retrieves entries but does not display them.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Return number of entries matched.  See also documentation for the variable
hyrolo-file-list."
  (interactive "sFind rolo whole word matches of: \nP")
  (let ((total-matches (hyrolo-grep (format "\\b%s\\b" (regexp-quote string))
				  max-matches
				  hyrolo-file count-only headline-only no-display)))
    (when (called-interactively-p 'interactive)
      (message "%s matching entr%s found in the rolo."
	       (if (= total-matches 0) "No" total-matches)
	       (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-yank (name &optional regexp-p)
  "Insert at point the first rolo entry matching NAME.
With optional prefix arg, REGEXP-P, treats NAME as a regular expression instead
of a string."
  (interactive "sInsert rolo entry named: \nP")
  (let ((hyrolo-display-buffer (current-buffer))
	(start (point))
	found)
    (save-excursion
      (setq found (if regexp-p
		      (hyrolo-grep name -1)
		    (hyrolo-grep (regexp-quote name) -1))))
    ;; Let user reformat the region just yanked.
    (if (and (= found 1) (fboundp hyrolo-yank-reformat-function))
	(funcall hyrolo-yank-reformat-function start (point)))
    found))

;;; ************************************************************************
;;; Big Brother Database (BBDB) Integration
;;; ************************************************************************

;;;###autoload
(defun hyrolo-bbdb-fgrep (&optional arg)
  "Fgrep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match."
  (interactive "P")
  (hyrolo-bbdb-grep (not arg)))

;;;###autoload
(defun hyrolo-bbdb-grep (&optional arg)
  "Grep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> \".bbdb\"
======================================================================
* Jones     Tom                <tj@groovycat.org>
* Sera      Kate               <uptown@singular.net>
* Yako      Maso               <ym@destination.ny>"
  (interactive "P")
  (require 'bbdb)
  (let ((hyrolo-file-list (list bbdb-file))
	(hyrolo-entry-regexp "^\\[")
	(hyrolo-display-format-function #'hyrolo-bbdb-entry-format)
	;; Kill the bbdb file after use if it is not already in a buffer.
	(hyrolo-kill-buffers-after-use
	 (not (get-file-buffer (expand-file-name bbdb-file))))
	(current-prefix-arg))
    (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep))
    (read-only-mode 0)
    (re-search-forward "^\\*" nil t)
    (beginning-of-line)
    (shell-command-on-region (point) (point-max) "column -s: -t" nil t)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(defun hyrolo-bbdb-grep-file (hyrolo-file-or-buf regexp &optional max-matches count-only)
  "Retrieve entries in bbdb HYROLO-FILE-OR-BUF matching REGEXP.
Find a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Return number of matching entries found."
  (let ((hyrolo-entry-regexp "^\\[")
	(hyrolo-display-format-function #'hyrolo-bbdb-entry-format)
	;; Kill the bbdb file after use if it is not already in a buffer.
	(hyrolo-kill-buffers-after-use
	 (not (get-file-buffer (expand-file-name bbdb-file)))))
    (hyrolo-grep-file hyrolo-file-or-buf regexp max-matches count-only)))

(defun hyrolo-bbdb-entry-format (entry)
  (let ((v (read entry)))
    (format "* %s: %s: <%s>\n" (elt v 1) (elt v 0) (car (elt v 7)))))

;;; ************************************************************************
;;; Google Contacts Integration
;;; ************************************************************************

;;;###autoload
(defun hyrolo-google-contacts-fgrep (&optional arg)
  "Fgrep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match."
  (interactive "P")
  (hyrolo-google-contacts-grep (not arg)))

;;;###autoload
(defun hyrolo-google-contacts-grep (&optional arg)
  "Grep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> <buffer *Google Contacts*>
======================================================================
* Jones     Tom
* Sera      Kate
* Yako      Maso"
  (interactive "P")
  (require 'google-contacts)
  (let ((hyrolo-file-list (list google-contacts-buffer-name))
	;; Kill the google-contacts buffer after use if it is not already in use.
	(hyrolo-kill-buffers-after-use (not (get-buffer google-contacts-buffer-name)))
	(current-prefix-arg))
    (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep))
    (read-only-mode 0)
    (let (case-fold-search)
      (re-search-forward hyrolo-entry-regexp nil t))
    (beginning-of-line)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(defun hyrolo-google-contacts-grep-file (hyrolo-file-or-buf regexp &optional max-matches count-only)
  "Retrieve entries in google-contacts HYROLO-FILE-OR-BUF matching REGEXP.
Find a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Return number of matching entries found."
  ;; Kill the google-contacts buffer after use if it is not already in use.
  (let ((hyrolo-kill-buffers-after-use (not (get-buffer google-contacts-buffer-name))))
    (hyrolo-grep-file hyrolo-file-or-buf regexp max-matches count-only)))

;; Derived from google-contacts.el.
(defun hyrolo-google-contacts-insert-data (contacts _token contact-prefix-string)
  (if (not contacts)
      ;; No contacts, insert a string and return nil
      (insert "No result.")
    (print contacts (get-buffer-create "*contacts-data*"))
    (dolist (contact contacts)
      (let* ((name-value (nth 0 (xml-get-children contact 'gd:name)))
             (fullname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:fullName))))
             (givenname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:givenName))))
             (familyname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:familyName))))

             (nickname (xml-node-child-string (nth 0 (xml-get-children contact 'gContact:nickname))))
             (birthday (xml-get-attribute-or-nil (nth 0 (xml-get-children contact 'gContact:birthday)) 'when))

             (organization-value (nth 0 (xml-get-children contact 'gd:organization)))
             (organization-name (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgName))))
             (organization-title (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgTitle))))

             (notes (xml-node-child-string (nth 0 (xml-get-children contact 'content))))
             ;; Links
             ;; (links (xml-get-children contact 'link))

             ;; Multiple values
             ;; Format is ((rel-type . data) (rel-type . data) … )
             (events (google-contacts-build-node-list contact 'gContact:event
                                                      (xml-get-attribute (nth 0 (xml-get-children child 'gd:when)) 'startTime)))
             (emails (google-contacts-build-node-list contact 'gd:email
                                                      (xml-get-attribute child 'address)))
             (phones (google-contacts-build-node-list contact 'gd:phoneNumber))
             (websites (google-contacts-build-node-list contact 'gContact:website
                                                        (xml-get-attribute child 'href)))
             (relations (google-contacts-build-node-list contact 'gContact:relation))
             (postal-addresses (google-contacts-build-node-list contact 'gd:structuredPostalAddress
                                                                (xml-node-child-string
                                                                 (nth 0 (xml-get-children child 'gd:formattedAddress)))))
             (instant-messaging (google-contacts-build-node-list contact 'gd:im
                                                                 (cons
                                                                  (xml-node-get-attribute-type child 'protocol)
                                                                  (cdr (assoc 'address (xml-node-attributes child))))))
             (beg (point)))
        (unless (and (string-equal familyname "") (string-equal givenname "") (string-equal nickname ""))
	  (insert contact-prefix-string familyname (if (or (string-equal familyname "")
							   (string-equal givenname "")) "" ", ")
		  givenname
		  (if (string-equal nickname "")
		      ""
		    (format " (%s)" nickname))
		  "\n"))

        (unless (and (string-equal organization-name "")
                     (string-equal organization-title ""))
	  (insert (google-contacts-margin-element))
	  (if (string-equal organization-title "")
	      (insert organization-name "\n")
	    (insert organization-title " @ " organization-name "\n")))

        (hyrolo-google-contacts-insert-generic-list emails "E-mails")
        (hyrolo-google-contacts-insert-generic-list phones "Phones")
        (hyrolo-google-contacts-insert-generic-list postal-addresses "Addresses"
                                             (lambda (address)
                                               (google-contacts-add-margin-to-text (cdr address)
                                                                                   (+ 4 (length (car address))))))
        (hyrolo-google-contacts-insert-generic-list websites "Websites"
                                             (lambda (website) (cdr website)))
        (hyrolo-google-contacts-insert-generic-list events "Events")
        (hyrolo-google-contacts-insert-generic-list relations "Relations"
                                             (lambda (relation)
                                               (widget-create 'link
                                                              :button-prefix "" :button-suffix ""
                                                              :action (lambda (widget &optional _event)
                                                                        (google-contacts (widget-value widget)))
                                                              (cdr relation))
                                               ""))
        (hyrolo-google-contacts-insert-generic-list instant-messaging "Instant messaging"
                                             (lambda (im)
                                               (concat (cddr im) " (" (cadr im) ")")))

        (when birthday
          (insert "\n" (google-contacts-margin-element) "Birthday: " birthday "\n"))

        (unless (string-equal notes "")
          (insert "\n" (google-contacts-margin-element) "Notes:  "
                  (google-contacts-add-margin-to-text notes 8)
                  "\n"))

        ;; Insert properties
        (put-text-property beg (1+ beg) 'google-contacts t)
        (when emails
          (put-text-property beg (point)
                             'google-contacts-email (concat fullname " <" (cdr (nth 0 emails)) ">")))))
    (goto-char (point-min)))
  ;; Return contacts
  contacts)

;; Derived from google-contacts.el.
(defun hyrolo-google-contacts-insert-generic-list (items title &optional get-value)
  "Insert a text for rendering ITEMS with TITLE.
Use GET-VALUE fuction to retrieve the value from the cdr of the item,
otherwise just use the cdr of the item."
  (when items
    (insert "\n" (google-contacts-margin-element) (concat title ":\n"))
    (dolist (item items)
      (insert (google-contacts-margin-element) "  "
              (concat (car item) ":") " "
	      (if get-value
                  (funcall get-value item)
                (cdr item))
              "\n"))))

;; Derived from google-contacts.el.
(defun hyrolo-retrieve-google-contacts (&optional query-string force-refresh)
  (interactive
   (list (read-string "Look for: " (car google-contacts-history)
                      'google-contacts-history)
         current-prefix-arg))
  ;; Without this first let binding, the user would be prompted for
  ;; his passphrase on every hyrolo search.  This way it is cached.
  (let* ((plstore-cache-passphrase-for-symmetric-encryption t)
	 (buffer (google-contacts-make-buffer))
         (token (google-contacts-oauth-token))
         (google-contacts-expire-time (if force-refresh 0 google-contacts-expire-time))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (setq google-contacts-query-string query-string)
      (hyrolo-google-contacts-insert-data
       (xml-get-children (google-contacts-data query-string token)
			 'entry)
       token "* "))))

;;; ************************************************************************
;;; Org Package Integrations
;;; ************************************************************************

;;;###autoload
(defun hyrolo-helm-org-rifle (&optional context-only-flag)
  "Search with helm and interactively show all matches from `hyrolo-file-list'.
Prompt for the search pattern.
Only readable .org and .otl files are searched.  With optional
prefix arg CONTEXT-ONLY-FLAG, show only an extra line of context
around a matching line rather than entire entries."
  (interactive "P")
  (unless (package-installed-p 'helm-org-rifle)
    (package-install 'helm-org-rifle))
  (require 'helm-org-rifle)
  (let ((files (seq-filter (lambda (f) (string-match "\\.\\(org\\|otl\\)$" f))
			   (seq-filter #'file-readable-p hyrolo-file-list)))
	(helm-org-rifle-show-full-contents (not context-only-flag)))
    (save-excursion
      (mapc (lambda (file)
	      (set-buffer (hyrolo-find-file-noselect file))
	      (org-mode))
	    files))
    (helm-org-rifle-files files)))

;;;###autoload
(defun hyrolo-helm-org-directory-rifle (&optional context-only-flag)
  "Interactively search over `org-directory'.
With optional prefix arg CONTEXT-ONLY-FLAG, show only an extra
line of context around a matching line rather than entire
entries."
  (interactive)
  (unless (package-installed-p 'helm-org-rifle)
    (package-install 'helm-org-rifle))
  (require 'helm-org-rifle)
  (require 'org)
  (unless (file-readable-p org-directory)
    (make-directory org-directory))
  (if (file-readable-p org-directory)
      (let ((helm-org-rifle-show-full-contents (not context-only-flag)))
	(helm-org-rifle-org-directory))
    (error "(hyrolo-helm-org-directory-rifle): `org-directory', \"%s\", does not exist" org-directory)))

;;;###autoload
(defun hyrolo-helm-org-rifle-directories (&optional context-only-flag &rest dirs)
  "Interactively search over Emacs outline format files in rest of DIRS.
Only readable .org and .otl files are searched.  With optional
prefix arg CONTEXT-ONLY-FLAG, show only an extra line of context
around a matching line rather than entire entries."
  (interactive "P")
  (let ((hyrolo-file-list (hypb:filter-directories "\\.\\(org\\|otl\\)$" dirs)))
    (hyrolo-helm-org-rifle context-only-flag)))

;;;###autoload
(defun hyrolo-org (string &optional max-matches)
  "Search `org-directory' files for STRING or logic-based matches.
OPTIONAL prefix arg, MAX-MATCHES, limits the number of matches
returned to the number given."
  (interactive "sFind Org directory string (or logical sexpression): \nP")
  (require 'org)
  (unless (file-readable-p org-directory)
    (make-directory org-directory))
  (if (file-readable-p org-directory)
      (let ((hyrolo-file-list (directory-files org-directory t "\\.org$")))
	(hyrolo-fgrep string max-matches))
    (error "(hyrolo-org): `org-directory', \"%s\", does not exist" org-directory)))

;;;###autoload
(defun hyrolo-org-roam (string &optional max-matches)
  "Search Org Roam directory files for STRING or logical sexpression.
OPTIONAL prefix arg, MAX-MATCHES, limits the number of matches
returned to the number given."
  (interactive "sFind Org Roam directory string (or logical sexpression): \nP")
  (unless (package-installed-p 'org-roam)
    (package-install #'org-roam))
  (require 'org-roam)
  (unless (file-readable-p org-roam-directory)
    (make-directory org-roam-directory))
  (unless org-roam-db-autosync-mode
    (org-roam-db-autosync-mode))
  (if (file-readable-p org-roam-directory)
      (let ((hyrolo-file-list (directory-files org-roam-directory t "\\.org$")))
	(hyrolo-fgrep string max-matches))
    (error "(hyrolo-org-roam): `org-roam-directory', \"%s\", does not exist" org-roam-directory)))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hyrolo-fgrep-directories (file-regexp &rest dirs)
  "String/logical HyRolo search over files matching FILE-REGEXP in rest of DIRS."
  (hyrolo-search-directories #'hyrolo-fgrep file-regexp dirs))

(defun hyrolo-fgrep-file (hyrolo-file-or-buf string &optional max-matches count-only headline-only)
  "Retrieve entries in HYROLO-FILE-OR-BUF matching STRING.
Retrieve a maximum of optional MAX-MATCHES.

Nil value of MAX-MATCHES means find all matches, t value means
find all matches but omit file headers, negative values mean find
up to the inverse of that number of entries and omit file
headers.  Optional COUNT-ONLY non-nil omits matching entry
display.  Optional HEADLINE-ONLY non-nil searches headlines
only (first line of entries), rather than entire entries.

Return number of matching entries found."
  (hyrolo-grep-file hyrolo-file-or-buf (regexp-quote string) max-matches count-only headline-only))

;;;###autoload
(defun hyrolo-grep-directories (file-regexp &rest dirs)
  "Regexp HyRolo search over files matching FILE-REGEXP in rest of DIRS."
  (hyrolo-search-directories #'hyrolo-grep file-regexp dirs))

(defun hyrolo-next-regexp-match (regexp headline-only)
  "In a HyRolo source buffer, move past next occurrence of REGEXP or return nil."
  (re-search-forward regexp
		     (when headline-only
		       (save-excursion (end-of-visible-line) (point)))
		     t))

(defun hyrolo-grep-file (hyrolo-file-or-buf pattern &optional max-matches count-only headline-only)
  "Retrieve entries in HYROLO-FILE-OR-BUF matching REGEXP.
PATTERN is searched for using the function given by
`hyrolo-next-match-function', so it can be a text property for
example, rather than just a regexp matching buffer text.

Retrieve a maximum of optional MAX-MATCHES.  Nil value of
MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to
the inverse of that number of entries and omit file headers.
Optional COUNT-ONLY non-nil skips display of matching entries.
Optional HEADLINE-ONLY non-nil searches only the first line of
entries, rather than the full text.

Return number of matching entries found."
  ;;
  ;; Save pattern as last rolo search expression.
  (setq hyrolo-match-regexp pattern)
  ;;
  (let ((new-buf-p) (actual-buf)
	;; Disable magit-auto-revert-mode-enable-in-buffers for hyrolo
	;; buffers; not needed and can slow/hang file loading
	(after-change-major-mode-hook
	 (delq 'magit-auto-revert-mode-enable-in-buffers after-change-major-mode-hook)))
    (if (and (or (null max-matches) (eq max-matches t) (integerp max-matches))
	     (or (setq actual-buf (hyrolo-buffer-exists-p hyrolo-file-or-buf))
		 (when (file-exists-p hyrolo-file-or-buf)
		   (setq actual-buf (hyrolo-find-file-noselect hyrolo-file-or-buf)
			 new-buf-p t))))
	(let ((hdr-pos) (num-found 0) (curr-entry-level-len)
	      (incl-hdr t) start next-entry-exists)
	  (when  max-matches
	    (cond ((eq max-matches t)
		   (setq incl-hdr nil max-matches nil))
		  ((< max-matches 0)
		   (setq incl-hdr nil
			 max-matches (- max-matches)))))
	  (set-buffer actual-buf)
	  (when new-buf-p
	    (setq buffer-read-only t))
	  (save-excursion
	    (save-restriction
	      (hyrolo-widen)
	      ;; Ensure no entries in outline mode are hidden.
	      (outline-show-all)
	      (goto-char (point-min))
	      (when (re-search-forward hyrolo-hdr-regexp nil t 2)
		(forward-line)
		(setq hdr-pos (cons (point-min) (point))))
	      (let (case-fold-search)
		(re-search-forward hyrolo-entry-regexp nil t)
		(while (and (or (null max-matches) (< num-found max-matches))
			    (funcall hyrolo-next-match-function pattern headline-only))
		  (re-search-backward hyrolo-entry-regexp nil t)
		  (setq start (point)
			next-entry-exists nil)
		  (re-search-forward hyrolo-entry-regexp nil t)
		  (setq curr-entry-level-len (length (buffer-substring-no-properties start (point))))
		  (hyrolo-to-entry-end t curr-entry-level-len)
		  (or count-only
		      (if (and (zerop num-found) incl-hdr)
			  (let* ((src (or (buffer-file-name actual-buf)
					  actual-buf))
				 (src-line
				  (format
				   (concat (if (boundp 'hbut:source-prefix)
					       hbut:source-prefix
					     "@loc> ")
					   "%s")
				   (prin1-to-string src))))
			    (set-buffer hyrolo-display-buffer)
			    (goto-char (point-max))
			    (if hdr-pos
				(progn
				  (insert-buffer-substring-no-properties
				   actual-buf (car hdr-pos) (cdr hdr-pos))
				  (insert src-line "\n\n"))
			      (insert (format hyrolo-hdr-format src-line)))
			    (set-buffer actual-buf))))
		  (setq num-found (1+ num-found))
		  (or count-only
		      (hyrolo-add-match hyrolo-display-buffer pattern start (point)))))))
	  (hyrolo-kill-buffer actual-buf)
	  num-found)
      0)))

(defun hyrolo-map-level (func hyrolo-file-or-buf level-regexp &optional max-groupings)
  "Perform FUNC on groupings of HYROLO-FILE-OR-BUF entries at level LEVEL-REGEXP,
to a maximum of optional argument MAX-GROUPINGS.  Nil value of MAX-GROUPINGS
means all groupings at the given level.  FUNC should take two arguments, the
start and the end of the region that it should manipulate.  LEVEL-REGEXP
should match the prefix text of any rolo entry of the given level, not the
beginning of a line (^); an example, might be (regexp-quote \"**\") to match
level two.

Return number of groupings matched."
  (let ((actual-buf))
    (if (not (and (or (null max-groupings) (< 0 max-groupings))
		  (or (setq actual-buf (hyrolo-buffer-exists-p hyrolo-file-or-buf))
		      (when (file-readable-p hyrolo-file-or-buf)
			(setq actual-buf (hyrolo-find-file-noselect hyrolo-file-or-buf))
			t))))
	0
      (set-buffer actual-buf)
      (let* ((num-found 0)
	     (total-found 0)
	     (exact-level-regexp (concat "^\\(" level-regexp "\\)[ \t\n\r\f]"))
	     (buffer-read-only)
	     (level-len (/ (length level-regexp) 2)))
	(goto-char (point-min))
	;; Pass buffer header if it exists
	(when (re-search-forward hyrolo-hdr-regexp nil t 2)
	  (forward-line))
	;; With 'max-groupings' non-nil, loop over all following entries
	;; with the same parent matching 'level-regexp'.  Otherwise, maximally
	;; loop over 'max-groupings' such entries.
	(while (and (> level-len 0) (or (null max-groupings) (< total-found max-groupings))
		    (< 0 (setq num-found
			       (hyrolo-map-single-subtree func exact-level-regexp level-len buffer-read-only))))
	  (setq total-found (+ num-found total-found)))
	;; Caller may have adjusted entry visibility, so don't do this: (outline-show-all)
	total-found))))

(defun hyrolo-map-single-subtree (func exact-level-regexp level-len read-only-flag)
  "See doc for `hyrolo-map-level'.  Return number of groupings matched."
  (let* ((start (point))
	 (end 0)
	 (num-found 0)
	 (higher-level-entry-regexp)
	 (buffer-read-only read-only-flag))
    ;; Move to the next instance of 'level-regexp'.
    ;; Although subtrees are hidden, searches will still see them.
    (when (re-search-forward exact-level-regexp nil t)
      ;; First entry exists
      (save-excursion
	(forward-line 0)
	(setq num-found (1+ num-found)
	      start (point)))
      ;; Move to start of next entry at equal
      ;; or higher level else, to buffer end.
      (setq higher-level-entry-regexp (format "^\\(\\*\\{1,%d\\}\\)[ \t]" (1- level-len)))
      (if (and (/= level-len 1)
	       (re-search-forward higher-level-entry-regexp nil t))
	  (forward-line 0)
	(goto-char (point-max))
	(skip-chars-backward " \t\n\r\f"))
      (save-excursion
	(setq end (point))
	(goto-char start)
	(funcall func start end)))
    num-found))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hyrolo-add-match (hyrolo-matches-buffer regexp start end)
  "Insert in HYROLO-MATCHES-BUFFER an entry matching REGEXP from current region.
Entry is inserted before point.  The region is between START to END."
  (let ((hyrolo-buf (current-buffer))
	(hyrolo-entry (buffer-substring start end))
	opoint)
    (set-buffer (get-buffer-create hyrolo-matches-buffer))
    (setq opoint (point))
    (insert (funcall hyrolo-display-format-function hyrolo-entry))
    (hyrolo-highlight-matches regexp opoint (point))
    (set-buffer hyrolo-buf)))

(defun hyrolo-buffer-exists-p (hyrolo-buf)
  "Return buffer given by HYROLO-BUF or nil.
HYROLO-BUF may be a file-name, `buffer-name', or buffer."
  (car (memq (get-buffer (or (and (stringp hyrolo-buf)
				  (get-file-buffer hyrolo-buf))
			     hyrolo-buf))
	     (buffer-list))))

(defun hyrolo-current-date ()
  "Return the current date (a string) in a form used for rolo entry insertion."
  (format-time-string hyrolo-date-format))

(defun hyrolo-display-to-entry-end ()
  "Go to end of current entry, ignoring sub-entries."
  (let (case-fold-search)
    (if (re-search-forward (concat hyrolo-hdr-regexp "\\|"
				   hyrolo-entry-regexp) nil t)
	(progn (beginning-of-line) (point))
      (goto-char (point-max)))))


(defun hyrolo-format-name (name-str first last)
  "Reverse order of NAME-STR field given my regexp match field FIRST and LAST."
  (when (match-beginning last)
    (concat (substring name-str (match-beginning last) (match-end last))
	    ", "
	    (substring name-str (match-beginning first) (match-end first)))))

(defun hyrolo-highlight-matches (regexp start _end)
  "Highlight matches for REGEXP in region from START to END."
  (when (fboundp 'hproperty:but-add)
    (let ((hproperty:but-emphasize-flag))
      (save-excursion
	(goto-char start)
	(while (re-search-forward regexp nil t)
	  (hproperty:but-add (match-beginning 0) (match-end 0)
			     (or hyrolo-highlight-face
				 hproperty:highlight-face)))))))

(defun hyrolo-isearch-for-regexp (regexp fold-search-flag)
  "Interactively search forward for the next occurrence of REGEXP.
Then add characters to further narrow the search."
  (hyrolo-verify)
  (if (stringp regexp)
      (progn (setq unread-command-events
		   (append unread-command-events (string-to-list regexp)))
	     (let ((case-fold-search fold-search-flag))
	       (isearch-forward-regexp)))
    (error "(hyrolo-isearch-for-regexp): 'regexp' must be a string, not: %s" regexp)))

(defun hyrolo-kill-buffer (&optional hyrolo-buf)
  "Kill optional HYROLO-BUF if unchanged and `hyrolo-kill-buffers-after-use' is t.
Default is current buffer."
  (or hyrolo-buf (setq hyrolo-buf (current-buffer)))
  (and hyrolo-kill-buffers-after-use (not (buffer-modified-p hyrolo-buf))
       (kill-buffer hyrolo-buf)))

(defun hyrolo-name-and-email ()
  "If point is in a mail message, return list of (name email-addr) of sender.
Name is returned as `last, first-and-middle'."
  (let ((email) (name) (from))
    (save-window-excursion
      (if (or (hmail:lister-p) (hnews:lister-p))
	  (other-window 1))
      (save-excursion
	(save-restriction
	  (goto-char (point-min))
	  (if (search-forward "\n\n" nil t)
	      (narrow-to-region (point-min) (point)))
	  (setq email (mail-fetch-field "reply-to")
		from  (mail-fetch-field "from")))))
    (if from
	(cond
	 ;; Match: email, email (name), email "name"
	 ((string-match
	   (concat "^\\([^\"<>() \t\n\r\f]+\\)"
		   "\\([ \t]*[(\"][ \t]*\\([^\"()]+\\)[ \t]+"
		   "\\([^\" \t()]+\\)[ \t]*[)\"]\\)?[ \t]*$")
	   from)
	  (setq name (hyrolo-format-name from 3 4))
	  (or email (setq email (match-string 1 from))))
	 ;; Match: <email>, name <email>, "name" <email>
	 ((string-match
	   (concat "^\\(\"?\\([^\"<>()\n]+\\)[ \t]+"
		   "\\([^\" \t()<>]+\\)\"?[ \t]+\\)?"
		   "<\\([^\"<>() \t\n\r\f]+\\)>[ \t]*$")
	   from)
	  (setq name (hyrolo-format-name from 2 3))
	  (or email (setq email (match-string 4 from))))))
    (if (or name email)
	(list name email))))

(defun hyrolo-name-at ()
  "If point is on an entry in `hyrolo-display-buffer', return its name, else nil."
  (when (string-equal (buffer-name) hyrolo-display-buffer)
    (save-excursion
      (beginning-of-line)
      (let (case-fold-search)
	(when (looking-at hyrolo-entry-regexp)
	  (goto-char (match-end 0))
	  (skip-chars-forward " \t")
	  (when (or (looking-at "[^ \t\n\r]+ ?, ?[^ \t\n\r]+")
		    (looking-at "\\( ?[^ \t\n\r]+\\)+"))
	    (match-string-no-properties 0)))))))

(defun hyrolo-save-buffer (&optional hyrolo-buf)
  "Save optional HYROLO-BUF if changed and `hyrolo-save-buffers-after-use' is t.
Default is current buffer.  Used, for example, after a rolo entry is killed."
  (unless hyrolo-buf
    (setq hyrolo-buf (current-buffer)))
  (and hyrolo-save-buffers-after-use (buffer-modified-p hyrolo-buf)
       (set-buffer hyrolo-buf) (save-buffer)))

(defun hyrolo-set-date ()
  "Add a line with the current date at the end of the current hyrolo entry.
Does not add a date if in a Koutline buffer.

Suitable for use as an entry in `hyrolo-add-hook' and `hyrolo-edit-hook'.
The date format is determined by the setting, `hyrolo-date-format', with
a default of MM/DD/YYYY."
  (unless (derived-mode-p 'kotl-mode)
    (save-excursion
      (skip-chars-forward "*")
      (hyrolo-to-entry-end)
      (skip-chars-backward " \t\n\r\f")
      (skip-chars-backward "^\n\r\f")
      (if (looking-at "\\s-+[-0-9./]+\\s-*$") ;; a date
	  (progn (delete-region (point) (match-end 0))
		 (insert "\t" (hyrolo-current-date)))
	(end-of-line)
	(insert "\n\t" (hyrolo-current-date))))))

(defun hyrolo-min-matched-level ()
  "Return the minimum hyrolo level within a single file of matches."
  (goto-char (point-min))
  (let ((min-level (hyrolo-mode-outline-level)))
    (while (outline-next-heading)
      (setq min-level (min min-level (hyrolo-mode-outline-level))))
    min-level))

(defun hyrolo-back-to-visible-point ()
  (interactive)
  (while (and (not (bobp)) (invisible-p (point)))
    ;; Move back one character at a time here because using this fails
    ;; and ends up at the beginning of buffer every time under Emacs 27.1:
    ;; (goto-char (previous-single-char-property-change (point) 'invisible))))
    (goto-char (1- (point)))))

(defun hyrolo-search-directories (search-cmd file-regexp &rest dirs)
  "Search HyRolo over files matching FILE-REGEXP in rest of DIRS."
  (when (or (null file-regexp) (string-empty-p file-regexp))
    (setq file-regexp hyrolo-file-suffix-regexp))
  (let ((hyrolo-file-list (hypb:filter-directories file-regexp dirs)))
    (call-interactively search-cmd)))

(defun hyrolo-show-levels (num-levels)
  "Show only the first line of up to NUM-LEVELS of rolo matches.
NUM-LEVELS must be 1 or greater and is relative to the first
level of matches, so if NUM-LEVELS is 2 and the first level
matched from an outline is level 3, then levels 3 and 4 will be
shown."
  (outline-show-all)
  (save-excursion
     (goto-char (point-min))
     (if (not (re-search-forward hyrolo-hdr-regexp nil t 2))
	 (outline-hide-sublevels num-levels)
       (goto-char (point-min))
       (let (start
	     end
	     max-level-to-show)
	 (while (re-search-forward hyrolo-hdr-regexp nil t 2)
	   (forward-line)
	   (setq start (point)
		 end (if (re-search-forward hyrolo-hdr-regexp nil t)
			 (progn (beginning-of-line) (point))
		       (goto-char (point-max))))
	   (save-restriction
	     (narrow-to-region start end)
	     (if (> num-levels 20)
		 (setq max-level-to-show num-levels)
	       (setq max-level-to-show (+ (hyrolo-min-matched-level)
					  (1- num-levels))))
	     (outline-hide-sublevels max-level-to-show)))))
     (goto-char (point-min))
     ;; This pause forces a window redisplay that maximizes the
     ;; entries displayed for any final location of point.
     (sit-for 0.001))
  ;; Need to leave point on a visible character or since
  ;; hyrolo uses reveal-mode, redisplay will rexpand
  ;; hidden entries to make point visible.
  (hyrolo-back-to-visible-point))

(defun hyrolo-shrink-window ()
  (let* ((lines (count-lines (point-min) (point-max)))
	 (height (window-height))
	 (window-min-height 2)
	 (desired-shrinkage (1- (min (- height lines)))))
    (and (>= lines 0)
	 (/= desired-shrinkage 0)
	 (> (frame-height) (1+ height))
	 (shrink-window
	  (if (< desired-shrinkage 0)
	      (max desired-shrinkage (- height (/ (frame-height) 2)))
	    (min desired-shrinkage (- height window-min-height)))))))

(defun hyrolo-to (name &optional file-list)
  "Move point to entry for NAME within optional FILE-LIST.
`hyrolo-file-list' is used as default when FILE-LIST is nil.
Leaves point immediately after match for NAME within entry.
Switches internal current buffer but does not alter the frame.
Return point where matching entry begins or nil if not found."
  (or file-list (setq file-list hyrolo-file-list))
  (let ((found) file)
    (while (and (not found) file-list)
      (setq file (car file-list)
	    file-list (cdr file-list))
      (cond ((and file (or (not (stringp file)) (string-equal file "")))
	     (error "(hyrolo-to): Invalid file: `%s'" file))
	    ((and (file-exists-p file) (not (file-readable-p file)))
	     (error "(hyrolo-to): File not readable: `%s'" file)))
      (set-buffer (or (get-file-buffer file) (hyrolo-find-file-noselect file)))
      (let ((case-fold-search t) (real-name name) (parent "") (level) end)
	(hyrolo-widen) (goto-char 1)
	(while (string-match "\\`[^\]\[<>{}\"]*/" name)
	  (setq end (1- (match-end 0))
		level nil
		parent (substring name 0 end)
		name (substring name (min (1+ end) (length name))))
	  (cond ((progn
		   (while (and (not level) (search-forward parent nil t))
		     (save-excursion
		       (beginning-of-line)
		       (if (looking-at (concat hyrolo-entry-regexp (regexp-quote parent)))
			   (setq level (match-string-no-properties hyrolo-entry-group-number)))))
		   level))
		((equal name real-name)) ;; Try next file.
		(t ;; Found parent but not child
		 (setq buffer-read-only nil)
		 (hyrolo-to-buffer (current-buffer))
		 (error "(hyrolo-to): `%s' part of name not found in \"%s\""
			parent file)))
	  (when level
	    (narrow-to-region (point)
			      (save-excursion
				(hyrolo-to-entry-end t (length level)) (point)))))
	(goto-char (point-min))
	(while (and (search-forward name nil t)
		    (not (save-excursion
			   (beginning-of-line)
			   (setq found
				 (when (looking-at (concat hyrolo-entry-regexp (regexp-quote name)))
				   (point))))))))
      (unless found
	(hyrolo-kill-buffer))) ;; conditionally kill
    (hyrolo-widen)
    found))

(defun hyrolo-to-buffer (buffer &optional other-window-flag _frame)
  "Pop to BUFFER."
  (pop-to-buffer buffer other-window-flag))

(defun hyrolo-to-entry-end (&optional include-sub-entries curr-entry-level-len)
  "Move point to the end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
CURR-ENTRY-LEVEL-LEN is the integer length of the last entry
header found.  If INCLUDE-SUB-ENTRIES is nil,
CURR-ENTRY-LEVEL-LEN is not needed.  Return current point."
  ;; Set free variable, next-entry-exists, for speed.
  (while (and (setq next-entry-exists
		    (re-search-forward hyrolo-entry-regexp nil t))
	      include-sub-entries
	      ;; Prevents including trailing whitespace in entry level
	      ;; length which in turn causes moving to (point-max).
	      (goto-char (or (match-end hyrolo-entry-group-number) (match-end 0)))
	      (> (- (point) (line-beginning-position))
		 curr-entry-level-len)))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

(defun hyrolo-mode-outline-level ()
  "Heuristically determine `outline-level' function to use in HyRolo match buffer."
  (cond ((looking-at (default-value 'outline-regexp))
	 ;; on an entry from a star-outline
	 (funcall (default-value #'outline-level)))
	((looking-at hyrolo-hdr-regexp)
	 0)
	((and (featurep 'kview)
	      (looking-at kview:outline-regexp))
	 ;; Assume on an entry from an alpha or legal Koutline
	 ;; with default outline settings
	 (let ((lbl-sep-len (length kview:default-label-separator)))
	   (floor (/ (- (or (kcell-view:indent nil lbl-sep-len)) lbl-sep-len)
		     kview:default-level-indent))))
	;; Just default to top-level if no other outline type is found
	(t 1)))

(defun hyrolo-mode ()
  "Major mode for the rolo match buffer.
Calls the functions given by `hyrolo-mode-hook'.
\\{hyrolo-mode-map}"
  (interactive)
  (unless (eq major-mode 'hyrolo-mode)
    (make-local-variable 'outline-regexp)
    (setq outline-regexp (default-value 'outline-regexp))
    (make-local-variable 'hyrolo-entry-regexp)
    (setq hyrolo-entry-regexp (default-value 'hyrolo-entry-regexp))
    (make-local-variable 'outline-level)
    (setq outline-level #'hyrolo-mode-outline-level)
    (reveal-mode 1)) ;; Expose hidden text as move into it.
  (setq major-mode 'hyrolo-mode
	mode-name "HyRolo")
  (use-local-map hyrolo-mode-map)
  ;;
  (set-syntax-table hyrolo-mode-syntax-table)
  ;;
  (when (fboundp 'outline-minor-mode)
    (outline-minor-mode 1))
  (run-hooks 'hyrolo-mode-hook))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(define-obsolete-variable-alias 'rolo-display-buffer 'hyrolo-display-buffer "06.00")
(defvar hyrolo-display-buffer "*HyRolo*"
  "Buffer used to display set of last matching rolo entries.")

(defvar hyrolo-entry-group-number 1
  "Group number whose length represents the level of any entry matched.
See `hyrolo-entry-regexp'")

(defvar hyrolo-entry-trailing-space-group-number 2
  "Group number within `hyrolo-entry-regexp; containing trailing space.")

(define-obsolete-variable-alias 'rolo-entry-regexp 'hyrolo-entry-regexp "06.00")
(defvar hyrolo-entry-regexp "^\\(\\*+\\)\\([ \t]+\\)"
  "Regular expression to match the beginning of a rolo entry.
This pattern must match the beginning of a line.
`hyrolo-entry-group-number' must capture the entry's level in the
hierarchy.  `hyrolo-entry-trailing-space-group-number' must capture
the whitespace following the entry hierarchy level.")

;; Support hyrolo searches in markdown files
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (make-local-variable 'hyrolo-entry-regexp)
	    (make-local-variable 'hyrolo-entry-group-number)
	    (make-local-variable 'hyrolo-entry-trailing-space-group-number)
	    (setq hyrolo-entry-regexp markdown-regex-header
		  hyrolo-entry-group-number 4
		  ;; `hyrolo-add' handles removing # prefix from
		  ;; trailing-space grouping below
		  hyrolo-entry-trailing-space-group-number 4)))

(defconst hyrolo-hdr-format
  (concat
   "======================================================================\n"
   "%s\n"
   "======================================================================\n")
  "Header to insert preceding a file's first rolo entry match when
file has none of its own.  Used with one argument, the file name.")

(defconst hyrolo-hdr-regexp "^==="
  "Regular expression to match the first and last lines of rolo file headers.
This header is inserted into hyrolo-display-buffer before any entries from the
file are added.")

(defconst hyrolo-match-regexp nil
  "Last regular expression used to search the rolo.
Nil before a search is done, including after a logical search is done.
String search expressions are converted to regular expressions.")

(defvar hyrolo--wconfig nil
  "Saves frame's window configuration prior to a rolo search.")

(defvar hyrolo-mode-syntax-table nil
  "Syntax table used while in hyrolo match mode.")

(if hyrolo-mode-syntax-table
    ()
  (setq hyrolo-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  ;; Support syntactic selection of delimited e-mail addresses.
  (modify-syntax-entry ?\<  "(>" hyrolo-mode-syntax-table)
  (modify-syntax-entry ?\>  ")<" hyrolo-mode-syntax-table))

(defvar hyrolo-mode-map nil
  "Keymap for the rolo match buffer.")

(if hyrolo-mode-map
    nil
  (setq hyrolo-mode-map (make-keymap))
  (if (fboundp 'set-keymap-name)
      (set-keymap-name hyrolo-mode-map 'hyrolo-mode-map))
  (suppress-keymap hyrolo-mode-map)
  (define-key hyrolo-mode-map "<"        'beginning-of-buffer)
  (define-key hyrolo-mode-map ">"        'end-of-buffer)
  (define-key hyrolo-mode-map "."        'beginning-of-buffer)
  (define-key hyrolo-mode-map ","        'end-of-buffer)
  (define-key hyrolo-mode-map "?"        'describe-mode)
  (define-key hyrolo-mode-map "\177"     'scroll-down)
  (define-key hyrolo-mode-map " "        'scroll-up)
  (define-key hyrolo-mode-map "a"        'outline-show-all)
  (define-key hyrolo-mode-map "b"        'outline-backward-same-level)
  (define-key hyrolo-mode-map "e"        'hyrolo-edit-entry)
  (define-key hyrolo-mode-map "f"        'outline-forward-same-level)
  (define-key hyrolo-mode-map "h"        'hyrolo-hide-subtree)
  (define-key hyrolo-mode-map "l"        'hyrolo-locate)
  (define-key hyrolo-mode-map "m"        'hyrolo-mail-to)
  (define-key hyrolo-mode-map "n"        'outline-next-visible-heading)
  (define-key hyrolo-mode-map "o"        'hyrolo-overview)
  (define-key hyrolo-mode-map "p"        'outline-previous-visible-heading)
  (define-key hyrolo-mode-map "q"        'hyrolo-quit)
  (define-key hyrolo-mode-map "r"        'hyrolo-grep-or-fgrep)
  (define-key hyrolo-mode-map "s"        'outline-show-subtree)
  (define-key hyrolo-mode-map "\M-s"     'hyrolo-isearch)
  (define-key hyrolo-mode-map "t"        'hyrolo-top-level)
  (define-key hyrolo-mode-map "\C-i"     'hyrolo-next-match)      ;; {TAB}
  (define-key hyrolo-mode-map "\M-\C-i"  'hyrolo-previous-match)  ;; {M-TAB}
  (define-key hyrolo-mode-map [backtab]  'hyrolo-previous-match)  ;; {Shift-TAB}
  (define-key hyrolo-mode-map "u"        'outline-up-heading))

;; Prompt user to rename old personal rolo file to new name, if necessary.
(unless noninteractive
  (call-interactively 'hyrolo-rename))

(provide 'hyrolo)

;;; hyrolo.el ends here
