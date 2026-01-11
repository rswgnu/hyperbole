;;; hsys-consult.el --- Hyperbole interactive consult-grep convenience functions     -*- lexical-binding: t; -*-
;; Author:       Bob Weiner
;;
;; Orig-Date:     4-Jul-24 at 09:57:18
;; Last-Mod:     30-Dec-25 at 14:42:23 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024-2025  Free Software Foundation, Inc.
;; Licensed under the GNU General Public License, version 3.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This library automatically installs the `consult' package for its
;;  `consult-grep' command only when one of the functions in here is
;;  called.  At that time, if `consult' has not been installed, it will
;;  be automatically downloaded and installed via the Emacs package
;;  system, so don't `require' it herein.

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

;; Don't (require 'consult) here since want to create that dependency only
;; when a function within this library is called.

(require 'hbut)
(require 'hargs)
(require 'hproperty)
(require 'hsys-org-roam)
(require 'find-func)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function hyrolo-at-tags-p "hyrolo")
(declare-function hywiki-at-tags-p "hywiki")
(declare-function hsys-org-directory-at-tags-p "hsys-org")
(declare-function hsys-org-at-tags-p "hsys-org")

(declare-function consult--grep "ext:consult")
(declare-function consult--grep-make-builder "ext:consult")
(declare-function consult--read "ext:consult")
(declare-function consult--ripgrep-make-builder "ext:consult")
(declare-function consult-grep "ext:consult")
(declare-function consult-ripgrep "ext:consult")
(declare-function org-roam-db-autosync-mode "ext:org-roam")
(declare-function org-roam-node-find "ext:org-roam")
(declare-function org-roam-node-level "ext:org-roam")

;; Forward declarations
(defvar consult-async-split-style)
(defvar consult-grep-args)
(defvar consult-preview-key)
(defvar consult-ripgrep-args)
(defvar org-roam-db-autosync-mode)
(defvar org-roam-directory)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hsys-consult-exit-value nil
  "Value from a user-defined exit-hook sent to `hsys-consult-get-exit-value'.")

(defcustom hsys-consult-flag t
  "Non-nil means use the consult package with vertico for filtering searches.
When non-nil and interactively calling non-consult-specific
Hyperbole search and yank commands, if consult is installed it
will be used to filter to matching file lines.  For Hyperbole
consult-specific commands, when this is non-nil and consult is
not installed, automatically install it and then run the command.
When nil, trigger an error that consult is not installed."
  :type 'boolean
  :group 'hyperbole-commands)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hsys-consult-active-p ()
  "Return non-nil if `hsys-consult-flag' is true and `consult-grep' is fboundp."
  (and hsys-consult-flag (fboundp 'consult-grep)))

;;;###autoload
(defun hsys-consult-apropos (&optional include-all-flag)
  "Summarize all meaningful symbols matching interactively chosen terms.
With optional INCLUDE-ALL-FLAG (prefix arg interactively) non-nil,
include all bound symbols.

Requires use of `vertico' for completions."
  (interactive "P")
  (apropos
   (split-string
    (hsys-consult-get-exit-value
     '(car vertico--input)
     #'consult--read
     obarray
     :prompt "Consult Apropos: ")
    "[ \t]+" t)
   include-all-flag))

;;;###autoload
(defun hsys-consult-get-version ()
  "Return the string version of the installed consult package or nil."
  (let* ((consult-file (find-library-name "consult"))
	 (buffer-existed (get-file-buffer consult-file))
	 (buffer-modified (when buffer-existed (buffer-modified-p buffer-existed)))
	 (buf (or buffer-existed (find-file-noselect consult-file))))
    (with-current-buffer buf
      (prog1 (package-get-version)
	(unless buffer-modified
	  (kill-buffer buf))))))

;;;###autoload
(defun hsys-consult-grep (grep-includes ripgrep-globs &optional regexp max-matches
					path-list prompt)
  "Interactively search PATH-LIST with a consult package grep command.

With GREP-INCLUDES or RIPGREP-GLOBS file suffixes to include, search
for optional REGEXP up to MAX-MATCHES in PATH-LIST.

Use ripgrep (rg) if found, otherwise, plain grep.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit matches
per file to the absolute value of MAX-MATCHES, if given and not 0.  If
0, match to headlines only (lines that start with a '^[*#]+[ \t]+' regexp).

With optional PROMPT string, use this as the first part of the grep prompt;
omit any trailing colon and space in the prompt."
  (unless hsys-consult-flag
    (error "`%s' command requires `hsys-consult-flag' set to t" this-command))
  (unless (package-installed-p 'consult)
    (package-install 'consult))
  (require 'consult)

  (let ((consult-version (hsys-consult-get-version)))
    ;; Multi-file support added after consult version "0.32"
    (when (not (and consult-version (string-greaterp consult-version "0.32")))
      (error "(hsys-consult-grep): consult package version is %s; update required"
	     consult-version)))
  (let* ((consult-grep-args
	  (if (listp consult-grep-args)
	      (append consult-grep-args (list grep-includes))
	    (concat consult-grep-args " " grep-includes)))
	 (consult-ripgrep-args
	  (if (listp consult-ripgrep-args)
	      (append consult-ripgrep-args (list ripgrep-globs))
            (concat consult-ripgrep-args " " ripgrep-globs)))
	 (paths (if find-file-wildcards
		    ;; Use only the directory of paths with wildcards
		    ;; since the grep command filters to desired file
		    ;; types much more efficiently.
		    (mapcar (lambda (path)
			      (if (string-match "[\\/]?\\([^*?\\/]*[*?][^\\/]+\\'\\)" path)
				  (substring path 0 (match-beginning 1))
				path))
			    path-list)
		  path-list)))
    (hsys-consult--grep-paths paths regexp max-matches prompt)))

(defun hsys-consult-grep-headlines-with-prompt (grep-function prompt
					        &optional regexp)
  "Call Hyperbole consult GREP-FUNCTION over headlines with PROMPT.
Optional REGEXP is the initial pattern for the grep.
Suppress preview and return the selected \"file:line:line-contents\".

GREP-FUNCTION must take these arguments: regexp max-matches path-list
prompt."
  (let ((consult-preview-key nil))
    (funcall grep-function regexp 0 nil prompt)))

(defun hsys-consult-grep-headlines-read-regexp (grep-function prompt
						&optional regexp)
  "With `consult', use GREP-FUNCTION and PROMPT to completing read.
The optional REGEXP is an initial pattern for the grep.  Suppress
preview and return the selected \"file:line:line-contents\".
GREP-FUNCTION must take these arguments: regexp max-matches path-list
prompt.

Without `consult', just read a REGEXP with PROMPT."
  (if (hsys-consult-active-p)
      (substring-no-properties
       (hsys-consult-get-exit-value
	nil
	#'hsys-consult-grep-headlines-with-prompt
	grep-function
	prompt
	regexp))
    (read-regexp (concat prompt ": ") regexp)))

(defun hsys-consult-grep-tags (org-consult-grep-function)
  "When on an Org tag, call ORG-CONSULT-GREP-FUNCTION to find matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point.

The function determines the org files searched for matches and is
given two arguments when called: a regexp of tags to match and a 0
max-count which finds all matches within headlines only."
  (interactive)
  (when (and hsys-consult-flag (hsys-org-at-tags-p))
    (funcall org-consult-grep-function (hsys-consult--org-grep-tags-string) 0)))

(defun hsys-consult-hyrolo-grep-tags ()
  "When on a HyRolo tag, use `consult-grep' to list all HyRolo tag matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-consult-grep-tags #'hyrolo-consult-grep))

(defun hsys-consult-hywiki-grep-tags ()
  "When on a HyWiki tag, use `consult-grep' to list all HyWiki tag matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-consult-grep-tags #'hywiki-consult-grep))

;;;###autoload
(defun hsys-consult-org-grep-tags-p ()
  "When on an Org tag, return appropriate `consult-grep' function.
Use `default-directory' and buffer name to determine which function to
call."
  (when (hsys-org-at-tags-p)
    (cond ((hsys-org-directory-at-tags-p t)
	   #'hsys-consult-org-grep-tags)
	  ((hsys-org-roam-directory-at-tags-p t)
	   #'hsys-consult-org-roam-grep-tags)
	  ((hywiki-at-tags-p t)
	   #'hsys-consult-hywiki-grep-tags)
	  ((hyrolo-at-tags-p t)
	   #'hsys-consult-hyrolo-grep-tags))))

(defun hsys-consult-org-grep-tags ()
  "When on an `org-directory' tag, use `consult-grep' to list dir tag matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-consult-grep-tags #'hsys-org-consult-grep))

(defun hsys-consult-org-roam-grep-tags ()
  "When on an `org-roam-directory' tag, use `consult-grep' to list tag matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point."
  (interactive)
  (hsys-consult-grep-tags #'hsys-org-roam-consult-grep))

;;;###autoload
(defun hsys-consult-org-roam-grep (&optional regexp max-matches)
  "Interactively narrow and select Org Roam nodes by line.
Use ripgrep (rg) if found, otherwise, plain grep to search Org
files within `org-roam-directory'.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit
matches per file to the absolute value of MAX-MATCHES, if given
and not 0.  If 0, match to the start of headline text only (lines
that start with the '^[*#]+[ \t]*' regexp)."
  (interactive "i\nP")
  (hsys-consult--org-roam-call-function
   (lambda ()
     (let ((consult-grep-args
	    (if (listp consult-grep-args)
		(append consult-grep-args (list "--include *.org"))
	      (concat consult-grep-args " --include *.org")))
	   (consult-ripgrep-args
	    (if (listp consult-ripgrep-args)
		(append consult-ripgrep-args (list "--glob *.org"))
              (concat consult-ripgrep-args " --glob *.org"))))
       (hsys-consult--grep-paths (list org-roam-directory) regexp max-matches
				 "Grep Org Roam Nodes")))))

;;;###autoload
(defun hsys-consult-org-roam-title ()
  "Interactively narrow and select Org Roam nodes by title."
  (interactive)
  (hsys-consult--org-roam-call-function
   (lambda ()
     (org-roam-node-find nil nil (lambda (node) (zerop (org-roam-node-level node)))))))

;;;###autoload
(defun hsys-consult-get-exit-value (exit-value consult-function &rest args)
  "With minibuffer EXIT-VALUE, call CONSULT-FUNCTION with rest of ARGS.
If EXIT-VALUE is non-nil, i.e. an sexpression or function of no
arguments, store and return its result value into `hsys-consult-exit-value',
Otherwise, return the selection from CONSULT-FUNCTION."
  (unless hsys-consult-flag
    (error "`%s' command requires `hsys-consult-flag' set to t" this-command))
  (unless (functionp consult-function)
    (user-error "(hsys-consult-get-exit-value): First arg must be a function, not `%s'"
		consult-function))

  (save-excursion
    (save-window-excursion
      (if exit-value
	  (let ((exit-hook `(lambda ()
			      (setq hsys-consult-exit-value
				    (if (functionp ',exit-value)
				       (funcall ',exit-value)
				      (eval ',exit-value))))))
	    (unwind-protect
		(progn (push exit-hook minibuffer-exit-hook)
		       (apply consult-function args)
		       hsys-consult-exit-value)
	      (setf minibuffer-exit-hook (delq exit-hook minibuffer-exit-hook))))
	(apply consult-function args)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hsys-consult--grep-paths (paths &optional regexp max-matches prompt)
  "Interactively search PATHS with a consult package grep command.
Use ripgrep (rg) if found, otherwise, plain grep.  Interactively
show all matches from PATHS; see the documentation for the `dir'
argument in `consult-grep' for valid values of PATHS.

Initialize search with optional REGEXP and interactively prompt
for changes.  Limit matches per file to the absolute value of
optional MAX-MATCHES, if given and not 0.  If 0, match to the
start of headline text only (lines that start with a '^[*#]+[
\t]*' regexp).

With optional PROMPT string, use this as the first part of the
grep prompt; omit any trailing colon and space in the prompt."
  (unless hsys-consult-flag
    (error "`%s' command requires `hsys-consult-flag' set to t" this-command))
  (unless (package-installed-p 'consult)
    (package-install 'consult))
  (require 'consult)
  (let ((consult-version (hsys-consult-get-version)))
    ;; Multi-file support added after consult version "0.32"
    (when (not (and consult-version (string-greaterp consult-version "0.32")))
      (error "(hsys-consult-grep): consult package version is %s; update required"
	     consult-version)))
  (when max-matches
    (setq max-matches (prefix-numeric-value max-matches)))
  (when (and (integerp max-matches) (zerop max-matches))
    ;; Final space in leading regexp in next line makes it work with
    ;; the Orderless package.
    (setq regexp (concat "^[*#]+[ \t]* " (or regexp ""))))
  (let ((consult-grep-args (if (and (integerp max-matches) (not (zerop max-matches)))
			       (if (listp consult-grep-args)
				   (append consult-grep-args
					   (list (format "-m %d" (abs max-matches))))
				 (concat consult-grep-args
					 (format " -m %d" (abs max-matches))))
			     consult-grep-args))
	(consult-ripgrep-args (if (and (integerp max-matches) (not (zerop max-matches)))
				  (if (listp consult-ripgrep-args)
				      (append consult-ripgrep-args
					      (list (format "-m %d" (abs max-matches))))
				    (concat consult-ripgrep-args
					    (format " -m %d" (abs max-matches))))
				consult-ripgrep-args)))

    ;; Ensure any env or lisp variables in paths are replaced so
    ;; grep does not ignore them.
    (setq paths (mapcar #'hpath:expand paths))

    ;; Consult split style usually uses '#' as a separator char but
    ;; that interferes with matching to Markdown # chars at the start
    ;; of a line in the regexp, so disable the separator char as it is
    ;; not needed for simple regexp searches.
    (let ((consult-async-split-style nil))
      (if (executable-find "rg")
	  (consult--grep (or prompt "Ripgrep")
			 #'consult--ripgrep-make-builder paths regexp)
	(consult--grep (or prompt "Grep")
		       #'consult--grep-make-builder paths regexp)))))


(defun hsys-consult--org-grep-tags-string ()
  "When on or between Org tags, return a `consult-grep' match string for them.
If on a colon, match to headlines with all tags around point, in any order.
e.g. \":tag1: :tag2: :tag3: \".  Otherwise, just match to the single
tag around point."
  (let (range
	tags)
    (if (equal (char-after) ?:)
	;;  On colon, search for HyWiki headings with all tags on line
	(setq range (hproperty:char-property-range nil 'face 'org-tag)
	      tags (when range (buffer-substring-no-properties (car range) (cdr range))))
      ;;   Else on a specific tag, search for HyWiki headings with that tag only
      (setq range (hargs:delimited ":" ":" nil nil t)
	    tags (nth 0 range)
	    range (cons (nth 1 range) (nth 2 range))))
    (when (and tags range)
      (ibut:label-set tags (car range) (cdr range))
      (concat ".*" (string-join (mapcar (lambda (tag)
					  (concat ":" (regexp-quote tag) ":"))
					(split-string tags ":" t))
				" ")))))

(defun hsys-consult--org-roam-call-function (func)
  "Install Org Roam if necessary and then call an Org Roam FUNC."
  (unless hsys-consult-flag
    (error "`%s' command requires `hsys-consult-flag' set to t" this-command))
  (unless (package-installed-p 'org-roam)
    (package-install 'org-roam))
  (require 'org-roam)
  (unless (file-readable-p org-roam-directory)
    (make-directory org-roam-directory))
  (unless org-roam-db-autosync-mode
    (org-roam-db-autosync-mode))

  (if (file-readable-p org-roam-directory)
      (funcall func)
    (error "`org-roam-directory', \"%s\", does not exist" org-roam-directory)))

(provide 'hsys-consult)

;;; hsys-consult.el ends here
