;;; hsys-consult.el --- Hyperbole interactive consult-grep convenience functions     -*- lexical-binding: t; -*-
;; Author:       Bob Weiner
;;
;; Orig-Date:     4-Jul-24 at 09:57:18
;; Last-Mod:     12-Jul-24 at 22:05:30 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
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

(declare-function consult-grep "ext:consult")
(declare-function consult-ripgrep "ext:consult")
(declare-function org-roam-db-autosync-mode "ext:org-roam")
(declare-function org-roam-node-find "ext:org-roam")
(declare-function org-roam-node-level "ext:org-roam")

;; Forward declarations
(defvar consult-grep-args)
(defvar consult-ripgrep-args)
(defvar consult-async-split-style)
(defvar org-roam-db-autosync-mode)
(defvar org-roam-directory)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

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
(defun hsys-consult-grep (grep-includes ripgrep-globs &optional regexp max-matches path-list)
  "Interactively search PATH-LIST with a consult package grep command.

With GREP-INCLUDES or RIPGREP-GLOBS file suffixes to include, search
for optional REGEXP up to MAX-MATCHES in PATH-LIST.

Use ripgrep (rg) if found, otherwise, plain grep.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit matches
per file to the absolute value of MAX-MATCHES, if given and not 0.  If
0, match to headlines only (lines that start with a '^[*#]+[ \t]+' regexp)."
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
    (hsys-consult--grep-paths paths regexp max-matches)))

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

(defun hsys-consult-grep-tags (org-consult-grep-function)
  "When on an Org tag, call ORG-CONSULT-GREP-FUNCTION to find matches.
If on a colon, match to sections with all tags around point;
otherwise, just match to the single tag around point.

The function determines the org files searched for matches and is
given two arguments when called: a regexp of tags to match and a 0
max-count which finds all matches within headlines only."
  (interactive)
  (when (hsys-org-at-tags-p)
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
       (hsys-consult--grep-paths (list org-roam-directory) regexp max-matches)))))

;;;###autoload
(defun hsys-consult-org-roam-title ()
  "Interactively narrow and select Org Roam nodes by title."
  (interactive)
  (hsys-consult--org-roam-call-function
   (lambda ()
     (org-roam-node-find nil nil (lambda (node) (zerop (org-roam-node-level node)))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hsys-consult--grep-paths (paths &optional regexp max-matches)
  "Interactively search PATHS with a consult package grep command.
Use ripgrep (rg) if found, otherwise, plain grep.  Interactively
show all matches from PATHS; see the documentation for the `dir'
argument in `consult-grep' for valid values of PATHS.

Initialize search with optional REGEXP and interactively prompt
for changes.  Limit matches per file to the absolute value of
MAX-MATCHES, if given and not 0.  If 0, match to the start of
headline text only (lines that start with a '^[*#]+[ \t]*' regexp)."
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
				consult-ripgrep-args))
	(grep-func (cond ((executable-find "rg")
			  #'consult-ripgrep)
			 (t #'consult-grep))))
    ;; Consult split style usually uses '#' as a separator char but
    ;; that interferes with matching to Markdown # chars at the start
    ;; of a line in the regexp, so disable the separator char as it is
    ;; not needed for simple regexp searches.
    (let ((consult-async-split-style nil))
      (funcall grep-func paths regexp))))

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
