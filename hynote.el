;;; hynote.el --- Link to Org and Org Roam notes by name   -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    23-Jun-24 at 12:50:37
;; Last-Mod:     30-Jun-24 at 11:27:57 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This is Hyperbole's note taking system, HyNote.  It utilizes the
;;  Org mode or the Koutliner file format plus UUIds and HyRolo for note lookups.
;;
;;  See all the autoloaded functions herein for interactive commands.
;;  See the Info manual entry "(hyperbole)HyNote" for usage information.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hbut)
(require 'hyrolo)
(require 'hywiki)

;;; ************************************************************************
;;; Public Implicit Button and Action Types
;;; ************************************************************************

(defun hynote-file-stem-start-end-at ()
  "Return (file-stem start end) if on a `hynote-directory-list' file stem.
Otherwise, return (nil nil nil)."
  (or (hpath:delimited-possible-path nil t)
      (list nil nil nil)))

(defib hynote-file ()
  "When on a HyNote file name stem, display the file and its optional section.
This type is active only in buffers where `hywiki-active-in-current-buffer-p'
is true.  This may require that (hywiki-mode) has been enabled."
  (when (hywiki-active-in-current-buffer-p)
    (cl-destructuring-bind (file-stem-name start end)
	(hynote-file-stem-start-end-at)
      (when file-stem-name
	(let ((file (hynote-get-file file-stem-name))
	      section)
	  (when (and file (file-readable-p file))
	    (setq section (when (string-match "#" file-stem-name)
			    (substring file-stem-name (match-beginning 0))))
	    (ibut:label-set file-stem-name start end)
	    (hact 'hynote-find-file file section)))))))

(defun hynote-find-file (file &optional section)
  "Display an existing FILE starting at SECTION.
SECTION must be the name of a heading from the FILE and should begin
with \"#\", though this function will add \"#\" if missing.

Return the absolute path to any file successfully found, else nil.
After successfully finding a file and reading it into a buffer, run
`hynote-find-file-hook'."
  (interactive (list (completing-read "Find HyNote file: "
				      (hynote-get-files))))
  (when (and (stringp file) (file-readable-p file))
    (when (and (stringp section) (not (string-prefix-p "#" section)))
      (setq section (concat "#" section)))
    (hpath:find (concat file section))
    (hywiki-maybe-highlight-page-names)
    (run-hooks 'hynote-find-file-hook)
    file))

(defun hynote-find-file-stem (file-stem-name)
  "Display an existing FILE-STEM-NAME from `hynote-directory-list'.
Return the absolute path to any file successfully found, else nil.

After successfully finding a file and reading it into a buffer, run
`hynote-find-file-hook'."
  (interactive (list (completing-read "Find HyNote file: "
				      (hynote-get-file-stems))))
  (when (stringp file-stem-name)
    (let ((file (hynote-get-file file-stem-name))
	  section)
      (when (file-readable-p file)
	(setq section (when (string-match "#" file-stem-name)
			(substring file-stem-name (match-beginning 0))))
	(when file
	  (hpath:find (concat file section))
	  (hywiki-maybe-highlight-page-names)
	  (run-hooks 'hynote-find-file-hook)
	  file)))))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hynote-directory-list '("~/org/" "~/org-roam/")
  "Directories in which to find HyNote Org files.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hynote-get-file (file-stem-name)
  "Return existing file path in `hynote-directory-list' from FILE-STEM-NAME.
File name must end with `hyrolo-file-suffix-regexp'.  No
validation of FILE-STEM-NAME is done."
  ;; Remove any #section from `file-stem-name'
  (setq file-stem-name (if (string-match "#" file-stem-name)
			   (substring file-stem-name 0 (match-beginning 0))
			 file-stem-name))
  (locate-file file-stem-name hynote-directory-list 
	       '(".org" ".md" ".kotl" ".kot")))

(defun hynote-get-files ()
  "Return `hynote-directory-list' files ending with `hyrolo-file-suffix-regexp'.
File names returned are relative to `hynote-directory-list'."
  (mapcan
   (lambda (dir)
    (make-directory dir t)
    (when (file-readable-p dir)
      (directory-files dir nil (concat "^[^#]+" hyrolo-file-suffix-regexp))))
   hynote-directory-list))

(defun hynote-get-file-stems ()
  "Return the list of existing HyWiki files sans their `hynote-file-suffixes'.
This includes both Hynote page files and others.  Stems returned are
relative to `hynote-directory-list'."
  (mapcar #'file-name-sans-extension (hynote-get-files)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(provide 'hynote)
