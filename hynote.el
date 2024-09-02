;;; hynote.el --- Link to Org and Org Roam notes by name   -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    23-Jun-24 at 12:50:37
;; Last-Mod:      1-Sep-24 at 14:26:02 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  HyNote is a start on Hyperbole's note taking system.  It presently
;;  simply provides an experience similar to HyWikiWords for Org
;;  directory and Org Roam file and headline links.  In the future, it
;;  will also provide a universal way to easily link to information
;;  across many file formats.
;;
;;  HyNote supports Org, Markdown, Koutline and Emacs Outline file
;;  formats.  It uses UUIDs and HyRolo for quick note lookups across
;;  matching files in `hynote-directory-list'.
;;
;;  See all the autoloaded functions herein for interactive commands.
;;
;;  HyNote adds an implicit button type to Hyperbole:
;;    `hynote-file' displays Org and Org Roam links without the need
;;    for any delimiters or file suffixes, e.g. my-org-file-stem#headline
;;    would jump to the headline in the my-org-file.  There is no
;;    highlighting on such links but they flash when activated and {C-h A}
;;    will show what they do.
;;
;;  This is one of the lowest priority implicit button types, so such link
;;  buttons trigger only when other types are not recognized first.  Note
;;  that the `hywiki-word' type is recognized ahead of the `hynote-file'
;;  type.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hbut)
(require 'hywiki)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hynote-directory-list '("~/org/" "~/org-roam/")
  "Directories in which to find HyNote Org files.")

(defvar hynote-file-suffix-list
  '(".kotl" ".kot" ".md" ".markdown" ".mkd" ".mdown" ".mkdn" ".mdwn"
    ".org" ".otl" ".outl")
  "List of valid filename suffixes to search for HyNotes.
If you change this value, you must regenerate `hynote-file-suffix-regexp'.")

(defvar hynote-file-suffix-regexp
  (concat "\\(" (string-join (mapcar #'regexp-quote hynote-file-suffix-list) "\\|")
	  "\\)$")
  "Regular expression matching valid filename suffixes to search for HyNotes.")

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

;;;###autoload
(defun hynote-find-file (file &optional section)
  "Display an existing HyNote FILE starting at SECTION.
SECTION must be the name of a heading from the FILE and should begin
with \"#\", though this function will add \"#\" if it is missing.

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

;;;###autoload
(defun hynote-find-file-stem (file-stem-name)
  "Display an existing HyNote FILE-STEM-NAME from `hynote-directory-list'.
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
;;; Public functions
;;; ************************************************************************

(defun hynote-get-file (file-stem-name)
  "Return existing file path in `hynote-directory-list' from FILE-STEM-NAME.
File name must end with default suffixes from `hynote-file-suffix-list'.
No validation of FILE-STEM-NAME is done."
  ;; Remove any #section from `file-stem-name'
  (setq file-stem-name (if (string-match "#" file-stem-name)
			   (substring file-stem-name 0 (match-beginning 0))
			 file-stem-name))
  (locate-file file-stem-name hynote-directory-list hynote-file-suffix-list))

(defun hynote-get-files ()
  "Return `hynote-directory-list' files ending with `hynote-file-suffix-regexp'.
File names returned are relative to `hynote-directory-list'."
  (mapcan
   (lambda (dir)
    (make-directory dir t)
    (when (file-readable-p dir)
      (directory-files dir nil (concat "^[^#]+" hynote-file-suffix-regexp))))
   hynote-directory-list))

(defun hynote-get-file-stems ()
  "Return the list of existing HyNote files sans their `hynote-file-suffixes'.
This includes both Hynote page files and others.  Stems returned are
relative to `hynote-directory-list'."
  (mapcar #'file-name-sans-extension (hynote-get-files)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(provide 'hynote)
