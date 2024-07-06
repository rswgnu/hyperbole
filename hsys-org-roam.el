;;; hsys-org.el --- GNU Hyperbole support functions for Org Roam  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    26-Feb-23 at 11:20:15 by Bob Weiner
;; Last-Mod:      6-Jul-24 at 00:11:29 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2023-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   The autoloaded function, `hsys-org-roam-consult-grep', uses
;;   consult-grep to do a full-text search over notes included
;;   into the user's Org Roam database.
;;
;;   Use `org-roam-migrate-wizard' to import any Org note files and
;;   assign them UUIDs required for indexing by Org Roam.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'package)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar consult-org-roam-grep-func)
(defvar org-roam-directory)
(declare-function org-roam-db-autosync-mode "ext:org-roam")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hsys-org-roam-consult-grep ()
  "Prompt for search terms and run consult grep over `org-roam-directory'.
Actual grep function used is given by the variable,
`consult-org-roam-grep-func'."
  (interactive)
  (unless (package-installed-p 'consult-org-roam)
    (package-install 'consult-org-roam))
  (require 'consult-org-roam)
  (let ((grep-func (when (and (boundp 'consult-org-roam-grep-func)
			      (fboundp consult-org-roam-grep-func))
		     consult-org-roam-grep-func)))
    (if grep-func
	(funcall grep-func org-roam-directory)
      (error "(hsys-org-roam-consult-grep): `%s' is an invalid function"
	     consult-org-roam-grep-func))))

(defun hsys-org-roam-directory-at-tags-p (&optional at-tag-flag)
  "Return non-nil if point is in an `org-roam-directory' buffer and at Org tags."
  (and (featurep 'org-roam)
       (or at-tag-flag (hsys-org-at-tags-p))
       (and buffer-file-name
	    (string-prefix-p (expand-file-name org-roam-directory)
			     buffer-file-name))))

;;;###autoload
(defun hsys-org-roam-tags-view (&optional todo-only match view-buffer-name)
  "Prompt for colon-separated Org Roam tags and display matching headlines.
With optional prefix arg TODO-ONLY, limit matches to Org Roam
todo items only.  With optional VIEW-BUFFER-NAME, use that rather
than the default, \"*Org Roam Tags*\"."
  (interactive "P")
  (require 'org-agenda)
  (unless (package-installed-p 'org-roam)
    (package-install 'org-roam))
  (require 'org-roam)
  (let* ((org-agenda-files (list org-roam-directory))
	 (org-agenda-buffer-name (or view-buffer-name "*Org Roam Tags*"))
	 ;; `org-tags-view' is mis-written to require setting this next
	 ;; tmp-name or it will not properly name the displayed buffer.
	 (org-agenda-buffer-tmp-name org-agenda-buffer-name))
    ;; This prompts for the tags to match and uses `org-agenda-files'.
    (org-tags-view todo-only match)
    (when (equal (buffer-name) org-agenda-buffer-name)
      ;; Set up {C-u r} redo cmd
      (let (buffer-read-only)
	(put-text-property (point-min) (point-max) 'org-redo-cmd
			   `(hsys-org-roam-tags-view
			       ,todo-only
			       nil
			       ,org-agenda-buffer-name)))
      (forward-line 2))))

(provide 'hsys-org-roam)

;;; hsys-org-roam.el ends here
