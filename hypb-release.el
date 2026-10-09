;;; hypb-release.el --- Release utility functions for Hyperbole maintainers -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     5-Oct-26 at 21:58:13
;; Last-Mod:      6-Oct-26 at 22:31:38 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'lisp-mnt)

(defvar hypb-release-version-files
  '("hversion.el"
    "HY-ABOUT"
    "HY-ANNOUNCE"
    "HY-NEWS"
    "Makefile"
    "README.md"
    "man/hyperbole.texi")
  "Files in which the Hyperbole version number is stored.
The list does not include hyperbole.el since that needs to be updated in
the last commit for a release.  That update is controlled by a make
target.  See also `hypb-release-update-hyperbole-version-header'.")

(defun hypb-release-update-version (new-version)
  "Replace old-version with NEW-VERSION in `hypb-release-version-files'.

Old-version is fetched from hyperbole.el.  Signal an error if a file
does not exist or old-version is not found in every file.

All files are checked before any changes are made.

Files are saved normally by Emacs, so hooks are applied as if edited manually."
  (interactive
   (list (read-string "New version: ")))

  (let ((old-version (lm-version "hyperbole.el")))
    ;; Check all files before any action.
    (dolist (file hypb-release-version-files)
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (with-temp-buffer
        (insert-file-contents file)
        (unless (search-forward old-version nil t)
          (error "Version %s not found in %s"
                 old-version file))))

    ;; Substitute and save all.
    (let ((replacements 0))
      (dolist (file hypb-release-version-files)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (while (search-forward old-version nil t)
              (replace-match new-version t t)
              (setq replacements (1+ replacements))))
          (save-buffer)))

      (message "Version %s -> %s: %d replacements in %d files"
               old-version new-version
               replacements (length hypb-release-version-files)))))

(defun hypb-release-update-hyperbole-version-header (new-version)
  "Update the version header in hyperbole.el to NEW-VERSION.
This update needs to be in the last commit for a release and is run by a
Makefile target."
  (let ((hypb-release-version-files '("hyperbole.el")))
    (hypb-release-update-version new-version)))

(provide 'hypb-release)
;;; hypb-release.el ends here
