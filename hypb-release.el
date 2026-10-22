;;; hypb-release.el --- Release utility functions for Hyperbole maintainers -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     5-Oct-26 at 21:58:13
;; Last-Mod:     22-Oct-26 at 11:29:33 by Bob Weiner
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
(require 'cl-lib)

(defvar hypb-release-version-file-list
  '(("hversion.el" "(defconst hyperb:version \"{version}\"")
    ("HY-ABOUT" "Version {version}")
    ("hyperbole-pkg.el" "{version}" t)
    ("Makefile" "HYPB_VERSION = {version}")
    ("README.in.md" "# GNU Hyperbole {version} - The Everyday Hypertextual Information Manager")
    ("man/hyperbole.texi" "@set EDITION {version}")
    ("man/hyperbole.texi" "@set VERSION {version}"))
  "Files in which the Hyperbole version number is stored.
Each list item contains the filename, a pattern for locating the version
number and an optional flag that if non-nil means the file is not
required to exist.  Note that the list does not include hyperbole.el
since that needs to be updated in the last commit for a release.  That
update is controlled by a make target.  See also
`hypb-release-update-hyperbole-version-header'.")

(defun hypb-release--replace-version (pattern old-version new-version &optional check)
  "Replace OLD-VERSION with NEW-VERSION using PATTERN.
PATTERN contains the placeholder {version}.  When CHECK is non-nil, only
check that the pattern matches.  Return non-nil if the pattern was
found."
  (save-excursion
    (goto-char (point-min))
    (let ((search (string-replace "{version}" old-version pattern))
          (replacement (string-replace "{version}" new-version pattern)))
      (when (search-forward search nil t)
        (unless check
          (replace-match replacement t t))
        t))))

;;;###autoload
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
    (dolist (v hypb-release-version-file-list)
      (let ((file (nth 0 v))
            (pattern (nth 1 v))
            (optional (nth 2 v)))
        (unless (or optional (file-exists-p file))
          (error "File does not exist: %s" file))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (unless (hypb-release--replace-version pattern old-version new-version 'dry-run)
              (error "Version %s not found in %s"
                     old-version file))))))

    ;; Substitute and save all.
    (let ((replacements 0)
          (files))
      (dolist (v hypb-release-version-file-list)
        (let ((file (nth 0 v))
              (pattern (nth 1 v)))
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (unless (hypb-release--replace-version pattern old-version new-version)
                (error "Version %s not found in %s" old-version file))
              (setq replacements (1+ replacements))
              (cl-pushnew file files :test 'equal)
              (save-buffer)))))

      (message "Version %s -> %s: %d replacements in %d files"
               old-version new-version
               replacements (length files)))))

(defun hypb-release-update-hyperbole-version-header (new-version)
  "Update the version header in hyperbole.el to NEW-VERSION.
This update needs to be in the last commit for a release and is run by a
Makefile target."
  (let ((hypb-release-version-file-list '(("hyperbole.el" ";; Version:      {version}"))))
    (hypb-release-update-version new-version)))

(provide 'hypb-release)
;;; hypb-release.el ends here
