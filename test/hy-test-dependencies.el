;;; hy-test-dependencies.el --- Hyperbole test dependencies      -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:16:00
;; Last-Mod:     17-Jan-24 at 23:32:33 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Load prerequisites for running the tests.

;;; Code:

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'hload-path)
(require 'hyperbole)
(add-to-list 'load-path (expand-file-name "test" hyperb:dir))

(defun hy-test-ensure-package-installed (pkg-symbol)
  (unless (package-installed-p pkg-symbol)
    (package-refresh-contents)
    (package-install pkg-symbol)))

(mapc (lambda (sym) (hy-test-ensure-package-installed sym))
      '(el-mock package-lint with-simulated-input))

;; Needed when `hypb:display-file-with-logo' uses `org-mode'.
(setq hsys-org-enable-smart-keys t)

;; From compat.el package
(unless (fboundp 'string-replace)
(defun string-replace (fromstring tostring instring)
  "Replace FROMSTRING with TOSTRING in INSTRING each time it occurs."
  (when (equal fromstring "")
    (signal 'wrong-length-argument '(0)))
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     (regexp-quote fromstring)
     tostring instring
     t t))))

(require 'pp)
(terpri)
(print (format "Org source dir = %S" (ignore-errors (org-find-library-dir "org"))))
(print (format "Org load dir   = %S" (ignore-errors (org-find-library-dir "org-loaddefs"))))
(print (format "Org version    = %S" (org-release)))
(terpri)

(let ((org-reloaded (hsys-org-fix-version)))
  (if org-reloaded
      (message "Mixed Org versions fixed and reloaded\n  version is now %s\n  source dir is now %S"
	       org-version (ignore-errors (org-find-library-dir "org")))
    (message "Correct, single version of Org is active %s" org-version)))

(provide 'hy-test-dependencies)
;;; hy-test-dependencies.el ends here
