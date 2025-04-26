;;; hy-test-dependencies.el --- Hyperbole test dependencies      -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:16:00
;; Last-Mod:     25-Apr-25 at 19:28:36 by Mats Lidell
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
(when (equal (system-name) "norlinux")
  ;; Next load line resolves a cyclic dependency issue that breaks 11 tests
  ;; on rsw's linux system; please leave it here.
  (load "pcomplete"))

(defun hy-test-ensure-package-installed (pkg-symbol)
  (unless (package-installed-p pkg-symbol)
    (package-refresh-contents)
    (package-install pkg-symbol))
  (require pkg-symbol))

(mapc (lambda (sym) (hy-test-ensure-package-installed sym))
      '(el-mock))

;; Needed when `hypb:display-file-with-logo' uses `org-mode'
(setq hsys-org-enable-smart-keys t)

;; Log and fix any mixed version Org installation
(hsys-org-log-and-fix-version)

(provide 'hy-test-dependencies)
;;; hy-test-dependencies.el ends here
