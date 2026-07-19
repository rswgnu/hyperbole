;;; hy-test-dependencies.el --- Hyperbole test dependencies      -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:16:00
;; Last-Mod:     18-Jul-26 at 15:13:20 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Load prerequisites for running the tests.

;;; Code:

(require 'package)

;; Advice with retry for package-install
(defun hypb:package-install-advice-for-retry (pkg-install &rest args)
  "Add a retry layer on top of package-install."
  (let ((retries 3)
        (attempt 0)
        done
        result)
    (while (not done)
      (setq attempt (1+ attempt))
      (condition-case err
          (progn
            (setq result (apply pkg-install args))
            (setq done t))
      	(wrong-type-argument
         ;; Reraise if error is not stringp nil
         (unless (equal (cdr err) '(stringp nil))
           (signal (car err) (cdr err)))
         (if (<= attempt retries)
             (message "package-install failed (attempt %d/%d): %s"
                      attempt retries (error-message-string err))
           (signal (car err) (cdr err))))))
    result))

;; Apply advice only for Emacs master branch version used in CI
(unless (version< emacs-version "32.0.50")
  (advice-add 'package-install :around #'hypb:package-install-advice-for-retry))

(declare-function markdown-ts-mode "ext:markdown-ts-mode")

;; Force markdown-mode to be selected first to avoid markdown-ts-mode
;; for now. It requires Tree-sitter grammars to be installed which
;; conflicts with our current CI setup.
(when (fboundp #'markdown-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(setq package-pinned-packages
      '((markdown-mode . "nongnu")))

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

;; Log and fix any mixed version Org installation.
;; Ignore publishing-related errors that don't affect the tests.
(ignore-errors (hsys-org-log-and-fix-version))

;; Allow dynamic loading with no questions asked of dependencies
;; i.e. markdown-mode
(setq hypb:ask-to-install-package-flag nil)

(provide 'hy-test-dependencies)
;;; hy-test-dependencies.el ends here
