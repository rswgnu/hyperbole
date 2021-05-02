;;; smart-org-tests.el --- smart-org-el tests            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mats Lidell

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 23-Apr-21 at 22:21:00
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for smart-org in "../hui-mouse.el"

;;; Code:

(require 'ert)
(require 'hui-mouse)
(require 'hact)

(declare-function hy-test-helpers:hypb-function-should-call-hpath:find "hy-test-helpers")

(ert-deftest smart-org-mode-on-header-cycles-visibility ()
  "With smart keys on an outline header `smart-org' cycles visibility."
  (with-temp-buffer
    (let ((bn (buffer-name))
          (hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "* 1\n** 2\n*** 3\n")
      (goto-char 1)
      (should (not (org-check-for-hidden 'headlines)))
      (smart-org)
      ;; Setting the buffer should not be needed but for some reason
      ;; it looks like we get into ert buffer after smart-org
      (set-buffer bn)
      (should (org-check-for-hidden 'headlines))
      (next-line)
      (should (equal (line-number-at-pos) 4)))))

(ert-deftest smart-org-mode-with-smart-keys-on-file-is-hypb-button ()
  "With smart keys on file name is hypb button."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'smart-org "/tmp"))))

(ert-deftest smart-org-mode-with-smart-keys-buttons-on-file-is-hypb-button ()
  "With smart keys on file name is hypb button."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'smart-org "/tmp"))))

(ert-deftest smart-org-mode-with-no-smart-keys-on-file-is-hypb-button ()
  "With no smart keys on file calls org mode M-RET."
  (with-temp-buffer
    (let ((bn (buffer-name))
          (hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (smart-org)
      ;; Setting the buffer should not be needed but for some reason
      ;; it looks like we get into ert buffer after smart-org
      (set-buffer bn)
      (should (equal (buffer-string) "* /tmp")))))

(provide 'smart-org-tests)
;;; smart-org-tests.el ends here
