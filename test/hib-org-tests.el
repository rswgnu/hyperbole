;;; hib-org-tests.el --- hib-org-el tests            -*- lexical-binding: t; -*-

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

;; Tests for "../hib-org.el"

;;; Code:

(require 'ert)
(require 'hib-org)
(require 'hact)

(declare-function hy-test-helpers:hypb-function-should-call-hpath:find "hy-test-helpers")

(ert-deftest hib-org:org-mode-on-header-cycles-visibility ()
  "With smart keys on an outline header `ibtypes::org-mode' cycles visibility."
  (with-temp-buffer
    (let ((bn (buffer-name))
          (hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "* 1\n** 2\n*** 3\n")
      (goto-char 1)
      (should (not (org-check-for-hidden 'headlines)))
      (ibtypes::org-mode)
      ;; Setting the buffer should not be needed but for some reason
      ;; it looks like we get into ert buffer after ibtypes::org-mode
      (set-buffer bn)
      (should (org-check-for-hidden 'headlines))
      (next-line)
      (should (equal (line-number-at-pos) 4)))))

(ert-deftest hib-org:org-mode-with-smart-keys-on-file-is-hypb-button ()
  "With smart keys on file name is hypb button."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'ibtypes::org-mode "/tmp"))))

(ert-deftest hib-org:org-mode-with-smart-keys-buttons-on-file-is-hypb-button ()
  "With smart keys on file name is hypb button."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'ibtypes::org-mode "/tmp"))))

(ert-deftest hib-org:org-mode-with-no-smart-keys-on-file-is-hypb-button ()
  "With no smart keys on file calls org mode M-RET."
  (with-temp-buffer
    (let ((bn (buffer-name))
          (hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (ibtypes::org-mode)
      ;; Setting the buffer should not be needed but for some reason
      ;; it looks like we get into ert buffer after ibtypes::org-mode
      (set-buffer bn)
      (should (equal (buffer-string) "* /tmp")))))

(provide 'hib-org-tests)
;;; hib-org-tests.el ends here
