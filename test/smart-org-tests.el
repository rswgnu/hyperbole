;;; smart-org-tests.el --- smart-org-el tests            -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    23-Apr-21 at 22:21:00
;; Last-Mod:     22-May-22 at 15:04:44 by Bob Weiner
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for smart-org in "../hui-mouse.el"

;;; Code:

(require 'ert)
(require 'hui-mouse)
(require 'hact)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")

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

;; Smart Key Context
(ert-deftest smart-org-mode-with-smart-keys-on-delimited-thing-activates ()
  "With smart keys on delimited thing activates selection."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "(hy per bo le)\n")
      (goto-char 14)
      (action-key)
      (should (equal (point) 1)))))

;; Hyperbole Button
(ert-deftest smart-org-mode-with-smart-keys-on-hypb-button-activates ()
  "With smart keys on hypb button activates the button."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'ibtypes::pathname "/tmp"))))

;; Hyperbole Button
(ert-deftest smart-org-mode-with-smart-keys-buttons-on-hypb-button-activates ()
  "With smart keys as buttons on hypb button activates the button."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'action-key "/tmp"))))

;; Org Link
(ert-deftest smart-org-mode-with-smart-keys-on-org-link-activates ()
  "With smart keys on `org-mode' link activates link."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "[[/tmp][desc]]")
      (goto-char 9)
      (with-mock
       (mock (org-open-at-point) => t)
       (smart-org)))))

;; Smart Key Context
(ert-deftest smart-org-mode-with-smart-keys-buttons-on-delimited-thing-calls-org-meta-return ()
  "With smart keys as buttons on delimited falls back to `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "(hy per bo le)\n")
      (goto-char 14)
      (with-mock
       (mock (hsys-org-meta-return) => t)
       (smart-org)))))

;; Org Link
(ert-deftest smart-org-mode-with-smart-keys-buttons-on-org-link-activates ()
  "With smart keys as buttons on `org-mode' link activates link."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "[[/tmp][desc]]")
      (goto-char 9)
      (with-mock
       (mock (org-open-at-point) => t)
       (smart-org)))))

;; Smart Key Context
(ert-deftest smart-org-mode-with-no-smart-keys-on-delimited-thing-calls-org-meta-return ()
  "With no smart keys on file calls `org-meta-return'."
  (with-temp-buffer
    (let ((bn (buffer-name))
          (hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "(hy per bo le)\n")
      (goto-char 14)
      (smart-org)
      (set-buffer bn)
      (should (string= (buffer-string) "(hy per bo le\n* )\n")))))

;; Hyperbole Button
(ert-deftest smart-org-mode-with-no-smart-keys-on-hypb-button-calls-org-meta-return ()
  "With no smart keys on file calls `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (with-mock
       (mock (hsys-org-meta-return) => t)
       (smart-org)))))

;; Org Link
(ert-deftest smart-org-mode-with-no-smart-keys-on-org-link-is-org-meta-return ()
  "With no smart keys on `org-mode' link calls `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "[[/tmp][desc]]")
      (goto-char 9)
      (with-mock
       (mock (hsys-org-meta-return) => t)
       (smart-org)))))

;; Compilation requires `el-mock' which is not `Package-Require'd.
;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'smart-org-tests)
;;; smart-org-tests.el ends here
