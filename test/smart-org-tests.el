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
(require 'el-mock)

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
(ert-deftest hib-org:org-mode-with-smart-keys-on-delimited-thing-activates ()
  "With smart keys on delimited thing activates selection."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "(hy per bo le)")
      (goto-char 1)
      (should (string= "(hy per bo le)" (substring-no-properties (current-kill 1)))))))

;; Hyperbole Button
(ert-deftest smart-org-mode-with-smart-keys-on-hypb-button-activates ()
  "With smart keys on hypb button activates."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys t))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'ibtypes::org-mode "/tmp"))))

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
       (ibtypes::org-mode)))))

;; Smart Key Context
(ert-deftest hib-org:org-mode-with-smart-keys-buttons-on-delimited-thing-calls-org-meta-return ()
  "With smart keys as buttons on delimited falls back to `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "(hy per bo le)")
      (goto-char 1)
      (ibtypes::org-mode)
      (should (string= "(hy per bo le)" (substring-no-properties (current-kill 1)))))))

;; Hyperbole Button
(ert-deftest smart-org-mode-with-smart-keys-buttons-on-hypb-button-activates ()
  "With smart keys as buttons on hypb button activates."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (hy-test-helpers:hypb-function-should-call-hpath:find 'smart-org "/tmp"))))

;; Org Link
(ert-deftest hib-org:org-mode-with-smart-keys-buttons-on-org-link-is-org-meta-return ()
  "With smart keys as buttons on `org-mode' link uses `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys 'buttons))
      (org-mode)
      (insert "[[/tmp][desc]]")
      (goto-char 9)
      (with-mock
       (mock (org-meta-return) => t)
       (ibtypes::org-mode)))))

;; Smart Key Context
(ert-deftest hib-org:org-mode-with-no-smart-keys-on-delimited-thing-calls-org-meta-return ()
  "With no smart keys on file calls `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "(hy per bo le)")
      (goto-char 1)
      (ibtypes::org-mode)
      (should (string= "(hy per bo le)" (substring-no-properties (current-kill 1)))))))

;; Hyperbole Button
(ert-deftest hib-org:org-mode-with-no-smart-keys-on-hypb-button-is-org-meta-return ()
  "With no smart keys on file calls `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "/tmp")
      (goto-char 1)
      (with-mock
       (mock (org-meta-return) => t)
       (ibtypes::org-mode)))))

;; Org Link
(ert-deftest hib-org:org-mode-with-no-smart-keys-on-org-link-is-org-meta-return ()
  "With no smart keys on `org-mode' link calls `org-meta-return'."
  (with-temp-buffer
    (let ((hsys-org-enable-smart-keys nil))
      (org-mode)
      (insert "[[/tmp][desc]]")
      (goto-char 9)
      (with-mock
       (mock (org-meta-return) => t)
       (ibtypes::org-mode)))))

(provide 'smart-org-tests)
;;; smart-org-tests.el ends here
