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

(ert-deftest hib-org:org-mode-on-header-cycles-visibility ()
  "On an outline header `ibtypes::org-mode' cycles visibility."
  (with-temp-buffer
    (let ((bn (buffer-name)))
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

(provide 'hib-org-tests)
;;; hib-org-tests.el ends here
