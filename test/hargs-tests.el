;;; hargs-tests.el --- Tests for hargs.el                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    04-Feb-22 at 23:00:00
;; Last-Mod:     25-Apr-25 at 19:57:44 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Tests for "../hargs.el"

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'hargs)
(require 'hy-test-helpers "test/hy-test-helpers")

(ert-deftest hargs-get-verify-extension-characters ()
  "Verify hyperbole extension characters are indentified."
  (skip-unless (not noninteractive))
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+I: ") "xyz")))
          (ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+L: ") "xyz")))
          (ert-simulate-keys (concat "(\"xyz\" \"" file "\")\r")
            (should (equal (hargs:get "+M: ") (list "xyz" file))))
          (ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+V: ") "xyz")))
          (ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+X: ") "(dir)xyz")))
          (should-error (hargs:get "+A: ") :type 'error))
      (hy-delete-file-and-buffer file))))

(ert-deftest hargs-get-verify-extension-characters-+K ()
  "Verify hyperbole extension character +K is indentified."
  (cl-letf (((symbol-function 'hargs:read) (lambda (prompt &optional a b c d) "xyz")))
    (should (string= (hargs:get "+K: ") "xyz"))))

(ert-deftest hargs-tests--sexpression-p ()
  "Verify behavior of `hargs:sexpression-p'."
  (with-temp-buffer
    (insert " (setq var (+ 1 2))  ")
    ;; pos ->123456789012345678901
    (dolist (v '((1 nil nil)
                 (2 "(setq var (+ 1 2))" "(setq var (+ 1 2))")
                 (3 "(setq var (+ 1 2))" nil)
                 (4 "(setq var (+ 1 2))" nil)
                 (11 "(setq var (+ 1 2))" nil)
                 (12 "(+ 1 2)" "(+ 1 2)")
                 (13 "(+ 1 2)" nil)
                 (18 "(+ 1 2)" nil)
                 (19 "(+ 1 2)" "(+ 1 2)")
                 (20 "(setq var (+ 1 2))" "(setq var (+ 1 2))")
                 (21 nil nil)))
      (goto-char (car v))
      (should (string= (cadr v) (hargs:sexpression-p)))
      (should (string= (caddr v) (hargs:sexpression-p t))))))

(provide 'hargs-tests)
;;; hargs-tests.el ends here
