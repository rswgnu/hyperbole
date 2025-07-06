;;; hargs-tests.el --- Tests for hargs.el                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    04-Feb-22 at 23:00:00
;; Last-Mod:      6-Jul-25 at 15:28:45 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2022-2025  Free Software Foundation, Inc.
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
          (hy-test-helpers:ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+I: ") "xyz")))
          (hy-test-helpers:ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+L: ") "xyz")))
          (hy-test-helpers:ert-simulate-keys (concat "(\"xyz\" \"" file "\")\r")
            (should (equal (hargs:get "+M: ") (list "xyz" file))))
          (hy-test-helpers:ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+V: ") "xyz")))
          (hy-test-helpers:ert-simulate-keys "xyz\r"
            (should (string= (hargs:get "+X: ") "(dir)xyz")))
          (should-error (hargs:get "+A: ") :type 'error))
      (hy-delete-file-and-buffer file))))

(ert-deftest hargs-get-verify-extension-characters-+K ()
  "Verify hyperbole extension character +K is indentified."
  (cl-letf (((symbol-function 'hargs:read) (lambda (_prompt &optional _a _b _c _d) "xyz")))
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

;;; Tests for hargs:delimited
(ert-deftest hargs-delimited-basic-test ()
  "Test basic functionality with simple delimiters."
  (with-temp-buffer
    (insert "Before [hello world] after")
    (goto-char 10) ; position inside "hello world"
    (should (string= (hargs:delimited "[" "]") "hello world"))))

(ert-deftest hargs-delimited-regexp-delimiters-test ()
  "Test with regexp delimiters."
  (with-temp-buffer
    (insert "Before (hello world) after")
    (goto-char 10) ; position inside "hello world"
    (should (string= (hargs:delimited "[\[<\(\{]" "[\]\}\)\>]" t t) "hello world"))))

(ert-deftest hargs-delimited-multiline-point-first-line-test ()
  "Test multiline expression with point on first line."
  (with-temp-buffer
    (insert "Before [hello\nworld] after")
    (goto-char 10) ; position in "hello" on first line
    (should (string= (hargs:delimited "[" "]") "hello world"))))

(ert-deftest hargs-delimited-multiline-point-second-line-test ()
  "Test multiline expression with point on second line."
  (with-temp-buffer
    (insert "Before [hello\nworld] after")
    (goto-char 15) ; position in "world" on second line
    (should (string= (hargs:delimited "[" "]") "hello world"))))

(ert-deftest hargs-delimited-escaped-delimiter-test ()
  "Test that escaped delimiters are ignored."
  (with-temp-buffer
    (insert "Before \\[not this\\] [hello world] after")
    (goto-char 28) ; position inside "hello world"
    (should (string= (hargs:delimited "[" "]") "hello world"))))

(ert-deftest hargs-delimited-matching-delimiters-test ()
  "Test proper delimiter matching."
  (with-temp-buffer
    (insert "Before [hello) world] after")
    (goto-char 10) ; position inside, but ) doesn't match [
    (should (string= (hargs:delimited "[" "]") "hello) world"))))

(ert-deftest hargs-delimited-wrong-matching-delimiters-test ()
  "Test that wrong delimiter pairs don't match."
  (with-temp-buffer
    (insert "Before [hello world) after")
    (goto-char 10) ; position inside, but ) doesn't match [
    (should-not (hargs:delimited "[" "]" nil nil))))

(ert-deftest hargs-delimited-list-positions-test ()
  "Test LIST-POSITIONS-FLAG returns list with positions."
  (with-temp-buffer
    (insert "Before [hello world] after")
    (goto-char 10) ; position inside "hello world"
    (let ((result (hargs:delimited "[" "]" nil nil t)))
      (should (listp result))
      (should (string= (car result) "hello world"))
      (should (= (nth 1 result) 9)) ; position after [
      (should (= (nth 2 result) 20))))) ; position before ]

(ert-deftest hargs-delimited-exclude-regexp-test ()
  "Test EXCLUDE-REGEXP parameter."
  (with-temp-buffer
    (insert "Before [excluded] after")
    (goto-char 10) ; position inside "excluded"
    (should (null (hargs:delimited "[" "]" nil nil nil "excluded")))))

(ert-deftest hargs-delimited-as-key-none-test ()
  "Test AS-KEY = 'none returns t."
  (with-temp-buffer
    (insert "Before [hello world] after")
    (goto-char 10) ; position inside "hello world"
    (should (eq (hargs:delimited "[" "]" nil nil nil nil 'none) t))))

(ert-deftest hargs-delimited-as-key-button-test ()
  "Test AS-KEY non-nil returns button key format."
  (with-temp-buffer
    (insert "Before [hello world] after")
    (goto-char 10) ; position inside "hello world"
    (should (string= "hello_world" (hargs:delimited
				   "[" "]" nil nil nil nil 'button)))))

(ert-deftest hargs-delimited-normalization-test ()
  "Test string normalization with extra whitespace."
  (with-temp-buffer
    (insert "Before [  hello   world  ] after")
    (goto-char 12) ; position inside
    (should (string= (hargs:delimited "[" "]") "  hello   world  "))))

(ert-deftest hargs-delimited-multiline-normalization-test ()
  "Test multiline string normalization."
  (with-temp-buffer
    (insert "Before [hello\n   world test] after")
    (goto-char 10) ; position inside
    (should (string= (hargs:delimited "[" "]") "hello world test"))))

(ert-deftest hargs-delimited-no-match-test ()
  "Test when point is not within delimited region."
  (with-temp-buffer
    (insert "Before [hello world] after")
    (goto-char 5) ; position in "Before"
    (should (null (hargs:delimited "[" "]")))))

(ert-deftest hargs-delimited-no-end-delimiter-test ()
  "Test when end delimiter is not found."
  (with-temp-buffer
    (insert "Before [hello world after")
    (goto-char 10) ; position inside
    (should (null (hargs:delimited "[" "]")))))

(ert-deftest hargs-delimited-angle-brackets-test ()
  "Test with angle brackets."
  (with-temp-buffer
    (insert "Before <hello world> after")
    (goto-char 10) ; position inside
    (should (string= (hargs:delimited "<" ">") "hello world"))))

(ert-deftest hargs-delimited-curly-braces-test ()
  "Test with curly braces."
  (with-temp-buffer
    (insert "Before {hello world} after")
    (goto-char 10) ; position inside
    (should (string= (hargs:delimited "{" "}") "hello world"))))

(ert-deftest hargs-delimited-parentheses-test ()
  "Test with parentheses."
  (with-temp-buffer
    (insert "Before (hello world) after")
    (goto-char 10) ; position inside
    (should (string= (hargs:delimited "(" ")") "hello world"))))

(ert-deftest hargs-delimited-double-quotes-basic-test ()
  "Test basic functionality with double quotes."
  (with-temp-buffer
    (insert "Before \"hello world\" after")
    (goto-char 12) ; position inside "hello world"
    (should (string= (hargs:delimited "\"" "\"") "hello world"))))

(ert-deftest hargs-delimited-double-quotes-escaped-test ()
  "Test double quotes with escaped quotes inside."
  (with-temp-buffer
    (insert "Before \"hello \\\"quoted\\\" world\" after")
    (goto-char 12) ; position inside
    (should (string= (hargs:delimited "\"" "\"") "hello /\"quoted/\" world"))))

(ert-deftest hargs-delimited-double-quotes-multiline-test ()
  "Test double quotes with multiline content."
  (with-temp-buffer
    (insert "Before \"hello\nworld\" after")
    (goto-char 12) ; position in "hello" on first line
    (should (string= (hargs:delimited "\"" "\"") "hello world"))))

(ert-deftest hargs-delimited-double-quotes-multiline-second-line-test ()
  "Test double quotes with point on second line."
  (with-temp-buffer
    (insert "Before \"hello\nworld\" after")
    (goto-char 17) ; position in "world" on second line
    (should (string= (hargs:delimited "\"" "\"") "hello world"))))

(ert-deftest hargs-delimited-double-quotes-empty-test ()
  "Test double quotes with empty content."
  (with-temp-buffer
    (insert "\"Before \\\"\\\" after\"")
    (goto-char 9) ; position inside empty quotes
    (should (string= (hargs:delimited "\"" "\"") "Before /\"/\" after"))))

(ert-deftest hargs-delimited-double-quotes-whitespace-test ()
  "Test double quotes with only whitespace."
  (with-temp-buffer
    (insert "Before \"   \" after")
    (goto-char 11) ; position inside whitespace
    (should (string= (hargs:delimited "\"" "\"") "   "))))

(ert-deftest hargs-delimited-double-quotes-no-match-test ()
  "Test when point is outside quoted region."
  (with-temp-buffer
    (insert "Before \"hello world\" after")
    (goto-char 5) ; position in "Before"
    (should (null (hargs:delimited "\"" "\"")))))

(ert-deftest hargs-delimited-double-quotes-no-closing-test ()
  "Test when closing quote is missing."
  (with-temp-buffer
    (insert "Before \"hello world after")
    (goto-char 12) ; position inside
    (should (null (hargs:delimited "\"" "\"")))))

(ert-deftest hargs-delimited-double-quotes-multiple-pairs-test ()
  "Test with multiple quote pairs, should match the one containing point."
  (with-temp-buffer
    (insert "Before \"first\" and \"second\" after")
    (goto-char 24) ; position in "second"
    (should (string= (hargs:delimited "\"" "\"") "second"))))

(ert-deftest hargs-delimited-double-quotes-list-positions-test ()
  "Test LIST-POSITIONS-FLAG with double quotes."
  (with-temp-buffer
    (insert "Before \"hello world\" after")
    (goto-char 12) ; position inside "hello world"
    (let ((result (hargs:delimited "\"" "\"" nil nil t)))
      (should (listp result))
      (should (string= (car result) "hello world"))
      (should (= (nth 1 result) 9)) ; position after opening "
      (should (= (nth 2 result) 20))))) ; position before closing "

(ert-deftest hargs-delimited-double-quotes-as-key-test ()
  "Test AS-KEY with double quotes."
  (with-temp-buffer
    (insert "Before \"hello world\" after")
    (goto-char 12) ; position inside "hello world"
    (should (string= (hargs:delimited "\"" "\"" nil nil nil nil 'button) "hello_world"))))

;; New tests for complex regex patterns
(ert-deftest hargs-delimited-complex-regex-test ()
  "Test with complex regex patterns that caused hanging."
  (with-temp-buffer
    (insert "  hello world  ")
    (goto-char 5) ; position in "hello"
    (let ((result (hargs:delimited "\\(  \\|[][()<>;&,@]\\)+"
                                  "\\(  \\|[][()<>;&,@]\\)+"
                                  t t t)))
      (should (listp result))
      (should (stringp (car result)))
      (should (string= (car result) "hello world")))))

(ert-deftest hargs-delimited-whitespace-boundary-test ()
  "Test regex that matches whitespace boundaries."
  (with-temp-buffer
    (insert "word1 word2 word3")
    (goto-char 8) ; position in "word2"
    (let ((result (hargs:delimited "\\s-+" "\\s-+" t t)))
      (should (stringp result))
      (should (string= result "word2")))))

(ert-deftest hargs-delimited-punctuation-boundary-test ()
  "Test regex that matches punctuation boundaries."
  (with-temp-buffer
    (insert "word1,word2;word3")
    (goto-char 8) ; position in "word2"
    (let ((result (hargs:delimited "[,;]" "[,;]" t t)))
      (should (stringp result))
      (should (string= result "word2")))))

(ert-deftest hargs-delimited-line-boundary-test ()
  "Test regex that matches line boundaries."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char 8) ; position in "line2"
    (let ((result (hargs:delimited "^" "$" t t)))
      (should (stringp result))
      (should (string= result "line2")))))

(ert-deftest hargs-delimited-zero-width-match-test ()
  "Test handling of zero-width matches."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 5) ; position in "hello"
    (let ((result (hargs:delimited "\\b" "\\b" t t)))
      (should (stringp result))
      (should (string= result "hello")))))

(provide 'hargs-tests)
;;; hargs-tests.el ends here
