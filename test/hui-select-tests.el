;;; hui-select-tests.el --- Unit tests        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    14-Apr-22 at 23:45:52
;; Last-Mod:     15-Apr-22 at 00:31:20 by Mats Lidell
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of Hyperbole.
;;
;;; Commentary:
;;
;; Unit tests for "../hui-select.el"

;;; Code:

;;
;;; ************************************************************************
;;; Tests
;;; ************************************************************************

(require 'ert)
(require 'hui-select)

(ert-deftest hui-select__at-delimited-thing-p ()
  "At delimited thing p returns type of thing."
  (with-temp-buffer
    (insert "(\"x\") ")

    ;; hui-select-sexp-start
    (goto-char 1)
    (should (equal (hui-select-at-delimited-thing-p) 'hui-select-sexp-start))

    ;; hui-select-string
    (goto-char 2)
    (should (equal (hui-select-at-delimited-thing-p) 'hui-select-string))

    ;; nil
    (goto-char 3)
    (should-not (hui-select-at-delimited-thing-p))

    ;; hui-select-string
    (goto-char 4)
    (should (equal (hui-select-at-delimited-thing-p) 'hui-select-string))

    ;; hui-select-sexp-end
    (goto-char 5)
    (should (equal (hui-select-at-delimited-thing-p) 'hui-select-sexp-end))))

(ert-deftest hui-select__delimited-thing ()
  "Delimited thing marks region of thing."
  (with-temp-buffer
    (insert "(\"x\") ")

    ;; hui-select-sexp-start
    (goto-char 1)
    (should (hui-select-delimited-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "(\"x\")"))

    ;; hui-select-string
    (goto-char 2)
    (should (hui-select-delimited-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "\"x\""))))


(ert-deftest hui-select__delimited-thing-ending-in-newline ()
  "Delimited thing marks region of thing when next char after things is a newline."
  (with-temp-buffer
    (insert "(\"x\")\n")

    ;; hui-select-sexp-start
    (goto-char 1)
    (should (hui-select-delimited-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "(\"x\")\n"))))

(provide 'hui-select-tests)
;;; hui-select-tests.el ends here
