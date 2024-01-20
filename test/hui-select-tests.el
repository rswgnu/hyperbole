;;; hui-select-tests.el --- Unit tests        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    14-Apr-22 at 23:45:52
;; Last-Mod:     20-Jan-24 at 15:44:04 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2024  Free Software Foundation, Inc.
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
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:should-last-message "hy-test-helpers")

(ert-deftest hui-select--at-delimited-thing-p ()
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

(ert-deftest hui-select--delimited-thing ()
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


(ert-deftest hui-select--delimited-thing-ending-in-newline ()
  "Delimited thing marks region of thing when next char after things is a newline."
  (with-temp-buffer
    (insert "(\"x\")\n")

    ;; hui-select-sexp-start
    (goto-char 1)
    (should (hui-select-delimited-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "(\"x\")\n"))))

(ert-deftest hui-select--thing ()
  "`hui-select-thing' selects bigger sections of text when called repeatedly."
  (skip-unless (not noninteractive))
  (hui-select-reset)
  (with-temp-buffer
    (insert "Buffer\n\nParagraph\nline.  One word.")
    (forward-char -3)

    ;; word
    (should (hui-select-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
		     "word"))

    ;; symbol
    (should (hui-select-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
		     "word."))

    ;; sentence
    (should (hui-select-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
		     "One word."))

    ;; line
    (should (hui-select-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                     "line.  One word."))

    ;; paragraph
    (should (hui-select-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                     "\nParagraph\nline.  One word."))

    ;; buffer
    (should (hui-select-thing))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                     "Buffer\n\nParagraph\nline.  One word."))

    ;; error
    (should-not (hui-select-thing))
    (hy-test-helpers:should-last-message
     "(hui-select-boundaries): ‘buffer’ is the largest selectable region")))

(ert-deftest hui-select--thing-interactive-prints-type-of-match ()
  "`hui-select-thing' selects bigger sections of text when called repeatedly.
Verifies right type of match is printed when `hui-select-display-type' is set to t."
  (skip-unless (not noninteractive))
  (let ((hui-select-display-type t))
    (hui-select-reset)
    (with-temp-buffer
      (insert "Buffer\n\nParagraph\nline.  One word.")
      (forward-char -3)

      (should (call-interactively 'hui-select-thing))
      (hy-test-helpers:should-last-message "word")
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "word"))

      (should (call-interactively 'hui-select-thing))
      (hy-test-helpers:should-last-message "symbol")
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "word."))

      (should (call-interactively 'hui-select-thing))
      (hy-test-helpers:should-last-message "sentence")
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "One word."))

      (should (call-interactively 'hui-select-thing))
      (hy-test-helpers:should-last-message "line")
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                       "line.  One word."))

      (should (call-interactively 'hui-select-thing))
      (hy-test-helpers:should-last-message "paragraph")
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                       "\nParagraph\nline.  One word."))

      (should (call-interactively 'hui-select-thing))
      (hy-test-helpers:should-last-message "Buffer")
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                       "Buffer\n\nParagraph\nline.  One word."))

      (should-not (call-interactively 'hui-select-thing))
      (hy-test-helpers:should-last-message
       "(hui-select-boundaries): ‘buffer’ is the largest selectable region"))))

(provide 'hui-select-tests)
;;; hui-select-tests.el ends here
