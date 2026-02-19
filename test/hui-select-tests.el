;;; hui-select-tests.el --- Unit tests        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    14-Apr-22 at 23:45:52
;; Last-Mod:     10-Jun-25 at 17:44:04 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
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
(require 'ert-x)
(require 'hui-select)
(require 'hy-test-helpers "test/hy-test-helpers")

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
    (ert-with-message-capture cap
      (should-not (hui-select-thing))
      (hy-test-helpers:should-last-message "the largest selectable region" cap))))

(ert-deftest hui-select--thing-interactive-prints-type-of-match ()
  "`hui-select-thing' selects bigger sections of text when called repeatedly.
Verifies right type of match is printed when `hui-select-display-type' is set to t."
  (let ((hui-select-display-type t))
    (hui-select-reset)
    (with-temp-buffer
      (insert "Buffer\n\nParagraph\nline.  One word.")
      (forward-char -3)

      (ert-with-message-capture cap
        (should (call-interactively 'hui-select-thing))
        (hy-test-helpers:should-last-message "word" cap))
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "word"))

      (ert-with-message-capture cap
        (should (call-interactively 'hui-select-thing))
        (hy-test-helpers:should-last-message "symbol" cap))
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "word."))

      (ert-with-message-capture cap
        (should (call-interactively 'hui-select-thing))
        (hy-test-helpers:should-last-message "sentence" cap))
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "One word."))

      (ert-with-message-capture cap
        (should (call-interactively 'hui-select-thing))
        (hy-test-helpers:should-last-message "line" cap))
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                       "line.  One word."))

      (ert-with-message-capture cap
        (should (call-interactively 'hui-select-thing))
        (hy-test-helpers:should-last-message "paragraph" cap))
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                       "\nParagraph\nline.  One word."))

      (ert-with-message-capture cap
        (should (call-interactively 'hui-select-thing))
        (hy-test-helpers:should-last-message "buffer" cap))
      (should (string= (buffer-substring-no-properties (region-beginning) (region-end))
                       "Buffer\n\nParagraph\nline.  One word."))

      (ert-with-message-capture cap
        (should-not (call-interactively 'hui-select-thing))
        (hy-test-helpers:should-last-message "the largest selectable region" cap)))))

(provide 'hui-select-tests)
;;; hui-select-tests.el ends here
