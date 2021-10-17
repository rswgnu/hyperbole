;;; hypb-ert --- ert button support                   -*- lexical-binding: t; -*-

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 31-Mar-21 at 21:11:00
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Defines an implicit button for running an ert test
;;
;; Example:
;;   Run the test hbut-defal-url
;;   <hypb-ert-sym hbut-defal-url>
;;
;;   Run the tests that start with the string, "hbut-defal"
;;   <hypb-ert-sel hbut-defal>
;;
;;   Run all Hyperbole tests
;;   <hypb-ert-sel t>

;;; Code:

(require 'ert)
(require 'hbut)
(require 'hargs)

(defun hypb-run-ert-test-libraries ()
  "Return the list of test Lisp libraries to load."
  (directory-files (expand-file-name "test" hyperb:dir) t "^[a-zA-Z].*\\.el$"))

(defun hypb-run-ert-test-symbol (test-symbol)
  "Run the specified TEST-SYMBOL ert test."
  (mapc #'load-file (hypb-run-ert-test-libraries))
  (ert (intern test-symbol)))

(defun hypb-run-ert-test-selector (test-selector)
  "Run the specified TEST-SELECTOR defined ert test."
  (mapc #'load-file (hypb-run-ert-test-libraries))
  (ert test-selector))

(defal hypb-ert-sym "hypb-run-ert-test-symbol")
(defal hypb-ert-sel "hypb-run-ert-test-selector")

(defun hypb-run-all-tests ()
  "Run every ert test."
  (interactive)
  (mapc #'load-file (hypb-run-ert-test-libraries))
  (ert t))

(provide 'hypb-ert)
;;; hypb-ert.el ends here
