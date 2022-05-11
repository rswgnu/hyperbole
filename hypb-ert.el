;;; hypb-ert.el --- Hyperbole test runner action button types    -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org> and Bob Weiner <rsw@gnu.org>
;;
;; Orig-Date:    31-Mar-21 at 21:11:00
;; Last-Mod:     11-May-22 at 00:00:42 by Bob Weiner
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Creates two action link implicit button types for running any Hyperbole test
;; defined in "${hyperb:dir}/test/".
;;
;; Examples:
;;   Run the test named hbut-defal-url:
;;     <hyperbole-run-test hbut-defal-url>
;;
;;   Run the tests that start with the string, "hbut-defal":
;;     <hyperbole-run-tests hbut-defal>
;;
;;   Run all Hyperbole tests:
;;     <hyperbole-run-tests t>

;;; Code:

(require 'hload-path)
(require 'ert)
(require 'hbut)
(require 'hargs)

(defun hypb-ert-run-test (test-name)
  "Run the specified TEST-NAME ert test."
  (hypb-ert-require-libraries)
  (let ((test-sym (intern-soft test-name)))
    (if test-sym
	(ert test-sym)
      (user-error "Invalid test name: %s" test-name))))

(defun hypb-ert-run-tests (test-selector)
  "Run the specified TEST-SELECTOR defined ert test."
  (hypb-ert-require-libraries)
  (ert (regexp-quote test-selector)))

(defun hypb-ert-get-require-symbols ()
  "Return the list of test Lisp library symbols to require."
  (mapcar (lambda (file)
	    (intern (substring file 0 -3)))
	  (directory-files (expand-file-name "test" hyperb:dir) nil "^[a-zA-Z].*\\.el$")))

(defun hypb-ert-require-libraries ()
  (mapc #'require (hypb-ert-get-require-symbols)))

(defal hyperbole-run-test  "hypb-ert-run-test")
(defal hyperbole-run-tests "hypb-ert-run-tests")

(defun hypb-ert-run-all-tests ()
  "Run every ert test."
  (interactive)
  (hypb-ert-require-libraries)
  (ert t))

(provide 'hypb-ert)
;;; hypb-ert.el ends here
