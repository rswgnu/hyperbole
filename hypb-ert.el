;;; hypb-ert.el --- Hyperbole test runner action button types    -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org> and Bob Weiner <rsw@gnu.org>
;;
;; Orig-Date:    31-Mar-21 at 21:11:00
;; Last-Mod:     16-Jul-23 at 23:47:09 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
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

(eval-and-compile (mapc #'require '(lisp-mode hload-path ert hact hbut hargs)))

(defun hypb-ert-message-function (_msg-pat &rest _args)
  "Ignore messages ert outputs so can display messages from tests run."
  ;; (identity (apply #'format msg-pat args)))))))
  nil)

(defun hypb-ert (selector)
  (if (memq 'message-fn (actype:params #'ert-run-tests-interactively))
      ;; Suppress ert messages so last test case message stays in the minibuffer;
      ;; 3rd arg message-fn available only in Emacs 27 and earlier
      (ert selector nil #'hypb-ert-message-function)
    (ert selector)))

(defun hypb-ert-run-test (test-name)
  "Run the specified TEST-NAME ert test."
  (hypb-ert-require-libraries)
  (let ((test-sym (intern-soft test-name)))
    (if test-sym
	(hypb-ert test-sym)
      (user-error "Invalid test name: %s" test-name))))

(defun hypb-ert-run-tests (test-selector)
  "Run the specified TEST-SELECTOR defined ert test."
  (hypb-ert-require-libraries)
  (hypb-ert (regexp-quote test-selector)))

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
  (hypb-ert t))

;; The following expression is true only when an ert-deftest has been
;; instrumented by edebug:
;; (memq 'edebug-enter (flatten-tree (ert-test-body (ert-get-test test-sym))))

(defun hypb-ert-def-at-p (&optional start-end-flag)
  "Return test name if on the name in the first line of an ert test def."
  (unless (or (eolp)
	      (memq (char-after (point))
		    '(?\( ?\) ?\[ ?\] ?{ ?} ?< ?>)))
    (save-excursion
      (forward-line 0)
      (when (looking-at (concat "(ert-deftest[ \t]+\\("
				lisp-mode-symbol-regexp
				"\\)[ \t]+("))
	(if start-end-flag
	    (list (match-string-no-properties 1) (match-beginning 1) (match-end 1))
	  (match-string-no-properties 1))))))

(defun hypb-ert-run-test-at-definition (test-name &optional debug-it)
  "Assume on the name in the first line of an ert test def, eval and run the test.
With optional DEBUG-IT non-nil (when the assist-key is pressed), edebug the
test when it is run."
  (let (test-sym)
    (setq test-sym (intern-soft test-name))
    ;; Ensure run the latest version of the test, either with the
    ;; edebugger if already instrumented for it; otherwise, with the
    ;; normal evaluator.
    (if (and test-sym debug-it)
	(edebug-defun)
      (eval-defun nil))
    (setq test-sym (intern-soft test-name))
    (when (and test-sym (ert-test-boundp test-sym))
      (when (and buffer-file-name (string-prefix-p hyperb:dir buffer-file-name))
	(hypb-ert-require-libraries))
      (hypb-ert test-sym))))

(defib hyperbole-run-test-definition ()
  "If on the name in the first line of an ert test def, eval and run the test.
With an Assist Key press instead, edebug the test and step through it."
  (let ((test-name-and-positions (hypb-ert-def-at-p t)))
    (when test-name-and-positions
      (apply #'ibut:label-set test-name-and-positions)
      (hact 'hypb-ert-run-test-at-definition (car test-name-and-positions)))))

(defun hyperbole-run-test-definition:help (_hbut)
  "If on the name in the first line of an ert test def, edebug the test."
  (let ((test-name (hypb-ert-def-at-p)))
    (when test-name
      (hypb-ert-run-test-at-definition test-name t))))

(provide 'hypb-ert)
;;; hypb-ert.el ends here
