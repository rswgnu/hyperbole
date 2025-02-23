;;; hsys-ert.el --- Hyperbole support for jumping to ert 'should' source lines  -*- lexical-binding: t -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Jan-25
;; Last-Mod:     22-Feb-25 at 12:20:29 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  ERT is the Emacs Regression Test framework in "ert.el".  Hyperbole uses
;;  it for all of its regression testing as well.
;;
;;  Sometimes when building Emacs Lisp tests with the ERT package, multiple
;;  tests are added to a single test fixture/function.  Each subtest
;;  has its own `should'-type clause.  But if one of these subtests fails,
;;  ERT shows you the test name and the should clause in its results buffer
;;  but if there are 12 subtests, it is difficult to match the displayed
;;  should clause to the source line that ran it.
;;
;;  The `ert-should' implicit button type defined herein solves this problem.
;;  Hyperbole loads this and then a press of the Action Key within an ert
;;  results buffer (or another buffer to which the results have been copied)
;;  produces the following:
;;
;;    - If on the first line of the result with the test name, jump to the
;;      test definition.
;;
;;    - If on a highlighted button, activate the button.
;;
;;    - Otherwise if not at the end of a line and within a failed test result,
;;      find the `should' clause and go to the matching line within the test
;;      source code.  An easy way to use it is to put point at the beginning
;;      of a line other than the first within an ert result and press the
;;      Action Key.

;;; Code:

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hbut)  ;; For defib
(require 'hmouse-tag)  ;; For smart-lisp

;;; ************************************************************************
;;; Implicit button types
;;; ************************************************************************

(defib ert-should ()
  "Jump to the source code definition of a should expr from an ert test failure.
If on the first line of a failure, jump to the source definition of the
associated test."
  (when (or (and (derived-mode-p 'ert-results-mode)
		 (save-excursion
		   (forward-line 0)
		   (or (search-backward "(ert-test-failed\n" nil t)
                       (search-forward "(ert-test-failed\n" nil t))))
	    ;; In any other mode, consider only the current line
	    (save-excursion
	      (forward-line 0)
              (search-forward "(ert-test-failed" (line-end-position) t)))
    (catch 'exit
    (save-excursion
      (save-restriction
        (forward-line 0)
        (cond ((looking-at "\\`\\|^[AFPS] ")
	       ;; On a result line with a test name, jump to the test
               (goto-char (match-end 0))
	       ;; Use the test definition name as the ibut label
               (ibut:label-set (buffer-substring-no-properties
				(+ 2 (line-beginning-position))
				(line-end-position))
			       (+ 2 (line-beginning-position))
			       (line-end-position))
               (let ((major-mode 'emacs-lisp-mode))
                 (if (button-at (point))
                     ;; jump to source buffer
                     (push-button)
                   (throw 'exit (hact 'smart-lisp)))))
               ((looking-at "\\s-*(ert-test-failed\\s-")
		(when (re-search-forward "^\\s-+(\\((should\\)\\(-\\|\\s-\\)" nil t)
                  (goto-char (match-beginning 1))))
               ((looking-at "\\s-*(\\((should\\)\\(-\\|\\s-\\)")
		(goto-char (match-beginning 1)))
               ((re-search-backward "\\`\\|^[AFPS] " nil t)
		(let ((start (point)))
                  (goto-char (1+ (point)))
                  (when (re-search-forward "^[AFPS] \\|\\'" nil t)
                    (goto-char (1- (match-beginning 0)))
                    (narrow-to-region start (point))
                    (goto-char start)
                    (when (re-search-forward "^\\s-+(\\((should\\)\\(-\\|\\s-\\)" nil t)
                      (goto-char (match-beginning 1)))))))
              (when (looking-at "(should\\(-\\|\\s-\\)")
		(let ((should-regexp (regexp-quote (thing-at-point 'sexp))))
		  (setq should-regexp (replace-regexp-in-string
                                       "[ \t\n\r\f]+" "\\s-+" (string-trim should-regexp)
                                       t t))
		  ;; follow the function link to the source file of the function
		  (when (re-search-backward "^[AFPS] " nil t)
		    (goto-char (match-end 0))
		    (let ((major-mode 'emacs-lisp-mode))
                      (if (button-at (point))
			  ;; jump to source buffer
			  (push-button)
			(smart-lisp))
                      ;; re-search-forward for should-regexp
                      (when (re-search-forward should-regexp nil t)
			(goto-char (match-beginning 0))
			(ibut:label-set "(should" (point) (+ (point) 7))
			(hact 'identity t)))))))))))

(provide 'hsys-ert)

;;; hsys-ert.el ends here
