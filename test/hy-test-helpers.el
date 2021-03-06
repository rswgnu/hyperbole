;;; hy-test-helpers.el --- unit test helpers         -*- lexical-binding: t; -*-

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 30-Jan-21 at 12:00:00
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cl-macs)

(defun hy-test-helpers:consume-input-events ()
  "Use recusive-edit to consume the events kbd-key generate."
  (run-with-timer 0.1 nil (lambda () (if (< 0 (recursion-depth)) (exit-recursive-edit))))
  (recursive-edit))

(defun hy-test-helpers:should-last-message (msg)
  "Verify last message is MSG."
  (with-current-buffer "*Messages*"
    (should (save-excursion
              (goto-char (point-max))
              (search-backward msg (- (point-max) 350))))))

(defun hy-test-helpers:action-key-should-call-hpath:find (str)
  "Call action-key and check that hpath:find was called with STR."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'hpath:find)
               (lambda (filename)
                 (setq was-called (should (string= str filename))))))
      (action-key)
      (should was-called))))

(provide 'hy-test-helpers)
;;; hy-test-helpers.el ends here
