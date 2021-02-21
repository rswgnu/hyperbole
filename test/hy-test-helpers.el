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

(provide 'hy-test-helpers)
;;; hy-test-helpers.el ends here
