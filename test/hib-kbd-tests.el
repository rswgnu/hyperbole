;;; hib-kbd-tests.el --- unit test for hib-kbd        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     26-Mar-22 at 11:25:43 by Mats Lidell
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Helper functions

;;; Code:

(require 'ert)
(require 'hib-kbd)
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")
(declare-function hy-test-helpers:should-last-message "hy-test-helpers")

(ert-deftest kbd-key-hy-about-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (should (hact 'kbd-key "C-h h d a"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name (current-buffer)) "HY-ABOUT" )))
    (kill-buffer "HY-ABOUT")))

(ert-deftest kbd-key-hy-demo-factorial-test ()
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        ;; Preload DEMO file to avoid race with *ert* buffer and set
        ;; *ert* buffer current
        (hypb:display-file-with-logo "DEMO")
        (set-buffer "*ert*")

        (should (hact 'kbd-key "C-u C-h h d d"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name (current-buffer)) "DEMO" ))
        (should (hact 'kbd-key "C-h h a factorial RET"))
        (hy-test-helpers:consume-input-events)
        (sleep-for 0.1)
        (hy-test-helpers:should-last-message "Factorial of 5 = 120"))
    (kill-buffer "DEMO")))

(provide 'hib-kbd-tests)
;;; hib-kbd-tests.el ends here
