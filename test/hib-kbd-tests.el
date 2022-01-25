;;; hib-kbd-tests.el --- unit test for hib-kbd        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     24-Jan-22 at 00:38:30 by Bob Weiner
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
(load (expand-file-name "hy-test-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))
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
        (should (hact 'kbd-key "C-h h d d"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name (current-buffer)) "DEMO" ))
        (should (hact 'kbd-key "C-h h a factorial RET"))
        (hy-test-helpers:consume-input-events)
        (sleep-for 0.1)
        (hy-test-helpers:should-last-message "Factorial of 5 = 120"))
    (kill-buffer "DEMO")))

(provide 'hib-kbd-tests)
;;; hib-kbd-tests.el ends here
