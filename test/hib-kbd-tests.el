;;; hib-kbd-tests.el --- unit test for hib-kbd        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:     20-Jan-24 at 15:43:57 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2024  Free Software Foundation, Inc.
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
  "Test if HY-ABOUT file is displayed properly from the Hyperbole menus."
  (skip-unless (not noninteractive))
  (unwind-protect
      (progn
        (should (hact 'kbd-key "C-h h d a"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name (current-buffer)) "HY-ABOUT" )))
    (kill-buffer "HY-ABOUT")))

(ert-deftest kbd-key-hy-demo-factorial-test ()
  "Test if factorial button from DEMO file works properly."
  (skip-unless (not noninteractive))
  (unwind-protect
      (let ((enable-local-variables nil))
        (should (hact 'kbd-key "C-u C-h h d d"))
        (hy-test-helpers:consume-input-events)
        (should (string= (buffer-name (current-buffer)) "DEMO"))
        (should (hact 'kbd-key "C-h h a factorial RET"))
        (hy-test-helpers:consume-input-events))
    (kill-buffer "DEMO")))

(provide 'hib-kbd-tests)
;;; hib-kbd-tests.el ends here
