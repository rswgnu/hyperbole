;;; test-helpers-tests.el --- tests of the test helpers -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    20-Jan-25 at 20:22:05
;; Last-Mod:     20-Jan-25 at 20:44:26 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; These are tests for the test helpers used in other tests.

;;; Code:

(require 'ert)
(require 'hy-test-helpers)

(ert-deftest test-helpers-test--make-random-wikiword ()
  "Verify hy-make-random-wikiword."
  (should (= 12 (length (hy-make-random-wikiword))))
  (dolist (wwl '(3 9 21))
    (dolist (wl '(2 3 9))
      (let ((ww (hy-make-random-wikiword wwl wl)))
        (should (= wwl (length ww)))
        (should (char-uppercase-p (string-to-char (substring ww 0 1))))
        (should-not (char-uppercase-p (string-to-char (substring ww 1 2))))))))

(provide 'test-helpers-tests)
;;; test-helpers-tests.el ends here
