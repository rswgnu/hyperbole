;;; hactypes-test.el -- Ert tests for hactypes        -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:      8-Jul-23 at 14:11:49 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'hactypes)
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:should-last-message "hy-test-helpers")

(ert-deftest display-boolean-true-test ()
  (should (actypes::display-boolean t))
  (hy-test-helpers:should-last-message "Result = t; Boolean value = True"))

(ert-deftest display-boolean-false-test ()
  (should (actypes::display-boolean nil))
    (hy-test-helpers:should-last-message "Result = nil; Boolean value = False"))

(provide 'hactypes-tests)
;;; hactypes-tests.el ends here
