;;; hactypes-test.el -- Ert tests for hactypes        -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    30-Jan-21 at 12:00:00
;; Last-Mod:      6-Feb-22 at 00:56:35 by Bob Weiner
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
  (hy-test-helpers:should-last-message "Boolean result (True) = t"))

(ert-deftest display-boolean-false-test ()
  (should (actypes::display-boolean nil))
    (hy-test-helpers:should-last-message "Boolean result (False) = nil"))

(provide 'hactypes-tests)
;;; hactypes-tests.el ends here
