;;; hact-tests.el --- unit tests for hact                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    16-May-24 at 00:29:22
;; Last-Mod:     22-Feb-25 at 09:35:56 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'hact)

(ert-deftest hact-tests--action-params-with-lambdas ()
  "Lambda used with `action:params' should return the lambda parameters."
  (should (equal nil (action:params (lambda () nil))))
  (should (equal '(_x) (action:params (lambda (_x) nil))))
  (should (equal '(_x _y) (action:params (lambda (_x _y) nil)))))

(ert-deftest hact-tests--actype-act-with-lambdas ()
  "Lambda with `actype:act' should work over versions of Emacs.
Covers backwards incompatible change in Emacs 30."
  (should (= 2 (actype:act (lambda () 2))))
  (should (= 2 (actype:act (lambda (x) x) 2)))
  (should (= 2 (actype:act (lambda (x) (1+ x)) 1)))
  (should (= 2 (actype:act (lambda (x y) (+ x y)) 1 1))))

(provide 'hact-tests)
;;; hact-tests.el ends here
