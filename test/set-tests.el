;;; set-tests.el --- Hyperbole mathematical set library tests          -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:     5-Feb-23 at 09:12:52
;; Last-Mod:     29-May-24 at 00:56:52 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:

;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))

(ert-deftest set-tests--function-tests-equal ()
  "Test Hyperbole set library functions."
  (should (set:equal (set:create) nil))
  (should (set:empty (set:create)))
  (should-not (set:empty (set:create 'a)))

  (should (set:is (set:create)))
  (should (set:is (set:create 'a)))
  (should-not (set:is "string is not a set"))
  (should-not (set:is 'symbol-is-not-a-set))
  (should-not (set:is 1))
  (should-not (set:is '(a a)))

  (should (= (set:size (set:create)) 0))
  (should (= (set:size (set:create 'a)) 1))
  (should (= (set:size (set:create 'a 'b)) 2))
  (should (= (set:size (set:create 'a 'a)) 1))

  (should (set:equal (set:create) (set:create)))
  (should (set:equal (set:create 'a) (set:create 'a)))
  (should (set:equal (set:create 'b 'a) (set:create 'a 'b)))

  (should (set:equal (set:member 'a (set:create 'a)) (set:create 'a)))
  (should (set:equal (set:member 'a (set:create 'a 'b)) (set:create 'a 'b)))
  (should (set:equal (set:member 'b (set:create 'a 'b)) (set:create 'b)))
  (should (set:empty (set:member 'c (set:create 'a))))

  (should (set:is (set:combinations (set:create 'a 'b 'c))))
  (should (set:equal (set:combinations (set:create 'a)) '(nil a)))
  (should (set:equal (set:combinations (set:create 'a 'b)) '(nil a b (a b))))
  (should (set:equal (set:combinations (set:create 'a 'b 'c))
                     '(nil a b c (a b) (a c) (b c) (a b c))))

  (should (set:equal (set:combinations (set:create 'a 'b 'c) 1) (set:create 'a 'b 'c)))
  (should (set:equal (set:combinations (set:create 'a 'b 'c) 2) (set:create '(a b) '(b c) '(a c))))
  (should (set:equal (set:combinations (set:create 'a 'b 'c) 3) (set:create '(a b c))))

  (should (set:empty (set:intersection (set:create) (set:create))))
  (should (set:empty (set:intersection (set:create) (set:create 'a))))
  (should (set:empty (set:intersection (set:create 'a) (set:create 'c))))
  (should (set:equal (set:intersection (set:create 'a) (set:create 'a))
                     (set:create 'a)))
  (should (set:equal (set:intersection (set:create 'a 'b) (set:create 'a))
                     (set:create 'a)))
  (should (set:equal (set:intersection (set:create 'a 'b) (set:create 'a 'c) (set:create 'a 'd))
                     (set:create 'a)))

  (should (set:empty (set:difference (set:create) (set:create))))
  (should (set:empty (set:difference (set:create 'a) (set:create 'a))))
  (should (set:empty (set:difference (set:create) (set:create 'a))))
  (should (set:equal (set:difference (set:create 'a) (set:create))
                     (set:create 'a)))
  (should (set:equal (set:difference (set:create 'a) (set:create 'c))
                     (set:create 'a)))
  (should (set:equal (set:difference (set:create 'a 'b) (set:create 'a))
                     (set:create 'b)))
  (should (set:empty (set:difference (set:create 'a 'b) (set:create 'a) (set:create 'b))))

  (should (set:empty (set:union (set:create) (set:create))))
  (should (set:equal (set:union (set:create 'a) (set:create))
                     (set:create 'a)))
  (should (set:equal (set:union (set:create 'a) (set:create 'a))
                     (set:create 'a)))
  (should (set:equal (set:union (set:create 'a) (set:create 'b))
                     (set:create 'a 'b)))
  (should (set:equal (set:union (set:create 'a) (set:create 'b) (set:create 'c))
                     (set:create 'a 'b 'c)))

  (should (set:empty (set:difference (set:create) (set:create 'a))))
  (should (set:empty (set:difference (set:create 'a) (set:create 'a))))
  (should (set:equal (set:difference (set:create 'a) (set:create))
                     (set:create 'a)))
  (should (set:equal (set:difference (set:create 'a) (set:create 'c))
                     (set:create 'a)))
  (should (set:equal (set:difference (set:create 'a 'b) (set:create 'a))
                     (set:create 'b)))
  (should (set:empty (set:difference (set:create 'a 'b) (set:create 'a) (set:create 'b))))

  (should (set:subset (set:create) (set:create)))
  (should (set:subset (set:create) (set:create 'a)))
  (should (set:subset (set:create 'a) (set:create 'a)))
  (should-not (set:subset (set:create 'a) (set:create 'b)))

  (should (set:equal (set:add 'a (set:create)) (set:create 'a)))
  (should (set:equal (set:add 'a (set:create 'a)) (set:create 'a)))
  ;; Adding a list as an element in a set
  (should (set:equal (set:add '(b c) (set:create 'a)) (set:create '(b c) 'a)))

  (should (set:empty (set:remove 'a (set:create 'a))))
  (should (set:equal (set:remove 'a (set:create 'b)) (set:create 'b)))
  (should (set:equal (set:remove 'a (set:create 'a 'b)) (set:create 'b)))
  (should-not (set:equal (set:remove 'a (set:create 'a 'b)) (set:create 'a 'b)))

  (should (set:empty (set:remove-key-value 'a (set:create (cons 'a 'value-a)))))
  (should (set:equal (set:remove-key-value 'b (set:create (cons 'a 'value-a)))
                     (set:create (cons 'a 'value-a))))
  (should (set:equal (set:remove-key-value 'b (set:create (cons 'a 'value-a) (cons 'b 'value-b)))
                     (set:create (cons 'a 'value-a))))

  ;; set:get - requires elements to be of type (key . value)
  (should (equal (set:get 'a (set:create (cons 'a 'value-a))) 'value-a))
  (should (equal (set:get 'b (set:create (cons 'a 'value-a))) nil))

  (should (set:equal (set:replace 'a 'c (set:create 'a)) (set:create 'c)))
  (should (set:equal (set:replace 'a 'c (set:create)) (set:create 'c)))
  (should (set:equal (set:replace 'b 'c (set:create 'a 'b)) (set:create 'a 'c)))

  ;; set:replace-key-value - requires elements to be of type (key . value)
  (should (set:equal (set:replace-key-value 'a 'new-value-a (set:create (cons 'a 'value-a)))
                     (set:create '(a . new-value-a))))
  (should (set:equal (set:replace-key-value 'b 'new-value-b (set:create (cons 'a 'value-a)))
                     (set:create '(b . new-value-b) '(a . value-a))))
  (should (set:equal (set:replace-key-value 'b 'new-value-b (set:create (cons 'a 'value-a) (cons 'b 'value-b)))
                     (set:create '(b . new-value-b) '(a . value-a))))

  (should (set:equal (set:members '(1)) '(1)))
  (should (set:equal (set:members '(1 2)) '(1 2)))
  (should (set:equal (set:members '(1 1)) '(1)))
  (should (set:equal (set:members '(1 1 2 2)) '(1 2))))

(ert-deftest set-tests--equal-op-tests ()
  "Test Hyperbole set library functions with equal op always true."
  (let ((set:equal-op (lambda (_x _y) t)))
    (should (= (set:size (set:create 'a 'b)) 1))
    (should (= (set:size (set:union (set:create 'a) (set:create 'b))) 1))
    (should (set:equal (set:union (set:create 'a) (set:create 'b)) (set:create 'a)))))

(provide 'set-tests)
;;; set-tests.el ends here
