;;; set.el --- General mathematical operators for unordered sets  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    26-Sep-91 at 19:24:19
;; Last-Mod:      8-Jan-23 at 00:08:44 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   All set operations herein work with sets of arbitrary Lisp objects,
;;   including strings.  By default, they use `equal' for comparisons
;;   but this may be overidden by changing the function bound to
;;   the `set:equal-op' variable.  The empty set is equivalent to nil.

;;   (set:create) creates an empty set.

;;; Code:
;; ************************************************************************
;; Public variables
;; ************************************************************************

(defvar set:equal-op #'equal            ;FIXME: Should end in `-function'.
  "Comparison function used by set operators.
It must be a function of two arguments which returns non-nil only when
the arguments are equivalent.")

;; ************************************************************************
;; Public macros
;; ************************************************************************

(defun set:member (elt set)
  "Return non-nil if ELT is an element of SET.
The value is actually the tail of SET whose car is ELT.
Uses `set:equal-op' for comparison."
  (while (and set (not (funcall set:equal-op elt (car set))))
    (setq set (cdr set)))
  set)

;; ************************************************************************
;; Public functions
;; ************************************************************************

;;;###autoload
(defun set:create (&rest elements)
  "Return a new set created from any number of ELEMENTS.
If no ELEMENTS are given, return the empty set.  Uses `set:equal-op'
for comparison."
  (let ((set))
    (mapc (lambda (elt) (or (set:member elt set) (setq set (cons elt set))))
	  elements)
    (nreverse set)))

;; ************************************************************************
;; Private variables
;; ************************************************************************

(provide 'set)

;;; set.el ends here
