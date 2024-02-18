;;; set.el --- General mathematical operators for unordered sets  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    26-Sep-91 at 19:24:19
;; Last-Mod:     18-Feb-24 at 12:51:49 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
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

;;   (set:create) creates an empty set and (set:add 'element nil) creates
;;   a new set with the single member, 'element.

;;; Code:

;; ************************************************************************
;; Public variables
;; ************************************************************************

(defvar set:equal-op 'equal
  "Comparison function used by set operators.
It must be a function of two arguments which returns non-nil only when
the arguments are equivalent.")

;; ************************************************************************
;; Public functions
;; ************************************************************************

(defun set:add (elt set)
  "Add element ELT to SET and then return SET.
Uses `set:equal-op' for comparison.
Use (setq set (set:add elt set)) to assure set is always properly modified."
  (cond ((set:member elt set) set)
	 ((listp set) (setq set (cons elt set)))
	 (t (list elt))))

(defun set:member (elt set)
  "Return non-nil if ELT is an element of SET.
The value is actually the tail of SET whose car is ELT.
Uses `set:equal-op' for comparison."
  (pcase set:equal-op
    ('equal (member elt set))
    ('eq    (memq elt set))
    ('eql   (memql elt set))
    (_      (while (and set (not (funcall set:equal-op elt (car set))))
	      (setq set (cdr set)))
	    set)))

(defun set:remove (elt set)
  "Remove element ELT from SET and return new set.
Assume SET is a valid set.  Uses `set:equal-op' for comparison.
Use (setq set (set:remove elt set)) to assure set is always properly modified."
  (pcase set:equal-op
    ('equal (delete elt set))
    ((or 'eq 'eql) (delq elt set))
    (_  (let ((rest (set:member elt set))
	      (rtn set))
	  (when rest
	    (cond ((= (length rtn) 1) (setq rtn nil))
		  ((= (length rest) 1)
		   (setcdr (nthcdr (- (length rtn) 2) rtn) nil))
		  (t (setcar rest (cadr rest))
		     (setcdr rest (cddr rest)))))
	  rtn))))

(defun set:remove-key-value (key set)
  "Remove element whose car matches KEY in SET.
Return the set.  Use (setq set (set:remove-key-value key set)) to assure set is
always properly modified.

Use `set:equal-op' to match against KEY.  Assume each element in the set has a
car and a cdr."
  (assoc-delete-all key set set:equal-op))

(defun set:combinations (set &optional arity)
  "Return all possible combinations (subsets) of SET.
This includes the empty set and the SET itself.  Assume SET is a
valid set.  With optional ARITY, return only subsets with ARITY
members."
  (cond ((null arity)
	 (setq arity 0)
	 (cons nil (apply #'nconc (mapcar (lambda (_elt) (setq arity (1+ arity)) (set:combinations set arity))
					 set))))
	((= arity 1) set)
	((<= arity 0) '(nil))
	(t (let ((rest) (ctr 1))
	     (apply
	      'nconc
	      (mapcar (lambda (first)
			(setq rest (nthcdr ctr set)
			      ctr (1+ ctr))
			(mapcar (lambda (elt)
				  (if (listp elt)
				      (cons first elt)
				    (list first elt)))
				(set:combinations rest (1- arity))))
		      set))))))

;;;###autoload
(defun set:create (&rest elements)
  "Return a new set created from any number of ELEMENTS.
If no ELEMENTS are given, return the empty set.  Uses `set:equal-op'
for comparison."
  (pcase set:equal-op
    ('equal (delete-dups elements))
    (_ (let ((set))
	 (mapc (lambda (elt) (unless (set:member elt set)
			       (setq set (cons elt set))))
	       elements)
	 (nreverse set)))))

(defalias 'set:delete #'set:remove)
(defun set:difference (&rest sets)
  "Return difference of any number of SETS.
Difference is the set of elements in the first set that are not in any of the
other sets.  Uses `set:equal-op' for comparison."
  (let ((rtn-set (set:members (car sets))))
    (mapc (lambda (set)
	    (mapc (lambda (elem) (setq rtn-set (set:remove elem rtn-set)))
		  set))
     (cdr sets))
    rtn-set))

(defalias 'set:size #'length)

(defun set:empty (set)
  "Return t if SET is empty."
  (null set))

(defun set:equal (set1 set2)
  "Return t iff SET1 contains the same members as SET2.  Both must be sets.
Uses `set:equal-op' for comparison."
  (and (listp set1) (listp set2)
       (= (set:size set1) (set:size set2))
       (set:subset set1 set2)))

(defun set:get (key set)
  "Return the value associated with KEY in SET or nil.
Assume elements of SET are of the form (key . value)."
  (cdr (car (let ((set:equal-op (lambda (key elt) (equal key (car elt)))))
	      (set:member key set)))))

(defun set:intersection (&rest sets)
  "Return intersection of all SETS given as arguments.
Uses `set:equal-op' for comparison."
  (let (rtn-set)
    (mapc (lambda (elt) (or (memq nil (mapcar (lambda (set) (set:member elt set))
					      (cdr sets)))
			    (setq rtn-set (cons elt rtn-set))))
	    (car sets))
    (nreverse rtn-set)))

(defun set:is (obj)
  "Return t if OBJ is a set (a list with no repeated elements).
Uses `set:equal-op' for comparison."
  (and (listp obj)
       (let ((lst obj))
	 (while (and (not (set:member (car lst) (cdr lst)))
		     (setq lst (cdr lst))))
	 (null lst))))

(defalias 'set:map #'mapcar)

(defun set:members (list)
  "Return set of unique elements of LIST.
Uses `set:equal-op' for comparison.  See also `set:create'."
  (let ((set))
    (mapc (lambda (elt) (or (set:member elt set) (setq set (cons elt set))))
	  list)
    (nreverse set)))

(defun set:replace (old-elt new-elt set)
  "Replace OLD-ELT with NEW-ELT in SET.  Add NEW-ELT if OLD-ELT is not in SET.
Return the set.  Use (setq set (set:replace elt set)) to assure set is
always properly modified.

Use `set:equal-op' for element comparisons."
  (set:add new-elt (set:remove old-elt set)))

(defun set:replace-key-value (key value set)
  "Replace or add element whose car matches KEY with a cdr of VALUE in SET.
Return the set.  Use (setq set (set:replace elt set)) to assure set is
always properly modified.

Use `set:equal-op' to match against KEY.  Assume each element in the set has a
car and a cdr."
  (set:add (cons key value) (set:remove-key-value key set)))

(defun set:subset (sub set)
  "Return t iff set SUB is a subset of SET.
Uses `set:equal-op' for comparison."
  ;; The empty set, nil, is a subset of every set, including
  ;; itself. Each set includes it once as a subset.
  (let ((is t))
    (while (and sub (setq is (set:member (car sub) set)))
      (setq sub (cdr sub)))
    (and is t)))

(defun set:union (&rest sets)
  "Return union of all SETS given as arguments.
Uses `set:equal-op' for comparison."
  (let (rtn-set)
    (mapc (lambda (set) (mapc (lambda (elt) (setq rtn-set (set:add elt rtn-set)))
			      set))
	  sets)
    (nreverse rtn-set)))

;; ************************************************************************
;; Private variables
;; ************************************************************************

(provide 'set)

;;; set.el ends here
