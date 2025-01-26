;;; hasht.el --- Create hash tables from lists and operate on them  -*- lexical-binding: t -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    16-Mar-90 at 03:38:48
;; Last-Mod:     26-Jan-25 at 18:30:30 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1990-1995, 1997, 2016, 2024, 2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Featureful set of hash table operators for use in personal programs.
;;
;;   `hash-make' creates a hash table from an association list, `hash-add'
;;   adds a value-key pair to a hash table, and `hash-lookup' finds the value
;;   associated with a given key in a hash table, if any.
;;
;;   `hash-map' does the same thing as `mapcar' but operates on hash tables
;;   instead.
;;
;;   For a list of 300 items, these hash tables improve lookup times by a
;;   factor of between 8 and 10 to 1 over those for an unsorted list.
;;
;;   Public and private function names are alphabetized for easy location.


;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'subr-x)   ; for `hash-table-keys/values', `hash-table-empty-p'

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hash-merge-values-function 'hash-merge-values
  "*Hash-merge function to merge values from 2 hash tables with the same key.
It is sent the two values as arguments.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hash-add (value key hash-table)
  "Add and return VALUE of any type referenced by KEY, a string, to HASH-TABLE.
Replace any VALUE previously referenced by KEY.  VALUE should not be
nil or cannot determine whether VALUE was successfully added or not.
Do nothing and return nil if KEY or HASH-TABLE are of the wrong type.
Signal an error if anything goes wrong during addition."
  (when (and (hash-table-p hash-table)
	     (stringp key))
    (puthash (intern key) value hash-table)))

(defun hash-copy (hash-table)
  "Return a copy of HASH-TABLE.
List and vector elements are shared across both tables."
  (unless (hash-table-p hash-table)
    (error "(hash-copy): Invalid hash-table: `%s'" hash-table))
  (let ((htable-copy (make-hash-table :size (length hash-table))))
    (hash-map
     (lambda (val-key-cons) (hash-add (car val-key-cons) (cdr val-key-cons)
				      htable-copy))
     hash-table)
    htable-copy))

(defun hash-count (hash-table)
  "Return element count in HASH-TABLE or nil if not a valid hash table."
  (when (hash-table-p hash-table)
    (hash-table-count hash-table)))

(defun hash-delete (key hash-table)
  "Delete element referenced by KEY, a string, from HASH-TABLE.
Do nothing and return nil if KEY or HASH-TABLE are of the wrong type.
Otherwise, Return nil if KEY is not in HASH-TABLE or t otherwise."
  (when (and (hash-table-p hash-table)
	     (stringp key))
    (let ((key-sym (intern-soft key)))
      (when (gethash key-sym hash-table)
	(remhash key-sym hash-table)
	t))))

(defun hash-deep-copy (obj)
  "Return a copy of OBJ with new copies of all elements, except symbols."
  (cond ((null obj) nil)
	((stringp obj)
	 (copy-sequence obj))
	((hash-table-p obj)
	 (let ((htable-copy (make-hash-table :size (length obj))))
	   (maphash
	    (lambda (key _value)
	      (puthash key (hash-deep-copy obj) htable-copy))
	    obj)
	   htable-copy))
	((vectorp obj)
	 ;; convert to list for mapping
	 (setq obj (append obj nil))
	 ;; Return as a vector
	 (vconcat (mapcar 'hash-deep-copy obj)))
	((atom obj) obj)
	((consp obj)  ;; cons or list
	 (cons (hash-deep-copy (car obj)) (hash-deep-copy (cdr obj))))
	(t (error "(hash-deep-copy): Invalid type, `%s'" obj))))

(defun hash-empty-p (hash-table)
  "Return t if HASH-TABLE is empty, else nil."
  (and (hash-table-p hash-table) (hash-table-empty-p hash-table)))

(defalias  'hash-get  'hash-lookup)

(defun hash-key-p (key hash-table)
  "Return non-nil iff KEY, a string, is in HASH-TABLE.
KEY's hash table symbol is returned.  Do nothing and return nil
if KEY or HASH-TABLE are of the wrong type."
  (when (and (hash-table-p hash-table)
	     (stringp key))
    (let ((key-sym (intern-soft key)))
      (when (gethash key-sym hash-table)
	key-sym))))

(defun hash-lookup (key hash-table)
  "Lookup KEY, a string, in HASH-TABLE and return associated value.
Do nothing and return nil if KEY or HASH-TABLE are of the wrong type.
If value is nil, this function does not tell you whether or not KEY is
in the hash table.  Use `hash-key-p' instead for that function."
  (when (and (hash-table-p hash-table)
	     (stringp key))
    (let ((key-sym (intern-soft key)))
      (gethash key-sym hash-table))))

(defun hash-make (initializer &optional reverse)
  "Create and return a hash table from INITIALIZER.
INITIALIZER may be an alist with elements of the form (<value>. <key>)
from which the hash table is built (<key> must be a string).
Alternatively, it may be a non-negative integer which is used as the
minimum size of a new, empty hash table.  Optional non-nil second
argument REVERSE means INITIALIZER has elements of form
(<key> . <value>).

The resultant value associated with a <key> is the <value> from the last
entry in INITIALIZER with that <key>.  See `hash-make-prepend' to
merge all the values for a given <key> instead."
  (cond ((integerp initializer)
	 (if (>= initializer 0)
	     (make-hash-table :size initializer)
	   (error "(hash-make): Initializer must be >= 0, not `%s'"
		  initializer)))
	((numberp initializer) 
	 (error "(hash-make): Initializer must be a positive integer, not `%f'"
		initializer))
	(t (let* ((size (length initializer))
		  (hash-table (make-hash-table :size size))
		  key value sym)
	     (if reverse
		 (mapc (lambda (cns)
			 (when (consp cns)
			   (setq key (car cns) value (cdr cns)))
			 (when (setq sym (intern key))
			   (puthash sym value hash-table)))
		       initializer)
	       (mapc (lambda (cns)
		       (when (consp cns)
			 (setq key (cdr cns) value (car cns)))
		       (when (setq sym (intern key))
			 (puthash sym value hash-table)))
		     initializer))
	     hash-table))))

(defun hash-make-prepend (initializer &optional reverse)
  "Create and return a hash table from INITIALIZER.
INITIALIZER may be an alist with elements of the form (<value> . <key>) from
which the hash table is built (<key> must be a string).  Optional
non-nil second argument REVERSE means INITIALIZER has elements of form
\(<key> . <value>).

The resultant value associated with a <key> is a list of all of the <values>
given in INITIALIZER entries which contain the <key>.  The values are listed
in reverse order of occurrence (they are prepended to the list).  See
`hash-make' to use only the last value associated with a given <key>."
  (let* ((hash-table (make-hash-table :size (length initializer)))
	 key value key-sym)
    (mapc
     (lambda (cns)
       (when (consp cns)
	 (if reverse
	     (setq key (car cns) value (cdr cns))
	   (setq key (cdr cns) value (car cns))))
       (when (setq key-sym (intern key))
	 (puthash key-sym (cons value (gethash key-sym hash-table)) hash-table)))
     initializer)
    hash-table))

(defun hash-map (func hash-table)
  "Return list result of calling FUNC over each (<value> . <key>) in HASH-TABLE.
<key> is a symbol.

If FUNC is in \\='(cdr key second symbol-name), then return all <key>s
as strings.  If FUNC is in \\='(car value first symbol-value), then
return all <value>s."
  (unless (hash-table-p hash-table)
    (error "(hash-map): Invalid hash-table: `%s'" hash-table))
  (cond ((memq func '(cdr key second symbol-name))
	 (mapcar #'symbol-name (hash-table-keys hash-table)))
	((memq func '(car value first symbol-value))
	 (hash-table-values hash-table))
	(t (let ((result nil))
	     (maphash
	      (lambda (key value)
		(push (funcall func (cons value (symbol-name key)))
		      key)
		result)
	      hash-table)
	     result))))

(defun hash-merge (&rest hash-tables)
  "Merge any number of HASH-TABLES.  Return resultant hash table.
A single argument consisting of a list of hash tables may also be given.
Return an empty hash table if any argument from the merge list is other
than nil or a hash table.

Use the value of `hash-merge-values-function' to merge the values of entries
whose keys are the same."
  (let ((empty-ht (hash-make 1)))
    (and (not (hash-table-p (car hash-tables)))
	 (listp (car hash-tables))
	 ;; Handle situation where a list of hash-tables is passed in as a
	 ;; single argument, rather than as multiple arguments.
	 (setq hash-tables (car hash-tables)))
    (if (memq nil (mapcar (lambda (ht) (or (null ht) (hash-table-p ht)))
			  hash-tables))
	;; Return an empty hash table if any argument from the merge list is other
	;; than nil or a hash table
	empty-ht
      ;; Remove empty hash tables
      (setq hash-tables
	    (delq nil (mapcar (lambda (ht)
				(if (hash-table-empty-p ht) nil ht))
			      hash-tables)))
      (let ((len (length hash-tables)))
	(cond ((= len 0) empty-ht)
	      ((= len 1) (car hash-tables))
	      ;; Make the merged hash-table be 20% larger than the number of
	      ;; entries filled in all hash-tables to be merged, so that
	      ;; hash misses are minimized.
	      (t (let ((htable (hash-make
				(ceiling
				 (* 1.2 (apply '+ (mapcar 'hash-table-count
							  hash-tables))))))
		       key value)
		   (mapc
		     (lambda (ht)
		       (hash-map (lambda (val-key-cons)
				   (setq value (car val-key-cons)
					 key (cdr val-key-cons))
				   (if (gethash key htable)
				       ;; Merge values
				       (puthash
					key
					(funcall hash-merge-values-function
						 (gethash key htable)
						 value)
					htable)
				     (puthash key value htable)))
				 ht))
		     hash-tables)
		   htable)))))))

(defun hash-merge-first-value (value1 _value2)
  "Return a copy of VALUE1 for use in a hash table merge.

This is suitable for use as a value of `hash-merge-values-function'."
  ;; Copy list so that merged result does not share structure with the
  ;; hash tables being merged.
  (if (listp value1) (copy-sequence value1) value1))

(defun hash-merge-values (value1 value2)
  "Return a list from merging VALUE1 and VALUE2 or creating a new list.
Nil values are thrown away.  If both arguments are lists, their elements are
assumed to be strings and the result is a set of ordered strings.

This is suitable for use as a value of `hash-merge-values-function'."
  ;; Copy lists so that merged result does not share structure with the
  ;; hash tables being merged.
  (if (listp value1) (setq value1 (copy-sequence value1)))
  (if (listp value2) (setq value2 (copy-sequence value2)))
  (cond ((and (listp value1) (listp value2))
	 ;; Assume desired result is a set of strings.
	 (hash-set-of-strings (sort (append value1 value2) 'string-lessp)))
	((null value1)
	 value2)
	((null value2)
	 value1)
	((listp value1)
	 (cons value2 value1))
	((listp value2)
	 (cons value1 value2))
	(t (list value1 value2))))

(defun hash-prepend (value key hash-table)
  "Prepend VALUE onto the list value referenced by KEY, a string, in HASH-TABLE.
If KEY is not found in HASH-TABLE, it is added with a value of (list VALUE).

Trigger an error if an existing VALUE is not a list.  Do nothing and return nil
if KEY or HASH-TABLE are of the wrong type." 
  (when (and (hash-table-p hash-table)
	     (stringp key))
      (let* ((key-sym (intern key))
	     (key-val (gethash key-sym hash-table)))
	(if key-sym
	    (if (listp key-val) ;; allowed to be nil
		(puthash key-sym (cons value key-val) hash-table)
	      (error "(hash-prepend): `%s' key's value `%s' is not a list:" key key-val))
	  (error "(hash-prepend): Invalid hash-table key: %s" key)))))

(defun hash-prin1 (hash-table &optional stream reverse)
  "Output the printed representation of HASH-TABLE as a list.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is optional STREAM, or the value of `standard-output'.
With optional REVERSE non-nil, print each element with its key and
value in reverse order to that stored in the hash table."
  (if (not (hash-table-p hash-table))
      (progn (prin1 hash-table stream)
	     (princ "\n" stream))
    (princ "\(\n" stream)
    (if reverse
	(hash-map
	 (lambda (val-key-cons)
	   (prin1 (cons (cdr val-key-cons) (car val-key-cons)) stream)
	   (princ "\n" stream))
	 hash-table)
      (hash-map
       (lambda (val-key-cons)
	 (prin1 val-key-cons stream)
	 (princ "\n" stream))
       hash-table))
    (princ "\)\n" stream)))

(defun hash-replace (value key hash-table)
  "Replace VALUE referenced by KEY, a string, in HASH-TABLE and return VALUE.
Do nothing and return nil if KEY or HASH-TABLE are of the wrong type.
An error will occur if KEY is not found in HASH-TABLE."
  (when (and (hash-table-p hash-table)
	     (stringp key))
    (let ((key-sym (intern-soft key)))
	(if (gethash key-sym hash-table)
	    (puthash key-sym value hash-table)
	  (error "(hash-replace): `%s' key not found in hash table." key)))))

(defun hash-size (hash-table)
  "Return size of HASH-TABLE which is >= number of elements in the table.
Return nil if not a valid hash table."
  (when (hash-table-p hash-table)
    (hash-table-size hash-table)))

(defalias 'hash-length 'hash-size)

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hash-set-of-strings (sorted-strings &optional count)
  "Return SORTED-STRINGS list with any duplicate entries removed.
Optional COUNT conses number of duplicates on to front of list before return."
  (and count (setq count 0))
  (let ((elt1) (elt2) (lst sorted-strings)
	(test (if count
		  (lambda (a b)
		    (when (string-equal a b)
		      (setq count (1+ count))
		      t))
	        #'string-equal)))
    (while (setq elt1 (car lst) elt2 (car (cdr lst)))
      (if (funcall test elt1 elt2)
	  (setcdr lst (cddr lst))
	(setq lst (cdr lst)))))
  (if count (cons count sorted-strings) sorted-strings))

(provide 'hasht)
