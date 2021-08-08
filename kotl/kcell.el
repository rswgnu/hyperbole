;;; kcell.el --- Internal representation of koutline kcells used by kviews  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    5/1/1993
;;
;; Copyright (C) 1993-2021  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Defines kcells, nodes in Koutlines, along with a persistent representation
;;   called kcell-data for writing to files.  Node text content is stored
;;   separately in kview for efficiency. 

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(hinit htz klabel kview)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kcell:read-only-attributes
  '(idstamp creator create-time modifier mod-time)
  "List of kcell attributes which may not be modified by a user.
Add to this list but don't remove any of the default elements.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;
;;; kcell
;;;

(defun kcell:copy (kcell)
  "Return a copy of KCELL."
  (copy-tree kcell))

(defun kcell:create (idstamp &optional plist)
  "Return a new kcell which has permanent IDSTAMP (an integer) and optional additional property list, PLIST.
User id of `creator' of cell and `create-time' are added to cell's PLIST if
not already there."
  (unless (klabel:idstamp-p idstamp)
      (error "(kcell:create): Invalid `idstamp' argument: '%s'" idstamp))
  (nconc
   (list 'kcell t)
   (list 'idstamp idstamp)
   (unless (memq 'creator plist)
     (list 'creator hyperb:user-email
	   'create-time (htz:date-sortable-gmt)))
   plist))

(defun kcell:create-top (&optional file counter)
  "Return a new koutline top cell optionally attached to FILE with current idstamp COUNTER."
  (kcell:create 0
		;; id-counter = max idstamp value given out in this koutline
		(list 'id-counter (or counter 0) 'file file)))

(defalias 'kcell:get-attr 'plist-get)

(defun kcell:idstamp (kcell)
  "Return permanent idstamp of KCELL as an integer."
  (kcell:get-attr kcell 'idstamp))

(defun kcell:is-p (object)
  "Is OBJECT a kcell?"
  (and (listp object) (plist-get object 'kcell)))

(defalias 'kcell:plist 'identity)

(defun kcell:ref-to-id (cell-ref)
  "When CELL-REF is valid, return a CELL-REF string converted to a cell idstamp (integer).
If CELL-REF contains both a relative and a permanent id, the permanent id is
returned.  If CELL-REF is invalid, nil is returned.

CELL-REF may be a whole number:

  12       - permanent idstamp

or any of the following string forms:
  1 or 1b   - relative id, augment style
  1.2       - relative id, legal style
  012       - permanent idstamp
  1a=012    - both relative and permanent ids (in that order) separated by =
  |viewspec - a viewspec setting, rather than a cell reference
  :viewspec - an augment viewspec, ignored for now.

Optionally, any of these id forms (or the relative form) may be followed by
zero or more whitespace characters, a | and some view specification
characters.

Augment capabilities not yet implemented and ignored for now:
  1. Augment viewspec characters preceded by a colon
  2. Any of the above id forms followed by a period and some
     alpha characters indicating a location relative to the id."
  (cond ((integerp cell-ref)
	 (when (kproperty:position 'idstamp cell-ref)
	   cell-ref))
	((stringp cell-ref)
	 (setq cell-ref (hypb:replace-match-string "\\s-+" cell-ref "" t))
	 (let (specs
	       result)
	   ;; Ignore Augment :viewspecs.
	   (when (string-match ":" cell-ref)
	     (setq cell-ref (substring cell-ref 0 (match-beginning 0))))
	   ;; Separate koutline |viewspecs from cell id.
	   (when (string-match "\\(\\.[a-zA-Z]\\||\\)" cell-ref)
	     (setq specs (substring cell-ref (match-beginning 1))
		   cell-ref (substring cell-ref 0 (match-beginning 0))))
	   (setq result
		 (cond
		  ((string-match "[^.= \t\n\r\f0-9a-zA-Z]" cell-ref) nil)
		  ((or (string-match "^\\([.0-9a-zA-Z]+\\)=\\(0[0-9]*\\)$"
				     cell-ref)
		       ;; idstamp only
		       (string-match "^\\(\\)\\(0[0-9]*\\)$" cell-ref))
		   (setq result (string-to-number (match-string 2 cell-ref)))
		   ;; Validate that idstamp value exists, else return nil
		   (when (kproperty:position 'idstamp result)
		     result))
		  ((string-match "^\\([.0-9a-zA-Z]+\\)$" cell-ref)
		   ;; relative label
		   (setq result (match-string 1 cell-ref))
		   (save-excursion
		     (goto-char (point-min))
		     (when (re-search-forward (concat "^[ \t]*" (regexp-quote result)
						      (regexp-quote (kview:label-separator kview)))
					      nil t)

		       (setq result (string-to-number (kcell-view:idstamp)))
		       ;; Validate that idstamp value exists, else return nil
		       (when (kproperty:position 'idstamp result)
			 result))))))
	   (cond (result
		  (if specs (concat result specs) result))
		 (specs
		  (when (eq ?| (aref specs 0)) specs)))))))
	
(defun kcell:remove-attr (kcell attribute)
  "Remove KCELL's ATTRIBUTE, if any, and return modified KCELL."
  (let ((tail kcell)
	sym
	prev)
    (setq sym (car tail))
    (while (and sym (eq sym attribute))
      (setq tail (cddr tail)
	    sym (car tail)))
    (setq kcell tail
	  prev tail
	  tail (cddr tail))
    (while tail
      (setq sym (car tail))
      (if (eq sym attribute)
	  (setcdr (cdr prev) (cddr tail)))
      (setq prev tail
	    tail (cddr tail)))
    kcell))

(defalias 'kcell:set-attr 'plist-put)

(defun kcell:set-create-time (kcell)
  "Store the time of creation of KCELL."
  (kcell:set-attr kcell 'create-time (htz:date-sortable-gmt)))

(defun kcell:set-creator (kcell)
  "Store the current user's id as the creator of KCELL."
  (kcell:set-attr kcell 'creator hyperb:user-email))

(defun kcell:set-idstamp (kcell idstamp)
  "Set KCELL's permanent IDSTAMP (an integer) and return IDSTAMP."
  (kcell:set-attr kcell 'idstamp idstamp)
  (kcell:idstamp kcell))

;;;
;;; kcell-data - Persistent representation of kotl cells (written to files).
;;;

(defun kcell-data:create (cell)
  "Given a kotl CELL, return a kcell-data structure to write to a file.
If CELL, its idstamp, or its property list are nil, this repairs the cell by
assuming it is the cell at point and filling in the missing information."
   (let ((idstamp (kcell:idstamp cell))
	 (plist (nthcdr 2 (kcell:plist cell))))
     (if (and cell idstamp plist)
	 (vector idstamp plist)
       (kcell-data:create
	(kcell:create (or idstamp (kview:id-increment kview)) plist)))))

(defun kcell-data:idstamp (kcell-data)
  (aref kcell-data 0))

(defun kcell-data:plist-v2 (kcell-data)
  (aref kcell-data 2))

(defun kcell-data:plist-v3 (kcell-data)
  (aref kcell-data 1))

(defun kcell-data:to-kcell-v2 (kcell-data)
  (if (vectorp kcell-data)
      (kcell:create
       ;; Repair invalid idstamps on the fly.
       (or (kcell-data:idstamp kcell-data) (kview:id-increment kview))
       (kcell-data:plist-v2 kcell-data))
    ;; Repair invalid cells on the fly.
    (kcell:create (kview:id-increment kview))))

(defun kcell-data:to-kcell-v3 (kcell-data)
  (if (vectorp kcell-data)
      (kcell:create
       ;; Repair invalid idstamps on the fly.
       (or (kcell-data:idstamp kcell-data) (kview:id-increment kview))
       (kcell-data:plist-v3 kcell-data))
    ;; Repair invalid cells on the fly.
    (kcell:create (kview:id-increment kview))))

(provide 'kcell)

;;; kcell.el ends here
