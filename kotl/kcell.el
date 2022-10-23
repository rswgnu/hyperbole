;;; kcell.el --- Internal representation of koutline kcells used by kviews  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     1-May-93
;; Last-Mod:      1-Oct-22 at 15:11:43 by Bob Weiner
;;
;; Copyright (C) 1993-2022  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Defines kcells, in-memory, individually addressable elements of Koutlines,
;;   along with a persistent representation called kcell-data for writing to
;;   files.  Node text content is stored separately in the kview for efficiency.
;;
;;   To obtain the kcell at a point in the buffer, use `kcell-view:cell'.
;;   To move point to a specific cell given a reference string, use
;;   `kotl-mode:goto-cell' or `kview:goto-cell-id'.
;;
;;   For compatibility between kcell and kcell-data representations,
;;   the unique per Koutline permanent idstamp for each kcell is also stored
;;   separately.  This also allows fast retrieval.

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
;;; kcell - In-memory representation of Koutline cells
;;;

(defun kcell:copy (kcell)
  "Return a copy of KCELL."
  (copy-tree kcell))

(defun kcell:create (&optional plist)
  "Return a new kcell with optional property list, PLIST.
User id of `creator' of cell and `create-time' are added to cell's PLIST if
not already there."
  (nconc
   (unless (memq 'creator plist)
     (list 'creator hyperb:user-email
	   'create-time (htz:date-sortable-gmt)))
   plist))

(defun kcell:create-top (&optional top-cell-attributes)
  "Return a new top cell with optional property list of TOP-CELL-ATTRIBUTES.
The idstamp of the top cell is always 0 and this cell stores the
idstamp-counter."
  (kcell:create top-cell-attributes))

(defalias 'kcell:get-attr 'plist-get)

(defun kcell:is-p (object)
  "Is OBJECT a kcell?"
  (and (listp object) (plist-get object 'creator)))

;;;###autoload
(defun kcell:parse-cell-ref (cell-ref)
  "Parse CELL-REF string and return list of strings (<cell-id> <viewspec>).
If any item in the list is missing, it is nil."
  (let (cell-id
	kvspec)
    ;; !! Todo: Remove any relative specs and view specs from
    ;; cell-ref to form cell-id.  Really should account for Augment-style
    ;; relative specs here, but we don't yet support them.
    (if (and (stringp cell-ref)
	     (string-match "\\(\\.[a-zA-Z]+\\)?\\([|:][^|: \t\n\r\f] *\\)\\|\\.[a-zA-Z]+"
			   cell-ref))
	(setq cell-id (substring cell-ref 0 (match-beginning 0))
	      kvspec  (when (match-beginning 2)
			(match-string 2 cell-ref)))
      (setq cell-id cell-ref
	    kvspec nil))
    (list cell-id kvspec)))

(defun kcell:plist (kcell)
  "Return the property list of KCELL."
  (identity kcell))

;;;###autoload
(defun kcell:ref-to-id (cell-ref &optional kviewspec-flag)
  "Return a CELL-REF string converted to a cell idstamp (integer).
If CELL-REF contains both a relative and a permanent id, the permanent id is
returned.  If CELL-REF is invalid, nil is returned.

If optional KVIEWSPEC-FLAG is non-nil and CELL-REF includes a
viewspec, return the concatenation of the idstamp, an optional space
and the viewspec.

CELL-REF may be a whole number:

  12       - permanent idstamp

or any of the following string forms:
  1 or 1b   - relative id, augment style
  1.2       - relative id, legal style
  012       - permanent idstamp
  1a=012    - both relative and permanent ids (in that order) separated by =
  |viewspec - a koutliner viewspec setting, rather than a cell reference
  :viewspec - an augment viewspec, ignored for now.

Optionally, any of these id forms (or the relative form) may be followed by
zero or more whitespace characters and optionally a comma, followed by
the '|' character and some view specification characters.

Augment capabilities not yet implemented and ignored for now:
  1. Augment viewspec characters preceded by a colon
  2. Any of the above id forms followed by a period and some
     alpha characters indicating a location relative to the id."
    (cond ((integerp cell-ref)
	   (if (zerop cell-ref)
	       0
	     (when (kproperty:position 'idstamp cell-ref)
	       cell-ref)))
	  ((stringp cell-ref)
	   (let (kviewspec
		 idstamp-string)
	     ;; Remove whitespace and any comma
	     (setq cell-ref (replace-regexp-in-string "\\s-*,?\\s-*" "" cell-ref nil t))
	     (if (string-equal cell-ref "0")
		 "0"
	       ;; Ignore Augment :viewspec.
	       (when (string-match ":" cell-ref)
		 (setq cell-ref (substring cell-ref 0 (match-beginning 0))))
	       ;; Separate koutline |kviewspec from cell id.
	       (when (string-match "|" cell-ref)
		 (setq kviewspec (substring cell-ref (match-beginning 0))
		       cell-ref (substring cell-ref 0 (match-beginning 0))))
	       (setq idstamp-string
		     (cond
		      ((string-match-p "[^.= \t\n\r\f0-9a-zA-Z]" cell-ref) nil)
		      ((or (string-match "^\\([.0-9a-zA-Z]+\\)=\\(0[0-9]*\\)$"
					 cell-ref)
			   ;; idstamp only
			   (string-match "^\\(\\)\\(0[0-9]*\\)$" cell-ref))
		       (setq idstamp-string (match-string 2 cell-ref))
		       ;; Validate that idstamp value exists, else return nil
		       (when (kproperty:position 'idstamp (string-to-number idstamp-string))
			 idstamp-string))
		      ((string-match "^\\([.0-9a-zA-Z]+\\)$" cell-ref)
		       ;; relative label
		       (setq idstamp-string (match-string 1 cell-ref))
		       (save-excursion
			 (goto-char (point-min))
			 (when (re-search-forward (concat "^[ \t]*" (regexp-quote idstamp-string)
							  (regexp-quote (kview:label-separator kview)))
						  nil t)

			   (setq idstamp-string (kcell-view:idstamp))
			   ;; Validate that idstamp value exists, else return nil
			   (when (kproperty:position 'idstamp (string-to-number idstamp-string))
			     idstamp-string)))))))
	     (if idstamp-string
		 (if (and kviewspec-flag kviewspec)
		     (concat idstamp-string kviewspec)
		   (string-to-number idstamp-string))
	       kviewspec)))))
	
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

;;;
;;; kcell-data - Persistent representation of Koutline cells (written to files)
;;;

(defun kcell-data:create (cell idstamp)
  "Given a kotl CELL and IDSTAMP (an integer), return a kcell-data structure.
If CELL, its idstamp, or its property list are nil, this repairs the cell by
assuming it is the cell at point and filling in the missing information."
   (let ((plist (kcell:plist cell)))
     (if (and cell idstamp plist)
	 (vector idstamp plist)
       (kcell-data:create
	(kcell:create plist)
	(or idstamp (kview:id-increment kview))))))

(defun kcell-data:idstamp (kcell-data)
  (aref kcell-data 0))

(defun kcell-data:plist-v2 (kcell-data)
  (aref kcell-data 2))

(defun kcell-data:plist-v3 (kcell-data)
  (aref kcell-data 1))

(defun kcell-data:to-kcell-v2 (kcell-data)
  (if (vectorp kcell-data)
      (kcell:create (kcell-data:plist-v2 kcell-data))
    (kcell:create)))

(defun kcell-data:to-kcell-v3 (kcell-data)
  (if (vectorp kcell-data)
      (kcell:create (kcell-data:plist-v3 kcell-data))
    (kcell:create)))

(provide 'kcell)

;;; kcell.el ends here
