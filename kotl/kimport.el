;;; kimport.el --- Convert and insert other outline file formats into koutlines  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Nov-93 at 11:57:05
;; Last-Mod:     25-Feb-24 at 18:03:16 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1993-2022  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'kfile)
(require 'hyrolo)
(require 'klabel)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;;###autoload
(defvar kimport:mode-alist
  '((t . kimport:text)
    (outline-mode . kimport:star-outline))
  "Alist of (major-mode . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called if the major mode of the import file matches the car of an element in
this list.  If there is no match, then `kimport:suffix-alist' is checked.  If
that yields no match, the element in this list whose car is t is used.  It
normally does an import of a koutline or text file.

Each importation-function must take two arguments, a buffer/file to import
and a buffer/file into which to insert the imported elements and a third
optional argument, CHILDREN-FLAG, which when non-nil means insert imported cells
as the initial set of children of the current cell, if any.

   `outline-mode'  - imported as an Emacs outline whose entries begin with
                     asterisks; 
   .kot
   .kotl           - imported as a structured koutline

   all others      - imported as text.")

;;;###autoload
(defvar kimport:suffix-alist
  '((".org" . kimport:star-outline)
    (".otl" . kimport:star-outline)
    (".aug" . kimport:aug-post-outline))
  "Alist of (buffer-name-suffix . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called.  Each importation-function must take two arguments, a buffer/file to
import and a buffer/file into which to insert the imported elements.
A third optional argument, CHILDREN-FLAG, may be given; when non-nil, insert
imported cells as the initial set of children of the current cell, if any.

   .org  - import format is an Emacs outline whose entries begin with asterisks;
   .otl  - import format is an Emacs outline whose entries begin with asterisks;
   .kot
   .kotl - import format is a Koutline;
   .aug  - import format is an Augment post-numbered outline
           (see https://dougengelbart.org/content/view/148/).")

(defconst kimport:star-heading "^[ \t]*\\(\\*+\\)"
  "Regular expression matching a star outline heading.
The number of stars is given by grouping 1.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun kimport:file (import-from output-to &optional children-flag)
  "Import a buffer or file IMPORT-FROM into a koutline in buffer or file OUTPUT-TO.

Any suffix in IMPORT-FROM's buffer name is used to determine the type of
importation.  All others are imported as text, one paragraph per cell.

See the documentation for the variable, `kimport:suffix-alist' for
information on specific importation formats."
  (interactive "FImport from buffer/file: \nFInsert into koutline buffer/file: \nP")
  (let ((import-buf-name
	 (cond ((or (bufferp import-from)
		    (get-buffer import-from))
		(buffer-name (get-buffer import-from)))
	       ((get-file-buffer import-from)
		(buffer-name (get-file-buffer import-from)))
	       ((stringp import-from)
		(file-name-nondirectory import-from))
	       (t (error "(kimport:buffer): `%s' is an invalid `import-from' argument"
			 import-from))))
	(function))
    (set-buffer import-buf-name)
    (unless (setq function (cdr (assq major-mode kimport:mode-alist)))
      (let ((import-suffix (when (string-match "\\.[^.]+\\'" import-buf-name)
			     (match-string 0 import-buf-name))))
	(setq function (or (cdr (assoc import-suffix kimport:suffix-alist))
			   (cdr (assq t kimport:mode-alist))))))
    (when hkey-debug
      (message "(kimport:file): Buffer: %s; Import-Function: %s"
	       import-buf-name function))
    (funcall function import-from output-to children-flag)))

(defun kimport:insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive "*bInsert buffer: ")
  (insert-buffer-substring buffer)
  (kotl-mode:add-indent-to-region))

;;;###autoload
(defun kimport:insert-file (import-from children-flag)
  "Insert each element in IMPORT-FROM as a separate cell in the current view.
Insert as sibling cells following the current cell unless prefix arg,
CHILDREN-FLAG is non-nil, then insert as the initial children of the current
cell.

IMPORT-FROM may be a buffer name or file name (file name completion is
provided).

See documentation for `kimport:file' for information on how the type of
importation is determined."
  (interactive
   (list (read-file-name
	  (if current-prefix-arg
	      "Buffer or file to insert as children of current cell: "
	    "Buffer or file to insert as siblings of current cell: "))
	 current-prefix-arg))
  (kimport:file import-from (current-buffer) children-flag))

(defun kimport:insert-file-contents (filename)
  "Insert contents of file FILENAME into current cell after point.
Set mark after the inserted text."
  (interactive "*fInsert file: ")
  (let ((tem (insert-file-contents filename)))
    (push-mark (+ (point) (car (cdr tem)))))
  (kotl-mode:add-indent-to-region))

;;;###autoload
(defun kimport:insert-register (register &optional arg)
  "Insert contents of register REGISTER at point in current cell.
REGISTER is a character naming the register to insert.
Normally puts point before and mark after the inserted text.
If optional second arg is non-nil, puts mark before and point after.
Interactively, second arg is non-nil if prefix ARG is supplied."
  (interactive "*cInsert register: \nP")
  (push-mark)
  (let ((val (get-register register)))
    (cond ((consp val)
           (insert-rectangle val))
          ((stringp val)
	   (insert val)
	   (kotl-mode:add-indent-to-region))
          ((integerp val)
           (princ val (current-buffer)))
          ((and (markerp val) (marker-position val))
           (princ (marker-position val) (current-buffer)))
          (t
           (error "Register `%c' does not contain text" register))))
  (unless arg
    ;; Do want to activate the mark here.
    (exchange-point-and-mark)))

;;; Augment right-side numbered files, blank line between cells
;;;

;;;###autoload
(defun kimport:aug-post-outline (import-from output-to &optional children-flag)
  "Insert Augment outline statements from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

If OUTPUT-TO is a new koutline, the first statement inserted will be the
first cell.  Otherwise, it will be the successor of the current cell.

Each statement to be imported is delimited by an Augment relative id at the
end of the statement.  \"1\" = level 1, \"1a\" = level 2 in outline and so
on."
  (interactive "FImport from Augment post-numbered buffer/file: \nFBuffer/file to insert cells into: \nP")
  (let ((output-level 1) (klabel "1") (count 0)
	max-pos-and-count initially-empty-output no-renumber orig-point total)
    ;; Don't change the order of import-from and output-to inits here.
    (setq import-from (kimport:copy-and-set-buffer import-from)
	  output-to (kimport:initialize output-to t)
	  orig-point (point)
	  initially-empty-output (zerop (- (point-max) (point-min)))
	  no-renumber (or initially-empty-output
			  (not (if children-flag
				   (kcell-view:child-p)
				 (kcell-view:sibling-p)))))

    (when (eq import-from output-to)
      (error "(kimport:aug-post-outline): Import and output buffers may not be the same."))

    (set-buffer import-from)
    (outline-show-all)
    (save-excursion
      (goto-char (point-min))
      ;; Total number of Augment statements.
      (setq total (count-matches " +\\([0-9][0-9a-z]*\\)\n\\(\n\\|\\'\\)"))
      (unless initially-empty-output
	;; Insert first cell as sibling of current cell.
	(set-buffer output-to)
	(cond (children-flag
	       ;; Insert as children.
	       (setq klabel (klabel:child (kcell-view:label))
		     output-level (klabel:level klabel))
	       ;; Move to end of this cell since cell insertion will
	       ;; occur at point.
	       (goto-char (kcell-view:end)))
	      ((string-empty-p (kcell-view:contents))
	       ;; This is an unused cell, fill in from here.
	       (setq klabel (kcell-view:label)
		     output-level (klabel:level klabel))
	       (goto-char (kcell-view:start)))
	      (t ;; Insert as successors.
	       (setq klabel (klabel:increment (kcell-view:label))
		     output-level (klabel:level klabel))
	       ;; Move to start of line of next tree since cell insertion
	       ;; will occur at point.
	       (goto-char (kotl-mode:tree-end)))))
      (setq max-pos-and-count (kimport:aug-post-statements
			       import-from output-to klabel
			       output-level 1 count total 0)
	    count (cdr max-pos-and-count)))
    (pop-to-buffer output-to)
    (kfile:narrow-to-kcells)
    (unless no-renumber
      (klabel-type:update-labels klabel))
    (goto-char orig-point)
    (unless (kotl-mode:buffer-empty-p)
      (kotl-mode:to-valid-position))
    (message "Imported %d of %d Augment statements." count total)))

;;;
;;; Emacs outliner style files, leading `*' cell delimiters
;;;

;;;###autoload
(defun kimport:star-outline (import-from output-to &optional children-flag)
  "Insert star outline nodes from IMPORT-FROM into koutline OUTPUT-TO.
Display and leave point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and
OUTPUT-TO and for an explanation of where imported cells are
placed.

\"* \" = level 1, \"** \" = level 2 in outline and so on."
  (interactive "fImport from star delimited cells buffer/file: \nFBuffer/file to insert cells into: \nP")
  (when (and buffer-file-name (directory-name-p import-from))
    (setq import-from buffer-file-name))
  (when (and buffer-file-name (directory-name-p output-to))
    (setq output-to (concat (file-name-sans-extension buffer-file-name) ".kotl")))
  (let ((output-level 1) (klabel "1") (count 0)
	max-pos-and-count initially-empty-output no-renumber orig-point total)
    ;; Don't change the order of import-from and output-to inits here.
    (setq import-from (kimport:copy-and-set-buffer import-from)
	  ;; Set current buffer to output-to
	  output-to (kimport:initialize output-to t)
	  orig-point (point)
	  initially-empty-output (zerop (- (point-max) (point-min)))
	  no-renumber (or initially-empty-output
			  (not (if children-flag
				   (kcell-view:child-p)
				 (kcell-view:sibling-p)))))

    (when (eq import-from output-to)
      (error "(kimport:star-outline): Import and output buffers may not be the same."))

    (set-buffer import-from)
    (outline-show-all)
    (save-excursion
      (goto-char (point-min))
      ;; If initial text in buffer is not a star outline node, add a star to
      ;; make it one, so it is not deleted from the import.
      (unless (looking-at kimport:star-heading)
	(insert "* "))
      (goto-char (point-min))
      ;; Total number of top-level cells.
      (setq total (count-matches "^[ \t]*\\*\\s-"))
      (unless initially-empty-output
	;; Insert first cell as sibling of current cell.
	(set-buffer output-to)
	(cond (children-flag
	       ;; Insert as children.
	       (setq klabel (klabel:child (kcell-view:label))
		     output-level (klabel:level klabel))
	       ;; Move to end of this cell since cell insertion will
	       ;; occur at point.
	       (goto-char (kcell-view:end)))
	      ((string-empty-p (kcell-view:contents))
	       ;; This is an unused cell, fill in from here.
	       (setq klabel (kcell-view:label)
		     output-level (klabel:level klabel))
	       (goto-char (kcell-view:start)))
	      (t ;; Insert as successors.
	       (setq klabel (klabel:increment (kcell-view:label))
		     output-level (klabel:level klabel))
	       ;; Move to start of line of next tree since cell insertion
	       ;; will occur at point.
	       (goto-char (kotl-mode:tree-end)))))
      (setq max-pos-and-count (kimport:star-entries
			       import-from output-to klabel output-level 1 count total 0)
	    count (cdr max-pos-and-count)))
    (pop-to-buffer output-to)
    (kfile:narrow-to-kcells)
    (unless no-renumber
      (klabel-type:update-labels klabel))
    (goto-char orig-point)
    (unless (kotl-mode:buffer-empty-p)
      (kotl-mode:to-valid-position))
    (message "Imported %d of %d star outline trees." count total)))

;;;
;;; Generic text file import or koutline insertion.
;;;

;;;###autoload
(defun kimport:text (import-from output-to &optional children-flag)
  "Insert text paragraphs from IMPORT-FROM into koutline OUTPUT-TO.
Display and leave point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and
OUTPUT-TO and for an explanation of where imported cells are
placed.

Import Koutlines with their structure intact.  Import text
paragraphs as a sequence of same level cells.  The variable,
`paragraph-start,' is used to determine paragraphs."
  (interactive "FImport from text/koutline buffer/file: \nFInsert cells into koutline buffer/file: \nP")
  (when (and buffer-file-name (directory-name-p import-from))
    (setq import-from buffer-file-name))
  (when (and buffer-file-name (directory-name-p output-to))
    (setq output-to (concat (file-name-sans-extension buffer-file-name) ".kotl")))
  (let ((output-level 1) (klabel "1") (count 0)
        initially-empty-output no-renumber orig-point total)
    ;; Don't change the order of import-from and output-to inits here.
    (setq import-from (kimport:copy-and-set-buffer import-from)
	  ;; Set current buffer to output-to
	  output-to (kimport:initialize output-to t)
	  orig-point (point)
	  initially-empty-output (zerop (- (point-max) (point-min)))
	  no-renumber (or initially-empty-output
			  (not (if children-flag
				   (kcell-view:child-p)
				 (kcell-view:sibling-p)))))

    (when (eq import-from output-to)
      (error "(kimport:text): Import and output buffers may not be the same."))

    (set-buffer import-from)
    (let ((kotl-import (derived-mode-p 'kotl-mode))
	  visible-cells)
      (save-excursion
	(goto-char (point-min))
	(unless initially-empty-output
	  ;; Insert first cell as sibling of current cell.
	  (set-buffer output-to)
	  (cond (children-flag
		 ;; Insert as children.
		 (setq klabel (klabel:child (kcell-view:label))
		       output-level (klabel:level klabel))
		 ;; Move to end of this cell since cell insertion will
		 ;; occur at point.
		 (goto-char (kcell-view:end)))
		((string-empty-p (kcell-view:contents))
		 ;; This is an unused cell, fill in from here.
		 (setq klabel (kcell-view:label)
		       output-level (klabel:level klabel))
		 (goto-char (kcell-view:start)))
		(t ;; Insert as successors.
		 (setq klabel (klabel:increment (kcell-view:label))
		       output-level (klabel:level klabel))
		 ;; Move to start of line of next tree since cell insertion
		 ;; will occur at point.
		 (goto-char (kotl-mode:tree-end))))))

      (if kotl-import
	  ;; Importing from a koutline, so handle specially.
	  (progn (kotl-mode:beginning-of-buffer)
		 ;; Total number of cells.
		 (setq total (count-matches "[\n\r][\n\r]")
		       visible-cells (count-matches "\n\n")
		       count (save-excursion
			       ;; Incredible non-local exit to ensure that
			       ;; recursion ends at the right time.
			       (catch 'end
				 (kimport:kcells import-from output-to klabel
						 output-level 1
						 count total)))))
	(outline-show-all)
	(goto-char (point-min))
	;; Total number of paragraphs.
	(setq total (kimport:count-paragraphs)
	      count (kimport:text-paragraphs import-from output-to klabel
					     output-level count total)))
      (pop-to-buffer output-to)
      (kfile:narrow-to-kcells)
      (unless no-renumber
	(klabel-type:update-labels klabel))
      (goto-char orig-point)
      (unless (kotl-mode:buffer-empty-p)
	(kotl-mode:to-valid-position))
      (if kotl-import
	  (message "Imported %d of %d visible cells from a %d cell outline."
		   count visible-cells total)
	(message "Imported %d of %d paragraphs." count total)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kimport:count-paragraphs ()
  "Return the number of paragraphs in the buffer based on `paragraph-separate'."
  (interactive)
  (let ((count 0) (in-between))
    (save-excursion
      (goto-char (point-min))
      (if (or (looking-at paragraph-separate)
	      (looking-at "[ \t]*\\S-"))
	  ;; Don't count this first paragraph since there typically will be
	  ;; an extra match at the end of the buffer due to blank lines.
	  (setq in-between t))
      (while (zerop (forward-line 1))
	(if (looking-at paragraph-separate)
	    (if (not in-between)
		(setq count (1+ count)
		      in-between t))
	  (setq in-between nil)))
      count)))

;;; ************************************************************************
;;; Special Private functions - Don't call these functions from outside of
;;; this module or you may misuse them and cause data corruption.
;;; ************************************************************************

(defun kimport:aug-label-lessp (label1 label2)
  "Return non-nil iff Augment-style LABEL1 is less than LABEL2."
  (let ((lev1 (klabel:level-alpha label1))
	(lev2 (klabel:level-alpha label2)))
    (cond ((< lev1 lev2))
	  ((= lev1 lev2) (string-lessp label1 label2))
	  (t nil))))

(defun kimport:aug-post-statements (import-from output-to klabel output-level
 			            import-level count total max-pos)
  "Insert post-numbered Augment statements (contents only) from IMPORT-FROM.
Is inserted in existing OUTPUT-TO.

KLABEL is the label to use for the first imported statement.
OUTPUT-LEVEL is the level at which to insert the first statement.
IMPORT-LEVEL is the depth of the current statement in the import
file, \(initially 1).

COUNT of inserted cells starts at 0.  TOTAL is the total number
of statements in IMPORT-FROM, used to show a running tally of the
imported statements.  MAX-POS is the furthest position searched
in IMPORT-FROM so far (initially 0).

Return a cons of MAX-POS and COUNT."
  (set-buffer import-from)
  (let ((start (point))
	(cell-end-regexp " +\\([0-9][0-9a-z]*\\)\n\\(\n+\\|\\'\\)")
	(case-fold-search)
	max-pos-and-count
	contents subtree-p end end-contents statement-level
	child-label)
    ;; While find cells at import-level or deeper ...
    (while (and (setq start (point))
		(re-search-forward cell-end-regexp nil t)
		(<= import-level
		   (setq statement-level
			 (klabel:level-alpha (match-string 1)))))
      (setq end-contents (match-beginning 0)
	    end (match-end 0))
      (goto-char start)
      (skip-chars-forward " ")
      (setq contents (kimport:unindent-region (point) end-contents))
      (goto-char end)
      (setq subtree-p (save-excursion
			(if (re-search-forward cell-end-regexp nil t)
			    (< statement-level
			       (klabel:level-alpha (match-string 1))))))
      (with-current-buffer output-to
	(if (and (zerop count) (string-empty-p (kcell-view:contents)))
	    ;; Reuse this initial empty cell in koutline
	    (progn (kview:insert-contents (kcell-view:cell) contents 'no-fill
					  (make-string (kcell-view:indent) ?\ ))
		   (goto-char (kcell-view:end)))
	  ;; Add the cell starting at point.
	  (kview:add-cell klabel output-level contents nil t))
	(if subtree-p (setq child-label (klabel:child klabel)))
	(message "%d of %d statements converted..."
		 (if (= statement-level 1) (setq count (1+ count)) count)
		 total)
	(setq klabel (klabel:increment klabel)))
      ;;
      ;; Current buffer returns to `import-from' here.
      ;; Handle each sub-level through recursion.
      (when subtree-p
	;; Subtree exists so insert its cells.
	(setq max-pos-and-count
	      (kimport:aug-post-statements
	       import-from output-to child-label (1+ output-level)
	       (1+ import-level) count total max-pos)
	      max-pos (car max-pos-and-count)
	      count (cdr max-pos-and-count))))
    (goto-char (setq max-pos (max end max-pos))))
  (cons max-pos count))

;;;###autoload
(defun kimport:copy-and-set-buffer (source)
  "Copy and untabify SOURCE.
Set copy buffer as current buffer for this command and return the
copy buffer.  SOURCE may be a buffer name, a buffer or a file
name.  If SOURCE buffer name begins with a space, it is not
copied under the assumption that it already has been.  If SOURCE
is a koutline, it is not copied since there is no need to copy it
to import it."
  (setq source (set-buffer (or (get-buffer source)
			       (find-file-noselect source))))
  (let ((mode (or (if (boundp 'kotl-previous-mode) kotl-previous-mode)
		  major-mode))
	copy)
    (if (or (eq mode 'kotl-mode)
	    (eq ?\ (aref (buffer-name source) 0)))
	source
      ;; This buffer name format is used so that we can easily
      ;; extract any file name suffix from the buffer name.
      (setq copy (get-buffer-create
		  (concat " " (if (string-match ".+[|<]" (buffer-name))
				  (substring (buffer-name)
					     0 (1- (match-end 0)))
				(buffer-name)))))
      (set-buffer copy)
      ;; fundamental-mode can have an unusable value of paragraph-start so
      ;; use text-mode in such instances instead.
      (if (eq mode 'fundamental-mode)
	  (text-mode)
	(funcall mode))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-buffer-substring source)
      (untabify (point-min) (point-max))
      ;; Ensure buffer ends with a newline so that we don't miss the last
      ;; element during the import.
      (goto-char (point-max))
      (unless (eq (preceding-char) ?\n)
	(insert "\n"))
      (set-buffer-modified-p nil)
      copy)))

(defun kimport:initialize (output-to &optional erase-flag)
  "Setup to import elements into koutline OUTPUT-TO.
Return OUTPUT-TO buffer and set current buffer for the current command
to OUTPUT-TO.

With optional ERASE-FLAG non-nil, erase OUTPUT-TO before importing
elements (don't append them).

OUTPUT-TO may be a buffer, `buffer-name' or file name.  If OUTPUT-TO exists
already, it must be a koutline or an error will be signaled.  For an existing
OUTPUT-TO, the text cells are inserted after the cell at point or after the
first cell for a newly loaded koutline.  If OUTPUT-TO is nil, the current
buffer is used.

If OUTPUT-TO is an existing koutline, the first cell imported will be added
as the successor of the current cell.  If an existing file is read in as
OUTPUT-TO within this function, point is left at the end of this buffer so
that imported cells will be appended to the buffer.  For a new file, this
means the first cell imported will become the first outline cell.

If a non-nil third argument, CHILDREN-FLAG, is given to the caller of this
function and OUTPUT-TO contains at least one cell, then the imported cells
will be added as children of the cell where this function leaves point
\(either the current cell or for a newly read in outline, the last cell)."
  (let* ((output-existing-buffer-p
	  (when output-to
	    (or (get-buffer output-to) (get-file-buffer output-to))))
	 (output-exists-p
	  (if output-to
	      (or output-existing-buffer-p (file-exists-p output-to))
	    ;; current buffer will be used for output and it exists.
	    t)))
    (setq output-to (if output-to
			(or (get-buffer output-to)
			    (find-file-noselect output-to))
		      (current-buffer)))
    (set-buffer output-to)
    (if (and output-exists-p (not erase-flag))
	(if (eq major-mode 'kotl-mode)
	    (unless (kotl-mode:buffer-empty-p)
	      ;; Make imported cells be appended if the output buffer was
	      ;; just read in.
	      (unless output-existing-buffer-p
		(goto-char (point-max)))
	      (kotl-mode:to-valid-position))
	  (error
	   "(kimport:initialize): Second arg, %s, must be a koutline file."
	   (buffer-name output-to)))
      (when erase-flag
	(widen)
	(delete-region (point-min) (point-max)))
      (unless (kfile:is-p)
	(setq kotl-kview nil)
	(kotl-mode))))
  output-to)

(defun kimport:kcells (import-from output-to klabel output-level
		       import-level count total)
  "Insert visible koutline cells (contents and attributes) from IMPORT-FROM.
Insert into existing OUTPUT-TO.

KLABEL is the label to use for the first imported cell.
OUTPUT-LEVEL is the level at which to insert the first cell.
IMPORT-LEVEL is the depth of the current cell in the import file,
\(initially 1).

COUNT of inserted cells starts at 0.  TOTAL is the total number of cells
in IMPORT-FROM, used to show a running tally of the imported cells.

Return COUNT."
  (set-buffer import-from)
  (goto-char (kcell-view:start))
  (let ((again t) contents subtree-p child-label)
    ;; While find cells at import-level or deeper ...
    (while (<= import-level (kcell-view:level))
      (setq subtree-p (kcell-view:child-p nil t)
	    contents (kcell-view:contents))
      (goto-char (kcell-view:end-contents))
      (with-current-buffer output-to
	;; Add the cell starting at point.
	(kview:add-cell klabel output-level contents nil t)
	(if subtree-p (setq child-label (klabel:child klabel)))
	(message "%d of %d cells inserted..."
		 (setq count (1+ count)) total)
	(setq klabel (klabel:increment klabel)))
      ;;
      ;; Current buffer returns to `import-from' here.
      ;; Handle each sub-level through recursion.
      (when (and (setq again (kcell-view:next t)) subtree-p)
	;; Subtree exists so insert its cells.
	(setq count
	      (kimport:kcells
	       import-from output-to child-label (1+ output-level)
	       (1+ import-level) count total)))
      (unless again
	(throw 'end count))))
  count)

(defun kimport:star-entries (import-from output-to klabel output-level
                             import-level count total max-pos)
  "Insert visible star outline entries from IMPORT-FROM into existing OUTPUT-TO.

KLABEL is the label to use for the first imported entry.
OUTPUT-LEVEL is the level at which to insert the first entry.
IMPORT-LEVEL is the depth of the current entry in the import file,
\(initially 1).

COUNT of inserted entries starts at 0.  TOTAL is the total number
of entries in IMPORT-FROM, used to show a running tally of the
imported entries.  MAX-POS is the furthest position searched in
IMPORT-FROM so far (initially 0).

Return a cons of MAX-POS and COUNT."
  (set-buffer import-from)
  (let ((start (point))
	(hyrolo-hdr-and-entry-regexp kimport:star-heading)
	(case-fold-search)
	max-pos-and-count
	subtree-p end contents node-level child-label)
    ;; While find cells at import-level or deeper ...
    (while (and (re-search-forward hyrolo-hdr-and-entry-regexp nil t)
		(<= import-level
		    (setq node-level
			  (length (match-string 1)))))
      (skip-chars-forward " \t")
      (setq start (point)
	    end (hyrolo-to-entry-end)
	    subtree-p (if (looking-at hyrolo-hdr-and-entry-regexp)
			  (< node-level
			     (length (match-string 1)))))
      (skip-chars-backward "\n\r")
      (setq contents (kimport:unindent-region start (point)))
      (with-current-buffer output-to
	(if (and (zerop count) (string-empty-p (kcell-view:contents)))
	    ;; Reuse this initial empty cell in koutline
	    (progn (kview:insert-contents (kcell-view:cell) contents 'no-fill
					  (make-string (kcell-view:indent) ?\ ))
		   (goto-char (kcell-view:end)))
	  ;; Add the cell starting at point.
	  (kview:add-cell klabel output-level contents nil t))
	(if subtree-p (setq child-label (klabel:child klabel)))
	(message "%d of %d trees converted..."
		 (if (= node-level 1) (setq count (1+ count)) count)
		 total)
	(setq klabel (klabel:increment klabel)))
      ;;
      ;; Current buffer returns to `import-from' here.
      (goto-char end)
      
      ;;
      ;; Handle each sub-level through recursion.
      (when subtree-p
	;; Subtree exists so insert its cells.
	(setq max-pos-and-count
	      (kimport:star-entries import-from output-to child-label
				    (1+ output-level) (1+ import-level)
				    count total max-pos)
	      max-pos (car max-pos-and-count)
	      count (cdr max-pos-and-count))))
    (goto-char (setq max-pos (max end max-pos))))
  (cons max-pos count))

(defun kimport:text-paragraphs (import-from output-to klabel
			        output-level count total)
  "Insert text paragraphs from IMPORT-FROM into existing OUTPUT-TO.
First cell is inserted with KLABEL at OUTPUT-LEVEL, as the sibling of the
previous cell, with the COUNT of inserted paragraphs starting at 0.  TOTAL is
the total number of paragraphs in IMPORT-FROM, used to show a running tally
of the imported paragraphs.

The variable, `paragraph-start' is used to determine paragraphs.

Return COUNT."
  (set-buffer import-from)
  (let* (start end contents)
    ;; Next line is needed when importing into an existing kview.
    (goto-char (point-min))
    ;; Move past blank lines at point.
    (skip-chars-forward " \t\n\r")
    (beginning-of-line)
    (while (and (setq start (point))
		(progn (while (and (not (looking-at paragraph-start))
				   (zerop (forward-line 1))))
		       t)
		(if (looking-at paragraph-start)
		    (setq end (goto-char (match-end 0))))
		(/= start end))
      (setq contents (kimport:unindent-region start end))
      (with-current-buffer output-to
	(if (and (zerop count) (string-empty-p (kcell-view:contents)))
	    ;; Reuse this initial empty cell in koutline
	    (progn (kview:insert-contents (kcell-view:cell) contents 'no-fill
					  (make-string (kcell-view:indent) ?\ ))
		   (goto-char (kcell-view:end)))
	  ;; Add the cell starting at point.
	  (kview:add-cell klabel output-level contents nil t))
	(setq count (1+ count))
	(message "%d of %d paragraphs converted..."
		 count total)
	(setq klabel (klabel:increment klabel)))
      (set-buffer import-from)
      (goto-char end)
      ;; Move past blank lines separating paragraphs.
      (skip-chars-forward " \t\n\r")
      (beginning-of-line))
    (message "%d of %d paragraphs converted" count total)
    count))

(defun kimport:unindent-region (start end)
  "Calculate indent based upon the second line within the region START to END.
Remove the indent and return the remaining region as a string."
  (save-excursion
    (let (indent-regexp)
      (goto-char start)
      ;; Remove leading indent from lines in paragraph.  Base paragraph
      ;; indent on the 2nd paragraph line since the first line might be
      ;; further indented or outdented.
      (setq indent-regexp
	    (if (re-search-forward "[\n\r][ \t]+" end t)
		(concat "^" (make-string (current-column) ?\ ))))
      (if indent-regexp
	  (replace-regexp-in-string
			  indent-regexp "" (buffer-substring start end) nil t)
	(buffer-substring start end)))))

;; Do this at the end so kotl-mode can utilize kimport definitions.
(require 'kotl-mode)

(provide 'kimport)


;;; kimport.el ends here
