;;; hbut.el --- GNU Hyperbole button constructs  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    18-Sep-91 at 02:57:09
;; Last-Mod:     26-Apr-25 at 10:12:23 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1991-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(cl-lib elisp-mode help-mode hversion
				    hmoccur hbmap htz hbdata hact
				    hui-select view)))
(require 'hmouse-drv) ; For `hui--ignore-action-key-depress-prev-point'.


;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function www-url "hsys-www" (url))

(defconst hbut:max-len 200
  "Maximum length of a Hyperbole button label.
If 0, there is no limit and searches for button end delimiters can go
as far as the end of the buffer.

Use the function, (hbut:max-len), to read the proper value.")

(defsubst hbut:max-len ()
  "Return the value of `hbut:max-len' if non-zero else (point-max)."
  (if (zerop hbut:max-len) (point-max) hbut:max-len))

(defvar hproperty:but-face)
(defvar hproperty:ibut-face)
(defvar hywiki-org-link-type)

(declare-function hargs:delimited "hargs")
(declare-function hargs:read-match "hargs")
(declare-function hpath:find-noselect "hpath")
(declare-function hpath:find "hpath")
(declare-function hpath:substitute-var "hpath")
(declare-function hpath:symlink-referent "hpath")
(declare-function hpath:www-p "hpath")
(declare-function hpath:shorten "hpath")
(declare-function hsys-org-block-start-at-p "hsys-org")
(declare-function hsys-org-src-block-start-at-p "hsys-org")
(declare-function hui:buf-writable-err "hui")
(declare-function hui:ebut-rename "hui")
(declare-function hui:ibut-rename "hui")
(declare-function hui:key-dir "hui")
(declare-function hui:key-src "hui")
(declare-function hywiki-get-referent "hywiki")
(declare-function kbd-key:act "hib-kbd")
(declare-function kbd-key:is-p "hib-kbd")
(declare-function org-context "org")

(declare-function hpath:file-position-to-line-and-column "hpath")

(declare-function hyrolo-hdr-move-after-p "hyrolo")
(declare-function smart-eolp "hui-mouse")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hyperb:microsoft-os-p)

;; Move up internal defconst to appear before their use
(defconst ebut:label-start "<("
  "String matching the start of a Hyperbole explicit hyper-button.")

(defconst ebut:label-end   ")>"
  "String matching the end of a Hyperbole explicit hyper-button.")

(defconst hbut:instance-sep ":"
  "String of one character, separates an ebut label from its instance num.")

;; Move up internal defvar
(defvar   hattr:filename
  (if hyperb:microsoft-os-p "_hypb" ".hypb")
  "Per directory file name in which explicit button attributes are stored.
If you change its value, you will be unable to use buttons created by
others who use a different value!")

(defvar   ibut:label-separator-regexp "\\s-*[-:=|]*\\s-+"
  "Regular expression that separates an implicit button name from its button text.")

(defvar   ibut:label-separator " - "
  "Default separator string inserted between implicit button name and its text.

See also `ibut:label-separator-regexp' for all valid characters that may be
inserted manually to separate an implicit button label from its text.")

(defconst hbut:source-prefix moccur-source-prefix
  "String prefix for lines indicating the source of the matches.

It should not contain any characters needing regular expression quoting.

This expression should be followed immediately by a buffer or file name
indicating the source of any of its Hyperbole buttons.")

;;; ************************************************************************
;;; Public definitions
;;; ************************************************************************

;;; ========================================================================
;;; ebut class - Explicit Hyperbole buttons
;;; ========================================================================

(defvar   ebut:hattr-save t
  "*Non-nil value saves button data when button source is saved.
Nil disables saving.")

(defun    ebut:act (&optional ebut)
  "Perform action for optional explicit Hyperbole button symbol EBUT.
Default is the symbol hbut:current."
  (interactive (list (hbut:get (hargs:read-match "Activate labeled Hyperbole button: "
						 (ebut:alist)
						 nil t nil 'ebut))))
  (unless ebut
    (setq ebut 'hbut:current))
  (if (ebut:is-p ebut)
      (hbut:act ebut)
    (hypb:error "(ebut:act): Expected an ebut, instead given: `%s'" ebut)))

(defun    ebut:act-label (label)
  "Activate Hyperbole explicit button with LABEL from the current buffer."
  (interactive (list (hargs:read-match "Activate explicit button labeled: "
				       (ebut:alist)
				       nil t nil 'ebut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (ebut:get lbl-key)))
    (if but
	(hbut:act but)
      (hypb:error "(ebut:act-label): No explicit button labeled: `%s'" label))))

(defun    ebut:alist (&optional file)
  "Return alist of ebuts in FILE or the current buffer.
Each element is a list of just an explicit button label.  For use
as a completion table."
  (mapcar #'list (ebut:list file)))

(defun    ebut:at-p (&optional start-delim end-delim)
  "Return explicit Hyperbole button at point or nil.
Assume point is within first line of button label, if at all.
Optional START-DELIM and END-DELIM are strings that override default
button delimiters."
  (ebut:get nil nil nil start-delim end-delim))

(defun    ebut:create (&optional but-sym)
  "Create Hyperbole explicit button based on optional BUT-SYM.
Default is the symbol hbut:current.
Button should hold the following attributes (see `hattr:set'):
   lbl-key (normalized button label string),
   loc     (filename or buffer where button is located),
   dir     (directory name where button is located),
   actype  (action type that provides a default action for the button),
   action  (optional action that overrides the default),
   args    (list of arguments for action, if action takes a single
            argument of the button lbl-key, args may be nil).

If successful, return any instance number to append to button label
except when instance number would be \"1\", then return t.  On failure,
return nil.

If successful, leave point in button data buffer, so caller should use
`save-excursion'.  Does not save button data buffer."
  (let ((lbl-instance (hbdata:write nil but-sym)))
    (run-hooks 'ebut-create-hook)
    lbl-instance))

(defun    ebut:delete (&optional but-sym)
  "Delete Hyperbole explicit button based on optional BUT-SYM.
Default is the symbol hbut:current.
Return entry deleted (a list of attribute values) or nil."
  (unless but-sym
    (setq but-sym (ebut:at-p)))
  (when (ebut:is-p but-sym)
    (let* ((but-key (hattr:get but-sym 'lbl-key))
	   (loc     (hattr:get but-sym 'loc))
	   (entry   (hbdata:delete-entry but-key loc)))
      (run-hooks 'ebut-delete-hook)
      entry)))

(defun    ebut:edit (&optional lbl-key but-sym new-lbl-key)
  "Edit explicit Hyperbole button from optional LBL-KEY and BUT-SYM.
Defaults are the key for any button label at point and `hbut:current'.
If successful, return button's instance number, except when instance
number is 1, then return t.  On failure, as when button does not exist,
return nil.

Do not save button data buffer."
  (save-excursion
    (let ((lbl-instance (hbdata:write lbl-key but-sym new-lbl-key)))
      (run-hooks 'ebut-edit-hook)
      lbl-instance)))

(defun    ebut:get (&optional lbl-key buffer key-src start-delim end-delim)
  "Return explicit Hyperbole button symbol given by LBL-KEY and BUFFER.
KEY-SRC is given when retrieving global buttons and is the full
source pathname.  START-DELIM and END-DELIM are strings that
override default button delimiters.

Retrieve button data, convert into a button object and return a symbol
which references the button.

All arguments are optional.  When none are given, return a symbol for
the button that point is within.  BUFFER defaults to the current
buffer.

Return nil if no matching button is found."
  (hattr:clear 'hbut:current)
  (save-excursion
    (let (but-data
	  key-dir
	  key-file
	  lbl-end
	  lbl-key-and-pos
	  lbl-start)
      (unless lbl-key
	(setq lbl-key-and-pos (ebut:label-p nil start-delim end-delim t)
	      lbl-key   (nth 0 lbl-key-and-pos)
	      lbl-start (nth 1 lbl-key-and-pos)
	      lbl-end   (nth 2 lbl-key-and-pos)))
      (when buffer
	  (if (bufferp buffer)
	      (set-buffer buffer)
	    (error "(ebut:get): Invalid buffer argument: %s" buffer)))
      (when (not key-src)
	(when (not (equal lbl-key (ebut:label-p nil start-delim end-delim)))
	  (goto-char (point-min))
	  (ebut:next-occurrence lbl-key))
	(when (setq key-src (ebut:to-key-src 'full))
	  ;; `ebut:to-key-src' sets current buffer to key-src buffer.
	  (setq buffer (current-buffer))))
      (when (and (stringp lbl-key) key-src)
	(when (stringp key-src)
	  (setq key-dir (file-name-directory key-src)
		key-file (file-name-nondirectory key-src)))
	(setq but-data (and key-src
			    (hbdata:get-entry lbl-key (or key-file key-src)
					      key-dir)))
	(when but-data
	  (hattr:set 'hbut:current 'lbl-key lbl-key)
	  (when lbl-start
	    (hattr:set 'hbut:current 'lbl-start lbl-start))
	  (when lbl-end
	    (hattr:set 'hbut:current 'lbl-end lbl-end))
	  (hattr:set 'hbut:current 'loc key-src)
	  (hattr:set 'hbut:current 'categ 'explicit)
	  (hattr:set 'hbut:current 'action nil)
	  (hattr:set 'hbut:current 'actype
		     (intern (hbdata:actype but-data)))
	  (hattr:set 'hbut:current 'args (hbdata:args but-data))
	  (hattr:set 'hbut:current 'creator (hbdata:creator but-data))
	  (hattr:set 'hbut:current
		     'create-time (hbdata:create-time but-data))
	  (hattr:set 'hbut:current
		     'modifier (hbdata:modifier but-data))
	  (hattr:set 'hbut:current
		     'mod-time (hbdata:mod-time but-data))
	  'hbut:current)))))

(defun    ebut:is-p (object)
  "Return non-nil if OBJECT is a symbol representing an explicit Hyperbole button."
  (and (symbolp object)
       (eq (hattr:get object 'categ) 'explicit)))

(defun    ebut:key (ebut)
  "Return the key for Hyperbole explicit button symbol EBUT."
  (if (ebut:is-p ebut)
      (hattr:get ebut 'lbl-key)
    (error "(ebut:key): Argument is not a Hyperbole explicit button symbol, `%s'"
	   ebut)))

(defun    ebut:key-of-label-p (key label)
  "Return t iff KEY matches to LABEL in a case insensitive manner."
  (and (stringp key) (stringp label)
       (equal key (downcase (ebut:label-to-key label)))))

(defalias 'ebut:to-key-src         #'hbut:to-key-src)
(defalias 'ebut:key-src-set-buffer #'hbut:key-src-set-buffer)
(defalias 'ebut:key-src-fmt        #'hbut:key-src-fmt)
(defalias 'ebut:key-to-label       #'hbut:key-to-label)

(defvar   hbut:max-len)

(defun    ebut:label-p (&optional as-label start-delim end-delim pos-flag two-lines-flag)
  "Return key for the explicit button label that point is within, else nil.
This is the normalized key form of the explicit button's label.

Assume point is within the first line of any button label.  All
following arguments are optional.  If AS-LABEL is non-nil, return
label rather than the key derived from the label.  START-DELIM
and END-DELIM are strings that override default button
delimiters.  With POS-FLAG non-nil, return the list of label-or-key,
but-start-position, but-end-position.  Positions include
delimiters.  With TWO-LINES-FLAG non-nil, constrain label search
to two lines."
  (let ((opoint (point))
	(quoted "\\(^\\|[^\\{$]\\)")
	;; For <> delimited action buttons which can be long
	;; sexpressions, don't enforce the normal, short button length
	;; limit.  Setting this to 0 means unlimited length, assuming
	;; the TWO-LINES-FLAG is nil.
	(hbut:max-len
	 (if (and (string-equal start-delim "<")
		  (string-equal end-delim ">"))
	     0
	   hbut:max-len))
	npoint start lbl-key end but-start but-end start-regexp end-regexp)
    (unless start-delim (setq start-delim ebut:label-start))
    (unless end-delim (setq end-delim ebut:label-end))
    (setq npoint (+ opoint (length start-delim))
	  start-regexp (regexp-quote start-delim)
	  end-regexp (regexp-quote end-delim))
    ;; Ensure label is not blank and point is within matching delimiters
    (save-excursion
      (forward-line 0)
      (while (and (progn
		    (while (and (< (point) npoint)
				(re-search-forward (concat quoted start-regexp) npoint t))
		      (setq start t))
		    start)
		  ;; Handle expressions like:
		  ;; { M-x shell RET M-> (cd ${hyperb:dir}) RET }
		  (save-excursion
		    (when (eq ?\( (char-syntax (preceding-char)))
		      (ignore-errors (forward-char -1) (forward-list)))
		    (< (point) opoint))
		  (re-search-forward (concat "[^\\{]" end-regexp) opoint t))
	(setq start nil))
      (when start
	(setq start (point)
	      but-start (match-end 1))
	(if (eq ?\( (char-syntax (preceding-char)))
	    (condition-case ()
		(progn
		  (forward-char -1)
		  (forward-list)
		  (forward-char -2))
	      (error (goto-char (max (1- opoint) start))))
	  (goto-char (max (1- opoint) start)))
	(when two-lines-flag
	  (save-excursion
	    (forward-line 2)
	    (setq hbut:max-len (- (point) start))))
	(and (< (point) (+ start (hbut:max-len)))
	     (re-search-forward (concat quoted end-regexp) (+ start (hbut:max-len)) t)
	     (setq but-end (point)
		   end (- (point) (length end-delim))
		   lbl-key (ebut:label-to-key (buffer-substring-no-properties start end)))
	     (cond (pos-flag
		    (if as-label
			(list (ebut:key-to-label lbl-key) but-start but-end)
		      (list lbl-key but-start but-end)))
		   (t (if as-label (ebut:key-to-label lbl-key) lbl-key))))))))

(defalias 'ebut:label-regexp           #'hbut:label-regexp)
(defalias 'ebut:label-instances-regexp #'hbut:label-instances-regexp)

(defalias 'ebut:label-to-key #'hbut:label-to-key)

(defun    ebut:list (&optional file loc-p)
  "Return list of explicit button labels from in FILE or the current buffer.
Remove duplicate labels if optional LOC-P is omitted.  With LOC-P, return
list of elements (label start end) where start and end are the buffer
positions at which the button delimiter begins and ends."
  (interactive)
  (setq file (if file
		 (when (file-exists-p file)
		   (find-file-noselect file))
	       (current-buffer)))
  (when file
    (set-buffer file)
    (let ((buts (ebut:map (if loc-p
			      (lambda (lbl start end)
				;; Normalize label spacing
				(list (ebut:key-to-label (ebut:label-to-key lbl))
				      start end))
			    (lambda (lbl _start _end)
			      ;; Normalize label spacing
			      (ebut:key-to-label (ebut:label-to-key lbl)))))))
      (if loc-p buts (when buts (apply #'set:create buts))))))

(defalias 'map-ebut #'ebut:map)

(defun    ebut:map (but-func &optional regexp-match include-delims)
  "Apply BUT-FUNC to the explicit buttons in the visible part of current buffer.
If REGEXP-MATCH is non-nil, only buttons which match this argument are
considered.

BUT-FUNC must take precisely three arguments: the button label, the
start position of the delimited button label and its end position (positions
include delimiters when INCLUDE-DELIMS is non-nil)."
  (hbut:map-type but-func ebut:label-start ebut:label-end regexp-match include-delims))

(defun    ebut:next-occurrence (lbl-key &optional buffer)
  "Move point to next occurrence of button with LBL-KEY in optional BUFFER.
BUFFER defaults to current buffer.  It may be a buffer name.
Return non-nil iff occurrence is found.

Remember to use (goto-char (point-min)) before calling this in order to
move to the first occurrence of the button."
  (if buffer
      (if (not (or (bufferp buffer)
		   (and (stringp buffer) (get-buffer buffer))))
	  (error "(ebut:next-occurrence): Invalid buffer arg: %s" buffer)
	(switch-to-buffer buffer)))
  (if (re-search-forward (ebut:label-regexp lbl-key) nil t)
      (goto-char (+ (match-beginning 0) (length ebut:label-start)))))

(defun    ebut:operate (curr-label new-label)
  "Create an in-buffer ebutton named CURR-LABEL.  Modify if NEW-LABEL is given.

If CURR-LABEL is nil, the text in the active region is used as the
button label, if any, otherwise, an error is signaled.

Return instance string appended to label to form a per-buffer unique
label; nil if label is already unique.  Signal an error when no such
button is found in the current buffer."
  (let* ((lbl-key (ebut:label-to-key curr-label))
	 (lbl-regexp (ebut:label-regexp lbl-key))
	 (new-lbl-key (ebut:label-to-key new-label))
	 (modify new-label)
	 (new-instance-flag))

    (when (and new-label (or (not (stringp new-label)) (string-empty-p new-label)))
      (hypb:error "(ebut:operate): 'new-label' value must be a non-empty string, not: '%s'"
		  new-label))
    (when (and (null curr-label) (not (use-region-p)))
      (hypb:error "(ebut:operate): region must be active when 'curr-label' is nil"))

    ;; Error when on a read-only part of a buffer's text
    (when (plist-member (text-properties-at (point)) 'read-only)
      (hypb:error "(ebut:operate): Point must not be on a read-only Org element"))
    ;; Error when on an implicit button
    (when (or (eq (hattr:get 'hbut:current 'categ) 'implicit)
	      (string-prefix-p "ibtypes::" (symbol-name (hattr:get 'hbut:current 'categ))))
      (hypb:error "(ebut:operate): Point must not be on an implicit button: %s"
		  (ibut:label-to-key (hattr:get 'hbut:current 'lbl-key))))
    ;; Error when on an Emacs push-button
    (when (plist-member (text-properties-at (point)) 'button)
      (hypb:error "(ebut:operate): Point must not be on an Emacs push-button: %s"
		  (button-label (button-at (point)))))
    ;; Error when in read-only contexts of an Org file
    (when (hsys-org-at-read-only-p)
      (hypb:error "(ebut:operate): Point must not be in a read-only Org context"))

    (unless new-label
      (setq new-label curr-label))
    (hattr:set 'hbut:current 'lbl-key (ebut:label-to-key new-label))
    (save-excursion
      (when (setq new-instance-flag
		  (if modify (ebut:edit lbl-key nil new-lbl-key) (ebut:create)))
	(when (hmail:editor-p)
	  (hmail:msg-narrow))))
    (cond (modify
	    ;; Rename all occurrences of button - those with same label
	    (let* ((but-key-and-pos (ebut:label-p nil nil nil 'pos))
		   (at-but (equal (car but-key-and-pos)
				  (ebut:label-to-key new-label))))
	      (when at-but
		(ebut:delimit (nth 1 but-key-and-pos)
			      (nth 2 but-key-and-pos)
			      new-instance-flag))
	      (cond ((ebut:map
		      (lambda (_lbl start end)
			(delete-region start end)
			(ebut:delimit
			 (point)
			 (progn (insert new-label) (point))
			 new-instance-flag))
		      lbl-regexp 'include-delims))
		    (at-but)
		    ((hypb:error "(ebut:operate): No button matching: %s" curr-label)))))

	  (new-instance-flag
	   ;; Add a new button recording its start and end positions
	   (let (start end mark prev-point buf-lbl)
	     (cond ((not curr-label)
		    (setq start (point))
		    (insert new-label)
		    (setq end (point)))
		   ((and (hmouse-use-region-p)
			 (if (hyperb:stack-frame
			      '(hui:ebut-create hui:ebut-edit hui:ebut-edit-region
						hui:ebut-link-create hui:gbut-create
                       				hui:gbut-edit ebut:program
						hui:ibut-create hui:ibut-edit
						hui:ibut-link-create ibut:program))
			     ;; Ignore action-key-depress-prev-point
			     (progn (setq mark (marker-position (mark-marker))
					  start (region-beginning)
					  end (region-end)
					  buf-lbl (buffer-substring-no-properties start end))
				    (equal buf-lbl curr-label))
			   ;; Utilize any action-key-depress-prev-point
			   (setq mark (marker-position (mark-marker)))
			   (setq prev-point (and action-key-depress-prev-point
						 (marker-position action-key-depress-prev-point)))
			   (setq start (if (and prev-point mark (<= prev-point mark))
					   prev-point
					 (region-beginning))
				 end (if (and prev-point mark (> prev-point mark))
					 prev-point
				       (region-end))
				 buf-lbl (buffer-substring-no-properties start end))
			   (equal buf-lbl curr-label)))
		    nil)
		   ((progn (when start (goto-char start))
			   (looking-at (regexp-quote curr-label)))
		    (setq start (point)
			  end (match-end 0)))
		   (t (setq start (point))
		      (insert curr-label)
		      (setq end (point))))
	     (ebut:delimit start end new-instance-flag)
	     (goto-char start)))

	  (t (hypb:error
	      "(ebut:operate): Operation failed.  Check button attribute permissions: %s"
	      hattr:filename)))

    ;; Append any new-instance-flag string to the button label
    (when (stringp new-instance-flag)
      (setq new-label (concat new-label new-instance-flag))
      (hattr:set 'hbut:current 'lbl-key (ebut:label-to-key new-label)))

    ;; Position point
    (let ((new-key (ebut:label-to-key new-label)))
      (cond ((equal (ebut:label-p) new-key)
	     ;; In case right before the start of the desired
	     ;; button's delimiters.
	     (forward-char 2) (search-backward ebut:label-start nil t)
	     (goto-char (match-end 0)))
	    ((let ((regexp (ebut:label-regexp new-key)))
	       (or (re-search-forward  regexp nil t)
		   (re-search-backward regexp nil t)))
	     (goto-char (+ (match-beginning 0) (length ebut:label-start))))))

    (when (or (not (hypb:buffer-file-name)) (hmail:editor-p) (hmail:reader-p))
      (widen)
      (hmail:msg-narrow))

    ;; new-instance-flag might be 't which we don't want to return.
    (when (stringp new-instance-flag) new-instance-flag)))

(defun    ebut:program (label actype &rest args)
  "Programmatically create an explicit Hyperbole button at point.
Create button from LABEL, ACTYPE (action type), and optional actype ARGS.
Insert LABEL text at point surrounded by <( )> delimiters, adding any
necessary instance number of the button after the LABEL.  ACTYPE may
be a Hyperbole action type name (from defact) or an Emacs Lisp
function, followed by a list of arguments for the actype, aside from
the button LABEL which is automatically provided as the first argument.

For interactive creation, use `hui:ebut-create' instead."
  (save-excursion
     (let ((but-buf (current-buffer))
	   (actype-sym (actype:action actype)))
      (hui:buf-writable-err but-buf "ebut-create")
      (condition-case err
	  (progn
	    (hattr:clear 'hbut:current)
	    (hattr:set 'hbut:current 'categ 'explicit)
	    (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
	    (hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
            (if (or (and actype-sym (fboundp actype-sym))
		    (functionp actype))
		(hattr:set 'hbut:current 'actype actype)
	      (error "(%s)" actype))
	    (hattr:set 'hbut:current 'args args)
	    (ebut:operate label nil))
	(error (hattr:clear 'hbut:current)
	       (if (and (listp (cdr err)) (= (length (cdr err)) 1))
		   (error "(ebut:program): actype arg must be a bound symbol (not a string): %S" actype)
		 (error "(ebut:program): %S" err)))))))

(defun    ebut:search (string out-buf &optional match-part)
  "Write explicit button lines matching STRING to OUT-BUF.
Search across all files into which the user has previously saved
explicit buttons.  By default, find matches for whole button
labels only; optional MATCH-PART enables partial matches."
  (let*  ((buffers (mapcar (lambda (dir)
			     (expand-file-name hattr:filename dir))
			   (hbmap:dir-list)))
	  (total 0)
	  (firstmatch))
    (with-current-buffer out-buf
      (setq buffer-read-only nil)
      (widen)
      (erase-buffer)
      (let (currbuf currfile kill-buf src-matches dir)
	(while buffers
	  (setq currbuf (car buffers)
		currfile (if (stringp currbuf) currbuf)
		kill-buf (and currfile (not (get-file-buffer currfile)))
		buffers (cdr buffers))
	  (if currfile
	      (setq currbuf (and (file-readable-p currfile)
				 (find-file-noselect currfile))
		    dir (file-name-directory currfile))
	    (setq currfile (hypb:buffer-file-name currbuf)))
	  (and currfile currbuf
	       (unwind-protect
		   (setq src-matches
			 (hbdata:search currbuf string match-part))
		 (if kill-buf (kill-buffer currbuf))))
	  (if src-matches
	      (let (elt matches)
		(while src-matches
		  (setq elt (car src-matches))
		  (if (null elt) nil
		    (setq src-matches (cdr src-matches)
			  currfile (expand-file-name (car elt) dir)
			  matches (cdr elt)
			  currbuf (get-file-buffer currfile)
			  kill-buf (not currbuf)
			  currbuf (or currbuf
				      (and (file-readable-p currfile)
					   (find-file-noselect currfile))))
		    (if (null currbuf)
			(progn (set-buffer out-buf)
			       (insert "ERROR: (ebut:search): \"" currfile
				       "\" is not readable.\n\n"))
		      (set-buffer currbuf)
		      (unwind-protect
			  (save-excursion
			    (widen) (goto-char 1)
			    (let ((case-fold-search t)
				  (regexp
				   (ebut:match-regexp matches match-part)))
			      (setq firstmatch t)
			      (while (re-search-forward regexp nil t)
				(setq total (1+ total))
				(let* ((linenum (count-lines (point-min)
							     (point)))
				       (tag (format "\n%4d:" linenum))
				       lns start end)
				  (setq end (line-end-position)
					start (progn
						(goto-char (match-beginning 0))
						(line-beginning-position))
					lns (buffer-substring start end))
				  (goto-char end)
				  (with-current-buffer out-buf
				    (if firstmatch
					(progn
					  (insert hbut:source-prefix "\""
						  currfile "\"\n")
					  (setq firstmatch nil)))
				    (insert tag lns))))
			      (set-buffer out-buf)
			      (if (not firstmatch) (insert "\n\n"))))
			(if kill-buf (kill-buffer currbuf)))))))))))
    total))

(defun    ebut:to (lbl-key)
  "Find the nearest explicit button with LBL-KEY (a label or label key).
Search within the visible portion of the current buffer.  Leave
point inside the button label.  Return the symbol for the button,
else nil.

When LBL-KEY is nil, return the ebutton at point or nil if none."
  (if lbl-key
      (hbut:funcall (lambda (lbl-key _buffer _key-src)
		      ;; Handle a label given rather than a label key
		      (if (string-match-p "\\s-" lbl-key)
			  (setq lbl-key (ebut:label-to-key lbl-key)))
		      (let ((regexp (hbut:label-regexp lbl-key t))
			    pos
			    found)
			(save-excursion
			  ;; Since point might be in the middle of the matching button,
			  ;; move to the start of line to ensure don't miss it when
			  ;; searching forward.
			  (forward-line 0)
			  ;; re-search forward
			  (while (and (not found) (re-search-forward regexp nil t))
			    (setq pos (match-beginning 0)
				  found (equal (ebut:label-p nil nil nil nil t) lbl-key)))
			  ;; re-search backward
			  (while (and (not found) (re-search-backward regexp nil t))
			    (setq pos (match-beginning 0)
				  found (equal (ebut:label-p nil nil nil nil t) lbl-key))))
			(when found
			  (goto-char pos)
			  (ebut:at-p))))
		    lbl-key
		    (current-buffer))
    (ebut:at-p)))

;;; ------------------------------------------------------------------------
(defun    ebut:delimit (start end instance-flag)
  "Delimit explicit button label spanning region START to END in current buffer.
If button is already delimited or delimit fails, return nil, else t.
Insert INSTANCE-FLAG after END, before ending delimiter."
  (goto-char start)
  (when (looking-at (regexp-quote ebut:label-start))
    (forward-char (length ebut:label-start)))
  (unless (ebut:label-p)
    (setq start (move-marker (make-marker) start)
	  end (move-marker (make-marker) end))
    (set-marker-insertion-type end t)
    ;; instance-flag may be 't to indicate don't add an instance number
    (unless (stringp instance-flag)
      (setq instance-flag ""))
    (insert ebut:label-start)
    (goto-char end)
    (insert instance-flag ebut:label-end)
    ;; Insert any comment delimiter before the start marker.
    (set-marker-insertion-type start t)
    (hbut:comment start end)
    (when (fboundp 'hproperty:but-add)
      (hproperty:but-add start end hproperty:but-face))
    (goto-char end)
    (move-marker start nil)
    (move-marker end nil)
    t))

(defun    ebut:match-regexp (match-keys match-part)
  "Return regexp to match to all explicit button keys from MATCH-KEYS."
  (setq match-part (if match-part
		       (concat "[^" (substring ebut:label-end -1) "]*")
		     "[ \t\n\r]*"))
  (concat
   (regexp-quote ebut:label-start) match-part
   "\\(" (mapconcat (lambda (key) (ebut:label-regexp key 'no-delim))
		    match-keys "\\|")
   "\\)" match-part (regexp-quote ebut:label-end)))

;;; ========================================================================
;;; gbut class - Global Hyperbole buttons - activated by typing label name
;;; ========================================================================

(defun    gbut:act (label)
  "Activate Hyperbole global button with LABEL."
  (interactive (list (hargs:read-match "Activate global button labeled: "
				       (mapcar #'list (gbut:label-list))
				       nil t nil 'gbut)))
  (cond ((null label)
	 (error "(gbut:act): You have not created any global buttons"))
	((equal label "")
	 (error "(gbut:act): Please try again and type ? for a list of existing global button names"))
	(t (let* ((lbl-key (hbut:label-to-key label))
		  (but (gbut:get lbl-key)))
	     (if but
		 (progn
		   ;; Ensure gbut is activated with current-buffer as
		   ;; the context, not the gbut's source buffer.
		   (hattr:set but 'loc (current-buffer))
		   (hbut:act but))
	       (error "(gbut:act): No global button found for label: %s" label))))))

(defun    gbut:delete (&optional lbl-key)
  "Delete Hyperbole global button based on optional LBL-KEY or button at point.
Return entry deleted (a list of attribute values) or nil."
  (hbut:delete lbl-key nil (gbut:file)))

(defun    gbut:ebut-program (label actype &rest args)
  "Programmatically create a global explicit Hyperbole button at point.
Create button from LABEL, ACTYPE (action type), and optional actype ARGS.
Insert LABEL text at the end of the personal/global button file
surrounded by <( )> delimiters, adding any necessary instance
number of the button after the LABEL.  ACTYPE may be a Hyperbole
action type name (from defact) or an Emacs Lisp function,
followed by a list of arguments for the actype, aside from the
button LABEL which is automatically provided as the first
argument.

For interactive creation, use `hui:gbut-create' instead."
  (save-excursion
    (with-current-buffer (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user))
      (save-excursion
	(goto-char (point-max))
	(when (not (bolp))
	  (insert "\n"))
	(eval `(ebut:program ',label ',actype ,@args))))))

(defun    gbut:file ()
  "Return the absolute path for the global button (those accessed by name) file."
  (expand-file-name hbmap:filename hbmap:dir-user))

(defun    gbut:get (&optional lbl-key)
  "Return global button symbol given by optional LBL-KEY if found in (gbut:file).

Retrieve any button data, convert into a button object and return a symbol
which references the button.

All arguments are optional.  When none are given, return a symbol for
the button that point is within.

Return nil if no matching button is found."
  (hbut:get lbl-key nil (gbut:file)))

(defun    gbut:help (label)
  "Display help for Hyperbole global button with LABEL."
  (interactive (list (hargs:read-match "Report on global button labeled: "
				       (mapcar #'list (gbut:label-list))
				       nil t nil 'hbut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (hbut:get lbl-key nil (gbut:file))))
    (if but
	(hbut:report but)
      (error "(gbut:help): No global button labeled: %s" label))))

(defun    gbut:label-list ()
  "Return list of global button labels."
  (mapcar #'hbut:key-to-label (gbut:key-list)))

(defun    gbut:label-p (&optional as-label start-delim end-delim pos-flag two-lines-flag)
  "Return key for the Hyperbole global button label that point is within, else nil.
This is the normalized key form of the explicit button's label.

Assume point is within the first line of any button label.  All
following arguments are optional.  If AS-LABEL is non-nil, return
label rather than the key derived from the label.  START-DELIM
and END-DELIM are strings that override default button
delimiters.  With POS-FLAG non-nil, return the list of label-or-key,
but-start-position, but-end-position.  Positions include
delimiters.  With TWO-LINES-FLAG non-nil, constrain label search
to two lines."
  (when (equal (hypb:buffer-file-name) (gbut:file))
    (hbut:label-p as-label start-delim end-delim pos-flag two-lines-flag)))

(defun    gbut:save-buffer ()
  "Save global button file after an edit."
  (with-current-buffer (find-file-noselect (gbut:file))
    (save-buffer)))

(defun    gbut:to (lbl-key)
  "Find the global button with LBL-KEY (a label or label key).
Find it within the visible portion of the global button file.
Leave point inside the button label, if it has one.
Return the symbol for the button when found, else nil."
  (when (file-readable-p (gbut:file))
    (let ((obuf (current-buffer))
	  (opoint (point))
	  found)
      (set-buffer (find-file-noselect (gbut:file)))
      (setq found (hbut:to lbl-key))
      (if found
	  (hpath:display-buffer (current-buffer) 'this-window)
	(set-buffer obuf)
	(goto-char opoint))
      found)))

;;; ------------------------------------------------------------------------
(defun    gbut:key-list ()
  "Return list of global button label keys."
  (nconc (gbut:ebut-key-list) (gbut:ibut-key-list)))

(defun    gbut:ebut-key-list ()
  "Return a list of explicit button label keys from the global button file."
  (save-excursion
    (save-restriction
      (when (hbdata:to-entry-in-file (gbut:file))
	(let (gbuts)
	  (save-restriction
	    (narrow-to-region (point) (if (search-forward "\f" nil t)
					  (point) (point-max)))
	    (goto-char (point-min))
	    (ignore-errors
	      (while (setq gbuts (cons (car (read (current-buffer))) gbuts))))
	    gbuts))))))

(defun    gbut:ibut-key-list ()
  "Return a list of implicit button label keys from the global button file."
  (when (file-readable-p (gbut:file))
    (save-excursion
      (with-current-buffer (find-file-noselect (gbut:file))
	(save-restriction
	  (widen)
	  (ibut:label-map (lambda (label _start _end) (ibut:label-to-key label))))))))

;;; ========================================================================
;;; hattr class
;;; ========================================================================

(defun hattr:actype-is-p (actype-symbol &optional hbut-symbol)
  "Return t if ACTYPE-SYMBOL matches an hbut's \='actype attr value.
The hbut used defaults to \='hbut:current or the optional HBUT-SYMBOL."
  (hattr:is-p 'actype
	      (or (actype:def-symbol actype-symbol) actype-symbol)
	      hbut-symbol))

(defun    hattr:attributes (obj-symbol)
  "Return a list of OBJ-SYMBOL's attributes as symbols."
  (when (symbolp obj-symbol)
    (let* ((attr-val-list (symbol-plist obj-symbol))
	   (i -1))
      (delq nil (mapcar (lambda (elt)
			  (setq i (1+ i))
			  (and (zerop (% i 2)) elt))
			attr-val-list)))))

(defun    hattr:clear (hbut)
  "Remove all of HBUT's attributes except `variable-documentation'."
  (let (sublist)
    (or (symbolp hbut)
	(error "(hattr:clear): Argument not a Hyperbole button: %s" hbut))
    (if (setq sublist (memq 'variable-documentation (symbol-plist hbut)))
	(progn
	  (setcdr (cdr sublist) nil)
	  (setplist hbut sublist))
      (setplist hbut nil))))

(defun    hattr:copy (from-hbut to-hbut)
  "Copy attributes FROM-HBUT TO-HBUT, overwriting TO-HBUT attribute values.
Return TO-HBUT."
  (mapc (lambda (hbut)
	  (or (and hbut (symbolp hbut))
	      (error "(hattr:clear): Argument not a Hyperbole button: %s" hbut)))
	(list from-hbut to-hbut))
  (hattr:clear to-hbut)
  (setplist to-hbut (copy-sequence (symbol-plist from-hbut)))
  to-hbut)

(defun    hattr:emacs-button-attributes (button)
  "Return a property list of an Emacs BUTTON."
  (if (markerp button)
      ;; If on a text property button, button-at will
      ;; return a marker pointing to the button, not a
      ;; button with attributes.
      (with-current-buffer (marker-buffer button)
	(when (get-text-property button 'button)
	  (text-properties-at (point))))
    (let ((category (hattr:emacs-button-is-p button)))
      (when category
	(symbol-plist category)))))

(defun    hattr:emacs-button-is-p (button)
  "If BUTTON is a valid Emacs button, return its category, else return nil."
  (let* ((type (when (or (overlayp button) (markerp button))
		 (button-get button 'type)))
	 (category (when type (get type 'button-category-symbol))))
    category))

(defun    hattr:get (obj-symbol attr-symbol)
  "Return value of OBJ-SYMBOL's attribute ATTR-SYMBOL."
  (get obj-symbol attr-symbol))

(defun hattr:ibtype-is-p (ibtype-symbol &optional ibut-symbol)
  "Return t if IBTYPE-SYMBOL matches an ibut's \='categ attr value.
The ibut used defaults to \='hbut:current or the optional IBUT-SYMBOL."
  (hattr:is-p 'categ
	      (or (ibtype:elisp-symbol ibtype-symbol) ibtype-symbol)
	      ibut-symbol))

(defun hattr:is-p (attr value &optional hbut-symbol)
  "Return t if ATTR has VALUE for \='hbut:current or optional HBUT-SYMBOL."
  (and (symbolp attr) attr 
       (eq (hattr:get (or (and (symbolp hbut-symbol) hbut-symbol) 'hbut:current)
		      attr)
	   value)))

(defun    hattr:list (obj)
  "Return a property list of OBJ's attributes.
Each pair of elements is: <attrib-name> <attrib-value>."
  (cond ((hattr:emacs-button-attributes obj))
	((symbolp obj)
	 (symbol-plist obj))
	(t (error "(hattr:list): Argument not a symbol: %s" obj))))

(defun    hattr:memq (attr-symbol obj-symbol)
  "Return t if ATTR-SYMBOL is in OBJ-SYMBOL's attribute list, else nil."
  (and (symbolp obj-symbol) (symbolp attr-symbol)
       (let* ((attr-val-list (symbol-plist obj-symbol))
	      (attr-list (let ((i -1))
			   (delq nil (mapcar
				      (lambda (elt)
					(setq i (1+ i))
					(and (zerop (% i 2)) elt))
				      attr-val-list)))))
	 (when (memq attr-symbol attr-list) t))))

(defun    hattr:report (attrib-list)
  "Pretty print to `standard-output' attribute-value pairs from ATTRIB-LIST.
Ignore nil valued attributes.  Return t unless no attributes are printed."
  (let ((has-attr) attr val len)
    (unless (or (null attrib-list) (not (listp attrib-list))
		;; odd number of elements?
		(= (% (length attrib-list) 2) 1))
      (while (setq attr (car attrib-list))
	(setq val (car (cdr attrib-list))
	      attrib-list (cdr (cdr attrib-list)))
	(when val
	  (setq has-attr t
		attr (symbol-name attr)
		len (number-to-string (max (- 16 (length attr)) 1)))
	  (princ (format (concat "   %s:%" len "s%S\n") attr " "
			 (let (str)
			   (cond ((string-match "time" attr)
				  (htz:date-unix val (and (>= (aref val 0) ?0)
							  (<= (aref val 0) ?9)
							  "GMT") htz:local))
				 ((and (setq str (if (stringp val)
						     val
						   (prin1-to-string val)))
				       (string-match "\\`actypes::" str))
				  (intern (substring str (match-end 0))))
				 (t val)))))))
      has-attr)))

(defun    hattr:save ()
  "Save button attribute file for current directory, if modified.
Suitable for use as part of `write-file-functions'."
  (let* ((bd-file (expand-file-name hattr:filename default-directory))
	 (buf (and (stringp default-directory)
		   (get-file-buffer bd-file))))
    (if (and ebut:hattr-save buf (not (eq buf (current-buffer))))
	(let ((ebut:hattr-save)) ;; Prevents `write-file-functions' from looping.
	  (and (buffer-modified-p buf)
	       (with-current-buffer buf (save-buffer)
		 ;; Unlock button attribute file; kill buffer so user is
		 ;; never holding a buffer which is out of sync with file,
		 ;; due to some other user's edits.
		 ;; Maybe this should be user or site configurable.
		 (or (buffer-modified-p buf) (kill-buffer buf)))))))
  ;; Must return nil, so can be used as part of write-file-functions.
  nil)

(defun    hattr:set (obj-symbol attr-symbol attr-value)
  "Set OBJ-SYMBOL's attribute ATTR-SYMBOL to ATTR-VALUE and return ATR-VALUE."
  (put obj-symbol attr-symbol attr-value))

(defalias 'hattr:summarize #'hattr:report)

;;; ========================================================================
;;; hbut class - abstract
;;; ========================================================================

(defun    hbut:act (&optional hbut)
  "Perform action for optional explicit or implicit Hyperbole button symbol HBUT.
Default is the symbol hbut:current."
  (interactive (list (hbut:get (hargs:read-match "Activate labeled Hyperbole button: "
						 (nconc (ebut:alist) (ibut:alist))
						 nil t nil 'hbut))))
  (if (and (symbolp hbut)
	   (not (eq hbut 'hbut:current)))
      (hattr:copy hbut 'hbut:current)
    (setq hbut 'hbut:current))
  (cond ((hbut:is-p hbut)
	 (let ((orig-point (point-marker))
	       (action (hattr:get hbut 'action))
	       (loc (hattr:get hbut 'loc))
	       text-point)
	   (when loc
	     ;; Button's location may be different than the current
	     ;; buffer, so move point there if so.
	     (hbut:key-src-set-buffer loc))
	   (when (ibut:is-p hbut)
	     ;; Determine whether point is already within hbut; if
	     ;; not, it is moved there.
	     ;;
	     ;; The next line returns the key version of the optional
	     ;; name of the current button if and only if point is
	     ;; within the name; otherwise, including if point is on
	     ;; the text of the button, this returns nil.
	     (let* ((name-key-start-end (ibut:label-p nil nil nil t t))
		    (name-key (nth 0 name-key-start-end))
		    (delim-text-start (hattr:get hbut 'lbl-start))
		    (delim-text-end (hattr:get hbut 'lbl-end)))
	       (if (and name-key
			(or (equal loc (hypb:buffer-file-name))
			    (equal loc (current-buffer)))
			(equal name-key (ibut:label-to-key (hattr:get hbut 'name))))
		   (unless (and delim-text-start delim-text-end
				(< delim-text-start (point))
				(>= delim-text-end (point)))
		     (goto-char delim-text-start)
		     (skip-chars-forward "^-_a-zA-Z0-9"))
		 ;; Here handle when there is no name preceding the implicit button.
		 (unless (and (or (equal loc (hypb:buffer-file-name))
				  (equal loc (current-buffer)))
			      delim-text-start delim-text-end
			      (< delim-text-start (point))
			      (>= delim-text-end (point)))
		   (ibut:to-text (hattr:get hbut 'lbl-key))))))
	   (setq text-point (point-marker))
	   (prog1 (if action
		      (apply hrule:action action)
		    (apply hrule:action (hattr:get hbut 'actype) (hattr:get hbut 'args)))
	     ;; Restore point as it was prior to `ibut:to-text' call
	     ;; if the action switched buffers or did not move point
	     ;; within the current buffer.
	     (when (or (equal text-point (point-marker))
		       (not (eq (current-buffer) (marker-buffer orig-point))))
	       (with-current-buffer (marker-buffer orig-point)
		 (let ((owind (get-buffer-window nil t)))
		   (if owind
		       (set-window-point owind orig-point)
		     (goto-char orig-point)))))
	     (set-marker orig-point nil)
	     (set-marker text-point nil))))
	((and hbut (symbolp hbut))
	 (hypb:error "(hbut:act): Symbol, %s, has invalid Hyperbole button attributes:\n  %S" hbut (hattr:list hbut)))
	(t
	 (hypb:error "(hbut:act): Expected an ibut, instead given: `%s'" hbut))))

(defun    hbut:act-label (label)
  "Activate Hyperbole implicit button with <[LABEL]> from the current buffer."
  (interactive (list (hargs:read-match "Activate labeled Hyperbole button: "
				       (nconc (ebut:alist) (ibut:alist))
				       nil t nil 'hbut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (hbut:get lbl-key)))
    (if but
	(hbut:act but)
      (hypb:error "(hbut:act-label): No implicit button labeled: `%s'" label))))

(defun    hbut:action (hbut)
  "Return appropriate action name/function for Hyperbole button symbol HBUT."
  (let (atype
	action)
    (setq action (car (hattr:get hbut 'action))
	  atype  (hattr:get hbut 'actype))
    (if (and (symbolp atype)
	     (= (length (symbol-name atype)) 2))
	atype
      (or action (actype:action atype)))))

(defun    hbut:actype (hbut)
  "Return action type for Hyperbole button symbol HBUT."
  (when (hbut:is-p hbut)
    (hattr:get hbut 'actype)))

(defun    hbut:alist (&optional file)
  "Return alist of hbuts (ebuts and named ibuts) in FILE or the current buffer.
Each element is a list of just a button label.  For use as a completion table."
  (mapcar #'list (hbut:list file)))

(defun    hbut:at-p ()
  "Return symbol for explicit or implicit Hyperbole button at point or nil.
Then use (hbut:act) to activate the button.

When called interactively, display help for the button at point as well."
  (interactive)
  (if (called-interactively-p 'interactive)
      (let ((hbut (or (ebut:at-p) (ibut:at-p))))
	(when hbut
	  (if (fboundp #'hkey-help)
	      (hkey-help)
	    (message "%S" (symbol-plist hbut)))
	  hbut))
    (or (ebut:at-p) (ibut:at-p))))

(defun    hbut:comment (start end)
  "Comment button label spanning region START to END in current buffer.
Comment only when major mode is derived from `prog-mode' or `sgml-mode' and
`comment-start' is non-nil.  Ignore email-related buffers."
  (when (and comment-start (not (memq major-mode '(mail-mode message-mode)))
	     (derived-mode-p 'prog-mode 'sgml-mode) (not (hmail:mode-is-p)) )
    (save-excursion
      (if (or (equal comment-end "")
	      (null comment-end))
	  (progn
	    (beginning-of-line)
	    (unless (search-forward comment-start start t)
	      (goto-char start)
	      (insert comment-start)
	      (unless (eq (preceding-char) ?\ )
		(insert ?\ ))))
	;; Comments have both start and end delimiters
  	(unless (and (re-search-backward
		      (concat (regexp-quote comment-start) "\\|"
			      (regexp-quote comment-end))
		      nil t)
		     (looking-at (regexp-quote comment-start)))
	  (goto-char start)
	  (insert comment-start)
	  (unless (eq (preceding-char) ?\ )
	    (insert ?\ ))
	  (goto-char (+ (point) (- end start)))
	  (unless (eq (following-char) ?\ )
	    (insert ?\ ))
	  (insert comment-end))))))

;;; Regexps derived in part from "filladapt.el" by Kyle E. Jones under
;;; the GPL.
(defvar   hbut:fill-prefix-regexps
  '(
    ;; Included text in news or mail messages
    "^[ \t]*\\([:|<>]+ *\\)+.*\r\n?"
    ;; Included text generated by SUPERCITE.  We can't hope to match all
    ;; the possible variations.
    "^[ \t]*[^'`\"< \t]*> *.*\r\n?"
    ;; Lisp comments
    "^[ \t]*\\(;+[ \t]*\\)+.*\r?\n?"
    ;; UNIX shell comments
    "^[ \t]*\\(#+[ \t]*\\)+.*\r?\n?"
    ;; C++ comments
    "^[ \t]*//[/ \t]+.*\r?\n?"
    ;; C or Pascal comments, one open and close per line, so match close
    ;; then open; no nested comments
    "[/\(]\\*+[^*/]*\\*+[/\)][ \t\r\n]+"
    ;; SQL, Eiffel or Sather comments
    "^[ \t]*--[ \t]+.*\r?\n?"
    ;; Fortran comments
    "^[Cc][ \t]+.*\r?\n?"
    ;; Postscript comments
    "^[ \t]*\\(%+[ \t]*\\)+.*\r?\n?")
  "List of regexps of fill prefixes to remove from the middle of buttons.")

(defun    hbut:fill-prefix-remove (label)
  "Remove any recognized fill prefixes and comments from within LABEL.
`hbut:fill-prefix-regexps' is a list of fill prefixes to recognize."
  (when (string-match "\n" label)
    (mapc (lambda (prefix)
	    (when (string-match "\n" label)
	      (setq label (replace-regexp-in-string prefix " " label nil t))))
	  hbut:fill-prefix-regexps))
  label)

(defun    hbut:delete (&optional lbl-key buffer key-src)
  "Delete explicit or labeled implicit button symbol given by LBL-KEY and BUFFER.
KEY-SRC is given when retrieving global buttons and is the full source pathname.

Return a symbol which references the button or nil if not deleted.

All arguments are optional.  When none are given, operate on
the button or button label that point is within, if any.
BUFFER defaults to the current buffer."
  (let (but-sym)
    (if (setq but-sym (ebut:get lbl-key buffer key-src))
	(ebut:delete but-sym)
      (when (setq but-sym (ibut:get lbl-key buffer key-src))
	(ibut:delete lbl-key)))))

(defun    hbut:funcall (func &optional lbl-key buffer key-src)
  "Move to an implicit button and return the result of calling FUNC.
Call FUNC with optional argument values of LBL-KEY, BUFFER and
KEY-SRC.  The implicit button used is given by LBL-KEY (a label
or label key) within BUFFER or KEY-SRC (full path to global
button file) or within the current buffer if both are null.  Use
`save-excursion' around this call to prevent permanent movement
of point when desired.

Caller must have used (ibut:at-p) to create hbut:current prior to
calling this function.  When KEY-SRC is given, this set's
hbut:current's \\='loc attribute to KEY-SRC."
  (if buffer
      (if (bufferp buffer)
	  (set-buffer buffer)
	(error "(ibut:get): Invalid buffer argument: %s" buffer))
    (if key-src
	(hattr:set 'hbut:current 'loc key-src)
      (let ((loc (hattr:get 'hbut:current 'loc)))
	(when loc
	  (hbut:key-src-set-buffer loc)))
      (setq key-src (hbut:to-key-src 'full)
	    ;; `hbut:to-key-src' sets current buffer to key-src buffer.
	    buffer (or buffer (current-buffer))))
    (when (stringp lbl-key)
      (when key-src
	(set-buffer (if (bufferp key-src)
			key-src
		      (find-file-noselect key-src))))))
  (when (and (stringp lbl-key) (or buffer key-src))
      (funcall func lbl-key buffer key-src)))

(defun    hbut:get (&optional lbl-key buffer key-src)
  "Return explicit or labeled implicit button symbol given by LBL-KEY and BUFFER.
KEY-SRC is given when retrieving global buttons and is the full source pathname.

Return a symbol which references the button or nil if not found.

All arguments are optional.  When none are given, return a
symbol for the button or button label that point is within or
nil.  BUFFER defaults to the current buffer."
  (or (ebut:get lbl-key buffer key-src) (ibut:get lbl-key buffer key-src)))

(defun    hbut:get-key-src (&optional full-flag dir-flag)
  "Return key source (usually unqualified) for current Hyperbole button.
With optional FULL-FLAG when source is a pathname, return the full pathname.
With optional DIR-FLAG, return the default directory of the key source.

Return value may be a directory, filename or a buffer unless DIR-FLAG
is given."
  (save-excursion
    (let ((key-src (cond ((hmail:mode-is-p) (current-buffer))
			 ;; If buffer represents the output of a document
			 ;; formatter, e.g. an Info document produced from a
			 ;; Texinfo source, then return the Texinfo source
			 ;; file, for example.
			 ((hbut:key-src-fmt))
			 ;; Handle directory movement within `make' output.
			 ((save-excursion
			    (and (re-search-backward
				  "^[a-z]*make[^a-z]+\\(Entering\\|Leaving\\) directory `\\([^']+\\)'" nil t)
				 (string-equal "Entering" (match-string 1))))
			  (let ((limit (match-end 2))
				;; Latest working directory that `make' reported
				(wd (match-string 2))
				cd)
			    ;; But another cd or pushd command may have been issued.
			    ;; Return the closest directory from the make output.
			    (if (re-search-backward
				 "\\<\\(cd\\|pushd\\)\\s +[\"\']?\\([^;\"\'\n\r\^L\\]+\\)"
				 limit t)
				(progn (setq cd (match-string 2))
				       ;; Eliminate any trailing whitespace.
				       (setq cd (substring
						 cd 0 (string-match "\\s +\\'" cd)))
				       (expand-file-name cd wd))
			      wd)))
			 ((hypb:buffer-file-name)
			  (if full-flag
			      (hypb:buffer-file-name)
			    (file-name-nondirectory (hypb:buffer-file-name))))
			 ;; Handle any preceding @loc hyp-source implicit button location references.
			 ;; This is used in report buffers of explicit buttons, i.e. hui:hbut-report
			 ;; as well as the *HyRolo* display matches buffer.
			 ((save-excursion
			    (save-restriction
			      (widen)
			      (hyrolo-hdr-move-after-p)
			      (end-of-visible-line)
			      (when (and (search-backward hbut:source-prefix nil t)
					 (or (memq (preceding-char) '(?\n ?\r))
					     (= (point) (point-min))))
				(hbut:source full-flag)))))
			 (t (current-buffer)))))
      (if dir-flag
	  (if (stringp key-src)
	      (if (directory-name-p key-src)
		  key-src
		(file-name-directory key-src))
	    (buffer-local-value 'default-directory key-src))
	key-src))))

(defun    hbut:is-p (object)
  "Return non-nil if OBJECT is a symbol representing a Hyperbole button."
 (when (symbolp object)
   (hattr:get object 'categ)))

(defun    hbut:key (hbut)
  "Return the key for Hyperbole button symbol HBUT."
  (if (hbut:is-p hbut)
      (hattr:get hbut 'lbl-key)
    (error "(hbut:key): Argument is not a Hyperbole button symbol, `%s'"
	   hbut)))

(defun    hbut:to-key-src (&optional full-flag)
  "Return key source (usually unqualified) for current Hyperbole button.
Also set current buffer to key source.
With optional FULL-FLAG when source is a pathname, return the full pathname."
  (let ((src (hbut:get-key-src full-flag)))
    (hbut:key-src-set-buffer src)))

(defun    hbut:key-src-fmt ()
  "Return unformatted filename associated with formatted current buffer.
This is used to obtain the source of Hyperbole buttons for buffers that
represent the output of particular document formatters."
  (when (or (eq major-mode 'Info-mode)
	    (string-match "\\.info\\(-[0-9]+\\)?$" (buffer-name)))
    (let ((src (and (hypb:buffer-file-name)
		    (substring
		     (hypb:buffer-file-name)
		     0 (string-match "\\.[^.]+$" (hypb:buffer-file-name))))))
      (cond ((file-exists-p (concat src ".texi"))
	     (concat src ".texi"))
	    ((file-exists-p (concat src ".texinfo"))
	     (concat src ".texinfo"))
	    ((current-buffer))))))

(defun    hbut:key-src-set-buffer (src)
  "Temporarily set current buffer to SRC, a buffer, buffer name, or file.
If SRC is a directory, simply return it; otherwise, return set current
buffer to SRC and return it or return nil if SRC is invalid/unreadable."
  (cond ((null src) nil)
	((or (bufferp src) (get-buffer src))
	 (set-buffer src)
	 src)
	((file-directory-p src)
	 (file-name-as-directory src))
	((file-readable-p src)
	 (set-buffer (find-file-noselect src))
	 src)
	((file-readable-p (setq src (hpath:symlink-referent src)))
	 (set-buffer (find-file-noselect src))
	 src)
	;; Buffer may be newly created with an attached file that has
	;; not yet been saved, so the file does not exist and cannot
	;; be read.
	((get-file-buffer src)
	 (set-buffer (get-file-buffer src))
	 src)))

(defun    hbut:key-to-label (lbl-key)
  "Unnormalize LBL-KEY and return a label string for display.
If LBL-KEY is not a string or is just punctuation, return nil."
  (when (and (stringp lbl-key)
	     (or (/= (length lbl-key) 1)
		 ;; Can't be a single character of punctuation
		 (not (memq (char-syntax (aref lbl-key 0)) '(?. ?\" ?\( ?\))))))
    (let* ((pos 0) (len (length lbl-key)) (lbl "") c)
      (while (< pos len)
	(setq c (aref lbl-key pos)
	      lbl (concat lbl
			  (if (eq c ?_)
			      (if (or (= (1+ pos) len)
				      (not (eq (aref lbl-key (1+ pos)) ?_)))
				  " "
				(setq pos (1+ pos))
				"_")
			    (char-to-string c)))
	      pos (1+ pos)))
      lbl)))

(defun    hbut:label (hbut)
  "Return the label for Hyperbole button symbol HBUT."
  (if (hbut:is-p hbut)
      (hbut:key-to-label (hattr:get hbut 'lbl-key))
    (error "(hbut:label): Argument is not a Hyperbole button symbol, `%s'"
	   hbut)))

(defun    hbut:label-list ()
  "Return the list of Hyperbole button labels/names in the current buffer."
  (mapcar #'hbut:key-to-label (hbut:key-list)))

(defun    hbut:label-p (&optional as-label start-delim end-delim pos-flag two-lines-flag)
  "Return key for the Hyperbole button label that point is within, else nil.
Assume point is within the first line of any button label.  All
following arguments are optional.  If AS-LABEL is non-nil, return
the label rather than the key derived from the label.
START-DELIM and END-DELIM are strings that override default
button delimiters.  With POS-FLAG non-nil, return list of
label-or-key, but-start-position, but-end-position.  Positions
include delimiters.  With TWO-LINES-FLAG non-nil, constrain
label search to two lines."
  (or (ebut:label-p as-label start-delim end-delim pos-flag two-lines-flag)
      (ibut:label-p as-label start-delim end-delim pos-flag two-lines-flag)))

(defun    hbut:label-regexp (lbl-key &optional no-delim start-delim end-delim)
  "Unnormalize LBL-KEY.  Return regexp matching delimited button label.
Optional NO-DELIM leaves off delimiters and leading and trailing space.
Optional START-DELIM and END-DELIM are added around the returned
label; these default to `ebut:label-start' and `ebut:label-end'."
  (when lbl-key
   (let* ((pos 0)
	   (len (length lbl-key))
	   (c)
	   (sep0 "[ \t\n\r]*")
	   (sep "[ \t\n\r]+")
	   (regexp (if no-delim "" (concat (regexp-quote (or start-delim ebut:label-start)) sep0
					   "\\(")))
	   (case-fold-search))
      (while (< pos len)
	(setq c (aref lbl-key pos)
	      regexp (concat regexp
			     (if (eq c ?_)
				 (if (or (= (1+ pos) len)
					 (not (eq (aref lbl-key (1+ pos)) ?_)))
				     sep
				   (setq pos (1+ pos))
				   "_")
			       (regexp-quote (char-to-string c))))
	      pos (1+ pos)))
      (if no-delim
	  regexp
	(setq regexp (concat regexp
			     "\\)" sep0 (regexp-quote (or end-delim ebut:label-end))))))))

(defun    hbut:label-instances-regexp (lbl-key &optional no-delim start-delim end-delim)
  "Unnormalize LBL-KEY.
Return regexp matching all instances of delimited button label.
Optional NO-DELIM leaves off delimiters and leading and trailing space.
Optional START-DELIM and END-DELIM are added around the returned
label; these default to `ebut:label-start' and `ebut:label-end'."
  (when lbl-key
   (let* ((pos 0)
	   (len (length lbl-key))
	   (c)
	   (sep0 "[ \t\n\r]*")
	   (sep "[ \t\n\r]+")
	   (regexp (if no-delim "" (concat (regexp-quote (or start-delim ebut:label-start)) sep0
					   "\\(")))
	   (case-fold-search))
      (while (< pos len)
	(setq c (aref lbl-key pos)
	      regexp (concat regexp
			     (if (eq c ?_)
				 (if (or (= (1+ pos) len)
					 (not (eq (aref lbl-key (1+ pos)) ?_)))
				     sep
				   (setq pos (1+ pos))
				   "_")
			       (regexp-quote (char-to-string c))))
	      pos (1+ pos)))
      (if no-delim
	  regexp
	(setq regexp (concat regexp
			      (if (string-match (format "%s[0-9]+\\'" (regexp-quote hbut:instance-sep))
					       lbl-key)
				 ""
			       (concat "\\(" (regexp-quote hbut:instance-sep) "[0-9]+\\)?"))
			     "\\)" sep0 (regexp-quote (or end-delim ebut:label-end))))))))

(defun    hbut:label-to-key (label)
  "Normalize LABEL for use as a Hyperbole button key and return key.
Eliminate any fill prefix in the middle of the label, replace `_' with
`__', remove leading and trailing whitespace and replace all other
whitespace sequences with `_'."
  (when label
    (setq label (hbut:fill-prefix-remove label)
	  ;; Remove leading and trailing space.
	  label (replace-regexp-in-string "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'"
					   "" label nil t)
	  label (replace-regexp-in-string "_" "__" label nil t))
    (replace-regexp-in-string "[ \t\n\r]+" "_" label nil t)))

(defun    hbut:list (&optional file loc-p)
  "Return list of button labels from in FILE or the current buffer.
Remove duplicate labels if optional LOC-P is omitted.  With LOC-P, return
list of elements (label start end) where start and end are the buffer
positions at which the button delimiter begins and ends."
  (interactive)
  (setq file (if file
		 (when (file-exists-p file)
		   (find-file-noselect file))
	       (current-buffer)))
  (when file
    (set-buffer file)
    (let ((buts (hbut:map (if loc-p
			      (lambda (lbl start end)
				;; Normalize label spacing
				(list (hbut:key-to-label (hbut:label-to-key lbl))
				      start end))
			    (lambda (lbl _start _end)
			      ;; Normalize label spacing
			      (hbut:key-to-label (hbut:label-to-key lbl)))))))
      (if loc-p buts (when buts (apply #'set:create buts))))))

(defun    hbut:map (but-func &optional regexp-match include-delims)
  "Apply BUT-FUNC to a set of hbuttons in the visible part of the current buffer.
Finds both ebuts and named ibuts that match any optional REGEXP-MATCH
(may be a partial match but must include delimiters).

Any regexp given must have grouping 1 match the label.

BUT-FUNC must take precisely three arguments: the button label, the start
position of the delimited button label and its end position (positions
include delimiters when INCLUDE-DELIMS is non-nil)."
(sort
 (nconc
  (ebut:map but-func regexp-match include-delims)
  (ibut:map but-func regexp-match include-delims))
 #'string<))

(defun    hbut:map-type (but-func start-delim end-delim
			 &optional regexp-match include-delims)
  "Apply BUT-FUNC to a set of hbuttons in the visible part of the current buffer.
The set of buttons are those whose labels are delimited by START-DELIM
and END-DELIM and that match any optional REGEXP-MATCH (may be a partial
match but must include delimiters).

START-DELIM defaults to ebut:label-start; END-DELIM defaults to
ebut:label-end.  If END-DELIM is a symbol, e.g. t, then treat START-DELIM
as a regular expression which matches an entire button string including
instance numbers and delimiters (REGEXP-MATCH is ignored in such cases).

Any regexp given must have grouping 1 match the label.

BUT-FUNC must take precisely three arguments: the button label, the start
position of the delimited button label and its end position (positions
include delimiters when INCLUDE-DELIMS is non-nil)."
  (unless (stringp start-delim)
    (error "(hbut:map-type): `start-delim' must be a string, not '%s'"
	   start-delim))
  (unless end-delim
    (error "(hbut:map-type): `end-delim' must be non-nil"))

  (let* ((match-to-start-delim (when end-delim (symbolp end-delim)))
	 (end-char (unless match-to-start-delim
		     (substring end-delim -1)))
	 (result)
	 (ignore)
	 (regexp-to-match
	  (cond (match-to-start-delim
		 start-delim)
		((stringp regexp-match)
		 regexp-match)
		(t (concat (regexp-quote start-delim)
			   "\\([^" end-char "\"][^" end-char "]*\\)"
			   (regexp-quote end-delim)))))
	 start end delim-start lbl)
    (save-excursion
      (goto-char (point-min))
      (setq include-delims (if include-delims 0 1))
      (while (re-search-forward regexp-to-match nil t)
	(setq start (match-beginning include-delims)
	      end (match-end include-delims)
	      lbl (match-string-no-properties 1)
	      delim-start (match-beginning 0)
	      ;; If within a programming language buffer, ignore matches
	      ;; outside comments.
	      ignore (hbut:outside-comment-p))
	(save-excursion
	  (goto-char delim-start)
	  ;; Ignore matches with quoted delimiters.
	  (unless ignore
	    (setq ignore (memq (preceding-char) '(?\\ ?\{)))))
	(if ignore
	    (setq ignore nil)
	  (setq result (cons (funcall but-func lbl start end) result)))))
    (nreverse result)))

(defvar   hbut:syntax-table (copy-syntax-table emacs-lisp-mode-syntax-table)
  "Syntax table copied from Elisp for use with Action and Key Series buttons.
Make < > and { } into syntactically matching pairs after `hyperb:init'
calls `hbut:modify-syntax'.")

;;;###autoload
(defun    hbut:modify-syntax ()
  "Make <> and {} behave as syntactic character pairs in major syntax tables.
Modify `hbut:syntax-table' and `help-mode-syntax-table' to include <> and {}.
Modify `text-mode-syntax-table' and `fundamental-mode's syntax table
to include {} only.  For use with implicit button activations."
  ;; Treat angle brackets and braces as opening and closing delimiters
  ;; for ease  of matching.
  (mapc (lambda (syntax-table)
	  (modify-syntax-entry ?\< "(>" syntax-table)
	  (modify-syntax-entry ?\> ")<" syntax-table)
	  ;; Treat braces as opening and closing delimiters for ease of matching.
	  (modify-syntax-entry ?\{ "(}" syntax-table)
	  (modify-syntax-entry ?\} "){" syntax-table))
	(list hbut:syntax-table help-mode-syntax-table))
  (mapc (lambda (syntax-table)
	  ;; Treat braces as opening and closing delimiters for ease of matching.
	  (modify-syntax-entry ?\{ "(}" syntax-table)
	  (modify-syntax-entry ?\} "){" syntax-table))
	(list text-mode-syntax-table
	      ;; fundamental-mode syntax table
	      (standard-syntax-table)))
  nil)

(defun    hbut:outside-comment-p ()
  "True if in a programming mode and regexp match is outside a comment, else nil."
  (when (and (derived-mode-p 'prog-mode)
	     (not (derived-mode-p 'lisp-interaction-mode))
	     (not (memq major-mode hui-select-markup-modes)))
    ;; Match is outside of a programming language comment
    (not (nth 4 (syntax-ppss)))))

(defun    hbut:rename (but)
  "Interactively rename the Hyperbole button BUT from the current buffer."
  (cond ((ebut:is-p but)
         (ebut:to (ebut:key but))
         (call-interactively #'hui:ebut-rename))
        ((ibut:is-p but)
         (ibut:to (ibut:key but))
         (call-interactively #'hui:ibut-rename))
        (t
	 (hypb:error "(hbut:rename): Button is invalid; it has no attributes"))))

(defun    hbut:report (&optional arg)
  "Pretty print the attributes of a button or buttons.

Takes an optional ARG interpreted as follows:
  a button symbol - report on that button;
  nil             - report on button at point, if any;
  integer > 0     - report on all explicit buttons in buffer,
                    in lexicographical order;
  integer < 1     - report on all explicit buttons in buffer,
                    in occurrence order.

Return number of buttons reported on or nil if none."
  (setq arg (cond ((or (integerp arg) (symbolp arg)) arg)
		  ((listp arg)
		   (if (integerp (setq arg (car arg))) arg 1))
		  (t 1)))
  (let* ((but (if (and arg (symbolp arg)) arg (hbut:at-p)))
	 (curr-key (and but (hattr:get but 'lbl-key)))
	 (key-src (or (and but (hattr:get but 'loc)) (hbut:to-key-src)))
	 (lbl-lst (cond ((not arg)
			 (if curr-key (list (ebut:key-to-label curr-key))))
			((symbolp arg) (if curr-key
					   (list (hbut:key-to-label
						  (hattr:get arg 'lbl-key)))))
			((< arg 1) (ebut:list))
			(t (sort (ebut:list)
				 (lambda (s1 s2)
				   (string< (downcase s1) (downcase s2)))))))
	 (key-buf (current-buffer))
	 (buf-name (hypb:help-buf-name))
	 (attribs)
	 ;; Ensure these do not invoke with-output-to-temp-buffer a second time.
	 (temp-buffer-show-hook)
	 (temp-buffer-show-function))
    (when lbl-lst
      (with-help-window buf-name
	(princ hbut:source-prefix)
	(prin1 key-src)
	(terpri)
	(terpri)
	(mapcar
	 (lambda (lbl)
	   (when (setq but (cond ((or (null arg) (symbolp arg)) but)
				 (t (ebut:get (ebut:label-to-key lbl) key-buf)))
		       attribs (hattr:list but))
	     (princ (if (ibut:is-p but)
			lbl
		      (concat ebut:label-start lbl ebut:label-end)))
	     (terpri)
	     (let ((doc (actype:doc but (= 1 (length lbl-lst)))))
	       (when doc
		 (princ "  ")
		 (princ doc)
		 (terpri)))
	     (hattr:report attribs)
	     (terpri)))
	 lbl-lst))
      (length lbl-lst))))

(defun    hbut:source (&optional full-flag)
  "Return Hyperbole source buffer or file given at point.
If a file, always return a full path if optional FULL-FLAG is non-nil.
Caller must have successfully searched for `hbut:source-prefix' prior
to calling this."
  (save-excursion
    (goto-char (match-end 0))
    (cond ((looking-at "#<buffer \"?\\([^\n\"]+\\)\"?>")
	   (get-buffer (match-string 1)))
	  ((looking-at "\".+\"")
	   (let* ((file (buffer-substring-no-properties
			 (1+ (match-beginning 0))
			 (1- (match-end 0))))
		  (absolute (file-name-absolute-p file)))
	     (if (and full-flag (not absolute))
		 (expand-file-name file default-directory)
	       file))))))

(defalias 'hbut:summarize #'hbut:report)

(defun    hbut:to (lbl-key)
  "Find the nearest explicit button or labeled/named implicit button.
Button given by LBL-KEY (a label or label key) and within the
visible portion of the current buffer.  Leave point inside the
button label.  Return the symbol for the button, else nil."
  (or (ebut:to lbl-key) (ibut:to lbl-key)))

(defvar   hbut:current nil
  "The currently selected Hyperbole button.  Available to action routines.")

;;; ------------------------------------------------------------------------

(defun    hbut:key-list ()
  "Return list of explicit and named implicit button label keys in current buffer."
  (nconc (hbut:ebut-key-list) (hbut:ibut-key-list)))

(defun    hbut:ebut-key-list (&optional key-src)
  "Return a list of explicit button label keys.
Keys in optional KEY-SRC or the current buffer."
  (save-excursion
    (save-restriction
      (if (hbdata:to-entry-buf (or key-src (hypb:buffer-file-name)))
	  (let (hbuts)
	    (save-restriction
	      (narrow-to-region (point) (if (search-forward "\f" nil t)
					    (point) (point-max)))
	      (goto-char (point-min))
	      (ignore-errors
		(while (setq hbuts (cons (car (read (current-buffer))) hbuts))))
	      hbuts))))))

(defun    hbut:ibut-key-list (&optional key-src)
  "Return a list of implicit button label keys.
Keys in optional KEY-SRC or the current buffer."
  (save-excursion
    (when (hbut:key-src-set-buffer (or key-src (current-buffer)))
      (save-restriction
	(widen)
	(ibut:label-map (lambda (label _start _end) (ibut:label-to-key label)))))))

;;; ========================================================================
;;; ibut class - Implicit Hyperbole Buttons
;;; ========================================================================

(defun    ibut:act (&optional ibut)
  "Perform action for optional implicit Hyperbole button symbol IBUT.
Default is the symbol hbut:current."
  (interactive (list (hbut:get (hargs:read-match "Activate labeled Hyperbole button: "
						 (ibut:alist)
						 nil t nil 'ibut))))
  (unless ibut
    (setq ibut 'hbut:current))
  (if (ibut:is-p ibut)
      (hbut:act ibut)
    (hypb:error "(ibut:act): Expected an ibut, instead given: `%s'" ibut)))

(defun    ibut:act-label (label)
  "Activate Hyperbole implicit button with <[LABEL]> from the current buffer."
  (interactive (list (hargs:read-match "Activate implicit button labeled: "
				       (ibut:alist)
				       nil t nil 'ibut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (ibut:get lbl-key)))
    (if but
	(hbut:act but)
      (hypb:error "(ibut:act-label): No implicit button labeled: `%s'" label))))

(defun    ibut:alist (&optional file)
  "Return alist of labeled ibuts in FILE or the current buffer.
Each element is a list of just an implicit button label.  For use
as a completion table."
  (mapcar #'list (ibut:list file)))

(defun    ibut:at-p (&optional name-key-only)
  "Return symbol for implicit button at point, else nil.
Point may be on the implicit button text or its optional preceding
name.  With optional NAME-KEY-ONLY, return only the key format of the
<[name]> of the button, if any.

Any named implicit button must contain at least two characters,
excluding delimiters, not just one."
  ;; Since the Smart Keys handle end-of-line separately from whether
  ;; point is within an implicit button, always report not within one
  ;; when point is at the end of a line unless `flymake-mode' has added
  ;; an issue annotation there. -- RSW 02-16-2020, 12-31-2023
  (unless (smart-eolp)
    ;; Check for an implicit button at current point, record its
    ;; attributes in memory and return a button symbol for it.
    (when (ibut:create)
      (if name-key-only
	  (ibut:label-to-key (hattr:get 'hbut:current 'name))
	'hbut:current))))

(defun    ibut:at-type-p (ibut-type-symbol)
  "Return non-nil if point is on a button of type IBUT-TYPE-SYMBOL.
Point may be on the button text or its preceding name.

The return value is a list of the type's action type symbol and
associated arguments from the button."
  (and (setq ibut-type-symbol (ibtype:elisp-symbol ibut-type-symbol))
       (let ((ibut (ibut:at-p)))
	 (and ibut (eq (ibut:type ibut) ibut-type-symbol)))))

(defun    ibut:is-type-p (ibut ibut-type-symbol)
  "Return non-nil if IBUT is a button of type IBUT-TYPE-SYMBOL.
Use `ibut:at-type-p' to test the type of the implicit button at point."
  (when (setq ibut-type-symbol (ibtype:elisp-symbol ibut-type-symbol))
    (unless (ibut:is-p ibut)
      (setq ibut nil))
    (and ibut (eq (ibut:type ibut) ibut-type-symbol))))

(defun    ibut:set-name-and-label-key-p (&optional start-delim end-delim)
  "Set ibut name, lbl-key, lbl-start/end attributes in \\='hbut:current.
Point may be on the implicit button text or its optional preceding
name.  Return t if on a named or delimited text implicit button;
return nil otherwise.

Optional START-DELIM and END-DELIM may be given to find the
button text (not name); without these, try a series of matching
delimiters (double quotes, angle brackets, braces and square
brackets).

This will not set any button attributes aside from \\='lbl-start
unless valid button text is found and is delimited.  For example,
this will return nil on a non-delimited pathname implicit button.

Any implicit button name must contain at least two characters,
excluding delimiters, not just one."
  (let* ((opoint (point-marker))
	 ;; Next line finds the name only if point is on it, not on the
	 ;; text of the button.
	 (name-start-end (ibut:label-p t nil nil t t))
	 (name       (nth 0 name-start-end))
	 (name-start (nth 1 name-start-end))
	 (name-end   (nth 2 name-start-end))
	 lbl-start-end
	 lbl-start
	 lbl-end
	 lbl-key)
    (unwind-protect
	(progn
	  ;; Skip past any optional name and separators
	  (when name-end
	    (goto-char name-end)
	    (setq lbl-start (if (looking-at ibut:label-separator-regexp)
				(progn
				  ;; Move past up to 2 possible characters of ibut
				  ;; delimiters; this prevents recognizing named,
				  ;; delimited ibuts of a single character since
				  ;; no one should need that.
				  (goto-char (min (+ 2 (match-end 0)) (point-max)))
				  (match-end 0))
			      (prog1 (point)
				(goto-char opoint))))
	    (hattr:set 'hbut:current 'lbl-start lbl-start))

	  (setq lbl-start-end (if (and start-delim end-delim)
				  (ibut:label-p nil start-delim end-delim t t)
				(or (ibut:label-p nil "\"" "\"" t t)
				    ;; <action> buttons can be longer
				    ;; than two lines, so don't limit
				    ;; the length.
				    (ibut:label-p nil "<" ">" t)
				    (ibut:label-p nil "{" "}" t t)
				    (ibut:label-p nil "[" "]" t t))))
	  (when lbl-start-end
	    (setq lbl-key (nth 0 lbl-start-end)
		  lbl-start (nth 1 lbl-start-end)
		  lbl-end (nth 2 lbl-start-end))
	    (when (and (stringp lbl-key)
		       (string-prefix-p (concat hywiki-org-link-type ":") lbl-key t))
	      ;; Remove any HyWiki org-link-type prefix
	      (setq lbl-key (substring lbl-key 3)
		    lbl-start (+ lbl-start (length hywiki-org-link-type) 1))))
	  (when lbl-start
	    (hattr:set 'hbut:current   'loc (save-excursion
					      (hbut:to-key-src 'full)))
	    (hattr:set 'hbut:current 'categ 'implicit)
	    (hattr:set 'hbut:current 'lbl-key lbl-key)
	    (hattr:set 'hbut:current 'lbl-start lbl-start)
	    (hattr:set 'hbut:current 'lbl-end lbl-end))

	  (when (and lbl-start (not name-end))
	    ;; Point is within ibut text, not its name, so search
	    ;; backward for any name on the same line.
	    (goto-char lbl-start)
	    ;; Skip back past any likely opening delim preceding
	    ;; button text.
	    (skip-chars-backward "\"<{[| \t")
	    ;; Allow for looking back at any name separator space.
	    (skip-chars-forward " \t")
	    ;; Look back past any name separator.
	    (when (looking-back ibut:label-separator-regexp
				(line-beginning-position) t)
	      ;; Move to within delimiters of name
	      (goto-char (max (- (match-beginning 0) 3) (point-min)))
	      (setq name-start-end (ibut:label-p t nil nil t t)
		    name           (nth 0 name-start-end)
		    name-start     (nth 1 name-start-end)
		    name-end       (nth 2 name-start-end))))

	  (when (and lbl-start name)
	    (hattr:set 'hbut:current 'name name))
	  (when (and lbl-start name-start name-end)
	    (hattr:set 'hbut:current 'name-start name-start)
	    (hattr:set 'hbut:current 'name-end name-end))
	  (when lbl-start
	    t))
      (goto-char opoint)
      (set-marker opoint nil))))

(cl-defun ibut:create (&optional &key but-sym name lbl-key lbl-start lbl-end
				 loc dir categ actype args action)
  "Create an in-memory representation of an implicit button.

If successful, return button instance num string or t for first
instance; otherwise, return nil.  See `hbdata:ibut-instance' for
details.

If BUT-SYM is given, take button's arguments from its property
list.  Otherwise, button arguments can be given individually or
if CATEG and following arguments are not given, create the button
object from the implicit button at point, if any; in which case,
return nil if no implicit button is found at point.

If a new button is created, store its attributes in the symbol,
\\='hbut:current."
  (interactive)
  ;; :args is ignored unless :categ or :action is also given.

  ;; `lbl-key' attribute will be set from `but-sym' if any, the button
  ;; `name' if any; and, otherwise, from its text.

  ;; `lbl-start' and `lbl-end' will be set from `but-sym' if any; and,
  ;; otherwise, the start and end of the ibut text, excluding
  ;; delimiters, not of its name.

  (let* ((but-sym-flag (not (null but-sym)))
	 (types (htype:category 'ibtypes))
	 ;; Global var used in (hact) function, don't delete.
	 (hrule:action #'actype:identity)
	 (opoint (point-marker))
	 (itype)
	 (is-type categ)
	 (name-and-lbl-key-flag)
	 (text-start)
	 (text-end)
	 (ibtype-point))
    (unwind-protect
	(progn
	  (unless but-sym
	    ;; Set attributes of button at point, if any
	    (setq name-and-lbl-key-flag (ibut:set-name-and-label-key-p))

	    (when but-sym-flag
	      (setq name-and-lbl-key-flag nil))
	    ;; Since the Smart Keys handle end-of-line and end-of-buffer
	    ;; separately from whether point is within an implicit button,
	    ;; always report not within one when point is at the end of a line
	    ;; except when there is a `flymake-mode' issue annotation there.
	    ;; -- RSW  02-16-2020, 07-17-2022 and 12-31-2023
	    (unless (or is-type (smart-eolp) (eobp))
	      (unwind-protect
		  (progn (when (or but-sym-flag name-and-lbl-key-flag)
			   (setq text-start (or (hattr:get 'hbut:current 'lbl-start)
						(point))
				 text-end (hattr:get 'hbut:current 'lbl-end))
			   (unless (and text-start
					(<= text-start (point))
					text-end
					(>= text-end (point)))
			     ;; Move to text of ibut before trying to activate it
			     ;; (may be on name)
			     (goto-char (+ (or text-start (point)) 2))))
			 (setq ibtype-point (point-marker))
			 (while (and (not is-type) types)
			   (setq itype (car types))
			   ;; Any implicit button type check should leave point
			   ;; unchanged.  Trigger an error if not.
			   (unless (equal (point-marker) ibtype-point)
			     (hypb:error "(Hyperbole): ibtype %s improperly moved point from %s to %s"
					 itype opoint (point)))
			   (when (condition-case err
				     (and itype (setq args (funcall itype)))
				   (error (progn (message "%S: %S" itype err)
						 (switch-to-buffer "*Messages*")
						 ;; Show full stack trace
						 (debug))))
			     (setq is-type itype))
			   (setq types (cdr types))))
		(set-marker ibtype-point nil)
		(goto-char opoint)))
	    (set-marker opoint nil))

	  (when (or is-type but-sym)
	    (unless but-sym
	      (setq but-sym 'hbut:current))
	    (let ((current-categ      (hattr:get but-sym 'categ))
		  (current-name       (hattr:get but-sym 'name))
		  (current-name-start (hattr:get but-sym 'name-start))
		  (current-name-end   (hattr:get but-sym 'name-end))
		  (current-lbl-key    (hattr:get but-sym 'lbl-key))
		  (current-lbl-start  (hattr:get but-sym 'lbl-start))
		  (current-lbl-end    (hattr:get but-sym 'lbl-end))
		  (current-loc        (hattr:get but-sym 'loc))
		  (current-dir        (hattr:get but-sym 'dir))
		  (current-action     (hattr:get but-sym 'action))
		  (current-actype     (hattr:get but-sym 'actype))
		  (current-args       (hattr:get but-sym 'args))
		  name-start
		  name-end)

	      (when (and current-name (or but-sym-flag (null name)))
		(setq name current-name))
	      (when name
		(hattr:set 'hbut:current 'name name))

	      (when (and current-name-start (or but-sym-flag (null name-start)))
		(setq name-start current-name-start))
	      (when name-start
		(hattr:set 'hbut:current 'name-start name-start))

	      (when (and current-name-end (or but-sym-flag (null name-end)))
		(setq name-end current-name-end))
	      (when name-end
		(hattr:set 'hbut:current 'name-end name-end))

	      (when (and current-lbl-key (or but-sym-flag (null lbl-key)))
		(setq lbl-key current-lbl-key))
	      (when lbl-key
		(hattr:set 'hbut:current 'lbl-key lbl-key))

	      (when (and current-lbl-start (or but-sym-flag (null lbl-start)))
		(setq lbl-start current-lbl-start))
	      (when lbl-start
		(hattr:set 'hbut:current 'lbl-start lbl-start))

	      (when (and current-lbl-end (or but-sym-flag (null lbl-end)))
		(setq lbl-end current-lbl-end))
	      (when lbl-end
		(hattr:set 'hbut:current 'lbl-end lbl-end))

	      (when (and current-loc (or but-sym-flag (null loc)))
		(setq loc (or (save-excursion
				(hbut:to-key-src 'full))
			      current-loc)))
	      (when loc
		(hattr:set 'hbut:current 'loc loc))

	      (when (and current-dir (or but-sym-flag (null dir)))
		(setq dir (or (hui:key-dir (current-buffer))
			      current-dir)))
	      (when dir
		(hattr:set 'hbut:current 'dir dir))

	      (when (and current-action (or but-sym-flag (null action)))
		(setq action current-action))
	      (when action
		(hattr:set 'hbut:current 'action action))

	      (cond ((and current-categ but-sym-flag)
		     (setq categ current-categ))
		    ((null categ)
		     (setq categ (or is-type current-categ 'implicit))))
	      (when categ
		(hattr:set 'hbut:current 'categ categ))

	      (if (not categ)
		  (setq args nil)
		(unless action
		  (cond ((and but-sym-flag current-args)
			 (setq args current-args))
			(args)
			(current-args
			 (setq args current-args))))
		(setq args (copy-sequence args))
		(when (eq (car args) #'hact)
		  (setq args (cdr args))))

	      (when (and current-actype (or but-sym-flag (null actype)))
		(setq actype current-actype))
	      (unless actype
		(setq actype (or
			      ;; Hyperbole action type
			      (symtable:actype-p (car args))
			      ;; Regular Emacs Lisp function symbol
			      (car args))))
	      (hattr:set 'hbut:current 'actype actype)

	      (when args
		(hattr:set 'hbut:current 'args (if actype (cdr args) args)))

	      (when (and lbl-key (eq actype #'hywiki-find-referent))
		;; If a HyWikiWord ibut, save its referent as an attribute
		(let ((referent (hywiki-get-referent lbl-key)))
		  (hattr:set 'hbut:current 'referent-type (car referent))
		  (hattr:set 'hbut:current 'referent-value (cdr referent))))
	      (when lbl-key
		(when (called-interactively-p 'any)
		  (let (help-window-select)
		    (hbut:report)))))

	    (hbdata:ibut-instance-next (ibut:label-to-key name))))
      (set-marker opoint nil))))

(def-edebug-spec cl-defun
 (&define name lambda-key-list
          [&optional stringp]   ; Match the doc string, if present.
          def-body))

(def-edebug-spec lambda-key-list
 (([&rest arg]
   [&optional ["&optional" "&key" arg &rest arg]]
   [&optional ["&optional" arg &rest arg]]
   &optional ["&rest" arg])))

(defvar ibut:label-start)
(defvar ibut:label-end)

(defun    ibut:delete (&optional but-sym)
  "Delete Hyperbole implicit button based on optional BUT-SYM.
If it is a named button, delete all occurrences in the buffer; otherwise, delete
only if there is an occurrence at point.

Default is the symbol \\='hbut:current.  Return symbol for button deleted or nil."
  (unless but-sym
    (setq but-sym 'hbut:current))
  (when (ibut:is-p but-sym)
    (let ((name       (hattr:get but-sym 'name))
	  (loc        (hattr:get but-sym 'loc))
	  (lbl-start  (hattr:get but-sym 'lbl-start))
	  (lbl-end    (hattr:get but-sym 'lbl-end)))
      (when (and lbl-start lbl-end)
	(with-current-buffer (if (bufferp loc) loc (find-file-noselect loc))
	  (barf-if-buffer-read-only)
	  (save-excursion
	    (if name
		(ibut:map
		 (lambda (_name start _end)
		   (goto-char (+ start 2))
		   (when (ibut:set-name-and-label-key-p)
		     (ibut:delete-occurrence
		      (hattr:get but-sym 'name-start)
		      (hattr:get but-sym 'lbl-end))))
		 (ibut:name-instances-regexp (ibut:label-to-key name))
		 t)
	      (ibut:delete-occurrence lbl-start lbl-end)))
	  ;; Also delete in-memory version of the ibut
	  (hattr:clear but-sym))
	but-sym))))

(defun    ibut:delete-occurrence (start end)
  "Delete Hyperbole implicit button between START and END.
Assume caller has checked that there is an implicit button at point and
has saved any original point to which to return."
  (goto-char start)
  (delete-region start end)
  (just-one-space)
  (run-hooks 'ibut-delete-hook))

(defun    ibut:delimit (start end instance-flag)
  "Delimit implicit button name spanning region START to END in current buffer.
If button is already delimited or delimit fails, return nil, else t.
Insert INSTANCE-FLAG after END, before ending delimiter."
  (goto-char start)
  (when (looking-at (regexp-quote ibut:label-start))
    (forward-char (length ibut:label-start)))
  (unless (ibut:label-p)
    (setq start (move-marker (make-marker) start)
	  end (move-marker (make-marker) end))
    (set-marker-insertion-type end t)
    ;; instance-flag may be 't to indicate don't add an instance number
    (unless (stringp instance-flag)
      (setq instance-flag ""))
    (insert ibut:label-start)
    (goto-char end)
    (insert instance-flag ibut:label-end)
    ;; Insert any comment delimiter before the start marker.
    (set-marker-insertion-type start t)
    (hbut:comment start end)
    (let ((delim-end (point)))
      (unless (looking-at ibut:label-separator-regexp)
	(insert ibut:label-separator))
      (when (fboundp 'hproperty:but-add)
	(hproperty:but-add start end hproperty:ibut-face))
      (goto-char delim-end))
    (move-marker start nil)
    (move-marker end nil)
    t))

(defun    ibut:get (&optional lbl-key buffer key-src)
  "Return implicit Hyperbole button symbol given by LBL-KEY and BUFFER.
KEY-SRC is given when retrieving global buttons and is the full source pathname.

Return a symbol which references the button.

All arguments are optional.  When none are given, return a
symbol for the button or button label that point is within or
nil.  BUFFER defaults to the current buffer.

Return nil if no matching button is found."
  (hattr:clear 'hbut:current)
  ;; Build and return button symbol with button properties
  (save-excursion
    (unless lbl-key
      (setq lbl-key (ibut:label-p nil nil nil nil t)))
    (hbut:funcall (lambda (lbl-key _buffer _key-src)
		    (goto-char (point-min))
		    (ibut:next-occurrence lbl-key)
		    (ibut:at-p))
		  lbl-key buffer key-src)))

(defun    ibut:is-p (object)
  "Return non-nil if OBJECT is a symbol representing an implicit Hyperbole button."
  (when (symbolp object)
    (let ((categ (hattr:get object 'categ)))
      (and categ
	   (or (eq categ 'implicit)
	       (string-match "\\`ibtypes::" (symbol-name categ)))))))

(defun    ibut:label-map (but-func &optional regexp-match include-delims)
  "Apply BUT-FUNC to buttons delimited by `ibut:label-start' and `ibut:label-end'.
If REGEXP-MATCH is non-nil, only buttons which match this argument are
considered.

Map over portion of buffer visible under any current restriction.
BUT-FUNC must take precisely three arguments: the button label,
the start position of the delimited button label and its end
position (positions include delimiters when INCLUDE-DELIMS is
non-nil)."
  (hbut:map-type but-func ibut:label-start ibut:label-end regexp-match include-delims))

(defun    ibut:label-key-match (name-key)
  "Return a list of implicit button label keys fully matching NAME-KEY.
There may be multiple results if there are numbered instances
with the same label.  Names are returned in the order they are
first encountered."
  (when (stringp name-key)
    (apply #'set:create
	   (ibut:map
	    (lambda (lbl _start _end) (ibut:label-to-key lbl))
	    (ibut:name-instances-regexp name-key)))))

(defun    ibut:label-p (&optional as-label start-delim end-delim pos-flag two-lines-flag)
  "Return key for the implicit button name that point is within, else nil.

Without the start and end delimiter arguments, this is the normalized
key form of the optional name that may precede an implicit button.
If the delimiter arguments are given, return the key form of the
implicit button text at point between those delimiters.  Point must be
within the first line and after the opening delimiter of any button to
get the key.

Alternatively, use `ibut:at-p' to test if point is on either the
implicit button text itself or the name.

All following arguments are optional.  If AS-LABEL is non-nil, label is
returned rather than the key derived from the label.  START-DELIM and
END-DELIM are strings that override default button label delimiters.
With POS-FLAG non-nil, return list of label-or-key,
but-label-start-position, but-label-end-position.  Positions include
delimiters.  With TWO-LINES-FLAG non-nil, constrain label search to two
lines."
  (unless start-delim
    (setq start-delim ibut:label-start))
  (unless end-delim
    (setq end-delim ibut:label-end))

  (let ((lbl)
	(result (with-syntax-table hbut:syntax-table
		  (if (or (string-prefix-p "<" start-delim)
			  (string-suffix-p ">" end-delim))
		      (ebut:label-p as-label start-delim end-delim
				    pos-flag two-lines-flag)
		    ;; When delims do not end with <>, then filter out matches
		    ;; that are surrounded by angle brackets, e.g. [str] should
		    ;; not match to occurrences of <[str]>.
		    (hargs:delimited (concat "<?" (regexp-quote start-delim))
				     (concat (regexp-quote end-delim) ">?")
				     t t pos-flag "\\`<.*>\\'" (not as-label))))))
    (when result
      (setq lbl (if (listp result) (car result) result))
      ;; Ensure match does not contain delimiters, as it may have run
      ;; past the beginning of another button.
      (when lbl
	(unless (string-match (concat (regexp-quote start-delim) "\\|"
				      (regexp-quote end-delim))
			      lbl)
	  result)))))

(defun    ibut:label-set (label &optional start end)
  "Set current implicit button label attributes.
Provide arguments LABEL and optional START, END positions.
Return label.  When START and END are given, they specify the
region in the buffer to flash when this implicit button is
activated or queried for its attributes; this typically should
be the text of the button without any delimiters.

If LABEL is a list, it is assumed to contain all arguments.

For legacy reasons, the label here is actually the text of the
implicit button matched contextually and never the optional <[name]>
preceding the text."
  (save-match-data
    (cond ((stringp label)
	   (hattr:set 'hbut:current 'loc (save-excursion
					   (hbut:to-key-src 'full)))
	   (hattr:set 'hbut:current 'lbl-key (hbut:label-to-key label))
	   (when start (hattr:set    'hbut:current 'lbl-start start))
	   (when end   (hattr:set    'hbut:current 'lbl-end   end)))
	  ((and label (listp label))
	   (hattr:set 'hbut:current 'loc (save-excursion
					   (hbut:to-key-src 'full)))
	   (hattr:set 'hbut:current 'lbl-key (hbut:label-to-key (car label)))
	   (hattr:set 'hbut:current 'lbl-start (nth 1 label))
	   (hattr:set 'hbut:current 'lbl-end (nth 2 label)))
	  (t (error "(ibut:label-set): Invalid label arg: `%s'" label)))
    label))

(defun    ibut:label-sort-keys (lbl-keys)
  "Return a sorted list of ibutton LBL-KEYS with highest instance number first."
  (sort (delq nil lbl-keys) (lambda (key1 key2)
			      (setq key1
				    (cond ((null key1) 0)
					  ((string-match (concat (regexp-quote hbut:instance-sep)
								 "\\([0-9]+\\)\\'")
							 key1)
					   (string-to-number (match-string 1 key1)))
					  (t 1))
				    key2
				    (cond ((null key2) 0)
					  ((string-match (concat (regexp-quote hbut:instance-sep)
								 "\\([0-9]+\\)\\'")
							 key2)
					   (string-to-number (match-string 1 key2)))
					  (t 1)))
			      (> key1 key2))))

(defun    ibut:list (&optional file loc-p)
  "Return list of labels of named ibuts in FILE or the current buffer.
Remove duplicate labels if optional LOC-P is omitted.  With LOC-P, return
list of elements (label start end) where start and end are the buffer
positions at which the button label delimiter begins and ends."
  (interactive)
  (setq file (if file
		 (when (file-exists-p file)
		   (find-file-noselect file))
	       (current-buffer)))
  (when file
    (set-buffer file)
    (let ((buts (ibut:map (if loc-p
			      (lambda (lbl start end)
				;; Normalize label spacing
				(list (ibut:key-to-label (ibut:label-to-key lbl))
				      start end))
			    (lambda (lbl _start _end)
			      ;; Normalize label spacing
			      (ibut:key-to-label (ibut:label-to-key lbl)))))))
      (if loc-p buts (when buts (apply #'set:create buts))))))

(defun    ibut:key (ibut)
  "Return the text key for Hyperbole implicit button symbol IBUT."
  (if (ibut:is-p ibut)
      (hattr:get ibut 'lbl-key)
    (error "(ibut:key): Argument is not a Hyperbole implicit button symbol, `%s'"
	   ibut)))

(defalias 'ibut:to-key-src   #'hbut:to-key-src)
(defalias 'ibut:key-to-label #'hbut:key-to-label)
(defalias 'ibut:label-to-key #'hbut:label-to-key)
(defalias 'map-ibut          #'ibut:map)

(defun    ibut:map (but-func &optional regexp-match include-delims)
  "Apply BUT-FUNC to the visible, named implicit buttons.

If REGEXP-MATCH is non-nil, only buttons which match this argument
are considered.

BUT-FUNC must take precisely three arguments: the button name,
the start position of the delimited button name and its end
position (positions include delimiters when INCLUDE-DELIMS is
non-nil)."
  (hbut:map-type but-func ibut:label-start ibut:label-end regexp-match include-delims))

(defun    ibut:name-regexp (name-key &optional no-delim)
  "Unnormalize ibutton NAME-KEY.
Return regular expression matching delimited button name.
Optional NO-DELIM leaves off delimiters, leading and trailing space."
  (hbut:label-regexp name-key no-delim ibut:label-start ibut:label-end))

(defun    ibut:name-instances-regexp (name-key &optional no-delim)
  "Unnormalize ibutton NAME-KEY.
Return regular expression matching all instances of delimited button name.
Optional NO-DELIM leaves off delimiters, leading and trailing space."
  (hbut:label-instances-regexp name-key no-delim ibut:label-start ibut:label-end))

(defun    ibut:next-occurrence (name-key &optional buffer)
  "Move point to next occurrence of an implicit button with NAME-KEY.
Optional BUFFER defaults to current buffer.  It may be a buffer name.
Return non-nil iff occurrence is found.

Remember to use (goto-char (point-min)) before calling this in order to
move to the first occurrence of the button."
  (when buffer
    (if (not (or (bufferp buffer) (and (stringp buffer) (get-buffer buffer))))
	(error "(ibut:next-occurrence): Invalid buffer arg: %s" buffer)
      (switch-to-buffer buffer)))
  (when (or (re-search-forward (ibut:name-regexp name-key) nil t)
	    (re-search-forward (ibut:name-regexp name-key t) nil t))
    (goto-char (+ (match-beginning 0) (length ibut:label-start)))))

(defun    ibut:operate (&optional new-name edit-flag)
  "Insert/modify an ibutton based on `hbut:current' in current buffer.

This is for internal Hyperbole use only.  Use `ibut:program' and
`ibut:create' (an alias of `defib'), if programming applications
with Hyperbole.

IMPORTANT: Caller must either call `hbut:at-p' or manually set
the attributes of `hbut:current' prior to invoking this function,
i.e. there must be an ibutton stored in memory in `hbut:current
prior to invocation.  If point is on an existing Hyperbole
button, `edit-flag' must be set to t; otherwise, this may create
a new ibutton inserted within the prior one, making the prior one
unusable.

Optional non-nil NEW-NAME is the new name to give the button.  With
optional EDIT-FLAG non-nil, modify an existing in-buffer ibutton
rather than creating a new one.

If NEW-NAME is nil, use the active region text as the button
name, if any; if no such region, then create/modify an unnamed
implicit button.

Return instance string appended to name to form a per-buffer
unique name; nil if name is already unique or no name.  Signal an
error when no such button is found in the current buffer.

Summary of operations based on inputs (name arg from \\='hbut:current attrs):
|----+------+----------+--------+------+-----------------------------------------------|
|  # | name | new-name | region | edit | operation                                     |
|----+------+----------+--------+------+-----------------------------------------------|
|  1 | nil  | nil      | nil    | nil  | create: unnamed ibut from hbut:current attrs  |
|  2 | nil  | new-name | nil    | nil  | ERROR: edit-flag must be t to set new-name    |
|  3 | name | nil      | nil    | nil  | create: ibut with name                        |
|  4 | name | new-name | nil    | nil  | ERROR: create can't have name and new-name    |
|  5 | name | new-name | region | nil  | ERROR: create can't have name and new-name    |
|  6 | name | nil      | region | nil  | create: ibut with name (ignore region)        |
|  7 | nil  | nil      | region | nil  | create: region named ibut                     |
|  8 | nil  | new-name | region | nil  | ERROR: edit-flag must be t to set new-name    |
|----+------+----------+--------+------+-----------------------------------------------|
|  9 | nil  | nil      | nil    | t    | mod: remove any name from ibut                |
| 10 | nil  | new-name | nil    | t    | mod: add new-name as ibut's name attribute    |
| 11 | name | nil      | nil    | t    | mod: name of ibut from hbut:current attrs     |
| 12 | name | new-name | nil    | t    | mod: rename ibut with name to new-name        |
| 13 | name | new-name | region | t    | ERROR: Can't use region to mod existing ibut  |
| 14 | name | nil      | region | t    | ERROR: Can't use region to mod existing ibut  |
| 15 | nil  | nil      | region | t    | ERROR: Can't use region to mod existing ibut  |
| 16 | nil  | new-name | region | t    | ERROR: Can't use region to mod existing ibut  |
|----+------+----------+--------+------+-----------------------------------------------|"
  (barf-if-buffer-read-only)
  (let* ((name (hattr:get 'hbut:current 'name))
	 (name-regexp (ibut:name-regexp (ibut:label-to-key name)))
	 (region-flag (hmouse-use-region-p))
	 (instance-flag))
    (when (and new-name (or (not (stringp new-name)) (string-empty-p new-name)))
      (hypb:error "(ibut:operate): 'new-name' value must be a non-empty string, not: '%s'"
		  new-name))
    (when (and name new-name (not edit-flag))
      (hypb:error "(ibut:operate): 'edit-flag' must be t to rename a button (hbut:current name and new-name both given)"))
    (when (and new-name (not edit-flag))
      (hypb:error "(ibut:operate): 'edit-flag' must be t to rename a button"))
    (when (and region-flag edit-flag)
      (hypb:error "(ibut:operate): 'edit-flag' must be nil when region is highlighted to use region as new button name"))

    ;; Error when on a read-only part of a buffer's text
    (when (plist-member (text-properties-at (point)) 'read-only)
      (hypb:error "(ibut:operate): Point must not be on a read-only Org element"))
    ;; Error when on an explicit button
    (when (eq (hattr:get 'hbut:current 'categ) 'explicit)
      (hypb:error "(ibut:operate): Point must not be on an explicit button: %s"
		  (ibut:label-to-key (hattr:get 'hbut:current 'lbl-key))))
    ;; Error when on an Emacs push-button
    (when (plist-member (text-properties-at (point)) 'button)
      (hypb:error "(ibut:operate): Point must not be on an Emacs push-button: %s"
		  (button-label (button-at (point)))))
    ;; Error when in read-only contexts of an Org file
    (when (hsys-org-at-read-only-p)
      (hypb:error "(ibut:operate): Point must not be in a read-only Org context"))

    (unless new-name
      (setq new-name name
	    name nil))
    (when (stringp new-name)
      (hattr:set 'hbut:current 'name new-name))
    (save-excursion
      (if (progn
	    (setq instance-flag (hbdata:ibut-instance-last (ibut:label-to-key
							    (if edit-flag new-name name))))
	    (when (null instance-flag)
	      (setq instance-flag t))
	    instance-flag)
	  (when (hmail:editor-p)
	    (hmail:msg-narrow))
	(hypb:error "(ibut:operate): Failed to %s button %s%s%s in buffer %s"
		    (if edit-flag "modify" "create")
		    ibut:label-start name ibut:label-end
		    (buffer-name))))
    (let (start end)
      (cond (edit-flag
	     (cond (name
		    ;; Rename all occurrences of button - those with same name
		    (let* ((but-key-and-pos (ibut:label-p nil nil nil 'pos))
			   (at-but (equal (car but-key-and-pos)
					  (ibut:label-to-key new-name))))
		      (when at-but
			(ibut:delimit (nth 1 but-key-and-pos)
				      (nth 2 but-key-and-pos)
				      instance-flag))
		      (cond ((ibut:map
			      (lambda (_lbl start end)
				(delete-region start end)
				(when new-name
				  (ibut:delimit
				   (point)
				   (progn (insert new-name) (point))
				   instance-flag)))
			      name-regexp 'include-delims))
			    (at-but))))
		   (new-name
		    ;; Add new-name to nameless button at point
		    (goto-char (or (hattr:get 'hbut:current 'lbl-start) (point)))
		    ;; Skip back past any likely opening delim preceding button text.
		    (skip-chars-backward "\"<{[|")
		    (ibut:delimit (point)
				  (progn (insert new-name) (point))
				  instance-flag))
		   (t ;; Remove any existing name at point
		    (when (hattr:get 'hbut:current 'name-start)
		      (save-excursion
			(delete-region (goto-char (hattr:get 'hbut:current 'name-start))
				       (hattr:get 'hbut:current 'name-end))
			(when (looking-at ibut:label-separator-regexp)
			  (delete-region (match-beginning 0) (match-end 0))))
		      ;; Skip past any likely opening delim preceding button text.
		      (skip-chars-forward "\"<{[|")
		      (setq start (point))))))
	    (t
	     ;; Above flag is 't when we are creating the first instance
	     ;; of the button name
	     ;;
	     ;; Add a new implicit button in the buffer, recording its
	     ;; start and end positions; new-name is always nil here
	     (cond ((not (or name new-name region-flag))
		    ;; No name to insert, just insert ibutton text below
		    )
		   ((and region-flag
			 ;; ignore region when name or new-name are set
			 (not (or name new-name))
			 ;; new-name is always nil here
			 ;; Ignore action-key-depress-prev-point
			 (setq start (region-beginning)
			       end (region-end)
			       name (buffer-substring-no-properties start end)))
		    nil)
		   ((progn (when start (goto-char start))
			   (or (when name (looking-at (regexp-quote name)))
			       (when new-name (looking-at (regexp-quote new-name)))))
		    (setq start (point)
			  end (match-end 0)))
		   (name
		    (setq start (point))
		    (insert name)
		    (setq end (point)))
		   (new-name
		    (setq start (point))
		    (insert new-name)
		    (setq end (point)))
		   (t (hypb:error
		       "(ibut:operate): Operation failed.  Check button attribute permissions: %s"
		       hattr:filename)))))

      (unless edit-flag
	(when (and start end)
	  (ibut:delimit start end instance-flag))
	(when (hattr:get 'hbut:current 'actype)
	  (ibut:insert-text 'hbut:current)))

      (goto-char (or start (max (- (point) 2) (point-min))))
      (when start
	;; Skip past any inserted comment char
	(skip-syntax-forward "-<")
	;; Skip past any name or label opening delim chars
	(skip-chars-forward "\"<{[| \t\n\r")))

    ;; Set all in-memory hbut attributes for any button at point
    (ibut:at-p)

    (let ((lbl-key (hattr:get 'hbut:current 'lbl-key)))
      (unless (and (stringp lbl-key) (not (string-empty-p lbl-key)))
	(hypb:error "(ibut:operate): hbut:current lbl-key must be non-nil")))

    (run-hooks (if edit-flag 'ibut-edit-hook 'ibut-create-hook))

    ;; Position point
    (let ((new-key (ibut:label-to-key new-name)))
      (cond ((and new-key (equal (ibut:label-p) new-key))
	     ;; In case right before the start of the desired
	     ;; button's delimiters.
	     (goto-char (min (+ (point) 2) (point-max)))
	     (when (search-backward ibut:label-start nil t)
	       (goto-char (match-end 0))))
	    (new-key
	     (let ((regexp (ibut:name-regexp new-key)))
	       (or (re-search-forward  regexp nil t)
		   (re-search-backward regexp nil t)))
	     (goto-char (+ (match-beginning 0) (length ibut:label-start))))))

    ;; instance-flag might be 't which we don't want to return.
    (when (stringp instance-flag) instance-flag)))

(defun    ibut:insert-text (ibut)
  "Space, delimit and insert the text part of IBUT."
  (when (hattr:get ibut 'name)
    (cond ((looking-at ibut:label-separator-regexp)
	   (goto-char (match-end 0)))
	  ((not (or (string-empty-p (or (hattr:get ibut 'name) ""))))
	   (insert ibut:label-separator))))
  (let* ((orig-actype (or (hattr:get ibut 'actype)
			  (hattr:get ibut 'categ)))
	 (actype (or (actype:elisp-symbol orig-actype)
		     (and (symbolp orig-actype) (fboundp orig-actype)
			  orig-actype)))
	 (args   (hattr:get ibut 'args))
	 (arg1   (nth 0 args))
	 (arg2   (nth 1 args))
	 (arg3   (nth 2 args)))
    (pcase actype
      ('actypes::kbd-key
       (cond ((and (stringp arg1) (string-match "\\s-*{.+}\\s-*" arg1))
	      (insert arg1))
	     ((stringp arg1)
	      (insert "{" arg1 "}"))
	     (t (insert "{}"))))
      ((or 'actypes::link-to-directory 'actypes::link-to-Info-node 'actypes::link-to-Info-index-item)
       (insert "\"" arg1 "\""))
      ('actypes::annot-bib (insert "[" arg1 "]"))
      ('actypes::exec-shell-cmd (insert "\"!" arg1 "\""))
      ('actypes::exec-window-cmd (insert "\"&" arg1 "\""))
      ('actypes::link-to-gbut (insert "<glink:" arg1 ">"))
      ('actypes::link-to-ebut (progn (insert "<elink:" arg1)
				     (when arg2 (insert ": " arg2))
				     (insert ">")))
      ('actypes::link-to-ibut (progn (insert "<ilink:" arg1)
				     (when arg2 (insert ": " arg2))
				     (insert ">")))
      ('actypes::link-to-kcell
       (if arg2
	   (progn (insert "<")
		  (when arg1 (insert arg1))
		  (insert ", " arg2 ">"))
	 (insert "<@ ")
	 (when arg1 (insert arg1))
	 (insert ">")))
      ((or 'actypes::link-to-kotl 'klink:act)
       (when (stringp arg1)
	 (if (string-prefix-p "<" arg1)
	     (insert arg1)
	   (insert "<" arg1 ">"))))
      ('actypes::link-to-org-id (insert (format "\"id:%s\"" arg1)))
      ('actypes::link-to-rfc (insert (format "rfc%d" arg1)))
      ('man (insert arg1))
      ('actypes::man-show (insert arg1))
      ('actypes::link-to-file-line (insert (format "\"%s:%d\""
						   (hpath:shorten arg1) arg2)))
      ('actypes::link-to-file-line-and-column
       (insert
	(if (eq arg3 0)
	    (format "\"%s:%d\"" (hpath:shorten arg1) arg2)
	  (format "\"%s:%d:%d\"" (hpath:shorten arg1) arg2 arg3))))
      ('actypes::link-to-file
       ;; arg2 when given is a buffer position
       (insert "\""
	       (if arg2
		   ;; includes buffer pos that we translate to line:col
		   (hpath:file-position-to-line-and-column arg1 arg2)
		 ;; filename only
		 (hpath:shorten arg1))
	       "\""))
      ('actypes::link-to-string-match
       (insert (format "\"%s#%s%s\"" (hpath:shorten arg3) arg1
		       (if (<= arg2 1) "" (concat ":I" (number-to-string arg2))))))
      ('actypes::link-to-texinfo-node
       (insert (format "\"%s#%s\"" (hpath:shorten arg1) arg2)))
      ('nil (error "(ibut:insert-text): actype must be a Hyperbole actype or Lisp function symbol, not '%s'" orig-actype))
      ;; Generic action button type
      (_ (insert (format "<%s%s%s>" (or (actype:def-symbol actype) actype)
			 (if args " " "")
			 (if args (hypb:format-args args) "")))))
    (unless (looking-at "\\s-\\|\\'")
      (insert " "))))

(defun    ibut:previous-occurrence (name-key &optional buffer)
  "Move point to previous occurrence of an implicit button with NAME-KEY.
Optional BUFFER defaults to current buffer.  It may be a buffer name.
Return non-nil iff occurrence is found.

Remember to use (goto-char (point-max)) before calling this to search
the whole buffer."
  (when buffer
    (if (not (or (bufferp buffer) (and (stringp buffer) (get-buffer buffer))))
	(error "(ibut:previous-occurrence): Invalid buffer arg: %s" buffer)
      (switch-to-buffer buffer)))
  (when (or (re-search-backward (ibut:name-regexp name-key) nil t)
	    (re-search-backward (ibut:name-regexp name-key t) nil t))
    (goto-char (+ (match-beginning 0) (length ibut:label-start)))))

(defun    ibut:program (name actype &rest args)
  "Programmatically create an implicit Hyperbole button at point.
Create button from NAME, ACTYPE (action type), and optional actype ARGS.
Insert NAME text at point surrounded by <[ ]> delimiters, adding any
necessary instance number of the button after the NAME.  ACTYPE may
be a Hyperbole action type name (from defact) or an Emacs Lisp
function, followed by a list of arguments for the actype, aside from
the button NAME which is automatically provided as the first argument.

For interactive creation, use `hui:ibut-create' instead."
  (hui:buf-writable-err (current-buffer) "ibut:program")
  (when (ebut:at-p)
    (error "(ibut:program): Move off explicit button at point to create an implicit button"))
  (let ((ibut (ibut:at-p)))
    ;; Throw an error if on a named or delimited Hyperbole button since
    ;; cannot create another button within such contexts.
    (when ibut
      (let ((name (hattr:get ibut 'name))
	    (name-start (hattr:get ibut 'name-start))
	    (lbl (hbut:key-to-label (hattr:get ibut 'lbl-key)))
	    (lbl-start (hattr:get ibut 'lbl-start))
	    (lbl-end (hattr:get ibut 'lbl-end)))
	(when (or name lbl (and lbl-start lbl-end))
	  (error "(ibut:program): Cannot nest an ibut within the existing button: '%s'"
		 (or name lbl (buffer-substring-no-properties (or name-start lbl-start) lbl-end))))))

    (save-excursion
      (let ((but-buf (current-buffer))
	    (actype-sym (actype:action actype)))
	(hui:buf-writable-err but-buf "ibut:program")
	(hattr:clear 'hbut:current)
	(hattr:set 'hbut:current 'name name)
	(hattr:set 'hbut:current 'categ 'implicit)
	(hattr:set 'hbut:current 'loc (hui:key-src but-buf))
	(hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
	(if (or (and actype-sym (fboundp actype-sym))
		(functionp actype))
	    (hattr:set 'hbut:current 'actype actype)
	  (error "Actype arg must be a bound symbol (not a string): %S" actype))
	(hattr:set 'hbut:current 'args args)
	(condition-case err
	    (ibut:operate)
	  (error "(ibut:program): name: %S actype: %S args: %S - %S" name actype args err))))))

(defun    ibut:rename (old-lbl new-lbl)
  "Change an implicit button name in the current buffer from OLD-LBL to NEW-LBL.
Return t if the label is changed, else nil.

Signal an error when no such button is found in the current buffer or if either
OLD-LBL or NEW-LBL is empty.

Leave point at the start of the button label which may be elsewhere
than the current point; callers should use `save-excursion` to retain
the existing point."
  (cond ((or (not (stringp new-lbl)) (< (length new-lbl) 1))
	 (error "(ibut:rename): Invalid 'new-lbl' argument: \"%s\"" new-lbl))
	((or (not (stringp old-lbl)) (< (length old-lbl) 1))
	 (error "(ibut:rename): Invalid 'old-lbl' argument: \"%s\"" old-lbl))
	((ibut:to old-lbl)
         (unless (string-equal old-lbl new-lbl)
	   (delete-region (point) (search-forward ibut:label-end nil t))
	   (save-excursion (insert new-lbl ibut:label-end))
	   (hattr:clear 'hbut:current)
           t))
	(t (error "(ibut:rename): Button '%s' not found in visible portion of buffer" old-lbl))))

(defalias 'ibut:summarize #'hbut:report)

(defun    ibut:to (name-key)
  "Find the nearest implicit button with NAME-KEY (a name or name key).
Find within the visible portion of the current buffer.  Leave
point at the start of the button text or its optional name, if it
has one (excluding delimiters).  Return the symbol for the
button, else nil.

When NAME-KEY is nil, return the ibutton at point or nil if none."
  (if name-key
      (hbut:funcall (lambda (name-key _buffer _key-src)
		      (when name-key
			;; Handle a name given rather than a name key
			(when (string-match-p "\\s-" name-key)
			  (setq name-key (ibut:label-to-key name-key)))
			(let ((regexp (ibut:name-regexp name-key t))
			      (start (point))
			      at-name-key
			      pos
			      found)
			  (save-excursion
			    ;; Since point might be in the middle of the matching button,
			    ;; move to the start of line to ensure don't miss it when
			    ;; searching forward.
			    (forward-line 0)
			    ;; re-search forward
			    (while (and (not found) (re-search-forward regexp nil t))
			      (setq pos (match-beginning 0)
				    ;; Point might be on closing delimiter of ibut in which
				    ;; case ibut:label-p returns nil; move back one
				    ;; character to prevent this.
				    found (save-excursion
					    (goto-char (1- (point)))
					    (setq at-name-key (ibut:at-p t))
					    (equal at-name-key name-key))))
			    (unless found
			      (goto-char start))
			    ;; re-search backward
			    (while (and (not found) (re-search-backward regexp nil t))
			      (setq pos (match-beginning 0)
				    at-name-key (ibut:at-p t)
				    found (equal at-name-key name-key))))
			  (when found
			    (goto-char pos)
			    (ibut:at-p)))))
		    name-key
		    (current-buffer))
    (ibut:at-p)))

(defun    ibut:at-to-name-p (&optional ibut)
  "If point is on an implicit button, optional IBUT, move to the start of its name.
If name is found, leave point after its opening delimiter and set the name
and lbl-key properties of IBUT.  Return t if name is found, else nil."
  (let ((opoint (point))
	move-flag
	name
	start)
    (when (and (or (ibut:is-p ibut)
		   (setq ibut (ibut:at-p)))
	       (setq start (hattr:get ibut 'lbl-start)))
      (goto-char start)
      (forward-line 0)
      (while (search-forward ibut:label-start start t)
	(setq move-flag t))
      (if move-flag
	  (progn (setq name (ibut:label-p t nil nil nil t))
		 (when name
		   (hattr:set ibut 'name name)
		   (hattr:set ibut 'lbl-key (ibut:label-to-key name))))
	(setq ibut nil)
	(goto-char opoint)))
    move-flag))

(defun    ibut:to-name (lbl-key)
  "Move to the name of the nearest named implicit button matching LBL-KEY.
Find the nearest implicit button with LBL-KEY (a label or label
key), within the visible portion of the current buffer and move
to the start of its delimited button name (after opening
delimiter).  This will find an implicit button if point is within
its name or text or if LBL-KEY is a name/name-key of an existing
implicit button.  It will not find other unnamed implicit
buttons.

Return the symbol for the button if found, else nil."
  (unless lbl-key
    (setq lbl-key (ibut:label-p nil nil nil nil t)))
  (hbut:funcall
   (lambda (lbl-key _buffer _key-src)
     (let* ((name-start-end (ibut:label-p nil nil nil t t))
	    (name-start (nth 1 name-start-end))
	    (at-name (car name-start-end))
	    (at-text-key (ibut:label-p nil "\"" "\"" nil t))
	    ibut)
       (cond ((or (and at-name (equal at-name lbl-key))
		  (and lbl-key (equal at-text-key lbl-key)))
	      (setq ibut 'hbut:current))
	     ((and lbl-key (setq ibut (ibut:to lbl-key)))))
       ;; Skip past any optional name and separators
       (cond (name-start
	      (goto-char name-start)
	      (skip-chars-forward (regexp-quote ibut:label-start)))
	     ((ibut:at-to-name-p ibut)))
       ibut))
   lbl-key
   (current-buffer)))

(defun    ibut:to-text (lbl-key)
  "Move to the text of the nearest implicit button matching LBL-KEY.
Find the nearest implicit button with LBL-KEY (a name or name key)
within the visible portion of the current buffer and move to within
its button text.  This will find an implicit button if point is
within its name or text or if LBL-KEY is a name/name-key of an
existing implicit button.  It will not find other unnamed implicit
buttons.

The caller must have populated the attributes of \='hbut:current.

Return the symbol for the button if found, else nil."
  (unless (stringp lbl-key)
    (error "(ibut:to-text): %s 'lbl-key' arg must be a string, not: %S"
	   (hattr:get 'hbut:current 'categ)
	   lbl-key))
  (hbut:funcall
   (lambda (lbl-key _buffer _key-src)
     (let* ((name-end (hattr:get 'hbut:current 'name-end))
	    (at-name (hattr:get 'hbut:current 'name))
	    (at-text-key (hattr:get 'hbut:current 'lbl-key))
	    (opoint (point))
	    move-flag
	    start
	    ibut)
       ;; Do not move point if it is already in the text of an
       ;; implicit button matching LBL-KEY.  If on the name of
       ;; the same button, move into the text of the button.
       (cond ((and lbl-key (equal at-text-key lbl-key))
	      (setq ibut 'hbut:current))
	     ((and at-name (equal (ibut:label-to-key at-name) lbl-key))
	      (setq ibut 'hbut:current
		    move-flag t))
	     ((and lbl-key (setq ibut (ibut:to lbl-key)))
	      (setq move-flag t)))
       (when (and move-flag ibut)
	 ;; Skip past any optional name and separators
	 (if (setq start (hattr:get ibut 'lbl-start))
	     (goto-char start)
	   (when name-end
	     (goto-char name-end)
	     (if (looking-at ibut:label-separator-regexp)
		 ;; Move past up to 2 possible characters of ibut
		 ;; delimiters to ensure are inside the ibut name; this
		 ;; prevents recognizing labeled, delimited ibuts of a
		 ;; single character since no one should need that.
		 (goto-char (min (+ 2 (match-end 0)) (point-max)))
	       (goto-char opoint)))))
       ibut))
   lbl-key
   (current-buffer)))

(defun    ibut:type (ibut)
  "Return full implicit type name for IBUT, else nil."
  (when (ibut:is-p ibut)
    (hattr:get ibut 'categ)))

;;; ------------------------------------------------------------------------
(defconst ibut:label-start "<["
  "String matching the start of a Hyperbole implicit button optional name.")
(defconst ibut:label-end   "]>"
  "String matching the end of a Hyperbole implicit button optional name.")

;;; ========================================================================
;;; ibtype class - Implicit button types
;;; ========================================================================

(defmacro defib (type _params doc at-p &optional to-p style)
  "Create Hyperbole implicit button TYPE with PARAMS, described by DOC.
TYPE is an unquoted symbol.  PARAMS are presently ignored.

AT-P is a boolean form of no arguments which determines whether or not point
is within a button of this type.  When non-nil, it must contain a call
to `ibut:label-set' with the text and optional buffer region of the
button's label.  This almost always should be followed by a call to
`hact' with an action to be performed whenever a button of this type
is activated.

The action may be a regular Emacs Lisp function or a Hyperbole action
type created with `defact' but may not return nil since any nil value
returned is converted to t to ensure the implicit button checker
recognizes that the action has been executed.

Optional TO-P is a boolean form which moves point immediately after the next
button of this type within the current buffer and returns a list of (button-
label start-pos end-pos), or nil when none is found.

Optional STYLE is a display style specification to use when highlighting
buttons of this type; most useful when TO-P is also given.

Return symbol created when successful, else nil.  Nil indicates that action
type for ibtype is presently undefined."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp] ; Doc string, if present.
                           def-body)))
  (when type
    (let* ((to-func (when to-p (action:create nil (list to-p))))
	   (at-func (list at-p))
	   (at-func-symbols (flatten-tree at-func)))
      (progn (unless (or (member 'ibut:label-set at-func-symbols)
			 (member 'hsys-org-set-ibut-label at-func-symbols))
	       (error "(defib): `at-p' argument for %s must include a call to `ibut:label-set'" type))
	     `(progn (symtable:add ',type symtable:ibtypes)
		     (htype:create ,type ibtypes ,doc nil ,at-func
				   '(to-p ,to-func style ,style)))))))

;; Support edebug-defun for interactive debugging of ibtypes
(def-edebug-spec defib
 (&define name lambda-list
          [&optional stringp]   ; Match the doc string, if present.
          def-body))

(def-edebug-spec lambda-list
 (([&rest arg]
   [&optional ["&optional" arg &rest arg]]
   &optional ["&rest" arg])))

(defun ibtype:act (ibtype)
  "Execute IBTYPE's action in contexts where `ibtype:test-p' is true."
  (let ((elisp-sym (ibtype:elisp-symbol ibtype)))
    (when elisp-sym
      (funcall elisp-sym))))

(defalias 'ibtype:create #'defib)

(defun    ibtype:activate-link (referent)
  "Activate an implicit link REFERENT, either a key series, a url or a path."
  (when referent
    (let ((key-series (kbd-key:is-p referent)))
      (if key-series
	  (hact #'kbd-key:act key-series)
	(let ((encoded-path-to-display (when referent (url-encode-url referent))))
	  (if (hpath:www-p encoded-path-to-display)
	      (hact #'www-url encoded-path-to-display)
	    (hact #'hpath:find referent)))))))


(defmacro defil (type start-delim end-delim text-regexp link-expr
		 &optional start-regexp-flag end-regexp-flag doc)
  "Create Hyperbole implicit button link TYPE.
Use: TYPE (an unquoted symbol), START-DELIM and END-DELIM (strings),
TEXT-REGEXP and LINK-EXPR.

With optional START-REGEXP-FLAG non-nil, START-DELIM is treated
as a regular expression.  END-REGEXP-FLAG treats END-DELIM as a
regular expression.  Hyperbole automatically creates a doc string
for the type but you can override this by providing an optional
DOC string.

TEXT-REGEXP must match to the text found between a button's delimiters
in order for this type to activate.  The matched text is applied
to LINK-EXPR to produce the link's referent, which is then displayed.

LINK-EXPR may be:
  (1) a brace-delimited key series;
  (2) a URL;
  (3) a path (possibly with trailing colon-separated line and column numbers);
  (4) or a function or action type of one argument, the button text (sans the
      function name if an Action Button), to display it.

Prior to button activation, for the first three kinds of
LINK-EXPR, a `replace-match' is done on the expression to
generate the button-specific referent to display.  Thus, either
the whole button text (\\\\&) or any numbered grouping from
TEXT-REGEXP, e.g. \\\\1, may be referenced in the LINK-EXPR to
form the link referent.

Here is a sample use case.  Create a button type whose buttons
perform a grep-like function over a current repository's git
log entries.  The buttons use this format: [<text to match>].

The following defines the button type called search-git-log which
calls hypb:fgrep-git-log with the text of the button as an argument:

  (defil search-git-log \"[<\" \">]\" \".*\" #\\='hypb:fgrep-git-log)

Place point after one of the above expressions and evaluate it with
\\[eval-last-sexp] to define the implicit button type.  Then if you
have cloned the Hyperbole repo and are in a Hyperbole source buffer,
an Action Key press on a button of the form:

  ;; [<test release>]

will display one line per commit whose change set matches \"test
release\".  An Action Key press on any such line will then display the
commit changes."
  (declare (debug
            (&define name stringp stringp stringp [&or stringp lambda-list]
                     [&optional arg arg stringp]   ; Doc string, if present.
                     def-body)))
  (when type
    `(prog1
	 (defib ,type ()
	   (interactive)
	   (let* ((button-text-start-end (hargs:delimited ,start-delim ,end-delim
							  ,start-regexp-flag ,end-regexp-flag t))
		  (button-text (nth 0 button-text-start-end))
		  (lbl-start   (nth 1 button-text-start-end))
		  (lbl-end     (nth 2 button-text-start-end))
		  actype)
	     (when (and button-text (string-match ,text-regexp button-text))
	       ;; Get the action type when link-expr is a function
	       ;; symbol, symbol name or function body
	       (setq actype (cond ((or (functionp ,link-expr) (subrp ,link-expr))
				   ,link-expr)
				  (t (actype:action ,link-expr))))
	       (if actype
		   (if (and (equal ,start-delim "<") (equal ,end-delim ">"))
		       ;; Is an Action Button; send only the non-space
		       ;; text after the action to link-expr
		       (hact actype (progn (string-match "\\s-+" button-text)
					   (substring button-text (match-end 0))))
		     (ibut:label-set button-text lbl-start lbl-end)
		     (hact actype button-text))
		 (when (and (stringp ,link-expr) (string-match ,text-regexp button-text))
		   ;; Change %s format syntax in link-expr to \\1 regexp replacement syntax
		   (let ((referent (replace-match (save-match-data
						    (if (string-match "\\(\\`\\|[^%]\\)\\(%s\\)" ,link-expr)
							(replace-match "\\1\\\\1" t nil ,link-expr)
						      ,link-expr))
						  t nil button-text)))
		     ;; link-expr is a string
		     (when referent
		       (if (string-match "\\(\\`\\|[^%]\\)\\(%s\\)" ,link-expr)
			   (ibut:label-set referent (match-beginning 1) (match-end 1))
			 (ibut:label-set referent lbl-start lbl-end))
		       (ibtype:activate-link referent))))))))
       (put (intern (format "ibtypes::%s" ',type))
	    'function-documentation
	    (or ,doc
		(format "%s - %s\n\n%s %s%s%s\n%s %s" ',type "Hyperbole implicit button type"
			"  Recognizes buttons of the form:\n    "
			(if ,start-regexp-flag (regexp-quote ,start-delim) ,start-delim)
			,text-regexp
			(if ,end-regexp-flag (regexp-quote ,end-delim) ,end-delim)
			"  which display links with:\n    "
			(if (stringp ,link-expr) (regexp-quote ,link-expr) ,link-expr)))))))

(defmacro defal (type link-expr &optional doc)
  "Create Hyperbole action button link TYPE (an unquoted symbol).
Buttons of the type look like: <TYPE link-text> where link-text is
substituted into LINK-EXPR as grouping 1 (specified either as %s
or \\\\1).  Hyperbole automatically creates a doc string for the
type but you can override this by providing an optional DOC
string.

LINK-EXPR may be:
  (1) a brace-delimited key series;
  (2) a URL;
  (3) a path (possibly with trailing colon-separated line and column numbers);
  (4) or a function or action type of one argument, the button text sans the
      function name.

Prior to button activation, for the first three kinds of
LINK-EXPR, a `replace-match' is done on the expression to
generate the button-specific referent to display, substituting
%s or \\\\1 in the LINK-EXPR for the text/label from the button.

For the fourth kind, LINK-EXPR is a function of one argument which is
either the full button text or in the case of an Action Button, the
text following the function name at the start of the button.

Here is a sample use case.  If you use Python and have a
PYTHONPATH environment variable setup, then pressing
\\[eval-last-sexp] after this expression:

   (defal pylib \"${PYTHONPATH}/%s\")

defines a new action button link type called `pylib' whose buttons
take the form of:

   <pylib PYTHON-LIBRARY-FILENAME>

and display the associated Python libraries (typically Python source
files).  Optional colon separated line and column numbers may be given
as well.

Therefore an Action Key press on:

   <pylib string.py:5:7>

would display the source for \"string.py\" (wherever it is installed
on your system) from the Python standard library with point on the
fifth line at the seventh character.

For more flexible regular expression-based link type creation, see
`defil'.  For the most general implicit button type creation,
use `defib'."
  (declare (debug (&define name [&or stringp lambda-list]
                           [&optional stringp])))
  (when type
    `(defil ,type "<" ">" (format "%s\\s-+\"?\\([^\t\n\r\f'`\"]+\\)\"?" ',type)
       ,link-expr nil nil ,doc)))   ; Match the doc string, if present.

(defalias 'ibtype:create-action-link-type #'defal)
(defalias 'ibtype:create-regexp-link-type #'defil)

(defun    ibtype:def-symbol (ibtype)
  "Return the abbreviated symbol for IBTYPE used in its `defib'.
IBTYPE must be a symbol or string that begins with `ibtype::' or nil
is returned."
  (let ((name (if (stringp ibtype)
		  ibtype
		(symbol-name ibtype))))
    (when (string-match "\\`ibtypes::" name)
      (intern (substring name (match-end 0))))))

(defun    ibtype:delete (type)
  "Delete an implicit button TYPE (a symbol).
Return TYPE's symbol if it existed, else nil."
  (interactive (list (intern (hargs:read-match
			      (concat "Delete from " (symbol-name 'ibtypes) ": ")
			      (mapcar 'list (htype:names 'ibtypes))
			      nil t nil 'ibtypes))))
  (htype:delete type 'ibtypes))

;; Return the full Elisp symbol for IBTYPE, which may be a string or symbol.
(defalias 'ibtype:elisp-symbol #'symtable:ibtype-p)

(defun ibtype:test-p (ibtype)
  "Return t if IBTYPE would activate in the current buffer context, else nil."
  (let ((elisp-sym (ibtype:elisp-symbol ibtype))
	(hrule:action #'actype:identity))
    (and elisp-sym (funcall elisp-sym) t)))

(provide 'hbut)
;;; hbut.el ends here
