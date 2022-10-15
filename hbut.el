;;; hbut.el --- GNU Hyperbole button constructs  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    18-Sep-91 at 02:57:09
;; Last-Mod:     15-Oct-22 at 18:44:48 by Bob Weiner
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(cl-lib elisp-mode help-mode hversion
				    hmoccur hbmap htz hbdata hact view)))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function www-url "hsys-www" (url))

(defvar hproperty:but-face)

;;; ************************************************************************
;;; Public definitions
;;; ************************************************************************

;;; ========================================================================
;;; ebut class - Explicit Hyperbole buttons
;;; ========================================================================

(defvar   ebut:hattr-save t
  "*Non-nil value saves button data when button source is saved.
Nil disables saving.")

(defun    ebut:act (label)
  "Activate Hyperbole explicit button with LABEL from the current buffer."
  (interactive (list (hargs:read-match "Activate explicit button labeled: "
				       (ebut:alist)
				       nil t nil 'ebut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (ebut:get lbl-key)))
    (if but
	(hbut:act but)
      (error "(ebut:act): No explicit button labeled: %s" label))))

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
Default is `hbut:current'.
Button should hold the following attributes (see `hattr:set'):
   lbl-key (normalized button label string),
   loc     (filename or buffer where button is located),
   dir     (directory name where button is located),
   actype  (action type that provides a default action for the button),
   action  (optional action that overrides the default),
   args    (list of arguments for action, if action takes a single
            argument of the button lbl-key, args may be nil).

If successful, return any instance number to append to button label
except when instance number would be 1, then return t.  On failure,
return nil.

If successful, leave point in button data buffer, so caller should use
`save-excursion'.  Does not save button data buffer."
  (let ((lbl-instance (hbdata:write nil but-sym)))
    (run-hooks 'ebut-create-hook)
    lbl-instance))

(defun    ebut:delete (&optional but-sym)
  "Delete Hyperbole explicit button based on optional BUT-SYM.
Default is `hbut:current'.
Return entry deleted (a list of attribute values) or nil."
  (unless but-sym
    (setq but-sym 'hbut:current))
  (when (ebut:is-p but-sym)
    (let* ((but-key (hattr:get but-sym 'lbl-key))
	   (loc     (hattr:get but-sym 'loc))
	   (entry   (hbdata:delete-entry but-key loc)))
      (run-hooks 'ebut-delete-hook)
      entry)))

(defun    ebut:edit (&optional lbl-key but-sym)
  "Edit existing Hyperbole button from optional LBL-KEY and BUT-SYM.
Defaults are the key for any button label at point and `hbut:current'.
If successful, return button's instance number, except when instance
number is 1, then return t.  On failure, as when button does not exist,
return nil.

Do not save button data buffer."
  (save-excursion
    (let ((lbl-instance (hbdata:write lbl-key but-sym)))
      (run-hooks 'ebut-edit-hook)
      lbl-instance)))

(defun    ebut:get (&optional lbl-key buffer key-src start-delim end-delim)
  "Return explicit Hyperbole button symbol given by LBL-KEY and BUFFER.
KEY-SRC is given when retrieving global buttons and is the full source pathname.

Retrieve button data, convert into a button object and return a symbol
which references the button.

All arguments are optional.  When none are given, return a symbol for
the button that point is within.  BUFFER defaults to the current
buffer.

Return nil if no matching button is found."
  (hattr:clear 'hbut:current)
  (save-excursion
    (let (actype
	  but-data
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
		     (intern (setq actype (hbdata:actype but-data))))
	  ;; Hyperbole V1 referent compatibility
	  (when (= (length actype) 2)
	    (hattr:set 'hbut:current 'referent
		       (hbdata:referent but-data)))
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

(defalias 'ebut:to-key-src            #'hbut:to-key-src)
(defalias 'ebut:key-src-set-buffer #'hbut:key-src-set-buffer)
(defalias 'ebut:key-src-fmt        #'hbut:key-src-fmt)
(defalias 'ebut:key-to-label       #'hbut:key-to-label)

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
	(hbut:max-len hbut:max-len)
	npoint start lbl-key end but-start but-end start-regexp end-regexp)
    (unless start-delim (setq start-delim ebut:start))
    (unless end-delim (setq end-delim ebut:end))
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
		      (condition-case ()
			  (progn
			    (forward-char -1)
			    (forward-list))
			(error nil)))
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

(defalias 'ebut:label-regexp #'hbut:label-regexp)

(defalias 'ebut:label-to-key #'hbut:label-to-key)

(defun    ebut:list (&optional file loc-p)
  "Return list of button labels from in FILE or the current buffer.
Remove duplicate labels if optional LOC-P is omitted.  With LOC-P, return
list of elements (label start end) where start and end are the buffer
positions at which the button delimiter begins and ends."
  (interactive)
  (setq file (if file (and (file-exists-p file) (find-file-noselect file))
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
  (hbut:map but-func ebut:start ebut:end regexp-match include-delims))

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
      (goto-char (+ (match-beginning 0) (length ebut:start)))))

(defun    ebut:operate (curr-label new-label)
  "Operate on and modify properties of an explicit button given by CURR-LABEL.
When NEW-LABEL is non-nil, this is substituted for CURR-LABEL and the
associated button is modified.  Otherwise, a new button is created.

If CURR-LABEL is nil, the text in the active region is used as the
button label, if any, otherwise, an error is signaled.

Return instance string appended to label to form a per-buffer unique
label; nil if label is already unique.  Signal an error when no such
button is found in the current buffer."
  (let* ((lbl-key (ebut:label-to-key curr-label))
	 (lbl-regexp (ebut:label-regexp lbl-key))
	 (modify new-label)
	 (instance-flag))
    (unless new-label
      (setq new-label curr-label))
    (hattr:set 'hbut:current 'lbl-key (ebut:label-to-key new-label))
    (save-excursion
      (when (setq instance-flag
		  (if modify (ebut:edit lbl-key) (ebut:create)))
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
			      instance-flag))
	      (cond ((ebut:map
		      (lambda (_lbl start end)
			(delete-region start end)
			(ebut:delimit
			 (point)
			 (progn (insert new-label) (point))
			 instance-flag))
		      lbl-regexp 'include-delims))
		    (at-but)
		    ((hypb:error "(ebut:operate): No button matching: %s" curr-label)))))

	  (instance-flag
	   ;; Add a new button recording its start and end positions
	   (let (start end mark prev-point buf-lbl)
	     (cond ((not curr-label)
		    (setq start (point))
		    (insert new-label)
		    (setq end (point)))
		   ((and (hmouse-use-region-p)
			 (if (hyperb:stack-frame
			      '(hui:ebut-create hui:ebut-edit hui:ebut-edit-region hui:gbut-create
                       				hui:gbut-edit hui:link-create ebut:program))
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
	     (ebut:delimit start end instance-flag)
	     (goto-char start)))

	  (t (hypb:error
	      "(ebut:operate): Operation failed.  Check button attribute permissions: %s"
	      hattr:filename)))

    ;; Append any instance-flag string to the button label
    (when (stringp instance-flag)
      (setq new-label (concat new-label instance-flag))
      (hattr:set 'hbut:current 'lbl-key (ebut:label-to-key new-label)))

    ;; Position point
    (let ((new-key (ebut:label-to-key new-label)))
      (cond ((equal (ebut:label-p) new-key)
	     ;; In case right before the start of the desired
	     ;; button's delimiters.
	     (forward-char 2) (search-backward ebut:start nil t)
	     (goto-char (match-end 0)))
	    ((let ((regexp (ebut:label-regexp new-key)))
	       (or (re-search-forward  regexp nil t)
		   (re-search-backward regexp nil t)))
	     (goto-char (+ (match-beginning 0) (length ebut:start))))))

    ;; instance-flag might be 't which we don't want to return.
    (when (stringp instance-flag) instance-flag)))



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
	    (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
	    (hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
            (if (or (and actype-sym (fboundp actype-sym))
		    (functionp actype))
		(hattr:set 'hbut:current 'actype actype)
	      (error (format "(%s)" actype)))
	    (hattr:set 'hbut:current 'args args)
	    (ebut:operate label nil))
	(error (hattr:clear 'hbut:current)
	       (if (and (listp (cdr err)) (= (length (cdr err)) 1))
		   (error (format "(ebut:program): actype arg must be a bound symbol (not a string): %s" actype))
		 (error "(ebut:program): %s" err)))))))

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
	    (setq currfile (buffer-file-name currbuf)))
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
				  (setq end (progn (end-of-line) (point))
					start (progn
						(goto-char (match-beginning 0))
						(beginning-of-line) (point))
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
else nil."
  (unless lbl-key
    (setq lbl-key (ebut:label-p nil nil nil nil t)))
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
		lbl-key))

;;; ------------------------------------------------------------------------
(defun    ebut:delimit (start end instance-flag)
  "Delimit button label spanning region START to END in current buffer.
If button is already delimited or delimit fails, return nil, else t.
Insert INSTANCE-FLAG after END, before ending delimiter."
  (goto-char start)
  (when (looking-at (regexp-quote ebut:start))
    (forward-char (length ebut:start)))
  (unless (ebut:label-p)
    (setq start (move-marker (make-marker) start)
	  end (move-marker (make-marker) end))
    (set-marker-insertion-type end t)
    ;; instance-flag may be 't to indicate don't add an instance number
    (unless (stringp instance-flag)
      (setq instance-flag ""))
    (insert ebut:start)
    (goto-char end)
    (insert instance-flag ebut:end)
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
		       (concat "[^" (substring ebut:end -1) "]*")
		     "[ \t\n\r]*"))
  (concat
   (regexp-quote ebut:start) match-part
   "\\(" (mapconcat (lambda (key) (ebut:label-regexp key 'no-delim))
		    match-keys "\\|")
   "\\)" match-part (regexp-quote ebut:end)))

(defconst ebut:start "<("
  "String matching the start of a Hyperbole explicit hyper-button.")
(defconst ebut:end   ")>"
  "String matching the end of a Hyperbole explicit hyper-button.")
(defconst ebut:instance-sep ":"
  "String of one character, separates an ebut label from its instance num.")

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
		 (hbut:act but)
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
				       (mapcar 'list (gbut:label-list))
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
  (when (equal buffer-file-name (gbut:file))
    (hbut:label-p as-label start-delim end-delim pos-flag two-lines-flag)))

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
    (when (hbdata:to-entry-buf (gbut:file))
      (let (gbuts)
	(save-restriction
	  (narrow-to-region (point) (if (search-forward "\f" nil t)
					(point) (point-max)))
	  (goto-char (point-min))
	  (condition-case ()
	      (while (setq gbuts (cons (car (read (current-buffer))) gbuts)))
	    (error nil))
	  gbuts)))))

(defun    gbut:ibut-key-list ()
  "Return a list of implicit button label keys from the global button file."
  (when (file-readable-p (gbut:file))
    (save-excursion
      (with-current-buffer (find-file-noselect (gbut:file))
	(save-restriction
	  (widen)
	  (ibut:label-map #'(lambda (label _start _end) (ibut:label-to-key label))))))))

;;; ========================================================================
;;; hattr class
;;; ========================================================================

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

(defun    hattr:get (obj-symbol attr-symbol)
  "Return value of OBJ-SYMBOL's attribute ATTR-SYMBOL."
  (get obj-symbol attr-symbol))

(defun    hattr:list (obj-symbol)
  "Return a property list of OBJ-SYMBOL's attributes.
Each pair of elements is: <attrib-name> <attrib-value>."
  (if (symbolp obj-symbol)
      (symbol-plist obj-symbol)
    (error "(hattr:list): Argument not a symbol: %s" obj-symbol)))

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
				  (make-symbol (substring str (match-end 0))))
				 (t val)))))))
      has-attr)))

(defun    hattr:save ()
  "Save button attribute file for current directory, if modified.
Suitable for use as part of `write-file-functions'."
  (let* ((bd-file (expand-file-name hattr:filename default-directory))
	 (buf (and (stringp default-directory)
		   (get-file-buffer bd-file))))
    (if (and ebut:hattr-save buf (not (eq buf (current-buffer))))
	(let ((ebut:hattr-save));; Prevents `write-file-functions' from looping.
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

(defvar   hattr:filename
  (if hyperb:microsoft-os-p "_hypb" ".hypb")
  "Per directory file name in which explicit button attributes are stored.
If you change its value, you will be unable to use buttons created by
others who use a different value!")

;;; ========================================================================
;;; hbut class - abstract
;;; ========================================================================

(defconst hbut:max-len 200
  "Maximum length of a Hyperbole button label.
If 0, there is no limit and searches for button end delimiters can go
as far as the end of the buffer.

Use the function, (hbut:max-len), to read the proper value.")

(defsubst hbut:max-len ()
  "Return the value of `hbut:max-len' if non-zero else (point-max)."
  (if (zerop hbut:max-len) (point-max) hbut:max-len))

(defun    hbut:act (&optional hbut)
  "Perform action for optional explicit or implicit Hyperbole button symbol HBUT.
Default is `hbut:current'."
  (interactive (list (hbut:get (hargs:read-match "Activate labeled Hyperbole button: "
						 (nconc (ebut:alist) (ibut:alist))
						 nil t nil 'hbut))))
  (unless hbut
    (setq hbut 'hbut:current))
  (cond ((hbut:is-p hbut)
	 (let ((orig-point (point-marker))
	       (action (hattr:get hbut 'action))
	       text-point)
	   (when (ibut:is-p hbut)
	     ;; Determine whether point is already within hbut; if
	     ;; not, it is moved there.
	     ;;
	     ;; The next line returns the lbl-key of the current
	     ;; button only if point is within the optional name,
	     ;; otherwise, nil.
	     (let* ((lbl-key-start-end (ibut:label-p nil nil nil t t))
		    (lbl-key (nth 0 lbl-key-start-end))
		    (delim-text-start (or (nth 1 lbl-key-start-end)
					  (hattr:get hbut 'lbl-start)))
		    (delim-text-end (or (nth 2 lbl-key-start-end)
				       (hattr:get hbut 'lbl-end))))
	       (if (and lbl-key
			(or (equal (hattr:get hbut 'loc) (current-buffer))
			    (equal (hattr:get hbut 'loc) buffer-file-name))
			(equal lbl-key (hattr:get hbut 'lbl-key)))
		   (unless (and delim-text-start delim-text-end
				(< delim-text-start (point))
				(>= delim-text-end (point)))
		     (goto-char delim-text-start)
		     (skip-chars-forward "^-_a-zA-Z0-9"))
		 ;; Here handle when there is no name preceding the
		 ;; implicit button.
		 (unless (and (or (equal (hattr:get hbut 'loc) (current-buffer))
				  (equal (hattr:get hbut 'loc) buffer-file-name))
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
	 (hypb:error "(hbut:act): Invalid Hyperbole button: %s" hbut))))

(defun    hbut:action (hbut)
  "Return appropriate action name/function for Hyperbole button symbol HBUT."
  (let ((categ (hattr:get hbut 'categ)) (atype) (action))
    (if (eq categ 'explicit)
	(progn (setq action (car (hattr:get hbut 'action))
		     atype  (hattr:get hbut 'actype))
	       (if (= (length (symbol-name atype)) 2)
		   atype
		 (or action (actype:action atype))))
      ;; Must be an implicit button.
      (when (functionp atype) atype))))

(defun    hbut:at-p ()
  "Return symbol for explicit or implicit Hyperbole button at point or nil.
Then use (hbut:act) to activate the button."
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
Use buffer commenting grammar, if any, otherwise don't comment.
Ignore email-related buffers."
  (save-excursion
    (if (and comment-start (not (hmail:mode-is-p))
	     (not (memq major-mode '(mail-mode message-mode))))
	(if (or (equal comment-end "")
		(null comment-end))
	    (progn
	      (beginning-of-line)
	      (if (search-forward comment-start start t)
		  nil
		(goto-char start)
		(insert comment-start)
		(if (not (eq (preceding-char) ?\ ))
		    (insert ?\ ))))
	  ;; Comments have both start and end delimiters
  	  (if (and (re-search-backward
		    (concat (regexp-quote comment-start) "\\|"
			    (regexp-quote comment-end))
		    nil t)
		   (looking-at (regexp-quote comment-start)))
	      nil
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
    "\n[ \t]*\\([:|<>]+ *\\)+"
    ;; Included text generated by SUPERCITE.  We can't hope to match all
    ;; the possible variations.
    "\n[ \t]*[^'`\"< \t]*> *"
    ;; Lisp comments
    "\n[ \t]*\\(;+[ \t]*\\)+"
    ;; UNIX shell comments
    "\n[ \t]*\\(#+[ \t]*\\)+"
    ;; C++ comments
    "\n[ \t]*//[/ \t]+"
    ;; C or Pascal comments, one open and close per line, so match close
    ;; then open.
    "\\*+[/\)][ \t\r]*\n+[ \t]*[/\(]\\*+"
    "}[ \t\r]*\n+[ \t]*{"
    ;; Eiffel or Sather comments
    "\n[ \t]*--[ \t]+"
    ;; Fortran comments
    "\n[Cc][ \t]+"
    ;; Postscript comments
    "\n[ \t]*\\(%+[ \t]*\\)+")
  "List of regexps of fill prefixes to remove from the middle of buttons.")

(defun    hbut:fill-prefix-remove (label)
  "Remove any recognized fill prefix from within LABEL.
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
Call FUNC with optional argument values of LBL-KEY, BUFFER and KEY-SRC.
The implicit button used is given by LBL-KEY (a label or label key)
within BUFFER or KEY-SRC (full path to global button file).  Use
`save-excursion' around this call to prevent permanent movement of
point when desired."
  (when buffer
    (if (bufferp buffer)
	(set-buffer buffer)
      (error "(ibut:get): Invalid buffer argument: %s" buffer)))
  (when (null key-src)
    (let ((loc (hattr:get 'hbut:current 'loc)))
      (when loc
	(set-buffer (or (get-buffer loc) (find-file-noselect loc)))))
    (setq key-src (hbut:to-key-src 'full)
	  ;; `hbut:to-key-src' sets current buffer to key-src buffer.
	  buffer (or buffer (current-buffer))))
  (when (stringp lbl-key)
    (when key-src
      (set-buffer (if (bufferp key-src)
		      key-src
		    (find-file-noselect key-src))))
    (when (or buffer key-src)
      (funcall func lbl-key buffer key-src))))

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
		      (buffer-file-name
		       (if full-flag
			   buffer-file-name
			 (file-name-nondirectory buffer-file-name)))
		      ;; Handle any preceding @loc hyp-source implicit button location references.
		      ;; This is used in report buffers of explicit buttons, i.e. hui:hbut-report
		      ;; and the *HyRolo* abd *HyNote* output buffers.
		      ((save-excursion
			 (save-restriction
			   (widen)
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
      key-src)))

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
    (let ((src (and buffer-file-name
		    (substring
		     buffer-file-name
		     0 (string-match "\\.[^.]+$" buffer-file-name)))))
      (cond ((file-exists-p (concat src ".texi"))
	     (concat src ".texi"))
	    ((file-exists-p (concat src ".texinfo"))
	     (concat src ".texinfo"))
	    ((current-buffer))))))

(defun    hbut:key-src-set-buffer (src)
  "Set buffer to SRC, a buffer, buffer name, file, directory or symlink.
Return SRC or nil if invalid."
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
	;; not yet been saved, so it can't be read.
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
  "Unnormalize LBL-KEY.  Return regular expr matching delimited button label.
Optional NO-DELIM leaves off delimiters and leading and trailing space.
Optional START-DELIM and END-DELIM are added around the returned
label; these default to `ebut:start' and `ebut:end'."
  (when lbl-key
   (let* ((pos 0)
	   (len (length lbl-key))
	   (c)
	   (sep0 "[ \t\n\r]*")
	   (sep "[ \t\n\r]+")
	   (regexp (if no-delim "" (concat (regexp-quote (or start-delim ebut:start)) sep0)))
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
	(setq regexp (concat regexp sep0 (regexp-quote (or end-delim ebut:end))))))))

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

(defun    hbut:map (but-func &optional start-delim end-delim
			     regexp-match include-delims)
  "Apply BUT-FUNC to a set of hbuttons in the visible part of the current buffer.
The set of buttons are those whose labels are delimited by
optional START-DELIM and END-DELIM and that match any optional
REGEXP-MATCH.

START-DELIM defaults to ebut:start; END-DELIM defaults to ebut:end.
If END-DELIM is a symbol, e.g. t, then treat START-DELIM as a regular
expression which matches an entire button string.

BUT-FUNC must take precisely three arguments: the button label, the
start position of the delimited button label and its end position (positions
include delimiters when INCLUDE-DELIMS is non-nil)."
  (or start-delim (setq start-delim ebut:start))
  (or end-delim (setq end-delim ebut:end))
  (let* ((regexp (symbolp end-delim))
	 (end-sym (or regexp (substring end-delim -1)))
	 (rtn)
	 (ignore)
	 start end but lbl)
    (save-excursion
      (goto-char (point-min))
      (setq include-delims (if include-delims 0 1))
      (while (re-search-forward
	      (if regexp start-delim
		(concat (regexp-quote start-delim)
			"\\([^" end-sym "\"][^" end-sym "]*\\)"
			(regexp-quote end-delim)))
	      nil t)
	(setq start (match-beginning include-delims)
	      end (match-end include-delims)
	      but (match-string 0)
	      lbl (match-string 1)
	      ;; If within a programming language buffer, ignore matches outside comments.
	      ignore (hbut:outside-comment-p))
	(save-excursion
	  (goto-char start)
	  ;; Ignore matches with quoted delimiters.
	  (or ignore (setq ignore (memq (preceding-char) '(?\\ ?\{)))))
	(cond (ignore (setq ignore nil))
	      ((or (not regexp-match)
		   (string-match regexp-match but))
	       (setq rtn (cons (funcall but-func lbl start end) rtn))))))
    (nreverse rtn)))

(defvar   hbut:syntax-table (copy-syntax-table emacs-lisp-mode-syntax-table)
  "Modified Elisp syntax table for use with Action and Key Series buttons.
Makes < > and { } into syntactically matching pairs after `hyperb:init'
calls `hbut:modify-syntax'.")

;;;###autoload
(defun    hbut:modify-syntax ()
  "Modify syntactic character pairs in syntax tables.
Modify `hbut:syntax-table' and `help-mode-syntax-table'.  For use
with implicit button activations."
  ;; Treat angle brackets as opening and closing delimiters for ease
  ;; of matching.
  (mapc (lambda (syntax-table)
	  (modify-syntax-entry ?\< "(>" syntax-table)
	  (modify-syntax-entry ?\> ")<" syntax-table)
	  ;; Treat braces as opening and closing delimiters for ease of matching.
	  (modify-syntax-entry ?\{ "(}" syntax-table)
	  (modify-syntax-entry ?\} "){" syntax-table))
	(list hbut:syntax-table help-mode-syntax-table))
  nil)

(defun    hbut:outside-comment-p ()
  "True if in a programming mode and regexp match is outside a comment, else nil."
  (when (and (derived-mode-p 'prog-mode)
	     (not (eq major-mode 'lisp-interaction-mode))
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
		      (concat ebut:start lbl ebut:end)))
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
If a file, always return a full path if optional FULL-FLAG is non-nil."
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
	     file)))))

(defalias 'hbut:summarize 'hbut:report)

(defun    hbut:to (lbl-key)
  "Find the nearest explicit button or labeled/named implicit button.
Button given by LBL-KEY (a label or label key) and within the
visible portion of the current buffer.  Leave point inside the
button label.  Return the symbol for the button, else nil."
  (or (ebut:to lbl-key) (ibut:to lbl-key)))

(defvar   hbut:current nil
  "The currently selected Hyperbole button.  Available to action routines.")

(defconst hbut:source-prefix moccur-source-prefix
  "String found at start of a buffer containing only a hyper-button menu.
This expression should be followed immediately by a file-name indicating the
source file for the buttons in the menu, if any.")

(defun    hbut:label-list ()
  "Return list of current buffer's Hyperbole button labels."
  (mapcar #'hbut:key-to-label (hbut:key-list)))

;;; ------------------------------------------------------------------------

(defun    hbut:key-list ()
  "Return list of explicit and named implicit button label keys in current buffer."
  (nconc (hbut:ebut-key-list) (hbut:ibut-key-list)))

(defun    hbut:ebut-key-list (&optional key-src)
  "Return a list of explicit button label keys.
Keys in optional KEY-SRC or the current buffer."
  (save-excursion
    (if (hbdata:to-entry-buf (or key-src (buffer-file-name)))
	(let (hbuts)
	  (save-restriction
	    (narrow-to-region (point) (if (search-forward "\f" nil t)
					  (point) (point-max)))
	    (goto-char (point-min))
	    (condition-case ()
		(while (setq hbuts (cons (car (read (current-buffer))) hbuts)))
	      (error nil))
	    hbuts)))))

(defun    hbut:ibut-key-list (&optional key-src)
  "Return a list of implicit button label keys.
Keys in optional KEY-SRC or the current buffer."
  (save-excursion
    (when (hbut:key-src-set-buffer (or key-src (current-buffer)))
      (save-restriction
	(widen)
	(ibut:label-map #'(lambda (label _start _end) (ibut:label-to-key label)))))))

;;; ========================================================================
;;; ibut class - Implicit Hyperbole Buttons
;;; ========================================================================


(defun    ibut:act (label)
  "Activate Hyperbole implicit button with <[LABEL]> from the current buffer."
  (interactive (list (hargs:read-match "Activate implicit button labeled: "
				       (ibut:alist)
				       nil t nil 'ibut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (ibut:get lbl-key)))
    (if but
	(hbut:act but)
      (error "(ibut:act): No implicit button labeled: %s" label))))

(defun    ibut:alist (&optional file)
  "Return alist of labeled ibuts in FILE or the current buffer.
Each element is a list of just an implicit button label.  For use
as a completion table."
  (mapcar #'list (ibut:list file)))

(defun    ibut:at-p (&optional key-only)
  "Return symbol for implicit button at point, else nil.
Point may be on the implicit button text or its optional
preceding label.  With optional KEY-ONLY, return the label key
for button only.

Any labeled implicit button must contain at least two characters,
excluding delimiters, not just one."
  ;; Since the Smart Keys handle end-of-line separately from whether
  ;; point is within an implicit button, always report not within one
  ;; when point is at the end of a line.  -- RSW, 02-16-2020
  (unless (eolp)
    (let* ((opoint (point))
	   (name-start-end (ibut:label-p t nil nil t t))
	   (name       (nth 0 name-start-end))
	   (name-end   (nth 2 name-start-end))
	   (lbl-key (or (ibut:label-to-key name)
			(ibut:label-p nil "\"" "\"" nil t)
			(ibut:label-p nil "<" ">" nil t)
			(ibut:label-p nil "{" "}" nil t))))
      (unwind-protect
	  (progn
	    (when (not (hbut:outside-comment-p))
	      ;; Skip past any optional name and separators
	      (when name-start-end
		(goto-char name-end)
		(if (looking-at ibut:label-separator-regexp)
		    ;; Move past up to 2 possible characters of ibut
		    ;; delimiters; this prevents recognizing labeled,
		    ;; delimited ibuts of a single character since no one
		    ;; should need that.
		    (goto-char (min (+ 2 (match-end 0)) (point-max)))
		  (goto-char opoint))))
	    (if key-only
		lbl-key
	      ;; Check for an implicit button at current point, record its
	      ;; attributes and return a button symbol for it.  This call
	      ;; typically writes the text start and end attributes saved as
	      ;; `lbl-start' and `lbl-end' after finding the ibut type at point.
	      ;; So do not pass these attributes in to this call.
	      (ibut:create :name name :lbl-key lbl-key)))
	(goto-char opoint)))))

(defun    ibut:at-type-p (ibut-type-symbol)
  "Return non-nil if point is on a button of type IBUT-TYPE-SYMBOL.
Point must be on the button itself and not its label, if any.

The return value is a list of the type's action type symbol and
associated arguments from the button."
  (when (and ibut-type-symbol (symbolp ibut-type-symbol))
    (let ((type-name (symbol-name ibut-type-symbol)))
      (unless (string-match "::" type-name)
	(setq ibut-type-symbol (intern-soft (concat "ibtypes::" type-name))))
      (when ibut-type-symbol
	(let ((types (htype:category 'ibtypes))
	      ;; 'types' is a global var used in (hact) function, don't delete.
	      (hrule:action 'actype:identity))
	  (funcall ibut-type-symbol))))))

(cl-defun ibut:create (&optional &key name lbl-key lbl-start lbl-end
				 loc dir categ actype args action)
  "Return `hbut:current' symbol with attributes of implicit button at point.
Return nil if no implicit button at point."
  ;; :args is ignored unless :categ is also given.

  ;; `lbl-key' attribute will be set from the button name, if any;
  ;; otherwise, from its text.

  ;; `lbl-start' and `lbl-end' will be set from the start and end of the
  ;; ibut text, excluding delimiters, not of its name.

  ;; Since the Smart Keys handle end-of-line and end-of-buffer
  ;; separately from whether point is within an implicit button,
  ;; always report not within one when point is at the end of a line.
  ;; -- RSW, 02-16-2020 and 07-17-2022
  (unless (or (eolp) (eobp))
    (let* ((types (htype:category 'ibtypes))
	   ;; Global var used in (hact) function, don't delete.
	   (hrule:action #'actype:identity)
	   (ibpoint (point-marker))
	   (itype)
	   (is-type categ))

      (hattr:clear 'hbut:current)
      (unless is-type
	(while (and (not is-type) types)
	  (setq itype (car types))
	  (when (and itype (setq args (funcall itype)))
	    (setq is-type itype)
	    ;; Any implicit button type check should leave point
	    ;; unchanged.  Trigger an error if not.
	    (unless (equal (point-marker) ibpoint)
	      (hypb:error "(Hyperbole): `%s' at-p test improperly moved point from %s to %s"
			  is-type ibpoint (point-marker))))
	  (setq types (cdr types))))

      (set-marker ibpoint nil)

      (when is-type
	(let ((current-name      (hattr:get 'hbut:current 'name))
	      ;; (current-lbl-key   (hattr:get 'hbut:current 'lbl-key))
	      (current-lbl-start (hattr:get 'hbut:current 'lbl-start))
	      (current-lbl-end   (hattr:get 'hbut:current 'lbl-end))
	      ;; (current-categ     (hattr:get 'hbut:current 'categ))
	      (current-loc       (hattr:get 'hbut:current 'loc))
	      (current-dir       (hattr:get 'hbut:current 'dir))
	      (current-action    (hattr:get 'hbut:current 'action))
	      ;; (current-actype    (hattr:get 'hbut:current 'actype))
	      (current-args      (hattr:get 'hbut:current 'args)))

	  (if current-name
	      (setq name current-name)
	    (unless name
	      (setq name (ibut:label-p t nil nil nil t)))
	    (when name
	      (hattr:set 'hbut:current 'name name)))

	  ;; Need to ignore current-lbl-key and use name if any
	  (setq lbl-key (or (ibut:label-to-key name)
			    lbl-key
			    (ibut:label-p nil "\"" "\"" nil t)))
	  (when lbl-key
	    (hattr:set 'hbut:current 'lbl-key lbl-key))

	  (if current-lbl-start
	      (setq lbl-start current-lbl-start)
	    (when lbl-start
	      (hattr:set 'hbut:current 'lbl-start lbl-start)))

	  (if current-lbl-end
	      (setq lbl-end current-lbl-end)
	    (when lbl-end
	      (hattr:set 'hbut:current 'lbl-end lbl-end)))

	  (hattr:set 'hbut:current 'categ is-type)

	  (if current-loc
	      (setq loc current-loc)
	    (unless loc
	      (setq loc (save-excursion (hbut:to-key-src 'full))))
	    (when loc
	      (hattr:set 'hbut:current 'loc loc)))

	  (if current-dir
	      (setq dir current-dir)
	    (unless dir
	      (setq dir (hui:key-dir (current-buffer))))
	    (when dir
	      (hattr:set 'hbut:current 'dir dir)))

	  (if current-action
	      (setq action current-action)
	    (when action
	      (hattr:set 'hbut:current 'action action)))
	  (when action
	    (unless args (setq args action)))

	  (or current-args
	      (not (listp args))
	      (progn
		(setq args (copy-sequence args))
		(when (eq (car args) #'hact)
		  (setq args (cdr args)))
		(hattr:set 'hbut:current 'actype
			   (or
			    actype
			    ;; Hyperbole action type
			    (symtable:actype-p (car args))
			    ;; Regular Emacs Lisp function symbol
			    (car args)))
		(hattr:set 'hbut:current 'args (if actype args (cdr args))))))
	'hbut:current))))

(defun    ibut:delete (&optional but-sym)
  "Delete Hyperbole implicit button based on optional BUT-SYM.
Default is `hbut:current'.
Return symbol for button deleted or nil."
  (unless but-sym
    (setq but-sym 'hbut:current))
  (when (ibut:is-p but-sym)
    (let ((loc     (hattr:get but-sym 'loc))
	  (start   (hattr:get but-sym 'lbl-start))
	  (end     (hattr:get but-sym 'lbl-end)))
      (when (and start end)
	(save-excursion
	  (with-current-buffer (if (bufferp loc) loc (find-file-noselect loc))
	    (when (ibut:to (ibut:key but-sym))
	      (let (buffer-read-only)
		(if (< (point) start)
		    ;; Find beginning of button named label delimiter and delete
		    ;; from there.
		    (progn (goto-char (- (point) (length ibut:label-start)))
			   (delete-region (point) end))
		  ;; No label, just delete delimited ibutton text.
		  (delete-region start end))
		(when (looking-at "[ \t]*\r?\n")
		  (delete-region (point) (match-end 0)))
		(run-hooks 'ibut-delete-hook)))))
	but-sym))))

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
      (and categ (string-match "\\`ibtypes::" (symbol-name categ))))))

(defun    ibut:label-map (but-func &optional _start-delim _end-delim
				   _regexp-match _include-delims)
  "Apply BUT-FUNC to buttons delimited by optional START-DELIM and END-DELIM.
START-DELIM defaults to ibut:label-start; END-DELIM defaults to ibut:label-end.
If REGEXP-MATCH is non-nil, only buttons which match this argument are
considered.

Map over portion of buffer visible under any current restriction.
BUT-FUNC must take precisely three arguments: the button label, the
start position of the delimited button label and its end position (positions
include delimiters when INCLUDE-DELIMS is non-nil).
If END-DELIM is a symbol, e.g. t, then treat START-DELIM as a regular
expression which matches an entire button string."
  (hbut:map but-func ibut:label-start ibut:label-end))

(defun    ibut:rename (old-lbl new-lbl)
  "Change an implicit button name in the current buffer from OLD-LBL to NEW-LBL.
Return t if the label is changed, else nil.

Signal an error when no such button is found in the current buffer.

Leave point at the start of the button label which may be elsewhere
than the current point; callers should use `save-excursion` to retain
current."
  ;; !! Need to handle adding instances to labels, similar to ebut:operate.
  (cond ((or (not (stringp new-lbl)) (< (length new-lbl) 1))
	 (error "(ibut:rename): Invalid 'new-lbl' argument: \"%s\"" new-lbl))
	((or (not (stringp old-lbl)) (< (length old-lbl) 1))
	 (error "(ibut:rename): Invalid 'old-lbl' argument: \"%s\"" old-lbl))
	((ibut:to old-lbl)
         (unless (string-equal old-lbl new-lbl)
	   (delete-region (point) (search-forward ibut:label-end nil t))
	   (save-excursion (insert new-lbl ibut:label-end))
           t))
	(t (error "(ibut:rename): Button '%s' not found in visible portion of buffer." old-lbl))))

(defun    ibut:label-p (&optional as-label start-delim end-delim pos-flag two-lines-flag)
  "Return key for the implicit button label that point is within, else nil.
This is the normalized key form of an optional label that may
precede an implicit button.  Use `ibut:at-p' instead to test if
point is on either the implicit button text itself or the label.
Assume point is within the first line of any button label.

All following arguments are optional.  If AS-LABEL is non-nil,
label is returned rather than the key derived from the label.
START-DELIM and END-DELIM are strings that override default
button label delimiters.  With POS-FLAG non-nil, return list of
label-or-key, but-label-start-position, but-label-end-position.
Positions include delimiters.  With TWO-LINES-FLAG non-nil,
constrain label search to two lines."
  (with-syntax-table hbut:syntax-table
    (ebut:label-p as-label (or start-delim ibut:label-start)
		  (or end-delim ibut:label-end) pos-flag two-lines-flag)))

(defun    ibut:label-regexp (lbl-key &optional no-delim)
  "Unnormalize ibutton LBL-KEY.
Return regular expression matching delimited button label.
Optional NO-DELIM leaves off delimiters, leading and trailing space."
  (hbut:label-regexp lbl-key no-delim ibut:label-start ibut:label-end))

(defun    ibut:label-set (label &optional start end)
  "Set current implicit button attributes.
Get attributes from LABEL and optional START, END positions.
Return label.  When START and END are given, they specify the
region in the buffer to flash when this implicit button is
activated or queried for its attributes.  If LABEL is a list, it
is assumed to contain all arguments.

For legacy reasons, the label here is actually the text of the
implicit button matched contextually and never the optional delimited
name/label preceding the text."
  (cond ((stringp label)
	 (hattr:set 'hbut:current 'lbl-key (hbut:label-to-key label))
	 (when start (hattr:set    'hbut:current 'lbl-start start))
	 (when end   (hattr:set    'hbut:current 'lbl-end   end)))
	((and label (listp label))
	 (hattr:set 'hbut:current 'lbl-key (hbut:label-to-key (car label)))
	 (hattr:set 'hbut:current 'lbl-start (nth 1 label))
	 (hattr:set 'hbut:current 'lbl-end (nth 2 label)))
	(t (error "(ibut:label-set): Invalid label arg: `%s'" label)))
  label)

(defun    ibut:list (&optional file loc-p)
  "Return list of labels of labeled ibuts in FILE or the current buffer.
Remove duplicate labels if optional LOC-P is omitted.  With LOC-P, return
list of elements (label start end) where start and end are the buffer
positions at which the button label delimiter begins and ends."
  (interactive)
  (setq file (if file (and (file-exists-p file) (find-file-noselect file))
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
  "Return the key for Hyperbole implicit button symbol IBUT."
  (if (ibut:is-p ibut)
      (hattr:get ibut 'lbl-key)
    (error "(ibut:key): Argument is not a Hyperbole implicit button symbol, `%s'"
	   ibut)))

(defalias 'ibut:to-key-src   'hbut:to-key-src)
(defalias 'ibut:key-to-label 'hbut:key-to-label)
(defalias 'ibut:label-to-key 'hbut:label-to-key)
(defalias 'map-ibut          'ibut:map)

(defun    ibut:map (but-func &optional _start-delim _end-delim
			     regexp-match include-delims)
  "Apply BUT-FUNC to the visible, named implicit buttons.
If REGEXP-MATCH is non-nil, only buttons which match this argument
are considered.

BUT-FUNC must take precisely three arguments: the button label, the
start position of the delimited button label and its end position (positions
include delimiters when INCLUDE-DELIMS is non-nil)."
  (hbut:map but-func ibut:label-start ibut:label-end regexp-match include-delims))

(defun    ibut:next-occurrence (lbl-key &optional buffer)
  "Move point to next occurrence of an implicit button with LBL-KEY.
Optional BUFFER defaults to current buffer.  It may be a buffer name.
Return non-nil iff occurrence is found.

Remember to use (goto-char (point-min)) before calling this in order to
move to the first occurrence of the button."
  (when buffer
    (if (not (or (bufferp buffer) (and (stringp buffer) (get-buffer buffer))))
	(error "(ibut:next-occurrence): Invalid buffer arg: %s" buffer)
      (switch-to-buffer buffer)))
  (when (or (re-search-forward (ibut:label-regexp lbl-key) nil t)
	    (re-search-forward (ibut:label-regexp lbl-key t) nil t))
    (goto-char (+ (match-beginning 0) (length ibut:label-start)))))

(defun    ibut:previous-occurrence (lbl-key &optional buffer)
  "Move point to previous occurrence of an implicit button with LBL-KEY.
Optional BUFFER defaults to current buffer.  It may be a buffer name.
Return non-nil iff occurrence is found.

Remember to use (goto-char (point-max)) before calling this to search
the whole buffer."
  (when buffer
    (if (not (or (bufferp buffer) (and (stringp buffer) (get-buffer buffer))))
	(error "(ibut:previous-occurrence): Invalid buffer arg: %s" buffer)
      (switch-to-buffer buffer)))
  (when (or (re-search-backward (ibut:label-regexp lbl-key) nil t)
	    (re-search-backward (ibut:label-regexp lbl-key t) nil t))
    (goto-char (+ (match-beginning 0) (length ibut:label-start)))))

(defalias 'ibut:summarize 'hbut:report)

(defun    ibut:to (lbl-key)
  "Find the nearest implicit button with LBL-KEY (a label or label key).
Find within the visible portion of the current buffer.
Leave point inside the button text or its optional label, if it has one.
Return the symbol for the button, else nil."
  (unless lbl-key
    (setq lbl-key (ibut:label-p nil nil nil nil t)))
  (hbut:funcall (lambda (lbl-key _buffer _key-src)
		  (when lbl-key
		    ;; Handle a label given rather than a label key
		    (when (string-match-p "\\s-" lbl-key)
		      (setq lbl-key (ibut:label-to-key lbl-key)))
		    (let ((regexp (hbut:label-regexp lbl-key t))
			  (start (point))
			  at-lbl-key
			  ibut
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
					(setq ibut (ibut:at-p)
					      at-lbl-key (hattr:get ibut 'lbl-key))
					(equal at-lbl-key lbl-key))))
			(unless found
			  (goto-char start))
			;; re-search backward
			(while (and (not found) (re-search-backward regexp nil t))
			  (setq pos (match-beginning 0)
				ibut (ibut:at-p)
				at-lbl-key (hattr:get ibut 'lbl-key)
				found (equal at-lbl-key lbl-key))))
		      (when found
			(goto-char pos)
			ibut))))
		lbl-key))

(defun    ibut:at-to-name-p (&optional ibut)
  "If point is on an implicit button, optional IBUT, move to the start of its name.
If name is found, leave point after its opening delimiter and set the name
and lbl-key properties of IBUT.  Return t if name is found, else nil."
  (let ((opoint (point))
	move-flag
	name
	start)
    (when (or (ibut:is-p ibut)
	      (setq ibut (ibut:at-p)))
      (setq start (hattr:get ibut 'lbl-start))
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
	    (at-lbl-key (ibut:label-p nil "\"" "\"" nil t))
	    ibut)
       (cond ((or (and at-name (equal at-name lbl-key))
		  (and lbl-key (equal at-lbl-key lbl-key)))
	      (setq ibut 'hbut:current))
	     ((and lbl-key (setq ibut (ibut:to lbl-key)))))
       (when (not (hbut:outside-comment-p))
	 ;; Skip past any optional name and separators
	 (cond (name-start
		(goto-char name-start)
		(skip-chars-forward (regexp-quote ibut:label-start)))
	       ((ibut:at-to-name-p ibut))))
       ibut))
   lbl-key))

(defun    ibut:to-text (lbl-key)
  "Move to the text of the nearest implicit button matching LBL-KEY.
Find the nearest implicit button with LBL-KEY (a label or label
key) within the visible portion of the current buffer and move to
within its button text.  This will find an implicit button if
point is within its name or text or if LBL-KEY is a name/name-key
of an existing implicit button.  It will not find other unnamed
implicit buttons.

Return the symbol for the button if found, else nil."
  (unless lbl-key
    (setq lbl-key (ibut:label-p nil nil nil nil t)))
  (hbut:funcall
   (lambda (lbl-key _buffer _key-src)
     (let* ((name-start-end (ibut:label-p t nil nil t t))
	    (name-end (nth 2 name-start-end))
	    (at-name (car name-start-end))
	    (at-lbl-key (ibut:label-p nil "\"" "\"" nil t))
	    (opoint (point))
	    move-flag
	    start
	    ibut)
       ;; Do not move point if it is already in the text of an
       ;; implicit button matching LBL-KEY.  If on the name of
       ;; the same button, move into the text of the button.
       (cond ((and lbl-key (equal at-lbl-key lbl-key))
	      (setq ibut 'hbut:current))
	     ((and at-name (equal (ibut:label-to-key at-name) lbl-key))
	      (setq ibut 'hbut:current
		    move-flag t))
	     ((and lbl-key (setq ibut (ibut:to lbl-key)))
	      (setq move-flag t)))
       (when (and move-flag ibut (not (hbut:outside-comment-p)))
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
   lbl-key))

;;; ------------------------------------------------------------------------
(defconst ibut:label-start "<["
  "String matching the start of a Hyperbole implicit button label.")
(defconst ibut:label-end   "]>"
  "String matching the end of a Hyperbole implicit button label.")

(defvar   ibut:label-separator " "
  "String inserted immediately after a newly created implicit button name.
This separates it from the implicit button text.  See also
`ibut:label-separator-regexp' for all valid characters that may
manually inserted to separate an implicit button label from its
text.")

(defvar   ibut:label-separator-regexp "\\s-*[-:=]*\\s-+"
  "Regular expression that separates an implicit button name from its button text.")

;;; ========================================================================
;;; ibtype class - Implicit button types
;;; ========================================================================

(defmacro defib (type _params doc at-p &optional to-p style)
  "Create Hyperbole implicit button TYPE with PARAMS, described by DOC.
TYPE is an unquoted symbol.  PARAMS are presently ignored.

AT-P is a boolean form of no arguments which determines whether or not point
is within a button of this type and if it is, calls `hact' with an
action to be performed whenever a button of this type is activated.

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
    (let ((to-func (when to-p (action:create nil (list to-p))))
	  (at-func (list at-p)))
      `(progn (symtable:add ',type symtable:ibtypes)
	      (htype:create ,type ibtypes ,doc nil ,at-func
			    '(to-p ,to-func style ,style))))))

(put      'defib 'lisp-indent-function 'defun)

;; Support edebug-defun for interactive debugging of ibtypes
(def-edebug-spec defib
 (&define name lambda-list
          [&optional stringp]   ; Match the doc string, if present.
          def-body))

(def-edebug-spec lambda-list
 (([&rest arg]
   [&optional ["&optional" arg &rest arg]]
   &optional ["&rest" arg])))

(defalias 'ibtype:create #'defib)

(defun ibtype:activate-link (referent)
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
  "Create an implicit button link type.
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
		     (ibtype:activate-link referent)))))))
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
  "Create an action button link TYPE (an unquoted symbol).
The buttons look like: <TYPE link-text> where link-text is
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
      (make-symbol (substring name (match-end 0))))))

(defun    ibtype:delete (type)
  "Delete an implicit button TYPE (a symbol).
Return TYPE's symbol if it existed, else nil."
  (symtable:delete type symtable:ibtypes)
  (htype:delete type 'ibtypes))

(provide 'hbut)

;;; hbut.el ends here
