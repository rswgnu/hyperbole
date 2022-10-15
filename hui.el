;;; hui.el --- GNU Hyperbole button and hyperlink user interface  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Sep-91 at 21:42:03
;; Last-Mod:      7-Oct-22 at 23:36:14 by Mats Lidell
;;
;; Copyright (C) 1991-2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hargs)
(require 'set)
(require 'hmail)
(require 'hbut)
(eval-when-compile (require 'hactypes))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function texinfo-copy-node-name "texnfo-upd")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hui:hbut-delete-confirm-flag t
  "*Non-nil means prompt before interactively deleting explicit buttons."
  :type 'boolean
  :group 'hyperbole-buttons)

(defcustom hui:ebut-prompt-for-action nil
  "*Non-nil prompts for a button-specific action on explicit button creation."
  :type 'boolean
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Public Commands Bound to Keys
;;; ************************************************************************

;; Derived from copy-to-register of "register.el"
;;;###autoload
(defun hui-copy-to-register (register start end &optional delete-flag region-flag)
  "Copy region or thing into REGISTER.  With prefix arg, delete as well.
Called from program, takes five args: REGISTER, START, END, DELETE-FLAG,
and REGION-FLAG.  START and END are buffer positions indicating what to copy.
The optional argument REGION-FLAG if non-nil, indicates that we're not just
copying some text between START and END, but we're copying the region.

Interactively, reads the register using `register-read-with-preview'.

If called interactively, `transient-mark-mode' is non-nil, and
there is no active region, copy any delimited selectable thing at
point; see `hui:delimited-selectable-thing'."
  (interactive (list (register-read-with-preview "Copy to register: ")
		     (when mark-active (region-beginning))
		     (when mark-active (region-end))
		     current-prefix-arg
		     t))
  (let (thing-and-bounds
	thing
	str)
    (prog1 (setq str
		 ;; If called interactively, transient-mark-mode is
		 ;; enabled, and no region is active, copy thing at
		 ;; point, current kcell ref when in kotl-mode or
		 ;; button if on an ibut or ebut.
		 (cond ((and (called-interactively-p 'interactive)
			     transient-mark-mode
			     (not (use-region-p))
                             (or (ebut:label-p) (ibut:label-p)))
                        (hui-register-struct-at-point))
                       ((and (called-interactively-p 'interactive)
			     transient-mark-mode
			     (not (use-region-p))
			     (prog1 (setq thing-and-bounds (hui:delimited-selectable-thing-and-bounds)
					  start (nth 1 thing-and-bounds)
					  end   (nth 2 thing-and-bounds)
					  thing (nth 0 thing-and-bounds))
			       (when (and delete-flag start end)
				 (delete-region start end))))
			thing)
		       ((and start end region-flag)
			(funcall region-extract-function delete-flag))
		       ((and start end)
			(filter-buffer-substring start end delete-flag))
		       (t ;; no region
			(signal 'mark-inactive nil))))
      (set-register register str)
      (setq deactivate-mark t)
      (cond (delete-flag)
	    ((called-interactively-p 'interactive)
	     (if thing
		 (message "Saved selectable thing: %s" thing)
	       (indicate-copied-region)))))))

;; Override the {M-w} command from "simple.el" when hyperbole-mode is active
;; to allow copying kcell references or regions.
;;;###autoload
(defun hui-kill-ring-save (beg end &optional region)
  "Save the active region as if killed, but don't kill it.
In Transient Mark mode, deactivate the mark.
If `interprogram-cut-function' is non-nil, also save the text for a window
system cut and paste.

If called interactively, `transient-mark-mode' is non-nil, and
there is no active region, copy any delimited selectable thing at
point; see `hui:delimited-selectable-thing'.

If you want to append the killed region to the last killed text,
use \\[append-next-kill] before \\[kill-ring-save].

The copied text is filtered by `filter-buffer-substring' before it is
saved in the kill ring, so the actual saved text might be different
from what was in the buffer.

When called from Lisp, save in the kill ring the stretch of text
between BEG and END, unless the optional argument REGION is
non-nil, in which case ignore BEG and END, and save the current
region instead.

This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied."
  ;; Pass mark first, then point, because the order matters when
  ;; calling `kill-append'.
  (interactive (list (when mark-active (region-beginning))
		     (when mark-active (region-end))
		     (prefix-numeric-value current-prefix-arg)))
  (let (thing)
    (if (or (use-region-p)
	    (null transient-mark-mode)
	    (not (called-interactively-p 'interactive)))
	(copy-region-as-kill beg end region)
      (setq thing (hui:delimited-selectable-thing))
      (if (stringp thing)
	  (progn (kill-new thing)
		 (setq deactivate-mark t))
	(copy-region-as-kill beg end region)))
    ;; This use of called-interactively-p is correct because the code it
    ;; controls just gives the user visual feedback.
    (when (called-interactively-p 'interactive)
      (if thing
	  (message "Saved selectable thing: %s" thing)
	(indicate-copied-region)))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hui:global-bind-key (cmd &optional new-key)
  "Remove existing global key binding for CMD, rebind it to optional NEW-KEY.
If NEW-KEY is not provided, prompt for it.  Display a message confirming
the binding."
  (interactive "CCommand to change key binding of: \nKNew key to bind: ")
  (if (not (functionp cmd))
      (error "(hui:global-bind-key): Invalid command, `%s'" cmd))
  (let* ((old-key (where-is-internal cmd (current-global-map) t))
	 ;; Force multi-character key sequences to echo in the minibuffer
	 (echo-keystrokes 1)
	 old-key-text
	 new-key-text)
    (when old-key (setq old-key-text (key-description old-key)))
    (when (null new-key)
      (setq new-key
	    (with-selected-window (minibuffer-window)
	      (read-key-sequence
	       (if old-key
		   (format "{%s} runs `%s'; change it to key: " old-key-text cmd)
		 (format "New key to run `%s': " cmd))))))
    (cond ((equal new-key (kbd "\C-g"))
	   (keyboard-quit))
	  (new-key (global-set-key new-key cmd)
		   (setq new-key-text (key-description new-key))))
    (if old-key
	(progn (global-unset-key old-key)
	       (message "{%s} now runs `%s'; prior global {%s} binding removed" new-key-text cmd old-key-text))
      (message "{%s} now runs `%s'" new-key-text cmd))))

(defun hui:bind-key (cmd &optional new-key)
  "Remove existing Hyperbole key binding for CMD, rebind it to optional NEW-KEY.
If NEW-KEY is not provided, prompt for it.  Display a message confirming the
binding."
  (interactive "CCommand to change key binding of: \nKNew key to bind: ")
  (if (not (functionp cmd))
      (error "(hui:bind-key): Invalid command, `%s'" cmd))
  (let* ((old-key (where-is-internal cmd hyperbole-mode-map t))
	 ;; Force multi-character key sequences to echo in the minibuffer
	 (echo-keystrokes 1)
	 old-key-text
	 new-key-text)
    (when old-key
      (setq old-key-text (key-description old-key)))
    (when (null new-key)
      (setq new-key
	    (with-selected-window (minibuffer-window)
	      (read-key-sequence
	       (if old-key
		   (format "{%s} runs `%s'; change it to key: " old-key-text cmd)
		 (format "New key to run `%s': " cmd))))))
    (cond ((equal new-key (kbd "\C-g"))
	   (keyboard-quit))
	  (new-key (define-key hyperbole-mode-map new-key cmd)
		   (setq new-key-text (key-description new-key))))
    (if old-key
	(progn (define-key hyperbole-mode-map old-key nil)
	       (message "{%s} now runs `%s'; prior Hyperbole {%s} binding removed" new-key-text cmd old-key-text))
      (message "{%s} now runs `%s'" new-key-text cmd))))

(defun hui:delimited-selectable-thing ()
  "Return any delimited selectable thing at point as a string or nil if none.

With point:
  in a Koutline klink, copy the klink;
  in a Koutline cell, outside any klink,
    copy a klink reference to the current cell;
  on a Hyperbole button, copy the text of the button excluding delimiters;
  at the start of a paired delimiter,
    copy the text including the delimiters."
  (cond ((klink:absolute (klink:at-p)))
	((derived-mode-p 'kotl-mode)
	 (kcell-view:absolute-reference))
	((let* ((hbut (hbut:at-p))
		(start (when hbut (hattr:get hbut 'lbl-start)))
		(end (when hbut (hattr:get hbut 'lbl-end))))
	   (and start end
		(buffer-substring-no-properties start end))))
	((hui-select-at-delimited-thing-p)
	 (hui-select-get-thing))))

(defun hui:delimited-selectable-thing-and-bounds ()
  "Return a list of any delimited selectable thing at point.
The list is (<string> <start position of thing> <end position of thing>)
or nil if none.  Start and end may be nil if thing was
generated rather than extracted from a region."
  (let (thing-and-bounds thing start end)
    (cond ((setq thing-and-bounds (klink:at-p))
	   (when thing-and-bounds
	     (setcar thing-and-bounds (klink:absolute thing-and-bounds))
	     thing-and-bounds))
	  ((derived-mode-p 'kotl-mode)
	   (list (kcell-view:absolute-reference)))
	  ((setq thing (hbut:at-p)
		 start (when thing (hattr:get thing 'lbl-start))
		 end (when thing (hattr:get thing 'lbl-end)))
	   (and start end
		(list (buffer-substring-no-properties start end) start end)))
	  ((hui-select-at-delimited-thing-p)
	   (when (setq thing-and-bounds (hui-select-get-region-boundaries))
	     (list (buffer-substring-no-properties (car thing-and-bounds) (cdr thing-and-bounds))
		   (car thing-and-bounds)
		   (cdr thing-and-bounds)))))))

(defun hui:ebut-act (&optional but)
  "Activate optional explicit button symbol BUT in current buffer.
Default is the current button."
  (interactive
   (let ((but (ebut:at-p)) (lst))
     (list
      (cond (but)
	    ((setq lst (ebut:alist))
	     (ebut:get (ebut:label-to-key
			(hargs:read-match "Activate explicit button: " lst nil t
					  (ebut:label-p 'as-label) 'ebut))))
	    (t
	     (hypb:error "(ebut-act): No explicit buttons in buffer."))))))
  (hui:hbut-operate #'ebut:act "Activate explicit button: " but))

(defun hui:ebut-create (&optional start end)
  "Interactively create an explicit Hyperbole button.
Use any label between optional START and END region points.
Indicate button creation by delimiting and adding any necessary
instance number to the button label.

For programmatic creation, use `ebut:program' instead."
  (interactive (list (when (use-region-p) (region-beginning))
		     (when (use-region-p) (region-end))))
  (hypb:assert-same-start-and-end-buffer
    (let ((default-lbl) lbl but-buf actype)
      (save-excursion
	(setq default-lbl (hui:hbut-label-default start end (not (called-interactively-p 'interactive)))
	      lbl (hui:hbut-label default-lbl "ebut-create"))
	(unless (equal lbl default-lbl)
	  (setq default-lbl nil))

	(setq but-buf (if default-lbl (current-buffer) (hui:ebut-buf)))
	(hui:buf-writable-err but-buf "ebut-create")

	(hattr:set 'hbut:current 'loc (hui:key-src but-buf))
	(hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
	(setq actype (hui:actype))
	(hattr:set 'hbut:current 'actype actype)
	(hattr:set 'hbut:current 'args (hargs:actype-get actype))
	(hattr:set 'hbut:current 'action
		   (and hui:ebut-prompt-for-action (hui:action actype)))
	;; Adds instance number to in-buffer label if necessary
	(ebut:operate lbl nil)
	(when (called-interactively-p 'interactive)
	  (hui:ebut-message nil))))))

(defun hui:ebut-delete (but-key &optional key-src)
  "Delete explicit Hyperbole button given by BUT-KEY in optional KEY-SRC.
KEY-SRC may be a buffer or a pathname, when nil the current buffer is used.
Return t if button is deleted, nil if user chooses not to delete or signal
an error otherwise.  If called interactively, prompt user whether to delete
and derive BUT-KEY from the button that point is within.
Signal an error if point is not within a button."
  (interactive (list (when (ebut:at-p)
		       (hattr:get 'hbut:current 'lbl-key))))
  (cond ((null but-key)
	 (hypb:error
	  "(ebut-delete): Point is not over the label of an existing button"))
	((not (stringp but-key))
	 (hypb:error
	  "(ebut-delete): Invalid label key argument: '%s'" but-key)))
  (let ((interactive (called-interactively-p 'interactive)))
    (if (and hui:hbut-delete-confirm-flag interactive)
	(if (y-or-n-p (format "Delete button %s%s%s? "
			      ebut:start
			      (hbut:key-to-label but-key) ebut:end))
	    (hui:ebut-delete-op interactive but-key key-src)
	  (message "")
	  nil)
      (hui:ebut-delete-op interactive but-key key-src))))

(defun hui:ebut-edit-region ()
  "Create or edit an explicit Hyperbole button when conditions are met.
A region must have been delimited with the action-key and point must now be
within it before this function is called or it will do nothing.  The region
must be no larger than the size given by a call to (hbut:max-len).  It must
be entirely within or entirely outside of an existing explicit button.  When
region is within the button, the button is interactively edited.  Otherwise,
a new button is created interactively with the region as the default label."
  (interactive)
  (let ((m (mark))
	(op action-key-depress-prev-point) (p (point)) (lbl-key))
    (if (and m (eq (marker-buffer m) (marker-buffer op))
	     (< op m) (<= (- m op) (hbut:max-len))
	     (<= p m) (<= op p))
	(progn
	  (if (setq lbl-key (ebut:label-p))
	      (hui:ebut-edit lbl-key)
	    (hui:ebut-create op m))
	  t))))

(defun hui:ebut-edit (lbl-key)
  "Edit an explicit Hyperbole button given by LBL-KEY.
Signal an error when no such button is found in the current buffer."
  (interactive (list (save-excursion
		       (hui:buf-writable-err (current-buffer) "ebut-edit")
		       (ebut:label-to-key
			(hargs:read-match "Button to edit: "
					  (ebut:alist) nil t
					  (ebut:label-p t) 'ebut)))))
  (unless (stringp lbl-key)
    (if (called-interactively-p 'interactive)
	(error "(hui:ebut-edit): No explicit button to edit")
      (error "(hui:ebut-edit): 'lbl-key' argument must be a string, not '%s'" lbl-key)))

  (hypb:assert-same-start-and-end-buffer
    (let ((lbl (ebut:key-to-label lbl-key))
	  (but-buf (current-buffer))
	  actype but new-lbl)
      (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
      (hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
      (save-excursion
	(unless (called-interactively-p 'interactive)
	  (hui:buf-writable-err but-buf "ebut-edit"))

	(unless (setq but (ebut:get lbl-key but-buf))
	  (pop-to-buffer but-buf)
	  (hypb:error "(ebut-edit): Invalid button, no data for '%s'" lbl))

	(setq new-lbl
	      (hargs:read
	       "Change button label to: "
	       (lambda (lbl)
		 (and (not (string-equal lbl "")) (<= (length lbl) (hbut:max-len))))
	       lbl
	       (format "(ebut-edit): Enter a string of at most %s chars."
		       (hbut:max-len))
	       'string))

	(setq actype (hui:actype (hattr:get but 'actype)))
	(hattr:set 'hbut:current 'actype actype)
	(hattr:set 'hbut:current 'args (hargs:actype-get actype t))
	(hattr:set 'hbut:current 'action
		   (and hui:ebut-prompt-for-action (hui:action actype)))
	(set-buffer but-buf)
	(save-excursion
	  (ebut:operate lbl new-lbl)))
      (when (called-interactively-p 'interactive)
	(hui:ebut-message t)))))

(defun hui:ebut-rename (curr-label new-label)
  "Rename explicit Hyperbole button given by CURR-LABEL to NEW-LABEL.
If called interactively when point is within an explicit button:
   save button label and tell user to: 1. edit label and 2. invoke this
   same command again.  The second invocation changes the button's name
   from the stored value to the new value.
If called interactively when point is not within an explicit button:
   prompt for old and new button label values and perform the rename.
Signal an error if any problem occurs."
  (interactive
   (save-excursion
     (let (curr-label new-label)
       (hui:buf-writable-err (current-buffer) "ebut-rename")
       (if hui:ebut-label-prev
	   (setq curr-label hui:ebut-label-prev
		 new-label (ebut:label-p 'as-label))
	 (setq new-label nil
	       curr-label
	       (or (ebut:label-p 'as-label)
		   (let ((buts (ebut:alist)))
		     (if (null buts)
			 (hypb:error "(ebut-rename): No explicit buttons in buffer")
		       (prog1 (hargs:read-match
			       "Button label to rename: "
			       buts nil t nil 'ebut)
			 (setq new-label
			       (hargs:read
				"Rename button label to: "
			        (lambda (lbl)
				  (and (not (string-equal lbl ""))
				       (<= (length lbl) (hbut:max-len))))
				curr-label
				(format
				 "(ebut-rename): Use a quoted string of at most %s chars."
				 (hbut:max-len))
				'string))))))))
       (list curr-label new-label))))

  (save-excursion
    (unless (called-interactively-p 'interactive)
      (hui:buf-writable-err (current-buffer) "ebut-rename")
      (if (or (not (stringp curr-label)) (string-equal curr-label ""))
	  (hypb:error "(ebut-rename): 'curr-label' must be a non-empty string: %s"
		      curr-label))
      (and (stringp new-label) (string-equal new-label "")
	   (hypb:error "(ebut-rename): 'new-label' must be a non-empty string: %s"
		       new-label)))
    (or (ebut:get (ebut:label-to-key curr-label))
	(hypb:error "(ebut-rename): Can't rename %s since no button data"
		    curr-label)))
  (cond (new-label
	 (ebut:operate curr-label new-label)
	 (setq hui:ebut-label-prev nil)
	 (message "Renamed from '%s' to '%s'." curr-label new-label))
	(curr-label
	 (setq hui:ebut-label-prev curr-label)
	 (message "Edit button label and use same command to finish rename."))
	(t (hypb:error "(ebut-rename): Move point to within a button label"))))

(defun hui:ebut-search (string &optional match-part)
  "Show lines of files/buffers containing an explicit but match for STRING.
Return number of buttons matched and displayed.
By default, find only matches for whole button labels; optional MATCH-PART
enables partial matches.  Show matched lines in a buffer which serves as
a menu to find any of the occurrences."
  (interactive (list (read-string "Search for button string: ")
		     (y-or-n-p "Enable partial matches? ")))
  (if (not (stringp string))
      (hypb:error "(ebut-search): String to search for is required"))
  (let*  ((prefix (if (> (length string) 14)
		      (substring string 0 13) string))
	  (out-buf (get-buffer-create (concat "*" prefix " Hypb Search*")))
	  (total (ebut:search string out-buf match-part)))
    (if (> total 0)
	(progn
	  (set-buffer out-buf)
	  (moccur-mode)
	  (if (fboundp 'outline-minor-mode)
	      (and (progn (goto-char 1)
			  (search-forward "\C-m" nil t))
		   (outline-minor-mode 1)))
	  (if (fboundp 'hproperty:but-create)
	      (hproperty:but-create nil nil (regexp-quote
					     (if match-part string
					       (concat ebut:start string ebut:end)))))
	  (goto-char (point-min))
	  (pop-to-buffer out-buf)
	  (if (called-interactively-p 'interactive) (message "%d match%s." total
							     (if (> total 1) "es" ""))
	    total))
      (if (called-interactively-p 'interactive) (message "No matches.")
	total))))

(defun hui:gbut-create (lbl ibut-flag)
  "Create a Hyperbole global explicit button with LBL.

With prefix arg IBUT-FLAG non-nil, create a global implicit button instead.
See `hui:gibut-create' for details."
  (interactive (list (unless current-prefix-arg
                       (read-string "Create global explicit button labeled: "))
                     current-prefix-arg))
  (if ibut-flag
      (call-interactively #'hui:gibut-create)
    (hypb:assert-same-start-and-end-buffer
      (let (actype
            but-buf
            src-dir)
	(save-excursion
	  (setq src-dir default-directory
		actype (hui:actype)
		but-buf (find-file-noselect (gbut:file)))
	  (set-buffer but-buf)
          (hui:buf-writable-err (current-buffer) "gbut-create")
          ;; This prevents movement of point which might be useful to user.
          (save-excursion
	    (goto-char (point-max))
            (unless (bolp)
	      (insert "\n"))
	    ;; loc = Directory of the global button file
	    (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
	    ;; dir = default-directory of current buffer at the start of
	    ;; this `hui:gbut-create' function call (when button is created)
	    (hattr:set 'hbut:current 'dir src-dir)
	    (hattr:set 'hbut:current 'actype actype)
	    (hattr:set 'hbut:current 'args (hargs:actype-get actype))
	    (hattr:set 'hbut:current 'action (when hui:ebut-prompt-for-action
					       (hui:action actype)))
	    ;; Ensure ebut:operate is given but-buf as the current buffer
	    (set-buffer but-buf)
	    (setq lbl (concat lbl (ebut:operate lbl nil)))
	    (goto-char (point-max))
	    (insert "\n")
	    (save-buffer))
          (message "`%s' global explicit button created." lbl))))))

(defun hui:gbut-delete (but-key)
  "Delete global Hyperbole button given by BUT-KEY.
Return t if button is deleted, nil if user chooses not to delete or signal
an error otherwise.  If called interactively, prompt user whether to delete
and derive BUT-KEY from the button that point is within.
Signal an error if point is not within a button."
  (interactive (list (save-excursion
		       (hui:buf-writable-err
			(find-file-noselect (gbut:file)) "gbut-delete")
		       (hbut:label-to-key
			(hargs:read-match "Global button to delete: "
					  (mapcar #'list (gbut:label-list))
					  nil t nil 'gbut)))))
  (hui:hbut-delete but-key (gbut:file)))

(defun hui:gbut-edit (lbl-key)
  "Edit a global Hyperbole button given by LBL-KEY.
The button may be explicit or a labeled implicit button.
When called interactively, save the global button buffer after the
modification   Signal an error when no such button is found."
  (interactive (list (save-excursion
		       (hui:buf-writable-err
			(find-file-noselect (gbut:file)) "gbut-edit")
		       (hbut:label-to-key
			(hargs:read-match "Global button to edit: "
					  (mapcar #'list (gbut:label-list))
					  nil t (gbut:label-p t) 'gbut)))))
  (unless (stringp lbl-key)
    (if (called-interactively-p 'interactive)
	(error "(hui:gbut-edit): No global button to edit")
      (error "(hui:gbut-edit): 'lbl-key' argument must be a string, not '%s'" lbl-key)))


  (hypb:assert-same-start-and-end-buffer
    (let ((lbl (hbut:key-to-label lbl-key))
          (interactive-flag (called-interactively-p 'interactive))
	  (but-buf (find-file-noselect (gbut:file)))
	  (src-dir (file-name-directory (gbut:file)))
	  actype but new-lbl)
      (save-excursion
	(unless interactive-flag
	  (hui:buf-writable-err but-buf "gbut-edit"))

	(unless (setq but (gbut:get lbl-key))
	  (pop-to-buffer but-buf)
	  (hypb:error "(gbut-edit): Invalid button, no data for '%s'" lbl))

	(setq new-lbl
	      (hargs:read
	       "Change global button label to: "
	       (lambda (lbl)
		 (and (not (string-equal lbl "")) (<= (length lbl) (hbut:max-len))))
	       lbl
	       (format "(gbut-edit): Enter a string of at most %s chars."
		       (hbut:max-len))
	       'string))

	(if (eq (hattr:get but 'categ) 'explicit)
	    (progn
	      ;; Explicit buttons
	      (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
	      (hattr:set 'hbut:current 'dir src-dir)
	      (setq actype (hui:actype (hattr:get but 'actype)))
	      (hattr:set 'hbut:current 'actype actype)
	      (hattr:set 'hbut:current 'args (hargs:actype-get actype t))
	      (hattr:set 'hbut:current 'action
			 (and hui:ebut-prompt-for-action (hui:action actype)))
	      ;; Ensure ebut:operate is given but-buf as the current buffer
	      (set-buffer but-buf)
	      (save-excursion
		(ebut:operate lbl new-lbl))
              (when interactive-flag
		(save-buffer)))
	  ;; Implicit buttons
	  (with-current-buffer but-buf
	    (save-excursion
              (ibut:to lbl-key)
	      (if (and interactive-flag (ibut:at-p))
		  (progn
                    ;; lbl-start and lbl-end mark the text of the ibut, not
                    ;; its name.
	            (when (hattr:get 'hbut:current 'lbl-end)
                      (let* ((start (hattr:get 'hbut:current 'lbl-start))
                             (end (hattr:get 'hbut:current 'lbl-end))
                             (old-text (buffer-substring start end))
                             (new-text (read-string "Edit ibut text: " old-text)))
			(save-excursion
			  (goto-char start)
			  (delete-region start end)
			  (insert new-text))
			(hattr:set 'hbut:current 'lbl-key (ibut:label-to-key new-lbl))))
                    ;; Have to do name change after lbl-start/lbl-end are
                    ;; used so buffer positions do not change.
	            (ibut:rename lbl new-lbl)
                    (save-buffer)
	            (hui:ibut-message t))
		(when (and interactive-flag
			   (ibut:rename lbl new-lbl))
		  (save-buffer)
		  (message "Button renamed to %s%s%s"
			   ibut:label-start
			   new-lbl
			   ibut:label-end))))))))))

(defun hui:gbut-rename (label)
  "Interactively rename a Hyperbole global button with LABEL.
When in the global button buffer, the default is the button at point."
  (interactive (list (save-excursion
		       (hui:buf-writable-err
			(find-file-noselect (gbut:file)) "gbut-rename")
		       (hbut:label-to-key
			(hargs:read-match "Global button to rename: "
					  (mapcar #'list (gbut:label-list))
					  nil t nil 'gbut)))))
  (hbut:rename (gbut:to label)))

(defun hui:gibut-create (lbl text)
  "Create a Hyperbole global implicit button with LBL and button TEXT.

Use `hui:gbut-create' to create a global explicit button."
  (interactive "sCreate global implicit button labeled: \nsButton text (with any delimiters): ")
  (let (but-buf
	opoint
        delimited-label)
    (save-excursion
      (setq delimited-label (concat ibut:label-start lbl ibut:label-end)
	    but-buf (hpath:find-noselect (gbut:file)))
      (hui:buf-writable-err but-buf "gibut-create")
      ;; This prevents movement of point which might be useful to user.
      (set-buffer but-buf)
      (save-excursion
	(goto-char (point-max))
        (unless (bolp)
	  (insert "\n"))
	(setq opoint (point))
        (insert delimited-label ": " text "\n")
	(save-excursion
	  (goto-char (+ opoint (length ibut:label-start)))
	  (ibut:create))
	(save-buffer))
      (message "`%s' global implicit button created." lbl))))

(defun hui:hbut-act (&optional but)
  "Execute action for optional Hyperbole button symbol BUT in current buffer.
The default is the current button."
  (interactive (list (hbut:get (hargs:read-match "Activate labeled Hyperbole button: "
						 (nconc (ebut:alist) (ibut:alist))
						 nil t nil 'hbut))))
  (hui:hbut-operate #'hbut:act "Activate Hyperbole button: " but))

(defun hui:hbut-current-act ()
  "Activate Hyperbole button at point or signal an error if there is none."
  (interactive)
  (let ((but (hbut:at-p)))
    (cond ((null but)
	   (hypb:error "(hbut-act): No current button to activate"))
	  ((not (hbut:is-p but))
	   (hypb:error "(hbut-act): Button is invalid; it has no attributes"))
	  (t (hui:but-flash) (hbut:act but)))))

(defun hui:hbut-delete (&optional but-key key-src)
  "Delete a Hyperbole button given by optional BUT-KEY in optional KEY-SRC.
Use current buffer if no KEY-SRC is given.  Return t if button
is deleted, nil if user chooses not to delete, or signal an error
otherwise.  If called interactively, prompt user for whether to
delete and derive BUT-KEY from the button that point is within.
Signal an error if point is not within a button."
  (interactive)
  (when (and (null but-key) (hbut:at-p))
    (setq but-key (hattr:get 'hbut:current 'lbl-key)))
  (unless key-src
    (setq key-src (or buffer-file-name (current-buffer))))
  (cond ((null but-key)
	 (hypb:error
	  "(hbut-delete): Point is not over the label of an existing button"))
	((not (stringp but-key))
	 (hypb:error
	  "(hbut-delete): Invalid label key argument: '%s'" but-key)))
  (save-excursion
    (with-current-buffer (if (bufferp key-src) key-src (find-file-noselect key-src))
      (let ((interactive (called-interactively-p 'interactive))
	    (label (hbut:key-to-label but-key)))
	(cond ((ebut:to but-key)
	       (if (and hui:hbut-delete-confirm-flag interactive)
		   (if (y-or-n-p (format "Delete button %s%s%s? "
					 ebut:start label ebut:end))
		       (hui:ebut-delete-op interactive but-key key-src)
		     (message "")
		     nil)
		 (hui:ebut-delete-op interactive but-key key-src)))
	      ((ibut:to but-key)
	       (if (and hui:hbut-delete-confirm-flag interactive)
		   (if (y-or-n-p (format "Delete button %s%s%s? "
					 ibut:label-start label ibut:label-end))
		       (hui:ibut-delete-op interactive but-key key-src)
		     (message "")
		     nil)
		 (hui:ibut-delete-op interactive but-key key-src)))
	      (t (hypb:error "(hbut-delete): Invalid button '%s'" label)))))))

(defun hui:hbut-help (&optional but)
  "Check for and explain an optional button given by symbol, BUT.
BUT defaults to the button whose label point is within."
  (interactive)
  (setq but (or but (hbut:at-p)
		(ebut:get (ebut:label-to-key
			   (hargs:read-match "Help for button: "
					     (ebut:alist) nil t nil 'ebut)))))
  (unless but
    (hypb:error "(hbut-help):  Move point to a valid Hyperbole button"))
  (unless (hbut:is-p but)
    (cond (but (hypb:error "(hbut-help): Invalid button"))
	  (t   (hypb:error
		"(hbut-help): Not on an implicit button and no buffer explicit buttons"))))
  (let ((type-help-func (intern-soft
			 (concat
			  (htype:names 'ibtypes (hattr:get but 'categ))
			  ":help"))))
    (unless (equal (hypb:indirect-function 'hui:but-flash)
		   (lambda nil))
      ;; Only flash button if point is on it.
      (let ((lbl-key (hattr:get but 'lbl-key))
	    lbl-start
	    lbl-end)
	(and lbl-key
	     (or (equal lbl-key (ebut:label-p))
		 ;; Matches only ibuts with named labels
		 (equal lbl-key (ibut:label-p))
		 ;; If ibut text region specified, check that.
		 (progn
		   (setq lbl-start (hattr:get but 'lbl-start)
			 lbl-end   (hattr:get but 'lbl-end))
		   (when (and lbl-start lbl-end)
		     (equal lbl-key
			    (buffer-substring-no-properties lbl-start lbl-end)))))
	     (hui:but-flash))))
    (if (functionp type-help-func)
	(funcall type-help-func but)
      (let ((total (hbut:report but)))
	(when total (hui:help-ebut-highlight))))))

(defun hui:hbut-label (default-label func-name &optional prompt)
  "Read button label from user using DEFAULT-LABEL and caller's FUNC-NAME.
Optional PROMPT string replaces the standard prompt of 'Button label: '."
  (hargs:read (if (stringp prompt) prompt "Button label: ")
	      (lambda (lbl)
		(and (not (string-equal lbl "")) (<= (length lbl) (hbut:max-len))))
	      default-label
	      (format "(%s): Enter a string of at most %s chars."
		      func-name (hbut:max-len))
	      'string))

(defun hui:hbut-label-default (start end &optional skip-len-test)
  "Return default label based on START and END region markers or positions.
Optional SKIP-LEN-TEST means don't limit label to (hbut:max-len) length.
Return nil if START or END are invalid or if region fails length test.

Also has side effect of moving point to start of default label, if any."
  (when (markerp start) (setq start (marker-position start)))
  (when (markerp end) (setq end (marker-position end)))
  ;; Test whether to use region as default button label.
  (when (and (integerp start) (integerp end)
	     (or skip-len-test
		 (<= (max (- end start) (- start end)) (hbut:max-len))))
    (goto-char start)
    (buffer-substring-no-properties start end)))

(defun hui:hbut-rename ()
  "Interactively rename a Hyperbole button from the current buffer.
The default is the button at point."
  (cond ((ebut:at-p)
         (call-interactively #'hui:ebut-rename))
        ((ibut:at-p)
         (call-interactively #'hui:ibut-rename))
        (t
         (hui:hbut-operate #'hbut:rename "Rename Hyperbole button: "))))

(defun hui:hbut-report (&optional arg)
  "Pretty print attributes of current button, using optional prefix ARG.
See `hbut:report'."
  (interactive "P")
  (if (and arg (symbolp arg))
      (hui:hbut-help arg)
    (let ((total (hbut:report arg)))
      (when total
	(hui:help-ebut-highlight)
	(message "%d button%s." total (if (/= total 1) "s" ""))))))

(defalias 'hui:hbut-summarize #'hui:hbut-report)

(defun hui:ibut-act (&optional but)
  "Activate optional labeled implicit button symbol BUT in current buffer.
Default is any implicit button at point."
  (interactive
   (let ((but (ibut:at-p)) (lst))
     (list
      (cond (but)
	    ((setq lst (ibut:alist))
	     (ibut:get (ibut:label-to-key
			(hargs:read-match "Activate labeled implicit button: " lst nil t
					  (ibut:label-p 'as-label) 'ibut))))
	    (t
	     (hypb:error "(ibut-act): No labeled implicit buttons in buffer."))))))
  (hui:hbut-operate #'ibut:act "Activate labeled implicit button: " but))

(defun hui:ibut-edit (lbl-key)
  "Edit a named implicit Hyperbole button given by LBL-KEY.
Signal an error when no such button is found in the current buffer."
  (interactive (list (save-excursion
		       (hui:buf-writable-err (current-buffer) "ibut-edit")
		       (ibut:label-to-key
			(hargs:read-match "Button to edit: "
					  (ibut:alist) nil t
					  (ibut:label-p t) 'ibut)))))
  (unless (stringp lbl-key)
    (if (called-interactively-p 'interactive)
	(error "(hui:ibut-edit): No named implicit button to edit")
      (error "(hui:ibut-edit): 'lbl-key' argument must be a string, not '%s'" lbl-key)))

  (hypb:assert-same-start-and-end-buffer
    (let ((lbl (ibut:key-to-label lbl-key))
          (interactive-flag (called-interactively-p 'interactive))
	  (but-buf (current-buffer))
	  new-lbl)
      (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
      (hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
      (save-excursion
	(unless (called-interactively-p 'interactive)
	  (hui:buf-writable-err but-buf "ibut-edit"))

	(unless (ibut:get lbl-key but-buf)
	  (pop-to-buffer but-buf)
	  (hypb:error "(ibut-edit): Invalid button, no data for '%s'" lbl))

	(setq new-lbl
	      (hargs:read
	       "Change button name to: "
	       (lambda (lbl)
		 (and (not (string-equal lbl "")) (<= (length lbl) (hbut:max-len))))
	       lbl
	       (format "(ibut-edit): Enter a string of at most %s chars."
		       (hbut:max-len))
	       'string))

	;; Implicit buttons
	(with-current-buffer but-buf
	  (save-excursion
            (ibut:to lbl-key)
	    (if (and interactive-flag (ibut:at-p))
		(progn
                  ;; lbl-start and lbl-end mark the text of the ibut, not
                  ;; its name.
	          (when (hattr:get 'hbut:current 'lbl-end)
                    (let* ((start (hattr:get 'hbut:current 'lbl-start))
                           (end (hattr:get 'hbut:current 'lbl-end))
                           (old-text (buffer-substring start end))
                           (new-text (read-string "Edit ibut text: " old-text)))
                      (save-excursion
			(goto-char start)
			(delete-region start end)
			(insert new-text))
                      (hattr:set 'hbut:current 'lbl-key (ibut:label-to-key new-lbl))))
                  ;; Have to do name change after lbl-start/lbl-end are
                  ;; used so buffer positions do not change.
	          (ibut:rename lbl new-lbl)
                  (save-buffer)
	          (hui:ibut-message t))
              (when (and interactive-flag
			 (ibut:rename lbl new-lbl))
		(save-buffer)
		(message "Button renamed to %s%s%s"
			 ibut:label-start
			 new-lbl
			 ibut:label-end)))))))))

(defun hui:ibut-label-create ()
  "Create an implicit button label for an existing implicit button at point.
Add the label, preceding the button, and delimiters around it
plus any necessary label instance number.  Signal an error if
point is not on an implicit button or if the button already has a
label.

If the implicit button type does not specify the starting locations of
its buttons, the label is simply inserted at point."
  (interactive)
  (hui:buf-writable-err (current-buffer) "ibut-label-create")
  (let* ((ibut (ibut:at-p))
	 (ibut-start (when ibut (hattr:get 'hbut:current 'lbl-start)))
	 ;; non-nil when point is within an existing ibut label
	 (label-key-start-end (when ibut (ibut:label-p nil nil nil t t)))
	 lbl)
    (cond (label-key-start-end
	   (error "(hui:ibut-label-create): ibutton at point already has a label; try hui:ibut-rename"))
	  (ibut
	   (save-excursion
	     (when ibut-start
	       (goto-char ibut-start)
	       ;; Skip over any non-whitespace or symbol chars to move
	       ;; back past any opening delimiter
	       (skip-syntax-backward "^-_"))
	     (save-excursion
	       ;; Check if ibut has an existing preceding label
	       (skip-chars-backward "][:=<>a-zA-Z0-9#@!$%^&* -")
	       (skip-chars-forward " ")
	       (when (looking-at (concat (regexp-quote ibut:label-start) "\\s-*[:=a-zA-Z0-9#@!$%^&* -]+" (regexp-quote ibut:label-end)))
		 (error "(hui:ibut-label-create): ibutton at point already has a label; try hui:ibut-rename")))
	     (setq lbl (hui:hbut-label nil "ibut-label-create")) ; prompts for label
	     ;; !! Handle adding instance to label
	     (insert ibut:label-start lbl ibut:label-end ibut:label-separator))
	   (when (called-interactively-p 'interactive)
	     (hui:ibut-message nil)))
	  (t (error "(hui:ibut-label-create): To add a label, point must be within the text of an implicit button")))))

(defun hui:ibut-rename (lbl-key)
  "Rename a label preceding an implicit button in current buffer given by LBL-KEY.
Signal an error when no such button is found in the current buffer."
  (interactive (list (save-excursion
		       (hui:buf-writable-err (current-buffer) "ibut-rename")
		       (or (ibut:label-p)
			   (ibut:label-to-key
			    (hargs:read-match "Labeled implicit button to rename: "
					      (ibut:alist) nil t nil 'ibut))))))
  (let ((lbl (ibut:key-to-label lbl-key))
	(but-buf (current-buffer))
	new-lbl)
    (unless (called-interactively-p 'interactive)
      (hui:buf-writable-err but-buf "ibut-rename"))

    (unless (ibut:get lbl-key but-buf)
      (hypb:error "(ibut-rename): Invalid button: '%s'." lbl))

    (setq new-lbl
	  (hargs:read
	   "Change implicit button label to: "
	   (lambda (lbl)
	     (and (not (string-equal lbl "")) (<= (length lbl) (hbut:max-len))))
	   lbl
	   (format "(ibut-rename): Enter a string of at most %s chars."
		   (hbut:max-len))
	   'string))

    (save-excursion
      (ibut:rename lbl new-lbl)
      (when (and (called-interactively-p 'interactive)
		 (ibut:at-p))
	(hui:ibut-message t)))))

(defun hui:link (release-window)
  "Return a list of the selected window (where depressed) and the RELEASE-WINDOW."
  (list (selected-window) release-window))

(defun hui:link-directly (&optional depress-window release-window)
  "Create a link button at Action Key depress point, linked to release point.
With optional DEPRESS-WINDOW and RELEASE-WINDOW, use the points
from those instead.  See also documentation for
`hui:link-possible-types'."
  (interactive (hmouse-choose-windows #'hui:link))
  (let ((but-window (or depress-window action-key-depress-window))
	(referent-window (or release-window action-key-release-window (selected-window)))
	but-name but-edit link-types num-types type-and-args lbl-key but-loc but-dir)
    (select-window but-window)
    (hui:buf-writable-err (current-buffer) "link-directly")
    (if (ebut:at-p)
	(setq but-edit t
	      but-loc (hattr:get 'hbut:current 'loc)
	      but-dir (hattr:get 'hbut:current 'dir)
	      lbl-key (hattr:get 'hbut:current 'lbl-key))
      (setq but-loc (hui:key-src (current-buffer))
	    but-dir (hui:key-dir (current-buffer))
	    but-name (hui:hbut-label
		      (cond ((hmouse-prior-active-region)
			     hkey-region)
			    ((use-region-p)
			     (hui:hbut-label-default
			      (region-beginning) (region-end))))
		      "link-directly"
		      "Create button named: ")
	    lbl-key (hbut:label-to-key but-name)))
    (select-window referent-window)
    (setq link-types (hui:link-possible-types)
	  num-types (length link-types))

    ;; num-types is the number of possible link types to choose among
    (cond ((= num-types 0)
	   (error "(link-directly): No possible link type to create"))
	  ((= num-types 1)
	   (setq type-and-args (hui:list-remove-text-properties (car link-types)))
	   (hui:link-create but-edit but-window lbl-key but-loc but-dir type-and-args))
	  (t ;; more than 1
	   (let ((item)
		 type)
	     (setq type-and-args
		   (hui:menu-select
		    (cons '("Link to>")
			  (mapcar
			   (lambda (type-and-args)
			     (setq type (car type-and-args))
			     (list
			      (capitalize
			       (if (string-match
				    "^\\(link-to\\|eval\\)-"
				    (setq item (symbol-name type)))
				   (setq item (substring
					       item (match-end 0)))
				 item))
			      type-and-args
			      (documentation (symtable:actype-p type))))
			   link-types)))
		   type-and-args (hui:list-remove-text-properties type-and-args))
	     (hui:link-create
	      but-edit but-window
	      lbl-key but-loc but-dir type-and-args))))
    (hui:ebut-message but-edit)))

;;; ************************************************************************
;;; Private functions - used only within Hyperbole
;;; ************************************************************************

(defun hui:action (actype &optional prompt)
  "Prompt for and return an action to override action from ACTYPE."
  (and actype
       (let* ((act) (act-str)
	      (params (actype:params actype))
	      (params-no-keywords (actype:param-list actype))
	      (params-str (and params (concat " " (prin1-to-string params)))))
	 (while (progn
		  (while (and (setq act-str
				    (hargs:read
				     (or prompt (concat "Action" params-str
							": ")) nil nil
				     nil 'string))
			      (not (string-equal act-str ""))
			      (condition-case ()
				  (progn (setq act (read act-str)) nil)
				(error
				 (beep) (message "Invalid action syntax.")
				 (sit-for 3) t))))
		  (and (not (symbolp act))
		       params-no-keywords
		       ;; Use the weak condition that action must
		       ;; involve at least one of actype's parameters
		       ;; or else we assume the action is invalid, tell
		       ;; the user and provide another chance for entry.
		       (not (memq t
				  (mapcar
				   (lambda (param)
				     (setq param (symbol-name param))
				     (and (string-match
					   (concat "[\( \t\n\r,']"
						   (regexp-quote param)
						   "[\(\) \t\n\r\"]")
					   act-str)
					  t))
				   params-no-keywords)))))
	   (beep) (message "Action must use at least one parameter.")
	   (sit-for 3))
	 (let (head)
	   (while (cond ((listp act)
			 (and act (setq head (car act))
			      (not (memq head '(lambda defun defmacro defsubst defin)))
			      (setq act (list 'lambda params act))
			      nil  ;; terminate loop
			      ))
			((symbolp act)
			 (setq act (cons act params-no-keywords)))
			((stringp act)
			 (setq act (action:kbd-macro act 1)))
			;; Unrecognized form
			(t (setq act nil)))))
	 act)))

(defun hui:actype (&optional default-actype prompt)
  "Using optional DEFAULT-ACTYPE, PROMPT for and return a button action type.
DEFAULT-ACTYPE may be a valid symbol or symbol name."
  (when (and default-actype (symbolp default-actype))
    (setq default-actype (symbol-name default-actype)
	  default-actype (actype:def-symbol default-actype)
	  default-actype (when default-actype (symbol-name default-actype))))
  (if (or (null default-actype) (stringp default-actype))
      (let ((actype-name
	     (hargs:read-match (or prompt "Button's action type: ")
			       (nconc
				(mapcar #'list (htype:names 'actypes))
				(mapcar #'list (all-completions "" obarray)))
			       nil t default-actype 'actype)))
	(or (actype:def-symbol actype-name) (intern actype-name)))
    (hypb:error "(actype): Invalid default action type received")))

(defun hui:buf-writable-err (but-buf func-name)
  "If BUT-BUF is read-only, signal an error from FUNC-NAME."
  (let (err)
    ;; (unwritable (and buffer-file-name
    ;;		 (not (file-writable-p buffer-file-name))))
    ;; (if unwritable
    ;;     Commented error out since some people want to be able to create
    ;;     buttons within files which they have purposely marked read-only.
    ;;     (setq err
    ;;	     (format "(ebut-edit): Hyperbole lacks permission to write to '%s'."
    ;;		     (file-name-nondirectory buffer-file-name))))
    (with-current-buffer but-buf
      (when buffer-read-only
	(setq err
	      (format "(%s) Read-only error in Hyperbole button buffer '%s'.  Use {%s} to enable edits."
		      func-name (buffer-name but-buf) (hmouse-read-only-toggle-key)))))
    (when err
      (pop-to-buffer but-buf)
      (hypb:error err))))

(defvar hui:ignore-buffers-regexp "\\`\\( \\|BLANK\\'\\|\\*Pp \\|TAGS\\|*quelpa\\)"
  "When prompting for a buffer name, ignore any buffers whose names match to this.")

(defun hui:ebut-buf (&optional prompt)
  "Prompt for and return a buffer in which to place a button."
  (let ((buf-name))
    (while
	(progn
	  (setq buf-name
		(hargs:read-match
		 (or prompt "Button's buffer: ")
		 (delq nil
		       (mapcar
			;; Filter only buffer whose names start with a
			;; space, are read-only or are known not to be
			;; editable, since buttons can be
			;; in buffers without attached files now.
		        (lambda (buf)
			  (let ((b (buffer-name buf)))
			    (unless (or (string-match-p hui:ignore-buffers-regexp b)
					(buffer-local-value 'buffer-read-only buf))
			      (cons b nil))))
			(buffer-list)))
		 nil t (buffer-name) 'buffer))
	  (or (null buf-name) (equal buf-name "")))
      (beep))
    (get-buffer buf-name)))

(defun hui:ebut-delete-op (interactive but-key key-src)
  "INTERACTIVEly or not, delete explicit button given by BUT-KEY in KEY-SRC.
KEY-SRC may be a buffer or a pathname; when nil, the current
buffer is used.  Return t if button is deleted; signal an error
otherwise.  If called with INTERACTIVE non-nil, derive BUT-KEY
from the button that point is within."
  (let ((buf (current-buffer)) (ebut))
    (if (if interactive
	    (ebut:delete)
	  (cond ((or (null key-src) (and (bufferp key-src) (setq buf key-src)))
		 (setq ebut (ebut:get but-key nil key-src)))
		((and (stringp key-src)
		      (setq buf (find-file-noselect key-src)))
		 (setq ebut (ebut:get but-key buf)))
		(t (hypb:error "(ebut-delete): Invalid key-src: '%s'" key-src)))
	  (if ebut
	      (ebut:delete ebut)
	    (hypb:error "(ebut-delete): No valid %s button in %s"
			(ebut:key-to-label but-key) buf)))
	(with-current-buffer buf
	  (if interactive
	      (progn
		(call-interactively 'hui:ebut-unmark)
		(message "Button deleted."))
	    (hui:ebut-unmark but-key key-src))
	  (when (hmail:reader-p)
	    (hmail:msg-narrow))
	  (message "Button '%s' deleted." (ebut:key-to-label but-key))
	  t)
      (hypb:error "(ebut-delete): You may not delete buttons from this buffer"))))

(defun hui:ebut-message (but-edit-flag)
  (let ((actype (symbol-name (hattr:get 'hbut:current 'actype)))
	(args (hattr:get 'hbut:current 'args)))
    (setq actype (actype:def-symbol actype))
    (message "%s%s%s %s %S"
	     ebut:start
	     (hbut:key-to-label (hattr:get 'hbut:current 'lbl-key))
	     ebut:end
	     (if but-edit-flag "now executes" "executes")
	     (cons actype args))))

(defun hui:ebut-unmark (&optional but-key key-src directory)
  "Remove delimiters and any instance number from button.
Button is given by BUT-KEY in KEY-SRC of DIRECTORY.  All args are
optional, the current button and buffer file are the defaults.

With a prefix argument, also delete the button text between the delimiters."
  (interactive)
  (let ((form (lambda ()
		(let ((buffer-read-only) start-delim-pos end-delim-pos text-end)
		  (setq start-delim-pos (match-beginning 0)
			end-delim-pos (match-end 0))
		  (when (fboundp 'hproperty:but-delete)
		    (hproperty:but-delete start-delim-pos))
		  (goto-char (- (point) (length ebut:end)))
		  (skip-chars-backward " \t\n\r")
		  (setq text-end (point))
		  ;; Limit instance number removal to single digit 2-9
		  ;; in case button text contains a colon-separated
		  ;; number that is part of the text and  should not
		  ;; be removed.
		  (skip-chars-backward "2-9")
		  (skip-chars-backward ebut:instance-sep)
		  (when (looking-at (concat (regexp-quote ebut:instance-sep)
					    "[2-9]"
					    (regexp-quote ebut:end)))
		    (setq text-end (point)))
		  (when (search-backward ebut:start (- (point) (hbut:max-len)) t)
		    (if current-prefix-arg
			;; Remove button text, delimiters and preceding space, if any.
			(delete-region (max (point-min)
					    (1- (match-beginning 0)))
				       end-delim-pos)
		      ;;
		      ;; Remove button delimiters only.
		      ;;
		      ;; Remove button ending delimiter
		      (delete-region text-end end-delim-pos)
		      ;; Remove button starting delimiter
		      (delete-region (match-beginning 0) (match-end 0))))))))
    (if (called-interactively-p 'interactive)
	(save-excursion
	  (when (search-forward ebut:end nil t) (funcall form)))
      ;; Non-interactive invocation.
      (let (cur-flag)
	(if (and (or (null key-src) (eq key-src buffer-file-name))
		 (or (null directory) (eq directory default-directory)))
	    (setq cur-flag t)
	  (set-buffer (find-file-noselect (expand-file-name key-src directory))))
	(unless (stringp but-key)
	  (setq but-key (hbut:label-p))
	  (unless (stringp but-key)
	    (hypb:error "(ebut-unmark): No Hyperbole button at point to unmark")))
	(save-excursion
	  (goto-char (point-min))
	  (when (re-search-forward (ebut:label-regexp but-key) nil t)
	    (funcall form)
	    ;; If modified a buffer other than the current one, save it.
	    (when cur-flag
	      (save-buffer))))))))

(defun hui:file-find (file-name)
  "If FILE-NAME is readable, find it, else signal an error."
  (if (and (stringp file-name) (file-readable-p file-name))
      (find-file file-name)
    (hypb:error "(file-find): \"%s\" does not exist or is not readable"
		file-name)))

(defun hui:hbut-operate (operation operation-str &optional but)
  "Execute OPERATION func described by OPERATION-STR action on a Hyperbole button.
Either the button at point is used or if none, then one is prompted
for with completion of all labeled buttons within the current buffer."
  (unless (or but (setq but (hbut:at-p)))
    (let (lst)
      (cond ((setq lst (nconc (ebut:alist) (ibut:alist)))
	     (setq but (hbut:get (hbut:label-to-key
			          (hargs:read-match operation-str
						    lst nil t
						    (hbut:label-p 'as-label) 'hbut)))))
	    (t (hypb:error "(hbut-operate): No labeled buttons in buffer")))))
  (cond ((and (called-interactively-p 'interactive) (null but))
	 (hypb:error "(hbut-operate): No current button upon which to operate."))
	((progn (unless but (setq but 'hbut:current))
		(hbut:is-p but))
	 (hui:but-flash)
	 (apply hrule:action
		operation
		(list but)))
	((and but (symbolp but))
	 (hypb:error "(hbut-operate): Symbol, %s, has invalid Hyperbole button attributes:\n  %S" but (hattr:list but)))
	(t
	 (hypb:error "(hbut-operate): Invalid Hyperbole button: %s" but))))

(defun hui:hbut-term-highlight (start end)
  "For terminals only: Emphasize a button spanning from START to END."
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region (point-min) start)
      (sit-for 0)
      (setq inverse-video t)
      (goto-char (point-min))
      (widen)
      (narrow-to-region (point) end)
      (sit-for 0)
      (setq inverse-video nil))))

(defun hui:hbut-term-unhighlight (start _end)
  "For terminals only: Remove any emphasis from hyper-button at START to _END."
  (save-excursion
    (save-restriction
      (goto-char start)
      (narrow-to-region (point-min) start)
      (sit-for 0)
      (setq inverse-video nil))))

(defun hui:help-ebut-highlight ()
  "Highlight any explicit buttons in help buffer associated with current buffer."
  (when (fboundp 'hproperty:but-create)
    (with-current-buffer (get-buffer (hypb:help-buf-name))
      (hproperty:but-create))))

(defun hui:htype-delete (htype-sym)
  "Delete HTYPE-SYM from use in current Hyperbole session.
HTYPE-SYM must be redefined for use again."
  (and htype-sym (symbolp htype-sym)
       (let ((type
	      (intern (hargs:read-match
		       (concat "Delete from " (symbol-name htype-sym) ": ")
		       (mapcar 'list (htype:names htype-sym))
		       nil t nil htype-sym))))
	 (htype:delete type htype-sym))))

(defun hui:htype-help (htype-sym &optional no-sort)
  "Display documentation for types from HTYPE-SYM which match to a regexp.
Optional NO-SORT means display in decreasing priority order (natural order)."
  (and htype-sym (symbolp htype-sym)
       (let* ((tstr (symbol-name htype-sym))
	      (tprefix (concat tstr "::"))
	      (buf-name (hypb:help-buf-name (capitalize tstr)))
	      (temp-buffer-show-hook
	       (lambda (buf)
		 (set-buffer buf) (goto-char (point-min))
		 (while (re-search-forward "^" nil t)
		   (replace-match "  " t nil))
		 (goto-char (point-min))
		 (while (search-forward (concat "  " tprefix) nil t)
		   (replace-match "" t nil))
		 (goto-char (point-min))
		 (set-buffer-modified-p nil)
		 (display-buffer buf nil)))
	      (temp-buffer-show-function temp-buffer-show-hook)
	      (names (htype:names htype-sym))
	      (term (hargs:read-match
		     (concat (capitalize tstr)
			     (format " to describe (RET or '*' for all%s): "
				     (if (eq htype-sym 'ibtypes)
					 " in priority order"
				       "")))
		     (mapcar 'list (append '("" "*") names))
		     nil t nil htype-sym))
	      nm-list
	      doc-list)
	 (setq nm-list
	       (if (member term '("" "*"))
		   (let ((type-names
			  (mapcar (lambda (nm) (concat tprefix nm))
				  names)))
		     (if no-sort type-names
		       (sort type-names #'string<)))
		 (cons (concat tprefix term) nil))
	       doc-list (delq nil (mapcar
				   (lambda (name)
				     (let ((doc (documentation
						 (intern-soft name))))
				       (if doc (cons name doc))))
				   nm-list)))
	 (with-output-to-temp-buffer buf-name
	   (mapcar (lambda (nm-doc-cons)
		     (princ (car nm-doc-cons)) (terpri)
		     (princ (cdr nm-doc-cons)) (terpri)
		     (terpri))
		   doc-list)))))

(defun hui:htype-help-current-window (htype-sym &optional no-sort)
  "Display in the current window output from `hui:htype-help' using HTYPE-SYM.
Optional NO-SORT means display in decreasing priority order (natural order)."
  (let ((display-buffer-alist
	 '(("\\`*Help" . ((lambda (buf _alist) (switch-to-buffer buf)))))))
    (hui:htype-help htype-sym no-sort)))

(defun hui:ibut-delete-op (interactive but-key key-src)
  "INTERACTIVEly or not, delete explicit button given by BUT-KEY in KEY-SRC.
KEY-SRC may be a buffer or a pathname; when nil the current buffer is used.
Return t if button is deleted, signal error otherwise.  If called
with INTERACTIVE non-nil, derive BUT-KEY from the button that point is
within."
  (let ((buf (current-buffer)) (ibut))
    (if (if interactive
	    (ibut:delete)
	  (cond ((or (null key-src) (and (bufferp key-src) (setq buf key-src)))
		 (setq ibut (ibut:get but-key nil key-src)))
		((and (stringp key-src)
		      (setq buf (find-file-noselect key-src)))
		 (setq ibut (ibut:get but-key buf)))
		(t (hypb:error "(ibut-delete): Invalid key-src: '%s'" key-src)))
	  (if ibut
	      (ibut:delete ibut)
	    (hypb:error "(ibut-delete): No valid %s button in %s"
			(ibut:key-to-label but-key) buf)))
	(progn (set-buffer buf)
	       (when (hmail:reader-p) (hmail:msg-narrow))
	       (message "Button '%s' deleted." (ibut:key-to-label but-key)))
      (hypb:error "(ibut-delete): You may not delete buttons from this buffer"))))

(defun hui:ibut-message (but-edit-flag)
  (let ((actype (symbol-name (hattr:get 'hbut:current 'actype)))
	(args (hattr:get 'hbut:current 'args)))
    (setq actype (actype:def-symbol actype))
    (message "%s%s%s %s %S"
	     ibut:label-start
	     (hbut:key-to-label (hattr:get 'hbut:current 'lbl-key))
	     ibut:label-end
	     (if but-edit-flag "now executes" "executes")
	     (cons actype args))))

(defun hui:key-dir (but-buf)
  "Return button key src directory based on BUT-BUF, a buffer."
  (if (bufferp but-buf)
      (let ((file (buffer-file-name but-buf)))
	(if file
	    (file-name-directory (hpath:symlink-referent file))
	  (buffer-local-value 'default-directory but-buf)))
    (hypb:error "(hui:key-dir): '%s' is not a valid buffer")))

(defun hui:key-src (but-buf)
  "Return button key src location based on BUT-BUF, a buffer.
This is BUT-BUF when button data is stored in the buffer and the
button's source file name when the button data is stored externally."
  (with-current-buffer but-buf
    (cond ((hmail:mode-is-p) but-buf)
	  ((hpath:symlink-referent (buffer-file-name but-buf)))
	  (t but-buf))))

(defun hui:link-create (edit-flag but-window lbl-key but-loc but-dir type-and-args)
  "Create or edit a new Hyperbole explicit link button.
If EDIT-FLAG is non-nil, edit button at point in BUT-WINDOW,
otherwise, prompt for button label and create a button.
LBL-KEY is internal form of button label.  BUT-LOC is the file or buffer
in which to create button.  BUT-DIR is the directory of BUT-LOC.
TYPE-AND-ARGS is the action type for the button followed by any
arguments it requires.  Any text properties are removed from string
arguments."
  (hattr:set 'hbut:current 'loc but-loc)
  (hattr:set 'hbut:current 'dir but-dir)
  (hattr:set 'hbut:current 'actype (actype:elisp-symbol (car type-and-args)))
  (hattr:set 'hbut:current 'args (cdr type-and-args))
  (select-window but-window)
  (let ((label (ebut:key-to-label lbl-key)))
    (ebut:operate label (when edit-flag label))))

(defun hui:link-possible-types ()
  "Return list of possible link action types during editing of a Hyperbole button.
Point must be on the link referent, i.e. in the Action Key release buffer.
Each list element is a list of the link type and any arguments it requires.

The link types considered are fixed; this function must be changed to alter
the contexts recognized.  Defining new link types will not alter the
possible types.

Referent Context         Possible Link Type Returned
----------------------------------------------------
Global Button            link-to-gbut
Explicit Button          link-to-ebut
Implicit Button          link-to-ibut
Bookmarks List           link-to-bookmark
Info Index Item          link-to-Info-index-item
Info Node                link-to-Info-node
Texinfo Node             link-to-texinfo-node
Mail Reader Message      link-to-mail
Directory Name           link-to-directory
File Name                link-to-file
Koutline Cell            link-to-kcell
Outline Heading          link-to-string-match
Buffer attached to File  link-to-file
Buffer without File      link-to-buffer-tmp"
  ;; Elisp Buffer at Start
  ;; or End of Sexpression    eval-elisp

  (let (val
	hbut-sym
	lbl-key)
    (delq nil
	  (list (cond ((and (prog1 (setq hbut-sym (hbut:at-p))
			      ;; Next line forces use of any ibut name in the link.
			      (save-excursion (ibut:at-to-name-p hbut-sym)))
			    (setq lbl-key (hattr:get hbut-sym 'lbl-key))
			    (eq (current-buffer) (get-file-buffer (gbut:file))))
		       (list 'link-to-gbut lbl-key))
		      ((and hbut-sym (eq (hattr:get hbut-sym 'categ) 'explicit))
		       (list 'link-to-ebut lbl-key))
		      (hbut-sym
		       (list 'link-to-ibut lbl-key (or buffer-file-name (buffer-name)))))
		(cond ((and (require 'bookmark)
                            (derived-mode-p #'bookmark-bmenu-mode))
                       (list 'link-to-bookmark (bookmark-bmenu-bookmark))))
		(cond ((derived-mode-p #'Info-mode)
		       (if (and Info-current-node
				(member Info-current-node
					(Info-index-nodes Info-current-file))
				(Info-menu-item-at-p))
			   (let ((hargs:reading-type 'Info-index-item))
			     (list 'link-to-Info-index-item (hargs:at-p)))
			 (let ((hargs:reading-type 'Info-node))
			   (list 'link-to-Info-node (hargs:at-p)))))
                      ((derived-mode-p #'texinfo-mode)
                       (let (node)
                         (save-excursion
                           (beginning-of-line)
                           (when (and (not (looking-at "@node "))
                                      (not (re-search-backward "^@node " nil t)))
                             (hypb:error "(hui:link-possible-types): Not within a texinfo node"))
			   (require 'texnfo-upd)
                           (setq node (texinfo-copy-node-name)))
                         (list 'link-to-texinfo-node buffer-file-name node)))
		      ((hmail:reader-p)
		       (list 'link-to-mail
			     (list (rmail:msg-id-get) buffer-file-name))))
		(cond
		 ((let ((hargs:reading-type 'directory))
		    (setq val (hargs:at-p t)))
		  (list 'link-to-directory val))
		 ((let ((hargs:reading-type 'file))
		    (setq val (hargs:at-p t)))
		  (list 'link-to-file val (point)))
		 ((derived-mode-p #'kotl-mode)
		  (list 'link-to-kcell buffer-file-name (kcell-view:idstamp)))
		 ;; If link is within an outline-regexp prefix, use
		 ;; a link-to-string-match.
		 ((and (boundp 'outline-regexp)
		       (stringp outline-regexp)
		       (save-excursion
			 (<= (point)
			     (progn
			       (beginning-of-line)
			       (if (looking-at outline-regexp)
				   (match-end 0)
				 0)))))
		  (save-excursion
		    (end-of-line)
		    (let ((heading (buffer-substring-no-properties
				    (point)
				    (progn (beginning-of-line) (point))))
			  (occur 1))
		      (while (search-backward heading nil t)
			(setq occur (1+ occur)))
		      (list 'link-to-string-match
			    heading occur buffer-file-name))))
		 (buffer-file-name
		  (list 'link-to-file buffer-file-name (point)))
		 (t (list 'link-to-buffer-tmp (buffer-name))))
		;;
		;; Deleted link to elisp possibility as it can embed
		;; long elisp functions in the button data file and
		;; possibly not parse them correctly.
		;;
		;; (and (fboundp 'smart-emacs-lisp-mode-p)
		;;      (smart-emacs-lisp-mode-p)
		;;      (or (eq (char-syntax (following-char)) ?\()
		;; 	 (eq (char-syntax (preceding-char)) ?\)))
		;;      (setq val (hargs:sexpression-p))
		;;      (list 'eval-elisp val))
		))))

(defun hui:list-remove-text-properties (lst)
  "Return LST, a list, with text properties removed from any string elements."
  (mapcar (lambda (elt) (if (stringp elt) (substring-no-properties elt) elt))
	  lst))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************


(defvar hui:ebut-label-prev nil
  "String value of previous button name during an explicit button rename.
At other times, value must be nil.")

(provide 'hui)

;;; hui.el ends here
