;;; kotl-mode.el ---  Major mode for editing koutlines and associated commands  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    6/30/93
;; Last-Mod:      9-Oct-22 at 16:25:35 by Bob Weiner
;;
;; Copyright (C) 1993-2022  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Lisp Libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(cl-lib delsel hsettings hmail hypb kfile
				    kvspec kcell outline org org-table kotl-orgtbl)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kotl-mode-map nil
  "Keymap containing koutliner editing and viewing commands.")

(defvar kotl-previous-mode nil
  "Default mode of koutline buffers prior to invocation of kotl-mode.")

(defcustom kotl-mode:shrink-region-flag nil
  "*Non-nil means Koutliner commands automatically shrink the region.
The region is shrinked within the visible bounds of a single cell
before editing it.  The region then falls within the first
visible cell that was part of the region or that followed it.
Default value is nil."
  :type 'boolean
  :group 'hyperbole-koutliner)

(defcustom kotl-mode:refill-flag nil
  "*Non-nil means automatically refill cells during operations.
Operations are move, copy, promotion and demotion.  Default value
is nil.  Cells with a `no-fill' attribute are never refilled
during such operations, regardless of the value of this flag."
  :type 'boolean
  :group 'hyperbole-koutliner)

(defcustom kotl-mode:tab-flag nil
  "*Non-nil means {\\[kotl-mode:tab-command]} inserts a literal tab character and {\\[kotl-mode:untab-command]} deletes backward.
Nil means {\\[kotl-mode:tab-command]} demotes the current tree and
{\\[kotl-mode:untab-command]} promotes the tree.  The default is nil."
  :type 'boolean
  :group 'hyperbole-koutliner)

(defcustom kotl-mode:indent-tabs-mode t
  "*Non-nil means {\\[kotl-mode:tab-command]} may insert literal tab characters.
Tab characters are inserted rather than space characters when
`kotl-mode:tab-flag' is non-nil.  Default value is t.  The value
of this variable is local to each Koutline buffer."
  :type 'boolean
  :group 'hyperbole-koutliner)

;; Define these newer Emacs variables if Emacs has not already done so.
(defvar yank-window-start nil)
(defvar yank-undo-function nil
  "If non-nil, the function used by `yank-pop'.
It is used to delete the last stretch of yanked text.  Function
is called with two parameters, START and END corresponding to the
value of the mark and point; it is guaranteed that START <= END.
Normally set from the UNDO element of a yank-handler; see
`insert-for-yank'.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Koutline mode is suitable only for specially formatted data.
(put 'kotl-mode 'mode-class 'special)

;;;###autoload
(defun kotl-mode ()
  "The major mode used to edit and view koutlines.
It provides the following keys:
\\{kotl-mode-map}"
  (interactive)
  (use-local-map kotl-mode-map)
  (set-syntax-table text-mode-syntax-table)
  ;; Turn off filladapt minor mode if on, so that it does not interfere with
  ;; the filling code in "kfill.el".
  (when (and (fboundp 'filladapt-mode) (boundp 'filladapt-mode) filladapt-mode)
    (filladapt-mode 0))
  (if (/= 3 (length (action:params (symbol-function 'fill-paragraph))))
      ;; Some package such as filladapt has overwritten the primitives
      ;; defined in kfill.el, so reload it.
      (load "kfill"))
  (setq-local fill-paragraph-function #'kfill:fill-paragraph)
  ;;
  ;; Prevent insertion of characters outside of editable bounds,
  ;; e.g. after the mouse sets point to a non-editable position
  (add-hook 'pre-command-hook #'kotl-mode:pre-self-insert-command)
  ;;
  ;; Ensure that outline structure data is saved when save-buffer is called
  ;; from save-some-buffers, {C-x s}.
  (add-hook 'write-file-functions #'kotl-mode:update-buffer nil 'local)
  (mapc #'make-local-variable
	'(hyrolo-entry-regexp kotl-previous-mode
			      indent-line-function indent-region-function
			      outline-isearch-open-invisible-function
			      outline-level outline-regexp
			      line-move-ignore-invisible minor-mode-alist
			      selective-display-ellipses
			      paragraph-separate paragraph-start))
  ;; Used by kimport.el functions.
  (unless (and (boundp 'kotl-previous-mode) kotl-previous-mode)
    (setq hyrolo-entry-regexp (concat "^" kview:outline-regexp)
	  kotl-previous-mode major-mode
	  ;; Remove outline minor-mode mode-line indication.
	  minor-mode-alist (copy-sequence minor-mode-alist)
	  minor-mode-alist (set:remove '(outline-minor-mode " Outl")
				       minor-mode-alist)
 	  minor-mode-alist (set:remove '(selective-display " Outline")
				       minor-mode-alist)
	  minor-mode-alist (set:remove '(selective-display " Otl")
				       minor-mode-alist)
	  ;; Remove indication that buffer is narrowed.
	  mode-line-format (copy-sequence mode-line-format)
	  mode-line-format (set:remove "%n" mode-line-format)
	  outline-level  #'kcell-view:level
	  outline-regexp kview:outline-regexp))
  ;;
  (when (fboundp 'add-to-invisibility-spec)
    (add-to-invisibility-spec '(outline . t)))
  (setq indent-line-function 'kotl-mode:indent-line
	indent-region-function 'kotl-mode:indent-region
	outline-isearch-open-invisible-function 'kotl-mode:isearch-open-invisible
	line-move-ignore-invisible 'keep-column
	local-abbrev-table text-mode-abbrev-table
	;; These par* settings must be anchored to the bol since
	;; kfill.el and `kotl-mode:fill-paragraph' use them in regexp
	;; searches.
	paragraph-separate "^[ \t]*$\\|^\^L"
	paragraph-start "^[ \t]*$\\|^\^L"
	selective-display nil
	selective-display-ellipses t
	track-eol t)
  ;;
  ;; This major-mode setting must come after the local variable settings but
  ;; before the koutline is formatted.
  (setq major-mode 'kotl-mode
	mode-name "Kotl"
	;; Used when indenting cells.
	indent-tabs-mode nil)

  ;; Used when indenting lines within cells.
  (make-local-variable 'kotl-mode:indent-tabs-mode)

  (setq auto-fill-function 'kfill:do-auto-fill)

  ;; If buffer has not yet been formatted for editing, format it.
  (let (version)
    (cond
     ;; Koutline file that has been loaded and formatted for editing.
     ((kview:is-p kview)
      ;; The buffer might have been widened for inspection, so narrow to cells
      ;; only.
      (kfile:narrow-to-kcells))
     ;; Koutline file that has been loaded but not yet formatted for editing.
     ((setq version (kfile:is-p))
      (kfile:read
       (current-buffer)
       (and buffer-file-name (file-exists-p buffer-file-name))
       version)
      (kvspec:activate))
     ;; New koutline buffer or a foreign text buffer that must be converted to
     ;; koutline format.
     (t
      (kfile:create (current-buffer))
      (kvspec:activate))))
  ;; We have been converting a buffer from a foreign format to a koutline.
  ;; Now that it is converted, ensure that `kotl-previous-mode' is set to
  ;; koutline.
  (hyperb:with-suppressed-warnings ((free-vars kotl-previous-mode))
    (setq kotl-previous-mode 'kotl-mode))
  ;; Enable Org Table editing minor mode (user can disable via kotl-mode-hook
  ;; if desired).
  (orgtbl-mode 1)
  ;; Override org-tbl {M-RET} binding since Action Key provides the
  ;; same funcitonality when in a table, but may also be invoked from
  ;; a mouse button.
  (org-defkey orgtbl-mode-map "\M-\C-m"
              (orgtbl-make-binding 'orgtbl-meta-return 105
        			   "\M-\C-m" [(meta return)]))
  (org-defkey orgtbl-mode-map [(meta return)]
              (orgtbl-make-binding 'orgtbl-meta-return 106
        			   [(meta return)] "\M-\C-m"))
  (org-defkey orgtbl-mode-map "\C-d"
              (orgtbl-make-binding 'kotl-mode:delete-char 201
        			   "\C-d"))
  (org-defkey orgtbl-mode-map [(shift iso-lefttab)]
              (orgtbl-make-binding 'org-shifttab 202
        			   [(shift iso-lefttab)] [backtab] [(shift tab)]))
  (run-hooks 'kotl-mode-hook)
  (add-hook 'change-major-mode-hook #'kotl-mode:show-all nil t))

;;;###autoload
(defun kotl-mode:example (&optional example replace-flag)
  "Display the optional Koutliner EXAMPLE file.
This is for demonstration and editing use by a user.  With
optional REPLACE-FLAG non-nil, archive any existing file, and
replace it with the latest Hyperbole EXAMPLE.

EXAMPLE may be a file or directory name (\"EXAMPLE.kotl\" is appended).

If EXAMPLE is omitted or nil, create or edit the \"~/EXAMPLE.kotl\" file.

When called interactively, prompt for EXAMPLE if given a prefix
argument, archive any existing file, and replace it with the latest
Hyperbole EXAMPLE."
  (interactive
   (list (when current-prefix-arg
	   (read-file-name "Path to replace and save EXAMPLE.kotl file: "
			   nil nil nil "EXAMPLE.kotl"))))
  (when (and current-prefix-arg (called-interactively-p 'interactive))
    (setq replace-flag t))
  (let (personal-example
	original-example)
    (unless (stringp example)
      (setq example "EXAMPLE.kotl"))
    (when (file-directory-p example)
      (setq personal-example (expand-file-name "EXAMPLE.kotl" example)
	    example "EXAMPLE.kotl"))
    (unless personal-example
      (if (file-name-absolute-p example)
	  (setq personal-example example
		example (file-name-nondirectory example))
	(setq personal-example (expand-file-name example "~/"))))
    (setq original-example (expand-file-name example (expand-file-name "kotl/" hyperb:dir)))
    (when (or replace-flag
	      (file-newer-than-file-p original-example personal-example))
      (when (file-exists-p personal-example)
	;; Save personally edited one and use the newer Hyperbole Example file
	(rename-file personal-example (expand-file-name (concat "SAVED-" example) "~/") t)))
    (cond ((get-file-buffer personal-example)
	   (switch-to-buffer (get-file-buffer personal-example)))
	  ((file-readable-p personal-example)
	   (find-file personal-example))
	  ;; Create a new personal-example file
	  (t (switch-to-buffer (create-file-buffer personal-example))
	     (setq buffer-file-name personal-example
		   default-directory (expand-file-name "~/")
		   buffer-auto-save-file-name nil
		   buffer-read-only nil)
	     (insert-file-contents original-example)
	     (goto-char (point-min))
	     (kotl-mode)
	     (save-buffer)))))

;;; ------------------------------------------------------------------------
;;; Editing within a single kotl
;;; ------------------------------------------------------------------------

(defalias 'kotl-mode:backward-delete-char-untabify
  'kotl-mode:delete-backward-char)
(defalias 'kotl-mode:backward-delete-char
  'kotl-mode:delete-backward-char)
(defalias 'kotl-mode:delete-forward-char
  'kotl-mode:delete-char)
(defalias 'kotl-mode:left-char 'kotl-mode:backward-char)
(defalias 'kotl-mode:right-char 'kotl-mode:forward-char)

(defun kotl-mode:backward-kill-word (arg)
  "Kill up to prefix ARG words preceding point within a single cell."
  (interactive "*p")
  (unless arg
    (setq arg 1))
  (cond ((< arg 0)
	 (when (kotl-mode:eocp)
	   (error "(kotl-mode:backward-kill-word): End of cell")))
	((> arg 0)
	 (when (kotl-mode:bocp)
	   (error "(kotl-mode:backward-kill-word): Beginning of cell"))))
  (unless (= arg 0)
    (save-restriction
      (narrow-to-region (kcell-view:start) (kcell-view:end-contents))
      (backward-kill-word arg))))

(defun kotl-mode:backward-or-forward-delete-char (arg)
  "Delete ARG characters backwards or forwards.
Direction is determined from the value of `delete-key-deletes-forward' or
whether the Backspace key exists on the keyboard.  If there is no Backspace
key, the delete key should always delete backward one character."
  (interactive "*p")
  (kotl-mode:delete-char (if normal-erase-is-backspace arg (- arg)) nil))

(defun kotl-mode:center-line ()
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation so that it equals the distance between
the end of the text and `fill-column'."
  (interactive "*")
  (kotl-mode:maintain-region-highlight)
  (let ((indent (kcell-view:indent))
	(opoint (point-marker))
	(bocp)
	start)
    (setq start (kotl-mode:beginning-of-line))
    (when (setq bocp (kotl-mode:bocp))
      ;; Add a temporary fill-prefix since this is the 1st line of the cell
      ;; where label could interfere with centering.
      (insert "\n\n") (insert-char ?\  indent))
    (center-line)
    (when bocp
      ;; Delete temporary fill prefix.
      (delete-region start (+ start indent 2)))
    (goto-char opoint)
    ;; Move to editable point if need be.
    (kotl-mode:to-valid-position)))

(defun kotl-mode:center-paragraph ()
  "Center each nonblank line in the paragraph at or after point.
See `center-line' for more info."
  (interactive "*")
  (kotl-mode:maintain-region-highlight)
  (let ((indent (kcell-view:indent))
	(opoint (point-marker))
	start)
    (backward-paragraph)
    (kotl-mode:to-valid-position)
    (setq start (point))
    ;; Add a temporary fill-prefix for 1st line in cell which contains a
    ;; label, so is centered properly.
    (insert "\n\n") (insert-char ?\  indent)
    (kcell-view:operate 'center-paragraph)
    ;; Delete temporary fill prefix.
    (delete-region start (+ start indent 2))
    ;; Return to original point.
    (goto-char (min opoint (kcell-view:end-contents)))
    ;; Move to editable point if need be.
    (kotl-mode:to-valid-position)))

(defun kotl-mode:copy-kcell-reference-to-register (klink register)
  "Copy a KLINK at point or if in a kcell, a klink to that kcell, to a REGISTER.
The REGISTER is named by a single character."
  (interactive
   (let ((klink (klink:absolute (klink:at-p))))
     (list
      (cond (klink)
	    ((derived-mode-p 'kotl-mode)
	     (setq klink (kcell-view:absolute-reference))))
      (when klink
	(register-read-with-preview (format "Copy %s to register: " klink))))))
  (if (and (stringp klink) register)
      (set-register register klink)
    (user-error "(kotl-mode:copy-kcell-reference-to-register): Point is not within a Koutliner klink or kcell")))

(defun kotl-mode:copy-absolute-kcell-link-to-kill-ring (&optional pos)
  "Add an absolute kcell reference to the kill ring.
The kcell reference is from optional POS or point.  It is for use
outside the outline."
  (interactive "d")
  (kill-new (kcell-view:absolute-reference pos)))

(defun kotl-mode:copy-relative-kcell-link-to-kill-ring (&optional pos)
  "Add a relative kcell reference to the kill ring.
The kcell reference is from optional POS or point."
  (interactive "d")
  (kill-new (kcell-view:reference pos)))

(defun kotl-mode:copy-absolute-kcell-link-to-register (register pos)
  "Copy into REGISTER an absolute kcell reference (from optional POS or point)."
  (interactive
   (list (register-read-with-preview "Copy absolute link to current cell to register: ")
	 (point)))
  (set-register register (kcell-view:absolute-reference pos)))

(defun kotl-mode:copy-relative-kcell-link-to-register (register pos)
  "Copy into REGISTER a relative kcell reference (from optional POS or point)."
  (interactive
   (list (register-read-with-preview "Copy relative link to current cell to register: ")
	 (point)))
  (set-register register (kcell-view:reference pos)))

(defun kotl-mode:copy-region-as-kill (start end)
  "Copy region between START and END within a single kcell to kill ring."
  (interactive "r")
  (kotl-mode:kill-region start end t))

(defun kotl-mode:copy-to-register (register start end &optional delete-flag)
  "Copy into REGISTER the region START to END.
With optional prefix arg DELETE-FLAG, delete region."
  (interactive "cCopy to register: \nr\nP")
  (let ((indent (kcell-view:indent)))
    (set-register register
		  (replace-regexp-in-string
		   (concat "^" (make-string indent ?\ ))
		   ""
		   (buffer-substring start end) nil t)))
  (when delete-flag
    (delete-region start end)))

(defun kotl-mode:delete-backward-char (arg &optional kill-flag)
  "Delete up to the preceding prefix ARG characters.
Return number of characters deleted.
Optional KILL-FLAG non-nil means save in kill ring instead of deleting.
Do not delete across cell boundaries."
  (interactive "*P")
  (when (called-interactively-p 'interactive)
    (when current-prefix-arg
      (setq kill-flag t
	    arg (prefix-numeric-value current-prefix-arg))))
  (unless arg
    (setq arg 1))
  (kotl-mode:delete-char (- arg) kill-flag))

(defun kotl-mode:delete-blank-lines ()
  "On blank line in a cell, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete all blank lines that follow it.

If nothing but whitespace follows point until the end of a cell, delete all
whitespace at the end of the cell."
  (interactive "*")
  ;; If nothing but whitespace from point until the end of cell, remove all
  ;; cell trailing whitespace.
  (let ((end (kcell-view:end-contents))
	start)
    (if (save-excursion
	  (skip-chars-forward " \t\n\r" end)
	  (not (kotl-mode:eocp)))
	(kcell-view:operate #'delete-blank-lines)
      (setq start (kcell-view:start))
      (goto-char end)
      ;; delete any preceding whitespace
      (skip-chars-backward " \t\n\r" start)
      (delete-region (max start (point)) end)))
  (kotl-mode:to-valid-position))

(defun kotl-mode:delete-char (arg &optional kill-flag)
  "Delete up to prefix ARG characters following point.
Return number of characters deleted.
Optional KILL-FLAG non-nil means save in kill ring instead of deleting.
Do not delete across cell boundaries."
  (interactive "*P")
  (when (called-interactively-p 'interactive)
    (when current-prefix-arg
      (setq kill-flag t
	    arg (prefix-numeric-value current-prefix-arg))))
  (unless arg
    (setq arg 1))

  (if (not (and (boundp 'kview) (kview:is-p kview)))
      ;; Support use within Org tables outside of the Koutliner
      (delete-char arg kill-flag)
    (let ((del-count 0)
	  (indent (kcell-view:indent))
	  count start end)
      (cond ((> arg 0)
	     (if (kotl-mode:eocp)
		 (error "(kotl-mode:delete-char): End of cell")
	       (setq end (kcell-view:end)
		     arg (min arg (- end (point))))
	       (while (and (> arg 0) (not (kotl-mode:eocp)))
		 (if (kotl-mode:eolp)
		     (if (not (eq ?\  (char-syntax (following-char))))
			 (setq arg 0
			       del-count (1- del-count))
		       (delete-char 1 kill-flag)
		       ;; There may be non-whitespace characters in the
		       ;; indent area.  Don't delete them.
		       (setq count indent)
		       (while (and (> count 0)
				   (eq ?\ (char-syntax (following-char))))
			 (delete-char 1)
			 (setq count (1- count))))
		   (delete-char 1 kill-flag))
		 (setq arg (1- arg)
		       del-count (1+ del-count)))))
	    ((< arg 0)
	     (if (kotl-mode:bocp)
		 (error "(kotl-mode:delete-char): Beginning of cell")
	       (setq start (kcell-view:start)
		     arg (max arg (- start (point))))
	       (while (and (< arg 0) (not (kotl-mode:bocp)))
		 (if (kotl-mode:bolp)
		     (if (not (eq ?\  (char-syntax (preceding-char))))
			 (setq arg 0
			       del-count (1- del-count))
		       ;; There may be non-whitespace characters in the
		       ;; indent area.  Don't delete them.
		       (setq count indent)
		       (while (and (> count 0)
				   (eq ?\ (char-syntax (preceding-char))))
			 (delete-char -1)
			 (setq count (1- count)))
		       (if (zerop count)
			   (delete-char -1 kill-flag)))
		   (delete-char -1 kill-flag))
		 (setq arg (1+ arg)
		       del-count (1+ del-count))))))
      del-count)))

(defun kotl-mode:delete-horizontal-space ()
  "Delete all spaces and tabs around point."
  (interactive "*")
  (save-restriction
    (narrow-to-region
     (save-excursion
       (kotl-mode:beginning-of-line))
     (save-excursion
       (kotl-mode:to-end-of-line)))
    (delete-horizontal-space)))

(defun kotl-mode:delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this line.
With prefix ARG non-nil, join this line to the following line."
  (interactive "*P")
  (kcell-view:operate
   (lambda ()
     (let ((opoint (point)))
       (beginning-of-line)
       (when arg
	 (kfill:forward-line 1))
       (if (eq (preceding-char) ?\n)
	   (progn
	     (delete-region (point) (1- (point)))
	     ;; If the second line started with the fill prefix,
	     ;; delete the prefix.
	     (if (and fill-prefix
		      (<= (+ (point) (length fill-prefix)) (point-max))
		      (string-equal fill-prefix
				    (buffer-substring
				     (point) (+ (point) (length fill-prefix)))))
		 (delete-region (point) (+ (point) (length fill-prefix))))
	     (fixup-whitespace))
	 (goto-char opoint))))))

(defun kotl-mode:skip-filling-p (interactive-flag)
  "Return t if filling is to be skipped, or nil.
This can be due to a no-fill attribute or with point in a table."
  (not (cond ((and (fboundp #'org-at-table-p) (org-at-table-p))
	      (when interactive-flag
		(beep)
		(message "Filling is disabled within tables")
		nil))
	     ((kcell-view:get-attr 'no-fill)
	      (when interactive-flag
		(beep)
		(message "Current cell has a `do not fill' attribute")
		nil))
	     (t))))

(defun kotl-mode:fill-cell (&optional justify ignore-collapsed-p)
  "Fill current cell within current view if it lacks a non-nil `no-fill' attribute.
With optional JUSTIFY, justify cell as well.
IGNORE-COLLAPSED-P is used when caller has already expanded cell, indicating
it is not collapsed."
  (interactive "*P")
  (cond ((kotl-mode:skip-filling-p (called-interactively-p 'interactive)))
	((string-match "\\`[ \t\n\r]*\\'" (kcell-view:contents))
	  ;; Cell content is all whitespace.
	 nil)
	(t (let* ((indent (kcell-view:indent))
		  (opoint (set-marker (make-marker) (point)))
		  (start  (set-marker (make-marker) (kcell-view:start)))
		  (end    (set-marker (make-marker) (kcell-view:end-contents)))
		  (collapsed-p)
		  temp-prefix prev-point)
	     (goto-char start)
	     ;; Expand cell if collapsed so that filling is done properly.
	     (when (and (not ignore-collapsed-p)
			(kcell-view:collapsed-p start))
	       (setq collapsed-p (kview:get-cells-status kview start end))
	       (outline-flag-region start end nil))
	     (goto-char start)
	     ;; Add a temporary fill-prefix for first labeled line, so is
	     ;; filled properly.
	     (insert (setq temp-prefix
			   (concat "\n\n" (make-string indent ?\ ))))
	     (while (progn (if (fboundp 'fill-paragraph-and-align)
			       (fill-paragraph-and-align justify)
			     (fill-paragraph justify))
			   (setq prev-point (point))
			   (forward-paragraph)
			   (and (not (eq (point) prev-point))
				(< (point) (kcell-view:end-contents))
				(if (or (= (preceding-char) ?\n) (outline-invisible-p (1- (point))))
				    (not (or (= (following-char) ?\n) (outline-invisible-p)))
				  t))))
	     ;; Delete temporary fill prefix.
	     (goto-char start)
	     (when (looking-at temp-prefix)
	       (replace-match "" t t))
	     ;; Return to original point.
	     (set-marker end nil)
	     (setq end (kcell-view:end-contents))
	     (goto-char (min opoint end))
	     ;;
	     ;; If cell was collapsed before filling, restore its status.
	     (when (remq 0 collapsed-p)
	       (kview:set-cells-status kview start end collapsed-p))
	     ;;
	     ;; Remove markers.
	     (set-marker start nil)
	     (set-marker opoint nil))
	   ;; Move to editable point if need be.
	   (kotl-mode:to-valid-position))))

(defun kotl-mode:fill-paragraph (&optional justify)
  "Fill the current paragraph within the cell.
With optional JUSTIFY, justify the paragraph as well.
Ignore any non-nil no-fill attribute attached to the cell."
  (interactive "*P")
  (unless (kotl-mode:skip-filling-p (called-interactively-p 'interactive))
    (let ((indent (kcell-view:indent))
	  (opoint (point-marker))
	  start end)
      (re-search-backward (concat "\\`\\|" paragraph-separate))
      (kotl-mode:to-valid-position)
      (setq start (point-marker))
      ;; Add a temporary fill-prefix for the 1st line in the cell which
      ;; contains a label, so that it is filled properly.
      (insert "\n\n") (insert-char ?\  indent)
      (setq end (point-marker))
      ;; Return to original paragraph point.  This is the correct formula,
      ;; considering the fill prefix that was just added.
      (goto-char (min (max opoint (point)) (kcell-view:end-contents)))
      (if (fboundp 'fill-paragraph-and-align)
	  (fill-paragraph-and-align justify)
	(fill-paragraph justify))
      ;; Delete temporary fill prefix.
      (delete-region start end)
      ;; Return to original point.
      (goto-char (min opoint (kcell-view:end-contents)))
      ;; Move to editable point if need be.
      (kotl-mode:to-valid-position)
      ;; Remove markers
      (set-marker opoint nil)
      (set-marker start nil)
      (set-marker end nil))))

(defun kotl-mode:fill-tree (&optional top-p)
  "Refill each cell within the tree whose root is at point.
Skip cells with a non-nil no-fill attribute.
With optional prefix argument TOP-P non-nil, refill all cells in the outline."
  (interactive "P")
  ;; Temporarily expand, then refill cells lacking no-fill property.
  (kview:map-expanded-tree (lambda (_kview) (kotl-mode:fill-cell)) kview top-p))

(defun kotl-mode:just-one-space ()
  "Delete all spaces and tabs around point and leave one space."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-excursion
	(narrow-to-region (kotl-mode:beginning-of-line) (kotl-mode:to-end-of-line)))
      (just-one-space))))

(defun kotl-mode:kill-line (&optional arg)
  "Kill ARG lines from point."
  (interactive "*P")
  (if (and (null arg)
	   (kotl-mode:bolp)
	   (boundp 'kill-whole-line) kill-whole-line)
      (let ((indent (kcell-view:indent)))
	;; Kill whole line including newline, if any.
	(kcell-view:operate
	 (lambda ()
	   (let ((no-newline))
	     (kill-region (point)
			  (progn (setq no-newline
				       (not (search-forward "\n" nil 'stay)))
				 (point)))
	     (or no-newline (delete-char indent))))))
    ;; Kill part of a line or multiple lines.
    (let ((num-arg (prefix-numeric-value arg)))
      (cond
       ((and (null arg) (not (kotl-mode:eolp)))
	;; kill to eol but not newline
	(kill-region (point) (setq arg (kotl-mode:to-end-of-line))))
       ((= num-arg 0)
	;; kill to bol
	(kill-region (point) (setq arg (kotl-mode:beginning-of-line))))
       (t ;; (/= num-arg 0)
	;; Find start and end of region to kill
	(let ((start (point))
	      (end (min (kcell-view:end-contents)
			(save-excursion (kfill:forward-line num-arg) (point)))))
	  (kotl-mode:kill-region start end))))))
  (setq last-command 'kill-region))

(defalias 'kotl-mode:kill-visual-line 'kotl-mode:kill-line)

(defun kotl-mode:kill-whole-line (&optional arg)
  "Kill ARG lines from point."
  (interactive "*P")
  (let ((kill-whole-line t))
    (kotl-mode:kill-line arg)))

(defun kotl-mode:kill-region (start end &optional copy-p)
  "Kill region between START and END within a single kcell.
With optional COPY-P equal to t, copy region to kill ring but does not
kill it.  With COPY-P any other non-nil value, return region as a
string without affecting kill ring.

If called interactively, `transient-mark-mode' is non-nil, and
there is no active region, copy any delimited selectable thing at
point; see `hui:delimited-selectable-thing'.

If the buffer is read-only and COPY-P is nil, the region will not be deleted
but it will be copied to the kill ring and then an error will be signaled.

If a completion is active, this aborts the completion only."
  (interactive
   (progn (barf-if-buffer-read-only)
	  (list (when mark-active (region-beginning))
		(when mark-active (region-end)))))
  (let ((read-only (and (not copy-p) buffer-read-only))
	(kill-commands '(kill-region kotl-mode:completion-kill-region
                         kotl-mode:kill-region kotl-mode:copy-region-as-kill))
	thing-and-bounds
	thing)
    (when read-only
      (setq copy-p t))
    (prog1 (cond
	    ((eq last-command 'complete)
	     (delete-region (point) cmpl-last-insert-location)
	     (insert cmpl-original-string)
	     (setq completion-to-accept nil))
	    ;; If called interactively, transient-mark-mode is non-nil, and no region is active, copy thing at point
	    ((and (memq this-command kill-commands)
		  transient-mark-mode
		  (not (use-region-p))
		  (setq thing-and-bounds (hui:delimited-selectable-thing-and-bounds)
			start (nth 1 thing-and-bounds)
			end   (nth 2 thing-and-bounds)
			thing (nth 0 thing-and-bounds)))
	     (if (and copy-p (not (eq copy-p t)))
		 ;; Return thing as a string
		 thing
	       (kotl-mode:kill-or-copy-region start end copy-p thing)))
	    ;; If no thing to process, copy region whether active or not
	    ((and (number-or-marker-p start)
		  (number-or-marker-p end)
		  (eq (kcell-view:cell start)
		      (kcell-view:cell end)))
	     (save-excursion
	       (goto-char start)
	       (kotl-mode:kill-or-copy-region start end copy-p)))
	    (t (error "(kotl-mode:kill-region): Bad region or not within a single Koutline cell")))
      (when (and copy-p (memq this-command kill-commands))
	(if thing
	    (message "Saved selectable thing: %s" thing)
	  (indicate-copied-region))))))

(defun kotl-mode:kill-or-copy-region (start end copy-p &optional kill-str)
  (when (and start end)
    (let ((indent (kcell-view:indent))
	  subst-str)
      ;; Convert region to string
      ;; Convert all occurrences of newline + indent
      ;; to just newline, eliminating indent.
      ;; Then save to kill ring.
      (setq subst-str (concat "\\([\n\r]\\)" (make-string indent ?\ ))
	    kill-str
	    (replace-regexp-in-string
	     subst-str "\\1" (buffer-substring start end)))
      (unless copy-p
	;; If last char of region is a newline, then delete indent in
	;; following line.
	(delete-region
	 start (+ end (if (memq (char-after (1- (max start end)))
				'(?\n ?\r))
			  indent
			0))))))
  (cond ((and copy-p (not (eq copy-p t)))
	 ;; Return killed region as a string.
	 kill-str)
	((not (and start end))
	 (signal 'mark-inactive nil))
	(t (if (eq last-command 'kill-region)
	       (kill-append kill-str (< end start))
	     (kill-new kill-str))
	   (setq this-command 'kill-region)
	   (setq deactivate-mark t)
	   (when (and (not copy-p) buffer-read-only)
	     (barf-if-buffer-read-only))
	   nil)))

;; Bound to {C-w} when completion.el library is loaded.
(defalias 'kotl-mode:completion-kill-region 'kotl-mode:kill-region)

;; Bound to {M-w}
(defalias 'kotl-mode:kill-ring-save 'kotl-mode:copy-region-as-kill)

(defun kotl-mode:kill-sentence (&optional arg)
  "Kill up to prefix ARG (or 1) sentences following point within a single cell."
  (interactive "*p")
  (unless arg
    (setq arg 1))
  (cond ((> arg 0)
	 (when (kotl-mode:eocp)
	   (error "(kotl-mode:kill-sentence): End of cell")))
	((< arg 0)
	 (when (kotl-mode:bocp)
	   (error "(kotl-mode:kill-sentence): Beginning of cell"))))
  (unless (= arg 0)
    (kotl-mode:kill-region (point)
			   (save-excursion
			     (kotl-mode:forward-sentence arg)))))

(defun kotl-mode:kill-word (arg)
  "Kill up to prefix ARG words following point within a single cell."
  (interactive "*p")
  (unless arg
    (setq arg 1))
  (cond ((> arg 0)
	 (when (kotl-mode:eocp)
	   (error "(kotl-mode:kill-word): End of cell")))
	((< arg 0)
	 (when (kotl-mode:bocp)
	   (error "(kotl-mode:kill-word): Beginning of cell"))))
  (unless (= arg 0)
    (save-restriction
      (narrow-to-region (kcell-view:start) (kcell-view:end-contents))
      (kill-word arg))))

(defun kotl-mode:newline (&optional arg)
  "Insert a newline.  With optional ARG, insert ARG newlines.
In Auto Fill mode, if no numeric arg, break the preceding line if it is
too long."
  (interactive "*p")
  (kotl-mode:delete-horizontal-space)
  (let ((indent (kcell-view:indent)))
    (if (and arg (= arg 1))
	(progn
	  (save-excursion
	    (insert ?\n)
	    (insert-char ?\  indent))
	  (do-auto-fill)
	  (kfill:forward-line 1)
	  (kotl-mode:beginning-of-line))
      (while (> arg 0)
	(insert ?\n)
	(insert-char ?\  indent)
	(setq arg (1- arg))))))

(defalias 'kotl-mode:electric-indent-just-newline 'kotl-mode:newline)
(defalias 'kotl-mode:electric-newline-and-maybe-indent 'kotl-mode:newline)
(defalias 'kotl-mode:newline-and-indent 'kotl-mode:newline)
(defalias 'kotl-mode:reindent-then-newline-and-indent 'kotl-mode:newline)

(defun kotl-mode:open-line (arg)
  "Insert a newline and leave point before it.
With ARG N, insert N newlines."
  (interactive "*p")
  (let* ((bolp (and (kotl-mode:bolp) (not (kotl-mode:bocp))))
	 (indent (kcell-view:indent))
	 (add-prefix (and (stringp fill-prefix)
			  (not (string-empty-p fill-prefix)))))
    (while (> arg 0)
      (save-excursion
        (insert ?\n)
	(if (and (not bolp) add-prefix)
	    (insert fill-prefix)
	  (insert-char ?\  indent)))
      (setq arg (1- arg)))
    (when (and bolp add-prefix)
      (delete-horizontal-space)
      (insert fill-prefix))))

(defun kotl-mode:quoted-insert (arg)
  "Read next input character and insert it.
This is useful for inserting control characters.
With argument, insert ARG copies of the character.

If the first character you type after this command is an octal digit,
you should type a sequence of octal digits which specify a character code.
Any nondigit terminates the sequence.  If the terminator is a RET,
it is discarded; any other terminator is used itself as input.
The variable `read-quoted-char-radix' specifies the radix for this feature;
set it to 10 or 16 to use decimal or hex instead of octal.

In overwrite mode, this function inserts the character anyway, and
does not handle octal digits specially.  This means that if you use
overwrite as your normal editing mode, you can use this function to
insert characters when necessary.

In binary overwrite mode, this function does overwrite, and octal
digits are interpreted as a character code.  This is intended to be
useful for editing binary files."
  (interactive "*p")
  (let* ((char
	  ;; Avoid "obsolete" warnings for translation-table-for-input.
	  (with-no-warnings
	    (let (translation-table-for-input input-method-function)
	      (if (or (not overwrite-mode)
		      (eq overwrite-mode 'overwrite-mode-binary))
		  (read-quoted-char)
		(read-char))))))
    (unless (characterp char)
      (user-error "%s is not a valid character"
		  (key-description (vector char))))
    (if (> arg 0)
	(if (eq overwrite-mode 'overwrite-mode-binary)
	    (kotl-mode:delete-char arg)))
    (while (> arg 0)
      (insert-and-inherit char)
      (setq arg (1- arg)))))

(defun kotl-mode:set-fill-prefix (turn-off)
  "Set fill prefix to line up to point.
With prefix arg TURN-OFF or at begin of line, turns fill prefix off."
  (interactive "P")
  (set-fill-prefix (or turn-off (kotl-mode:bolp))))

(defun kotl-mode:tab-command (arg)
  "Tab over by ARG tab stops or demote the current tree a maximum of ARG levels.
Which command is run depends on the value of `kotl-mode:tab-flag'.  Toggle
its value by sending this command an explicit ARG of 1.  Use nil for ARG to
run the tab command once.

See also the documentation strings for `kotl-mode:indent-line' and
`kotl-mode:demote-tree'."
  (interactive "*P")
  (if (eq arg 1)
      (call-interactively 'kotl-mode:toggle-tab-flag)
    (setq arg (prefix-numeric-value arg))
    (cond (kotl-mode:tab-flag (kotl-mode:indent-line arg))
	  (t (kotl-mode:demote-tree arg)))))

(defun kotl-mode:toggle-indent-tabs-mode ()
  "Toggle the value of `kotl-mode:indent-tabs-mode' and explain its current usage."
  (interactive)
  (setq kotl-mode:indent-tabs-mode (not kotl-mode:indent-tabs-mode))
  (if (called-interactively-p 'interactive)
      (if kotl-mode:indent-tabs-mode
	  (message "Tab insertion now uses literal tabs.")
	(message "Tab insertion now uses spaces to form tabs."))))

(defun kotl-mode:toggle-tab-flag ()
  "Toggle the value of `kotl-mode:tab-flag' and explain its current usage."
  (interactive)
  (setq kotl-mode:tab-flag (not kotl-mode:tab-flag))
  (if (called-interactively-p 'interactive)
      (if kotl-mode:tab-flag
	  (message (substitute-command-keys
		    "{\\[kotl-mode:tab-command]} now inserts literal tabs; {\\[kotl-mode:untab-command]} removes tabs."))
	(message (substitute-command-keys
		  "{\\[kotl-mode:tab-command]} now demotes trees; {\\[kotl-mode:untab-command]} promotes trees.")))))

(defun kotl-mode:transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix ARG, take character before point and drag it forward past ARG
other characters (backward if ARG negative).  If no prefix ARG and at
the end of a line, the previous two characters are exchanged."
  (interactive "*P")
  (if (or (kotl-mode:bolp)
	  (and (null arg) (kotl-mode:eolp)
	       (save-excursion (kotl-mode:forward-char -1) (kotl-mode:bolp))))
    (error "(kotl-mode:transpose-chars): Insufficient characters to transpose"))
  (and (null arg) (kotl-mode:eolp) (kotl-mode:forward-char -1))
  (transpose-subr 'kotl-mode:forward-char (prefix-numeric-value arg)))

(defun kotl-mode:transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
If no previous line, exchange current with next line.
With prefix ARG, take previous line and move it past ARG lines.
With prefix ARG = 0, interchange the line that contains point with the line
that contains mark."
  (interactive "*p")
  (cond
   ((and (kotl-mode:first-line-p) (kotl-mode:last-line-p))
    (error "(kotl-mode:transpose-lines): Only one line in outline"))
   ;;
   ;; Transpose current and previous lines or current and next lines, if no
   ;; previous line.  Leave point after both exchanged lines.
   ((= arg 1)
    (let* ((point (point-marker))
	   (mark (set-marker (make-marker)
			     (if (kotl-mode:first-line-p)
				 (kotl-mode:next-line 1)
			       (kotl-mode:previous-line 1)))))
      (kotl-mode:transpose-lines-internal point mark)
      (goto-char (max point mark))
      (kotl-mode:next-line 1)
      (set-marker mark nil)))
   ;;
   ;; Transpose point and mark lines, leaving point on the line of text that
   ;; originally contained point.
   ((= arg 0)
    (kotl-mode:transpose-lines-internal (point-marker) (mark-marker))
    (kotl-mode:exchange-point-and-mark))
   ;;
   ;; Move previous line past ARG next lines and leave point after previous
   ;; line text.
   (t
    (if (kotl-mode:first-line-p)
	(error "(kotl-mode:transpose-lines): No previous line to transpose"))
    (kotl-mode:previous-line 1)
    (let* ((mark (set-marker (make-marker)
			     (save-excursion (kotl-mode:next-line arg))))
	   (line-to-move (kotl-mode:delete-line)))
      (condition-case ()
	  ;; Delete trailing newline if any, ignoring error.
	  (kotl-mode:delete-char 1)
	(error nil))
      (goto-char mark)
      (set-marker mark nil)
      (kotl-mode:to-end-of-line)
      (insert "\n")
      (insert-char ?\  (kcell-view:indent))
      (insert line-to-move)
      (kotl-mode:beginning-of-line)))))

(defun kotl-mode:transpose-paragraphs (arg)
  "Interchange this (or next) paragraph with previous one."
  (interactive "*p")
  (transpose-subr 'kotl-mode:forward-paragraph (prefix-numeric-value arg)))

(defun kotl-mode:transpose-sentences (arg)
  "Interchange this (next) and previous sentence."
  (interactive "*p")
  (transpose-subr 'kotl-mode:forward-sentence (prefix-numeric-value arg)))

(defun kotl-mode:transpose-words (arg)
  "Interchange words around point, leaving point after both words.
With prefix ARG, take word before or around point and drag it forward past
ARG other words (backward if ARG negative).  If ARG is zero, the words around
or after point and around or after mark are interchanged."
  (interactive "*p")
  (transpose-subr 'kotl-mode:forward-word (prefix-numeric-value arg)))

(defun kotl-mode:untab-command (arg)
  "Delete backwards or promote the current tree.
Delete backwards by ARG tab stops or promote the current tree a
maximum of ARG levels.  Which command is run depends on the value
of `kotl-mode:tab-flag'.  Toggle its value by sending this
command an explicit ARG of 1.  Use nil for ARG to run the untab
command once.

See also the documentation strings for `kotl-mode:delete-backward-char' and
`kotl-mode:promote-tree'."
  (interactive "*P")
  (if (eq arg 1)
      (call-interactively 'kotl-mode:toggle-tab-flag)
    (setq arg (prefix-numeric-value arg))
    (cond (kotl-mode:tab-flag (kotl-mode:delete-backward-char arg))
	  (t (kotl-mode:promote-tree arg)))))

(defun kotl-mode:zap-to-char (arg char)
  "Kill up to and including prefix ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap to char within current cell: ")
  (kcell-view:operate
   (lambda () (zap-to-char arg char))))

;;; ------------------------------------------------------------------------
;;; Editing across kotls
;;; ------------------------------------------------------------------------

(defun kotl-mode:append-cell (contents-cell append-to-cell)
   "Append CONTENTS-CELL (a cell ref) to APPEND-TO-CELL (a cell ref).
APPEND-TO-CELL is refilled if neither cell has a no-fill property and
kotl-mode:refill-flag is enabled."
  (interactive
   (let* ((label (kcell-view:label)))
     (hargs:iform-read
      '(interactive
	"*+KAppend contents of cell: \n+KAppend contents of cell <%s> to cell: ")
      (list label label))))
  (save-excursion
    (kotl-mode:goto-cell contents-cell)
    (let ((contents (kcell-view:contents))
	  (no-fill (kcell-view:get-attr 'no-fill)))
      (kotl-mode:goto-cell append-to-cell)
      (if no-fill nil (setq no-fill (kcell-view:get-attr 'no-fill)))
      (goto-char (kcell-view:end-contents))
      (let ((fill-prefix (make-string (kcell-view:indent) ?\ )))
	(unless (kotl-mode:bolp)
	  ;; Append contents of cell beginning on its own line.
	  (insert "\n" fill-prefix))
	(kview:insert-contents (kcell-view:cell) contents
			       (or no-fill (null kotl-mode:refill-flag))
			       fill-prefix)))))

(defun kotl-mode:clipboard-yank ()
  "Insert the clipboard contents, or the last stretch of killed text."
  (interactive "*")
  (let ((select-enable-clipboard t))
    (kotl-mode:yank)))

(defun kotl-mode:copy-after (from-cell-ref to-cell-ref child-p)
  "Copy tree rooted at FROM-CELL-REF to follow tree rooted at TO-CELL-REF.
If prefix arg CHILD-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF, otherwise make it the sibling following TO-CELL-REF.

Leave point at the start of the root cell of the new tree."
  (interactive
   (let* ((label (kcell-view:label)))
     (append
      (hargs:iform-read
       (list
	'interactive
	(format "*+KCopy tree: \n+KCopy tree <%%s> to follow as %s of cell: "
		(if current-prefix-arg "child" "sibling")))
       (list label label))
      (list current-prefix-arg))))
  ;;
  ;; Copy tree in current view and leave point at the start of the copy.
  (goto-char (kotl-mode:move-after from-cell-ref to-cell-ref child-p t))
  ;; Alter the copied tree so each cell appears to be newly created.
  (kview:map-tree
   (lambda (view)
     (kcell-view:set-cell (kcell:create) (kview:id-increment view)))
   kview))

(defun kotl-mode:copy-before (from-cell-ref to-cell-ref parent-p)
  "Copy tree rooted at FROM-CELL-REF to precede tree rooted at TO-CELL-REF.
If prefix arg PARENT-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF's parent, otherwise make it the preceding sibling of TO-CELL-REF.

Leave point at the start of the root cell of the new tree."
  (interactive
   (let* ((label (kcell-view:label)))
     (append
      (hargs:iform-read
       (list 'interactive
	     (format "*+KCopy tree: \n+KCopy tree <%%s> to be %s of cell: "
		     (if current-prefix-arg "first child of parent"
		       "preceding sibling")))
       (list label label))
      (list current-prefix-arg))))
  ;;
  ;; Copy tree in current view and leave point at the start of the copy.
  (goto-char (kotl-mode:move-before from-cell-ref to-cell-ref parent-p t))
  ;; Alter the copied tree so each cell appears to be newly created.
  (kview:map-tree
   (lambda (view)
     (kcell-view:set-cell (kcell:create) (kview:id-increment view)))
   kview))

(defun kotl-mode:move-after (from-cell-ref to-cell-ref child-p
			     &optional copy-p fill-p)
  "Move tree rooted at FROM-CELL-REF to follow tree rooted at TO-CELL-REF.
If prefix arg CHILD-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF, otherwise make it the sibling following TO-CELL-REF.
With optional COPY-P, copies tree rather than moving it.

Leave point at original location but return the tree's new start point."
  (interactive
   (let* ((label (kcell-view:label)))
     (append
      (hargs:iform-read
       (list
	'interactive
	(format "*+KMove tree: \n+KMove tree <%%s> to follow as %s of cell: "
		(if current-prefix-arg "child" "sibling")))
	  (list label label))
      (list current-prefix-arg))))
  (if (and (not copy-p) (equal from-cell-ref to-cell-ref))
      (error "(kotl-mode:move-after): Can't move tree after itself"))
  (let* ((lbl-sep-len (kview:label-separator-length kview))
	 (move-to-point (set-marker
			 (make-marker)
			 (kotl-mode:goto-cell to-cell-ref t)))
	 (to-label (kcell-view:label))
	 (to-indent (kcell-view:indent nil lbl-sep-len))
	 (from-label (progn (kotl-mode:goto-cell from-cell-ref t)
			    (kcell-view:label)))
	 (from-indent (kcell-view:indent nil lbl-sep-len))
	 (start (kotl-mode:tree-start))
	 (end   (kotl-mode:tree-end))
	 (sib-id (when (= 0 (kotl-mode:forward-cell 1))
		   (kcell-view:idstamp)))
	 (id-label-flag (eq (kview:label-type kview) 'id))
	 new-tree-start)
    ;;
    ;; We can't move a tree to a point within itself, so if that is the case
    ;; and this is not a copy operation, signal an error.
    (when (and (not copy-p) (>= move-to-point start) (<= move-to-point end))
      (error "(kotl-mode:move-after): Can't move tree <%s> to within itself"
	     from-label))
    ;;
    ;; If tree to move has a sibling, point is now at the start of the
    ;; sibling cell.  Mark its label with a property which will be deleted
    ;; whenever the cell label is renumbered.  This tells us whether or not
    ;; to renumber the sibling separately from the tree to move.
    (when sib-id
      ;; Move to middle of label and insert klabel-original temp property.
      (goto-char (- (point) lbl-sep-len 3))
      (kproperty:set 'klabel-original t))
    ;;
    ;; Position for insertion before deletion of tree-to-move from old
    ;; position, in case old position precedes new one.
    ;; Skip past either cell or tree at move-to-point.
    (goto-char move-to-point)
    (if child-p
	;; Move to insert position for first child of to-cell-ref.
	(progn (goto-char (kcell-view:end))
	       (setq to-label (klabel:child to-label)
		     to-indent (+ to-indent (kview:level-indent kview))))
      ;; Move to after to-cell-ref's tree for insertion as following sibling.
      (goto-char (kotl-mode:tree-end))
      (unless id-label-flag
	(setq to-label (klabel:increment to-label))))
    ;;
    ;; Insert tree-to-move at new location
    ;;
    (kview:move start end (point) from-indent to-indent copy-p
		(or fill-p kotl-mode:refill-flag))
    ;;
    ;; Ensure that point is within editable region of cell with to-label.
    (kotl-mode:to-valid-position)
    (setq new-tree-start (point))
    ;;
    ;; Update current cell and new siblings' labels within view.
    (klabel-type:update-labels to-label)
    ;;
    (unless copy-p
      ;;
      ;; Move to sibling of tree-to-move within view and update labels within
      ;; view of tree-to-move's original siblings.
      (when sib-id
	(kotl-mode:goto-cell sib-id t)
	;; Sibling labels may have already been updated if tree was
	;; moved somewhere preceding its siblings.
	(let ((label-middle (- (point) lbl-sep-len 2)))
	  (when (kproperty:get label-middle 'klabel-original)
	    (klabel-type:update-labels from-label)))))
    ;;
    (goto-char new-tree-start)
    ;;
    ;; Ensure that point is within editable region of a cell.
    (kotl-mode:to-valid-position)
    ;;
    (set-marker move-to-point nil)
    new-tree-start))

(defun kotl-mode:move-before (from-cell-ref to-cell-ref parent-p
			      &optional copy-p fill-p)
  "Move tree rooted at FROM-CELL-REF to precede tree rooted at TO-CELL-REF.
If prefix arg PARENT-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF's parent, otherwise make it the preceding sibling of TO-CELL-REF.
With optional COPY-P, copies tree rather than moving it.

Leave point at original location but return the tree's new start point."
  (interactive
   (let* ((label (kcell-view:label)))
     (append
      (hargs:iform-read
       (list 'interactive
	     (format "*+KMove tree: \n+KMove tree <%%s> to be %s of cell: "
		     (if current-prefix-arg "first child of parent"
		       "preceding sibling")))
	  (list label label))
      (list current-prefix-arg))))
  (when (and (not copy-p) (equal from-cell-ref to-cell-ref))
    (error "(kotl-mode:move-before): Can't move tree before itself"))
  (let* ((lbl-sep-len (kview:label-separator-length kview))
	 (move-to-point (set-marker
			 (make-marker)
			 (kotl-mode:goto-cell to-cell-ref t)))
	 (to-label (kcell-view:label))
	 (to-indent (kcell-view:indent nil lbl-sep-len))
	 (from-label (progn (kotl-mode:goto-cell from-cell-ref t)
			    (kcell-view:label)))
	 (from-indent (kcell-view:indent nil lbl-sep-len))
	 (start (kotl-mode:tree-start))
	 (end   (kotl-mode:tree-end))
	 (sib-id (when (= 0 (kotl-mode:forward-cell 1))
		   (kcell-view:idstamp)))
	 new-tree-start)
    ;;
    ;; We can't move a tree to a point within itself, so if that is the case
    ;; and this is not a copy operation, signal an error.
    (when (and (not copy-p) (>= move-to-point start) (<= move-to-point end))
      (error "(kotl-mode:move-before): Can't move tree <%s> to within itself"
	     from-label))
    ;;
    ;; If tree to move has a sibling, point is now at the start of the
    ;; sibling cell.  Mark its label with a property which will be deleted
    ;; whenever the cell label is renumbered.  This tells us whether or not
    ;; to renumber the sibling separately from the tree to move.
    (when sib-id
      ;; Move to middle of label and insert klabel-original temp property.
      (goto-char (- (point) lbl-sep-len 3))
      (kproperty:set 'klabel-original t))
    ;;
    ;; Position for insertion at succeeding-tree, before deletion of
    ;; tree-to-move from old position, in case old position precedes new one.
    (goto-char move-to-point)
    (if parent-p
	;; Move to insert position for first child of to-cell-ref's parent.
	(if (kcell-view:parent nil lbl-sep-len)
	    (progn (setq to-label (klabel:child (kcell-view:label)))
		   (goto-char (kcell-view:end)))
	  (error "(kotl-mode:move-before): to-cell-ref's parent not in current view"))
      ;; Move to before to-cell-ref for insertion as preceding sibling.
      (goto-char (kotl-mode:tree-start)))
    ;;
    ;; Insert tree-to-move at new location
    ;;
    (kview:move start end (point) from-indent to-indent copy-p
		(or fill-p kotl-mode:refill-flag))
    ;;
    ;; Ensure that point is within editable region of root of tree just moved.
    (kotl-mode:to-valid-position)
    (setq new-tree-start (point))
    ;;
    ;; Update current cell and new siblings' labels within view.
    (klabel-type:update-labels to-label)
    ;;
    (unless copy-p
      ;;
      ;; Move to sibling of tree-to-move within view and update labels within
      ;; view of tree-to-move's original siblings.
      (when sib-id
	(kotl-mode:goto-cell sib-id t)
	;; Sibling labels may have already been updated if tree was
	;; moved somewhere preceding its siblings.
	(let ((label-middle (- (point) lbl-sep-len 2)))
	  (when (kproperty:get label-middle 'klabel-original)
	    (klabel-type:update-labels from-label)))))
    ;;
    (goto-char new-tree-start)
    ;;
    ;; Ensure that point is within editable region of a cell.
    (kotl-mode:to-valid-position)
    ;;
    (set-marker move-to-point nil)
    new-tree-start))

(defun kotl-mode:yank (&optional arg)
  "Reinsert (\"paste\") the last stretch of killed text.
More precisely, reinsert the most recent kill, which is the
stretch of killed text most recently killed OR yanked.  Put point
at the end, and set mark at the beginning without activating it.
With just \\[universal-argument] as argument, put point at beginning, and mark at end.
With argument N, reinsert the Nth most recent kill.

When this command inserts text into the buffer, it honors the
`yank-handled-properties' and `yank-excluded-properties'
variables, and the `yank-handler' text property.  See
`insert-for-yank-1' for details.

See also the command `yank-pop' (\\[yank-pop])."
  (interactive "*P")
  (setq yank-window-start (window-start))
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark (point))
  (let* ((yank-text (current-kill (cond
				   ((listp arg) 0)
				   ((eq arg '-) -1)
				   (t (1- arg)))))
	 (indent (kcell-view:indent))
	 (indent-str (make-string indent ?\ )))
    ;; Convert all occurrences of newline to newline + cell indent.
    ;; Then insert into buffer.
    (insert-for-yank (replace-regexp-in-string
		      "[\n\r]" (lambda (match) (concat match indent-str)) yank-text)))
  (when (consp arg) (kotl-mode:exchange-point-and-mark))
  ;; If we do get all the way thru, make this-command indicate that.
  (when (eq this-command t) (setq this-command 'kotl-mode:yank))
  nil)

(defun kotl-mode:yank-pop (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is allowed only immediately after a `yank' or a `yank-pop'.
At such a time, the region contains a stretch of reinserted
previously-killed text.  `yank-pop' deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one.

When this command inserts killed text into the buffer, it honors
`yank-excluded-properties' and `yank-handler' as described in the
doc string for `insert-for-yank-1', which see."
  (interactive "*p")
  (if (not (eq last-command 'kotl-mode:yank))
      (error "Previous command was not a yank"))
  (setq this-command 'kotl-mode:yank)
  (unless arg
    (setq arg 1))
  (let ((inhibit-read-only t)
	(before (< (point) (mark t))))
    (if before
	(funcall (or yank-undo-function 'delete-region) (point) (mark t))
      (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
    (set-marker (mark-marker) (point) (current-buffer))
    (let* ((yank-text (current-kill arg))
	   (indent (kcell-view:indent))
	   (indent-str (make-string indent ?\ )))
      ;; Convert all occurrences of newline to newline + cell indent.
      ;; Then insert into buffer.
      (insert-for-yank (replace-regexp-in-string
			"[\n\r]" (concat "\\0" indent-str) yank-text)))
    ;; Set the window start back where it was in the yank command,
    ;; if possible.
    (set-window-start (selected-window) yank-window-start t)
    (when before (kotl-mode:exchange-point-and-mark)))
  nil)

;;; ------------------------------------------------------------------------
;;; Movement
;;; ------------------------------------------------------------------------

(defalias 'kotl-mode:scroll-down-command 'kotl-mode:scroll-down)
(defalias 'kotl-mode:scroll-up-command  'kotl-mode:scroll-up)

(defun kotl-mode:back-to-indentation ()
  "Move point to the first non-read-only non-whitespace character on this line."
  (interactive)
  (kotl-mode:maintain-region-highlight)
  (back-to-indentation)
  (kotl-mode:to-valid-position))

(defun kotl-mode:backward-cell (arg)
  "Move to prefix ARGth prior visible cell (same level) within current view.
Return number of cells left to move."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (if (< arg 0)
      (kotl-mode:forward-cell (- arg))
    (let ((prior (= arg 0))
	  (lbl-sep-len (kview:label-separator-length kview)))
      (when (not (kview:valid-position-p))
        (progn
          (kotl-mode:to-valid-position t)
          (kotl-mode:beginning-of-cell)
          (setq arg (1- arg))
          (setq prior t)))
      (while (and (> arg 0) (setq prior (kcell-view:backward t lbl-sep-len)))
	(setq arg (1- arg)))
      (if (or prior (not (called-interactively-p 'interactive)))
	  arg
	(error "(kotl-mode:backward-cell): No prior cell at same level")))))

(defun kotl-mode:backward-char (&optional arg)
  "Move point backward ARG (or 1) characters and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((kotl-mode:bobp)
	       (error "(kotl-mode:backward-char): Beginning of buffer"))
	      ((kotl-mode:bocp)
	       (when (kcell-view:previous t)
		 (kotl-mode:end-of-cell)))
	      ((kotl-mode:bolp)
	       (when (re-search-backward "[\n\r]" nil t)
		 (kotl-mode:to-valid-position t)))
	      (t (backward-char)))
	(setq arg (1- arg)))
    (kotl-mode:forward-char (- arg)))
  (point))

(defun kotl-mode:backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With ARG N, do it N times; negative ARG -N means move forward N paragraphs.
Return point.

A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line.

See `forward-paragraph' for more information."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (kotl-mode:forward-paragraph (- arg)))

(defalias 'kotl-mode:backward-para 'kotl-mode:backward-paragraph)

(defun kotl-mode:backward-sentence (&optional arg)
  "Move point backward ARG (or 1) sentences and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (let* ((lbl-sep-len (kview:label-separator-length kview))
	 ;; Setting fill prefix makes sentence commands properly recognize
	 ;; indented paragraphs.
	 (fill-prefix (make-string (kcell-view:indent nil lbl-sep-len) ?\ )))
    (if (kotl-mode:bobp)
	(error "(kotl-mode:backward-sentence): First sentence")
      (when (and (kotl-mode:bocp) (kcell-view:previous nil lbl-sep-len))
	(goto-char (kcell-view:end-contents)))
      (unless arg
	(setq arg 1))
      (save-restriction
	(when (= arg 1)
	  (narrow-to-region
	   (- (kcell-view:start nil lbl-sep-len)
	      (kcell-view:indent nil lbl-sep-len))
	   (kcell-view:end-contents)))
	(unwind-protect
	    (let ((opoint (point)))
	      (backward-sentence arg)
	      (when (= opoint (point))
		(kcell-view:previous nil lbl-sep-len)
		(backward-sentence arg)))
	  (kotl-mode:to-valid-position t)))))
  (point))

(defun kotl-mode:backward-word (&optional arg)
  "Move point backward ARG (or 1) words and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((kotl-mode:bobp) (setq arg 0))
	      ((kotl-mode:bocp)
	       (if (kcell-view:previous t)
		   (kotl-mode:end-of-cell))))
	(unwind-protect
	    (backward-word 1)
	  (kotl-mode:to-valid-position t))
	(setq arg (1- arg)))
    (kotl-mode:forward-word (- arg)))
  (point))

(defun kotl-mode:beginning-of-buffer ()
  "Move point to beginning of buffer and return point."
  (interactive)
  (kotl-mode:maintain-region-highlight)
  (goto-char (point-min))
  (when (= (point) 1)
    ;; Buffer is expanded past editable area, reset narrowing via kotl-mode.
    (kotl-mode))
  ;; Move to cell start.
  (goto-char (kcell-view:start)))

(defun kotl-mode:beginning-of-cell (&optional arg)
  "Move point to beginning of current or ARGth - 1 prior cell and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (unless (integer-or-marker-p arg)
    (error "(kotl-mode:beginning-of-cell): Wrong type arg, integer-or-marker, `%s'" arg))
  (if (= arg 1)
      (goto-char (kcell-view:start))
    (kotl-mode:backward-cell (1- arg)))
  (point))

(defun kotl-mode:beginning-of-line (&optional arg)
  "Move point to beginning of current or ARGth - 1 line and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (unless (integer-or-marker-p arg)
    (error "(kotl-mode:beginning-of-line): Wrong type arg, integer-or-marker, `%s'" arg))
  (kfill:forward-line (1- arg))
  (unless (eolp)
    (forward-char (prog1 (kcell-view:indent)
		    (beginning-of-line))))
  (point))

;;; This ensures that the key bound to `beginning-of-line' is replaced in kotl-mode.
(defalias 'kotl-mode:beginning-of-visual-line 'kotl-mode:beginning-of-line)
(defalias 'kotl-mode:move-beginning-of-line 'kotl-mode:beginning-of-line)

(defun kotl-mode:beginning-of-tree ()
  "Move point to the level 1 root of the current cell's tree.
Leave point at the start of the cell."
  (interactive)
  (kotl-mode:maintain-region-highlight)
  (let ((lbl-sep-len (kview:label-separator-length kview)))
    (when (/= (kcell-view:level nil lbl-sep-len) 1)
      ;; Enable user to return to this previous position if desired.
      (push-mark nil 'no-msg))
    (while (and (/= (kcell-view:level nil lbl-sep-len) 1)
		(kcell-view:parent nil lbl-sep-len)))
    (kotl-mode:beginning-of-cell)))

(defun kotl-mode:down-level (arg)
  "Move down prefix ARG levels lower within current tree."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (if (< arg 0)
      (kotl-mode:up-level (- arg))
    ;; Enable user to return to this previous position if desired.
    (push-mark nil 'no-msg)
    (let ((child))
      (while (and (> arg 0) (kcell-view:child))
	(unless child
	  (setq child t))
	(setq arg (1- arg)))
      ;; Signal an error if couldn't move down at least 1 child level.
      (or child
	  (progn
	    (goto-char (mark t))
	    (pop-mark)
	    (error "(kotl-mode:down-level): No child level to which to move"))))))

(defun kotl-mode:end-of-buffer ()
  "Move point to end of buffer and return point."
  (interactive)
  (kotl-mode:maintain-region-highlight)
  (goto-char (point-max))
  (when (looking-back "\\]\\s-*" nil)
    ;; Internal Koutline structures are exposed, re-narrow the Koutline
    (kotl-mode))
  ;; To move to cell end.
  (kotl-mode:to-valid-position t)
  (point))

(defun kotl-mode:end-of-cell (&optional arg)
  "Move point to the end of the current cell and return point.
With optional ARG > 0, move to the ARGth - 1 next visible cell.
With optional ARG < 0, move to the ARGth previous visible cell."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (unless (integer-or-marker-p arg)
    (error "(kotl-mode:end-of-cell): Wrong type arg, integer-or-marker, `%s'" arg))
  (kotl-mode:next-cell (if (> arg 0) (1- arg) arg))
  (goto-char (kcell-view:end-contents)))

;;; Avoid XEmacs byte-compiler bug which inserts nil for calls to this
;;; function if named kotl-mode:end-of-line.
;;;
(defun kotl-mode:to-end-of-line (&optional arg)
  "Move point to end of current or ARGth - 1 line and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (unless (integer-or-marker-p arg)
      (error "(kotl-mode:to-end-of-line): Wrong type arg, integer-or-marker, `%s'" arg))
  (kfill:forward-line (1- arg))
  (end-of-visible-line)
  ;; May have to move backwards to before label if support labels
  ;; at end of cells.
  (point))

;;; This ensures that the key bound to `end-of-line' is replaced in kotl-mode.
(defalias 'kotl-mode:end-of-line 'kotl-mode:to-end-of-line)
(defalias 'kotl-mode:end-of-visual-line 'kotl-mode:to-end-of-line)
(defalias 'kotl-mode:move-end-of-line 'kotl-mode:to-end-of-line)

(defun kotl-mode:end-of-tree ()
  "Move point to the start of the last cell in tree rooted at the current cell."
  (interactive)
  (kotl-mode:maintain-region-highlight)
  ;; Enable user to return to this previous position if desired.
  (push-mark nil 'no-msg)
  (let ((lbl-sep-len (kview:label-separator-length kview)))
    (if (kcell-view:forward nil lbl-sep-len)
	;; Move to cell preceding start of next tree.
	(kcell-view:previous nil lbl-sep-len)
      ;; Otherwise, no next tree, so move until find last cell in tree.
      (let ((cell-indent (kcell-view:indent nil lbl-sep-len))
	    (end-point (point)))
	;; Terminate when no further cells or when reach a cell at an equal
	;; or higher level in the outline than the first cell that we
	;; processed.
	(while (and (kcell-view:next nil lbl-sep-len)
		    (>= (- (kcell-view:indent nil lbl-sep-len) cell-indent)
			(kview:level-indent kview)))
	  (setq end-point (point)))
	(goto-char end-point)))
    (kotl-mode:beginning-of-cell)))

(defun kotl-mode:first-sibling ()
  "Move point to the first sibling of the present cell.
Leave point at the start of the cell or at its present position if it is
already within the first sibling cell."
  (interactive)
  (kotl-mode:maintain-region-highlight)
  (let ((lbl-sep-len (kview:label-separator-length kview)))
    (when (save-excursion (kcell-view:backward nil lbl-sep-len))
	;; Enable user to return to this previous position if desired.
      (push-mark nil 'no-msg))
    (while (kcell-view:backward nil lbl-sep-len))))

(defun kotl-mode:forward-cell (arg)
  "Move to prefix ARGth following cell (same level) within current view.
Return number of cells left to move."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (if (< arg 0)
      (kotl-mode:backward-cell (- arg))
    (let ((next (= arg 0))
	  (lbl-sep-len (kview:label-separator-length kview)))
      (while (and (> arg 0) (setq next (kcell-view:forward t lbl-sep-len)))
	(setq arg (1- arg)))
      (if (or next (not (called-interactively-p 'interactive)))
	  arg
	(error "(kotl-mode:forward-cell): No following cell at same level")))))

(defun kotl-mode:forward-char (&optional arg)
  "Move point forward ARG (or 1) characters and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((and (kotl-mode:eolp) (kotl-mode:last-line-p))
	       (error "(kotl-mode:forward-char): End of buffer"))
	      ((kotl-mode:eocp)
	       (kcell-view:next t))
	      ((kotl-mode:eolp)
	       (forward-char)
	       (kotl-mode:to-visible-position))
	      (t (forward-char)))
	(setq arg (1- arg)))
    (kotl-mode:backward-char (- arg)))
  (point))

(defun kotl-mode:forward-paragraph (&optional arg)
  "Move point forward until after the last character of the current paragraph.
With ARG N, do it N times; negative ARG -N means move backward N paragraphs.
Return point.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is one character before the beginning of a line which is not
part of the paragraph, or the end of the buffer."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (if (< arg 0)
      (progn
	(if (kotl-mode:bocp) (setq arg (1- arg)))
	(while (< arg 0)
	  (start-of-paragraph-text)
	  (setq arg (1+ arg))))
    (while (> arg 0)
      (end-of-paragraph-text)
      (setq arg (1- arg))))
  (kotl-mode:to-valid-position)
  (point))

(defalias 'kotl-mode:forward-para 'kotl-mode:forward-paragraph)

(defun kotl-mode:forward-sentence (&optional arg)
  "Move point forward ARG (or 1) sentences and return point."
  (interactive "P")
  (kotl-mode:maintain-region-highlight)
  (let* ((lbl-sep-len (kview:label-separator-length kview))
	 ;; Setting fill prefix makes sentence commands properly recognize
	 ;; indented paragraphs.
	 (fill-prefix (make-string (kcell-view:indent nil lbl-sep-len) ?\ )))
    (if (kotl-mode:eobp)
	(error "(kotl-mode:forward-sentence): Last sentence")
      (if (kotl-mode:eocp) (kcell-view:next nil lbl-sep-len))
      (unless arg
	(setq arg 1))
      (save-restriction
	(when (= arg 1)
	  (narrow-to-region
	   (- (kcell-view:start nil lbl-sep-len)
	      (kcell-view:indent nil lbl-sep-len))
	   (kcell-view:end-contents)))
	(unwind-protect
	    (let ((opoint (point)))
	      (forward-sentence arg)
	      (when (= opoint (point))
		(kcell-view:next nil lbl-sep-len)
		(forward-sentence arg)))
	  (kotl-mode:to-valid-position)))))
  (point))

(defun kotl-mode:forward-word (&optional arg)
  "Move point forward ARG (or 1) words and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (unless arg
    (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((kotl-mode:eobp) (setq arg 0))
	      ((kotl-mode:eocp)
	       (kcell-view:next t)))
	(unwind-protect
	    (forward-word 1)
	  (kotl-mode:to-valid-position))
	;; If point is at beginning of a cell after moving forward a word,
	;; then we moved over something other than a word (some
	;; punctuation or an outline autonumber); therefore, leave counter as
	;; is in order to move forward over next word.
	(unless (kotl-mode:bocp)
	  (setq arg (1- arg))))
    (kotl-mode:backward-word (- arg)))
  (point))

(defun kotl-mode:goto-cell (cell-ref &optional error-flag)
  "Move point to start of cell text given by CELL-REF (see `kcell:ref-to-id').
Return point if CELL-REF is found within current view, else nil.
See the doc for `kcell:ref-to-id', for valid formats.

With optional second arg ERROR-FLAG non-nil, or if called
interactively, will signal an error if CELL-REF is not found
within current view.

When called interactively, if a prefix argument is given, use
that value as CELL-REF and search for it as a permanent idstamp
regardless of cell label type; without a prefix argument, prompt
for CELL-REF."
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   (read-string "Goto cell label or id: "))))
  (setq cell-ref
	(or (kcell:ref-to-id cell-ref t)
	    (error "(kotl-mode:goto-cell): Invalid cell reference, `%s'" cell-ref)))
  (let* ((opoint (point))
	 (found))
    (if (and (stringp cell-ref) (eq ?| (aref cell-ref 0)))
	;; This is a standalone view spec, not a cell reference.
	(progn (kvspec:activate cell-ref) (setq found (point)))
      (cl-destructuring-bind (cell-id kvspec) (kcell:parse-cell-ref cell-ref)
	(goto-char (point-min))
	(when (or (integerp cell-id)
		  (eq ?0 (aref cell-id 0)))
	  ;; Is an idstamp
	  (when (kview:goto-cell-id cell-id)
	    (setq found (point))))
	(if found
	    ;; Activate any viewspec associated with cell-ref.
	    (when kvspec (kvspec:activate kvspec))
	  (goto-char opoint)
	  (when (or error-flag (called-interactively-p 'interactive))
	    (error "(kotl-mode:goto-cell): No `%s' cell in this view" cell-ref)))))
    found))

(defun kotl-mode:head-cell ()
  "Move point to the start of first visible cell at same level as current cell.
If at head cell already, do nothing and return nil."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (let ((moved)
	(lbl-sep-len (kview:label-separator-length kview)))
    (while (kcell-view:backward t lbl-sep-len)
      (setq moved t))
    moved))

(defun kotl-mode:last-sibling ()
  "Move point to the last sibling of the present cell.
Leave point at the start of the cell or at its present position if it is
already within the last sibling cell."
  (interactive)
  (kotl-mode:maintain-region-highlight)
  (let ((lbl-sep-len (kview:label-separator-length kview)))
    (when (save-excursion (kcell-view:forward nil lbl-sep-len))
      ;; Enable user to return to this previous position if desired.
      (push-mark nil 'no-msg))
    (while (kcell-view:forward nil lbl-sep-len))))

(defun kotl-mode:mark-paragraph ()
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point."
  (interactive)
  (forward-paragraph 1)
  (kotl-mode:to-valid-position t)
  (push-mark nil t t)
  (backward-paragraph 1)
  (kotl-mode:to-valid-position))

(defun kotl-mode:mark-whole-buffer ()
  "Put point at first editable character in buffer and mark at last such character."
  (interactive)
  (push-mark (point))
  (kotl-mode:end-of-buffer)
  (push-mark (point) nil t)
  (kotl-mode:beginning-of-buffer))

(defun kotl-mode:next-cell (arg)
  "Move to prefix ARGth next cell (any level) within current view."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (if (< arg 0)
      (kotl-mode:previous-cell (- arg))
    (let ((next (= arg 0))
	  (lbl-sep-len (kview:label-separator-length kview)))
      (while (and (> arg 0) (setq next (kcell-view:next t lbl-sep-len)))
	(setq arg (1- arg)))
      (if next
	  arg
	(error "(kotl-mode:next-cell): Last cell")))))

(defun kotl-mode:next-line (arg)
  "Move point to ARGth next line and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (kotl-mode:set-temp-goal-column)
  (let ((orig-arg arg))
    (cond ((> arg 0)
	   (while (and (> arg 0) (= 0 (kfill:forward-line 1)))
	     (cond ((kotl-mode:eobp)
		    (kfill:forward-line -1)
		    (goto-char (kcell-view:end-contents))
		    (and (called-interactively-p 'interactive) (= orig-arg arg)
			 (message "(kotl-mode:next-line): End of buffer") (beep))
		    (setq arg 0))
		   ;; Visible blank line between cells
		   ((and (looking-at "^$") (not (eq (kproperty:get (point) 'invisible) t)))
		    nil) ;; Don't count this line.
		   (t (setq arg (1- arg)))))
	   (kotl-mode:line-move 0)
	   (kotl-mode:to-valid-position))
	  ((< arg 0)
	   (kotl-mode:previous-line (- arg)))))
  (setq this-command 'next-line)
  (point))

(defun kotl-mode:next-tree ()
  "Move past current tree to the start of the next tree.
If no next tree go to the start of the last cell in tree.  Return
non-nil iff there is a next tree within the koutline."
  (let ((start-indent (kcell-view:indent))
	(lbl-sep-len (kview:label-separator-length kview))
	(same-tree t))
      (while (and (kcell-view:next nil lbl-sep-len)
		  (setq same-tree (>= (- (kcell-view:indent nil lbl-sep-len) start-indent)
				      (kview:level-indent kview)))))
      (not same-tree)))

(defun kotl-mode:previous-line (arg)
  "Move point to ARGth previous line and return point."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (kotl-mode:set-temp-goal-column)
  (let ((orig-arg arg))
    (cond ((> arg 0)
	   (while (and (> arg 0) (= 0 (kfill:forward-line -1)))
	     (cond ((kotl-mode:bobp)
		    (kotl-mode:beginning-of-cell)
		    (and (called-interactively-p 'interactive) (= orig-arg arg)
			 (message "(kotl-mode:previous-line): Beginning of buffer") (beep))
		    (setq arg 0))
		   ;; Visible blank line between cells
		   ((and (looking-at "^$") (not (eq (kproperty:get (point) 'invisible) t)))
		    nil) ;; Don't count this line.
		   (t (setq arg (1- arg)))))
	   (kotl-mode:line-move 0)
	   (kotl-mode:to-valid-position))
	  ((< arg 0)
	   (kotl-mode:next-line (- arg)))))
  (setq this-command 'previous-line)
  (point))

(defun kotl-mode:previous-cell (arg)
  "Move to prefix ARGth previous cell (any level) within current view."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (if (< arg 0)
      (kotl-mode:next-cell (- arg))
    (let ((previous (= arg 0))
	  (lbl-sep-len (kview:label-separator-length kview)))
      (when (not (kview:valid-position-p))
        (progn
          (kotl-mode:to-valid-position t)
          (kotl-mode:beginning-of-cell)
          (setq arg (1- arg))
          (setq previous t)))
      (while (and (> arg 0) (setq previous
				  (kcell-view:previous t lbl-sep-len)))
	(setq arg (1- arg)))
      (if previous
	  arg
	(error "(kotl-mode:previous-cell): First cell")))))

(defun kotl-mode:scroll-down (&optional arg)
  "Scroll text of current window downward ARG lines; or a windowful if no ARG."
  (interactive "P")
  (if (eq major-mode 'kotl-mode)
      (progn (kotl-mode:maintain-region-highlight)
	     (scroll-down arg)
	     (kotl-mode:to-valid-position t))
    (scroll-down arg)))

(defun kotl-mode:scroll-up (&optional arg)
  "Scroll text of current window upward ARG lines; or a windowful if no ARG."
  (interactive "P")
  (if (eq major-mode 'kotl-mode)
      (progn (kotl-mode:maintain-region-highlight)
	     (scroll-up arg)
	     (kotl-mode:to-valid-position))
    (scroll-up arg)))

(defun kotl-mode:tail-cell ()
  "Move point to start of last visible cell at same level as current cell.
Return t if successfull.
If at tail cell already, do nothing and return nil."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (let ((moved)
	(lbl-sep-len (kview:label-separator-length kview)))
    (while (kcell-view:forward t lbl-sep-len)
      (setq moved t))
    moved))

(defun kotl-mode:up-level (arg)
  "Move up prefix ARG levels higher in current outline view."
  (interactive "p")
  (kotl-mode:maintain-region-highlight)
  (if (< arg 0)
      (kotl-mode:down-level (- arg))
    ;; Enable user to return to this previous position if desired.
    (push-mark nil 'no-msg)
    (let ((parent)
	  (lbl-sep-len (kview:label-separator-length kview))
	  result)
      (while (and (> arg 0) (setq result (kcell-view:parent t lbl-sep-len)))
	(or parent (setq parent result))
	(setq arg (if (eq result 0) 0 (1- arg))))
      ;; Signal an error if couldn't move up at least 1 parent level.
      (or (and parent (not (eq parent 0)))
	  (progn
	    (goto-char (mark t))
	    (pop-mark)
	    (error "(kotl-mode:up-level): No parent level to which to move"))))))


;;; ------------------------------------------------------------------------
;;; Predicates
;;; ------------------------------------------------------------------------

(defun kotl-mode:bobp ()
  "Return point if at the start of the first cell in kview, else nil."
  (interactive)
  (or (bobp)
      (and (not (save-excursion (re-search-backward "[\n\r]" nil t)))
	   (kotl-mode:bolp))))

(defun kotl-mode:bocp ()
  "Return point if at beginning of a kcell, else nil."
  (and (kotl-mode:bolp)
       (let ((begin-point (kcell-view:plist-point))
	     (bol))
	 (when begin-point
	   (save-excursion
	     ;; If first line-begin is less than cell begin point,
	     ;; then we know we are on the first line of the cell.
	     (when (setq bol (re-search-backward "^" nil t))
	       (<= bol begin-point)))))
       (point)))

(defun kotl-mode:bolp ()
  "Return point if at beginning of a kview line, else nil."
  (when (= (current-column) (kcell-view:indent))
    (point)))

(defun kotl-mode:buffer-empty-p ()
  "Return non-nil iff there are no outline cells within current buffer."
  (save-excursion
    (goto-char (point-min))
    (looking-at "[\n\r]*\\'")))

(defun kotl-mode:eobp ()
  "Return point if after the end of the last cell in kview, else nil."
  (interactive)
  (when (looking-at "^[\n\r]*\\'") (point)))

(defun kotl-mode:eocp ()
  "Return point if in a visible position at the end of a kview cell, else nil."
  (when (or (smart-eobp)
	    (looking-at "[\n\r]+\\'")
	    (when (kotl-mode:eolp)
	      (save-excursion
		(skip-chars-forward "\n\r")
		(kotl-mode:beginning-of-line)
		(kotl-mode:bocp))))
    (point)))

(defun kotl-mode:eolp (&optional next-char-visible)
  "Return t if point is at the end of a visible line or the end of the buffer.
With optional NEXT-CHAR-VISIBLE, return t only if the following char is visible."
  (or (smart-eobp)
      (and (eolp)
	   (if next-char-visible
	       (not (kview:char-invisible-p))
	     (or (not (kview:char-invisible-p))
		 (not (kview:char-invisible-p (1- (point))))))
	   t)))

(defun kotl-mode:first-cell-p ()
  "Return t iff point is on the first cell of the outline."
  (save-excursion (not (kcell-view:previous))))

(defalias 'kotl-mode:first-line-p 'first-line-p)

(defun kotl-mode:last-cell-p ()
  "Return t iff point is on the last cell of the outline."
  (save-excursion (not (kcell-view:next))))

(defun kotl-mode:last-line-p ()
  "Return t iff point is on the last line of the outline."
  (save-excursion
    (kotl-mode:to-end-of-line)
    (looking-at "\n*\\'")))

;;; ------------------------------------------------------------------------
;;; Smart Key Support
;;; ------------------------------------------------------------------------


(defun kotl-mode:action-key ()
  "Collapses, expands, links to, and scrolls through koutline cells.
Invoked via a key press when in kotl-mode.  It assumes that its caller has
already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed:
 (1) at the end of buffer, uncollapse and unhide all cells in view;
 (2) within a cell, if its subtree is hidden then show it,
     otherwise hide it;
 (3) between cells or within the read-only indentation region to the left of
     a cell, then move point to prior location and begin creation of a
     klink to some other outline cell; press the Action Key twice to select the
     link referent cell;
 (4) anywhere else, invoke `action-key-eol-function', typically to scroll up
     a windowful."
  (interactive)
  (cond	((kotl-mode:eobp) (kotl-mode:show-all))
	((kotl-mode:eolp t) (funcall action-key-eol-function))
	((not (kview:valid-position-p))
	 (if (markerp action-key-depress-prev-point)
	     (progn (select-window
		     (get-buffer-window
		      (marker-buffer action-key-depress-prev-point)))
		    (goto-char (marker-position action-key-depress-prev-point))
		    (call-interactively 'klink:create))
	   (kotl-mode:to-valid-position)
	   (error "(kotl-mode:action-key): Action Key released at invalid position")))
	((and (/= (point) (point-max)) (= (following-char) ?|)
	      (or (org-at-table-p t) (looking-at "[| \t]+$")))
	 ;; On a | separator in a table, toggle Org table minor mode
	 (orgtbl-mode 'toggle)
	 (message "Org table minor mode %s" (if orgtbl-mode "enabled" "disabled")))
	((org-at-table-p t)
	 ;; Wrap the table cell or region
	 (org-table-wrap-region current-prefix-arg))
	(t ;; On a cell line (not at the end of line).
	 (if (smart-outline-subtree-hidden-p)
	     (kotl-mode:show-tree)
	   (kotl-mode:hide-tree))))
  (kotl-mode:to-valid-position))

(defun kotl-mode:assist-key ()
  "Displays properties of koutline cells, collapses all cells, and scrolls back.
Invoked via an assist-key press when in kotl-mode.  It assumes that its caller
has already checked that the assist-key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) at the end of buffer, collapse all cells and hide all non-level-one
     cells;
 (2) on a header line but not at the beginning or end, display properties of
     each cell in tree beginning at point;
 (3) between cells or within the read-only indentation region to the left of
     a cell, then move point to prior location and prompt to move one tree to
     a new location in the outline; press the Action Key twice to select the
     tree to move and where to move it;
 (4) anywhere else, invoke `assist-key-eol-function', typically to scroll down
     a windowful."
  (interactive)
  (cond ((kotl-mode:eobp) (kotl-mode:overview))
	((kotl-mode:eolp t) (funcall assist-key-eol-function))
	((not (kview:valid-position-p))
	 (if (markerp assist-key-depress-prev-point)
	     (progn (select-window
		     (get-buffer-window
		      (marker-buffer assist-key-depress-prev-point)))
		    (goto-char (marker-position
				assist-key-depress-prev-point))
		    (call-interactively 'kotl-mode:move-after))
	   (kotl-mode:to-valid-position)
	   (error "(kotl-mode:assist-key): Help Key released at invalid position")))
	((not (bolp))
	 ;; On an outline header line but not at the start/end of line,
	 ;; show attributes for tree at point.
	 (kotl-mode:cell-help (kcell-view:label) (or current-prefix-arg 2)))
	((smart-scroll-down)))
  (kotl-mode:to-valid-position))

;;; ------------------------------------------------------------------------
;;; Structure Editing
;;; ------------------------------------------------------------------------

(defun kotl-mode:add-child ()
  "Add a new cell to current kview as first child of current cell."
  (interactive "*")
  (kotl-mode:add-cell '(4)))

(defun kotl-mode:add-parent ()
  "Add a new cell to current kview as sibling of current cell's parent."
  (interactive "*")
  (kotl-mode:add-cell -1))

(defun kotl-mode:add-cell (&optional relative-level contents plist no-fill)
  "Add a cell.
Add cell following current cell at optional RELATIVE-LEVEL with
CONTENTS string, attributes in PLIST, a property list, and
NO-FILL flag to prevent any filling of CONTENTS.

Optional prefix arg RELATIVE-LEVEL means either:

 1. add as the next sibling if nil or >= 0;
 2. as the first child if equal to (4), given by the universal argument, {C-u};
 3. otherwise, as the first sibling of the current cell's parent.

If added as the next sibling of the current level, then RELATIVE-LEVEL is
used as a repeat count for the number of cells to add.

Return last newly added cell."
  (interactive "*P")
  (or (stringp contents) (setq contents nil))
  (let ((klabel (kcell-view:label))
	(lbl-sep-len (kview:label-separator-length kview))
	cell-level new-cell sibling-p child-p start parent
	cells-to-add)
    (setq cell-level (kcell-view:level nil lbl-sep-len)
	  child-p (equal relative-level '(4))
	  sibling-p (and (not child-p)
			 (cond ((not relative-level) 1)
			       ((>= (prefix-numeric-value relative-level) 0)
				(prefix-numeric-value relative-level))))
	  cells-to-add (or sibling-p 1))
    (if child-p
	(setq cell-level (1+ cell-level))
      (unless sibling-p
	;; Add as following sibling of current cell's parent.
	;; Move to parent.
	(setq cell-level (1- cell-level)
	      start (point)
	      parent (kcell-view:parent nil lbl-sep-len))
	(unless (eq parent t)
	  (goto-char start)
	  (error
	   "(kotl-mode:add-cell): No higher level at which to add cell")))
      ;; Skip from point past any children to next cell.
      (when (kotl-mode:next-tree)
	;; If found a new tree, then move back to prior cell so can add
	;; new cell after it.
	(kcell-view:previous nil lbl-sep-len)))
    (goto-char (kcell-view:end))
    ;;
    ;; Insert new cells into view.
    (if (= cells-to-add 1)
	(setq klabel
	      (cond (sibling-p
		     (klabel:increment klabel))
		    (child-p
		     (kview:id-increment kview)
		     (klabel:child klabel))
		    ;; add as sibling of parent of current cell
		    (t (klabel:increment (klabel:parent klabel))))
	      new-cell (kview:add-cell klabel cell-level contents plist
				       no-fill sibling-p))
      ;;
      ;; sibling-p must be true if we are looping here so there is no need to
      ;; conditionalize how to increment the labels.
      (while (>= (setq cells-to-add (1- cells-to-add)) 0)
	(setq klabel (klabel:increment klabel)
	      ;; Since new cells are at the same level as old one, don't fill
	      ;; any of their intial contents.
	      new-cell (kview:add-cell klabel cell-level contents plist t))))
    ;;
    ;; Move back to last inserted cell and then move to its following
    ;; sibling if any.
    (kotl-mode:to-valid-position t)
    (save-excursion
      (when (kcell-view:forward t lbl-sep-len)
	(let ((label-type (kview:label-type kview)))
	  (when (memq label-type '(alpha legal partial-alpha))
	    ;; Update the labels of these siblings and their subtrees.
	    (klabel-type:update-labels (klabel:increment klabel))))))
    ;;
    ;; Leave point within last newly added cell and return this cell.
    (kotl-mode:beginning-of-cell)
    new-cell))

(defun kotl-mode:demote-tree (arg)
  "Move current tree a maximum of prefix ARG levels lower in current view.
Each cell is refilled iff its `no-fill' attribute is nil and
kotl-mode:refill-flag is non-nil.  With prefix ARG = 0, cells are demoted up
to one level and kotl-mode:refill-flag is treated as true."
  (interactive "*p")
  (if (< arg 0)
      (kotl-mode:promote-tree (- arg))
    (let* ((lbl-sep-len (kview:label-separator-length kview))
	   (orig-id (kcell-view:idstamp))
	   (fill-p (= arg 0))
	   (orig-pos-in-cell
	    (- (point) (kcell-view:start (point) lbl-sep-len)))
	   start-level start-point prev prev-level)
      ;; Next line ensures point is in the root of the current tree if
      ;; the tree is at all hidden.
      (kotl-mode:beginning-of-line)
      (setq start-point (point)
	    start-level (kcell-view:level start-point lbl-sep-len))
      (when fill-p
	(setq arg 1))
      (unwind-protect
	  (progn
	    (backward-char 1)
	    (while (and (> arg 0)
			(setq prev
			      (kcell-view:previous nil lbl-sep-len)))
	      (when prev
		(setq prev-level
		      (kcell-view:level (point) lbl-sep-len))
		(cond ((> prev-level (+ start-level arg))
		       ;; Don't want to demote this far
		       ;; so keep looking at prior nodes.
		       nil)
		      ((= arg (- prev-level start-level))
		       ;; Demote to be sibling of this kcell.
		       (setq arg -1))
		      ((< prev-level start-level)
		       ;; prev is at higher level then
		       ;; orig, so can't demote
		       (setq prev nil
			     arg 0))
		      (t
		       ;; Demote below this kcell.  This is
		       ;; as far we can demote, though it may
		       ;; not be the full amount of arg.
		       (setq arg 0)))))
	    (when prev
	      (kotl-mode:move-after
	       (kcell-view:label start-point)
	       (kcell-view:label) (= arg 0)
	       nil fill-p)))
	;; Move to start of original cell
	(kotl-mode:goto-cell orig-id)
	;; Move to original pos within cell
	(forward-char (min orig-pos-in-cell
			   (- (kcell-view:end-contents)
			      (kcell-view:start))))
	(kotl-mode:to-valid-position))
      (unless prev
	(error "(kotl-mode:demote-tree): Cannot demote any further")))))

(defun kotl-mode:exchange-cells (cell-ref-1 cell-ref-2)
  "Exchange CELL-REF-1 with CELL-REF-2 in current view.  Don't move point."
  (interactive
   (hargs:iform-read
    '(interactive "*+KExchange cell: \n+KExchange cell <%s> with cell: ")
    (save-excursion
      (list (kcell-view:label)
	    (cond
	     ((kcell-view:previous t)
	      (kcell-view:label))
	     ((kcell-view:next t)
	      (kcell-view:label))
	     (t (error
		 "(kotl-mode:exchange-cells): No two visible cells available")))))))
  (unless (and (or (stringp cell-ref-1) (natnump cell-ref-1))
	       (or (stringp cell-ref-2) (natnump cell-ref-2)))
    (error "(kotl-mode:exchange-cells): Cell refs must be either strings or numbers >= 0, not: '%s' and '%s'"
	   cell-ref-1 cell-ref-2))
  (when (equal cell-ref-1 cell-ref-2)
    (error "(kotl-mode:exchange-cells): Cannot exchange as both cell refs are the same: '%s' and '%s'"
	   cell-ref-1 cell-ref-2))
  (save-excursion
    (let (kcell-1 contents-1 idstamp-1
	  kcell-2 contents-2 idstamp-2)
      ;;
      ;; Save cell-1 attributes
      (kotl-mode:goto-cell cell-ref-1 t)
      (setq kcell-1 (kcell-view:cell)
	    idstamp-1 (kcell-view:idstamp-integer)
	    contents-1 (kcell-view:contents))
      ;;
      ;; Save cell-2 attributes
      (kotl-mode:goto-cell cell-ref-2 t)
      (setq kcell-2 (cl-copy-list (kcell-view:cell))
	    idstamp-2 (kcell-view:idstamp-integer)
	    contents-2 (kcell-view:contents))

      ;; Substitute cell-1 contents into cell-2 location.
      (delete-region (kcell-view:start) (kcell-view:end-contents))
      (insert
       (replace-regexp-in-string
	"\\([\n\r]\\)"
	(concat "\\1" (make-string (kcell-view:indent) ?\ )) contents-1))
      (when kotl-mode:refill-flag
	(kotl-mode:fill-cell))

      ;; Substitute cell-2 contents into cell-1 location.
      (kotl-mode:goto-cell cell-ref-1 t)
      ;; Exchange cell contents.
      (delete-region (kcell-view:start) (kcell-view:end-contents))
      ;; Add indentation to all but first line.
      (insert
       (replace-regexp-in-string
	"\\([\n\r]\\)"
	(concat "\\1" (make-string (kcell-view:indent) ?\ )) contents-2))
      (when kotl-mode:refill-flag
	(kotl-mode:fill-cell))

      (save-excursion
	;;
	;; Substitute cell-1 attributes into cell-2 location.
	;;
	(kotl-mode:goto-cell cell-ref-2 t)
	;; Set kcell properties.
	(kcell-view:set-cell kcell-1 idstamp-1)
	;; If idstamp labels are on, then must exchange labels in view.
	(when (eq (kview:label-type kview) 'id)
 	  (klabel:set (format "0%d" idstamp-1))))

      ;;
      ;; Substitute cell-2 attributes into cell-1 location.
      ;;
      ;; Set kcell properties.
      (kcell-view:set-cell kcell-2 idstamp-2)
      ;; If idstamp labels are on, then must exchange labels in view.
      (when (eq (kview:label-type kview) 'id)
 	(klabel:set (format "0%d" idstamp-2))))))

(defun kotl-mode:kill-contents (arg)
  "Kill contents of cell from point to cell end.
With prefix ARG, kill entire cell contents."
  (interactive "*P")
  (kotl-mode:kill-region
   (if arg (kcell-view:start) (point))
   (kcell-view:end-contents)))

(defun kotl-mode:kill-tree (&optional arg)
  "Kill ARG following trees starting with tree rooted at point.
If ARG is a non-positive number, nothing is done."
  (interactive "*p")
  (or (integerp arg) (setq arg 1))
  (let ((killed) (label (kcell-view:label))
	(lbl-sep-len (kview:label-separator-length kview))
	start end sib)
    (while (> arg 0)
      (setq start (kotl-mode:tree-start)
	    end   (kotl-mode:tree-end)
	    sib   (kcell-view:sibling-p nil nil lbl-sep-len)
	    arg (1- arg)
	    killed t)
      ;; Don't want to delete any prior cells, so if on last cell, ensure
      ;; this is the last one killed.
      (if (kotl-mode:last-cell-p)
	  (progn (setq arg 0)
		 (kview:delete-region start end))
	(kview:delete-region start end)
	(kotl-mode:to-valid-position)))
    (when killed
      (cond (sib (klabel-type:update-labels label))
	    ((kotl-mode:buffer-empty-p)
	     ;; Always leave at least 1 visible cell within a view.
	     (kview:add-cell "1" 1)))
      (kotl-mode:to-valid-position))))

(defun kotl-mode:move-tree-backward (&optional num-trees)
  "Move current cell before prefix arg `num-trees' at the same level.
If arg is 0, make it 1; if arg is negative, move to before that number of trees."
  (interactive "p")
  (unless (and (integerp num-trees)
	       (/= num-trees 0))
    (setq num-trees 1))
  (kotl-mode:move-tree-forward (- num-trees)))

(defun kotl-mode:move-tree-forward (&optional num-trees)
  "Move current tree after prefix arg `num-trees' at the same level.
If arg is 0, make it 1; if arg is negative, move prior to that number of trees."
  (interactive "p")
  (unless (and (integerp num-trees)
	       (/= num-trees 0))
    (setq num-trees 1))
  (let* ((n num-trees)
	 (to-func   (if (> n 0) #'kcell-view:forward #'kcell-view:backward))
	 (move-func (if (> n 0) #'kotl-mode:move-after #'kotl-mode:move-before))
	 (increment (if (> n 0) -1 1))
	 (from-tree (kcell-view:label))
	 (point-offset (- (point) (kcell-view:start)))
	 to-tree)
    (save-excursion (while (and (/= n 0)
				(funcall to-func))
		      (setq n (+ n increment))
		      (when (zerop n)
			(setq to-tree (kcell-view:label)))))
    (if to-tree
	(goto-char (+ (funcall move-func from-tree to-tree nil)
		      point-offset))
      (error "(kotl-mode:move-tree): Cannot move past %d trees at the same level" num-trees))))

(defun kotl-mode:promote-tree (arg)
  "Move current tree a maximum of prefix ARG levels higher in current view.
Each cell is refilled iff its `no-fill' attribute is nil and
kotl-mode:refill-flag is non-nil.  With prefix ARG = 0, cells are promoted up
to one level and kotl-mode:refill-flag is treated as true."
  (interactive "*p")
  (if (< arg 0)
      (kotl-mode:demote-tree (- arg))
    (let* ((parent) (result)
	   (lbl-sep-len (kview:label-separator-length kview))
	   (orig-id (kcell-view:idstamp))
	   (fill-p (= arg 0))
	   (orig-pos-in-cell
	    (- (point) (kcell-view:start nil lbl-sep-len)))
	   start-point)
      ;; Next line ensures point is in the root of the current tree if
      ;; the tree is at all hidden.
      (kotl-mode:beginning-of-line)
      (setq start-point (point))
      (when fill-p
	(setq arg 1))
      (unwind-protect
	  (progn
	    (backward-char 1)
	    (while (and (> arg 0)
			(setq result (kcell-view:parent nil lbl-sep-len))
			(not (eq result 0)))
	      (setq parent result
		    arg (1- arg)))
	    (when parent
	      (kotl-mode:move-after
	       (kcell-view:label start-point)
	       (kcell-view:label) nil
	       nil fill-p)))
	;; Move to start of original cell
	(kotl-mode:goto-cell orig-id)
	;; Move to original pos within cell
	(forward-char (min orig-pos-in-cell
			   (- (kcell-view:end-contents)
			      (kcell-view:start))))
	(kotl-mode:to-valid-position))
      (unless parent
	(error "(kotl-mode:promote-tree): Cannot promote any further")))))

(defun kotl-mode:remove-cell-attribute (attribute &optional pos top-cell-flag)
  "Remove ATTRIBUTE from the current cell or the cell at optional POS.
With optional prefix arg TOP-CELL-FLAG non-nil, removethe hidden top cell's
ATTRIBUTE and ignore any value of POS."
  (interactive
   (let* ((plist (copy-sequence (kcell-view:plist)))
	  (existing-attributes plist)
	  (top-cell-flag (zerop (prefix-numeric-value
				 current-prefix-arg)))
	  attribute)
     (barf-if-buffer-read-only)
     ;; Remove attribute values leaving only attribute symbols in existing-attributes.
     (while plist
       (setcdr plist (cdr (cdr plist)))
       (setq plist (cdr plist)))
     ;; Remove read-only attributes
     (setq existing-attributes (apply #'set:create existing-attributes)
	   existing-attributes (set:difference
				existing-attributes
				kcell:read-only-attributes))

     (while (zerop (length (setq attribute
				 (completing-read
				  (format "Name of attribute to remove from cell <%s>: "
					  (if top-cell-flag
					      "0"
					    (kcell-view:label)))
				  (mapcar 'list
					  (mapcar 'symbol-name
						  existing-attributes))))))
       (beep))
     (setq attribute (intern attribute))
     (list attribute nil top-cell-flag)))
  (barf-if-buffer-read-only)
  (if top-cell-flag
      (kcell:remove-attr (kview:top-cell kview) attribute)
    (kcell-view:remove-attr attribute pos))
  ;; Note that buffer needs to be saved to store modified property list.
  (set-buffer-modified-p t)
  (when (called-interactively-p 'interactive)
    (message "Attribute `%s' removed from cell <%s>."
	     attribute (if top-cell-flag
			   "0"
			 (kcell-view:label pos)))))

(defun kotl-mode:set-cell-attribute (attribute value &optional pos top-cell-flag)
  "Include ATTRIBUTE VALUE with the current cell or the cell at optional POS.
Replace any existing value that ATTRIBUTE has.  With optional prefix arg
TOP-CELL-FLAG non-nil, modify the hidden top cell's ATTRIBUTE and ignore any
value of POS.

When called interactively, display the setting in the minibuffer as
confirmation."
  (interactive
   (let* ((plist (copy-sequence (kcell-view:plist)))
	  (existing-attributes plist)
	  (top-cell-flag (zerop (prefix-numeric-value
				 current-prefix-arg)))
	  attribute value)
     (barf-if-buffer-read-only)
     ;; Remove attribute values leaving only attribute symbols in existing-attributes.
     (while plist
       (setcdr plist (cdr (cdr plist)))
       (setq plist (cdr plist)))
     ;; Remove read-only attributes
     (setq existing-attributes (apply #'set:create existing-attributes)
	   existing-attributes (set:difference
				existing-attributes
				kcell:read-only-attributes))

     (while (zerop (length (setq attribute
				 (completing-read
				  (format "Name of attribute to set in cell <%s>: "
					  (if top-cell-flag
					      "0"
					    (kcell-view:label)))
				  (mapcar 'list
					  (mapcar 'symbol-name
						  existing-attributes))))))
       (beep))
     (setq attribute (intern attribute)
	   value (if top-cell-flag
		     (kcell:get-attr (kview:top-cell kview) attribute)
		   (kcell-view:get-attr attribute)))
     (if value
	 (setq value (read-minibuffer
		      (format "Change the value of `%s' to (use double quotes around a string): " attribute)
		      (prin1-to-string value)))
       (setq value (read-minibuffer
		    (format "Set attribute `%s' to (use double quotes around a string): " attribute))))
     (list attribute value nil current-prefix-arg)))
  (barf-if-buffer-read-only)
  (if top-cell-flag
      (kcell:set-attr (kview:top-cell kview) attribute value)
    (kcell-view:set-attr attribute value pos))
  ;; Note that buffer needs to be saved to store new attribute value.
  (set-buffer-modified-p t)
  (when (called-interactively-p 'interactive)
    (message "Attribute `%s' set to `%s' in cell <%s>."
	     attribute value   (if top-cell-flag
				   "0"
				 (kcell-view:label pos)))))

(defun kotl-mode:set-or-remove-cell-attribute (arg)
  "Run kotl-mode:remove-cell-attribute or kotl-mode:set-cell-attribute.
With numeric prefix ARG, interactively run kotl-mode:remove-cell-attribute;
otherwise, run kotl-mode:set-cell-attribute.
Prefix ARG selects the cells whose attributes are removed or set:
  If =  0, set one of the attributes of the invisible root cell;
  If <  0, remove one of the attributes of the invisible root cell;
  If =  1, set one of the attributes of the current cell;
  If >  1, remove one of the attributes of the current cell."
  (interactive "p")
  (cond ((not (integerp arg))
	 (error "(kotl-mode:set-or-remove-cell-attribute): ARG must be an integer, not '%s'" arg))
	((= arg 0)
	 (call-interactively #'kotl-mode:set-cell-attribute))
	((< arg 0)
	 (setq current-prefix-arg 0)
	 (call-interactively #'kotl-mode:remove-cell-attribute))
	((= arg 1)
	 (call-interactively #'kotl-mode:set-cell-attribute))
	((> arg 1)
	 (call-interactively #'kotl-mode:remove-cell-attribute))))

(defun kotl-mode:split-cell (&optional arg)
  "Split the current cell into two cells and move to the new cell.
The cell contents after point become part of the newly created cell.
The default is to create the new cell as a sibling of the current cell.
With optional universal ARG, {C-u}, the new cell is added as the child of
the current cell.  Non-read-only attributes from the current cell are
replicated in the new cell."
  (interactive "*P")
  (let ((new-cell-contents (kotl-mode:kill-region
			    (point) (kcell-view:end-contents) 'string))
	(start (kcell-view:start))
	(current-plist (kcell-view:plist))
	plist
	prop
	val)

    ;; Create a plist for the new cell, dropping any kcell:read-only-attributes
    (while current-plist
      (setq prop (nth 0 current-plist)
	    val  (nth 1 current-plist))
      (setq current-plist (nthcdr 2 current-plist))
      (unless (memq prop kcell:read-only-attributes)
	(setq plist (cons prop (cons val plist)))))

    ;; delete any preceding whitespace
    (skip-chars-backward " \t\n\r" start)
    (delete-region (max start (point)) (kcell-view:end-contents))
    (kotl-mode:add-cell arg new-cell-contents
			plist
			(kcell-view:get-attr 'no-fill))))

(defun kotl-mode:transpose-cells (arg)
  "Exchange current and previous visible cells, leaving point after both.
If no previous cell, exchange current with next cell.
With prefix ARG = 0, interchange the cell that contains point with the cell
that contains mark.
With any other non-nil prefix ARG, take the current tree and move it past
ARG visible cells."
  (interactive "*p")
  (let ((lbl-sep-len (kview:label-separator-length kview)))
    (cond
     ((save-excursion (not (or (kcell-view:next t lbl-sep-len)
			       (kcell-view:previous t lbl-sep-len))))
      (error "(kotl-mode:transpose-cells): Only one visible cell in outline"))
     ;;
     ;; Transpose current and previous cells or current and next cells, if no
     ;; previous cell.  Leave point after both exchanged cells or within last
     ;; visible cell.
     ((= arg 1)
      (let ((label-1 (kcell-view:label))
	    (prev (kcell-view:previous t lbl-sep-len))
	    label-2)
	(unless prev (kcell-view:next t lbl-sep-len))
	(setq label-2 (kcell-view:label))
	(kotl-mode:exchange-cells label-1 label-2)
	(kcell-view:next t lbl-sep-len)
	(when prev (kcell-view:next t lbl-sep-len))))
     ;;
     ;; Transpose point and mark cells, moving point to the new location of the
     ;; cell which originally contained point.
     ((= arg 0)
      (let ((label-1 (kcell-view:label))
	    label-2)
	(kotl-mode:exchange-point-and-mark)
	(setq label-2 (kcell-view:label))
	(kotl-mode:exchange-cells label-1 label-2)))
     ;;
     ;; Move current tree past ARG next visible cells and leave point after
     ;; original cell text.
     (t
      (let ((mark (set-marker (make-marker)
			      (save-excursion (kotl-mode:next-line arg)))))
	(kotl-mode:move-after
	 (kcell-view:label)
	 (progn (while (and (> arg 0) (kcell-view:next t lbl-sep-len))
		  (setq arg (1- arg)))
		(kcell-view:label))
	 nil)
	(goto-char mark)
	(set-marker mark nil))))))

;;; ------------------------------------------------------------------------
;;; Structure Insertion Across Buffers
;;; ------------------------------------------------------------------------

(defun kotl-mode:copy-region-to-buffer (target-buf start end &optional source-buf invisible-flag)
  "Copy to TARGET-BUF the region between START and END.
Copy from the current buffer or optional SOURCE-BUF (a buffer or buffer name).
Invisible text is expanded and included only if INVISIBLE-FLAG is non-nil."
  (interactive
   (hargs:iform-read
    '(interactive
      (let ((target-buf (hargs:read-buffer-name
			 (format "Buffer to copy region to (default %s): " (other-buffer)))))
	(when (buffer-local-value 'buffer-read-only (get-buffer target-buf))
	  (signal 'buffer-read-only (list target-buf)))
	(list target-buf
	      (region-beginning) (region-end)
	      (current-buffer)
	      (y-or-n-p "Include any invisible text? "))))))
  (unless source-buf
    (setq source-buf (current-buffer)))
  (when (stringp source-buf)
    (setq source-buf (get-buffer source-buf)))
    (save-excursion
      (set-buffer source-buf)
      (hypb:insert-region target-buf start end invisible-flag)))

(defun kotl-mode:copy-tree-to-buffer (target-buf cell-ref invisible-flag)
  "Copy to point in TARGET-BUF the text of the outline tree rooted at CELL-REF.
Use \"0\" for whole outline buffer.  Invisible text is expanded and
included only if INVISIBLE-FLAG is non-nil."
  (interactive
   (let ((label-default (kcell-view:label)))
     (hargs:iform-read
      `(interactive
	(list
	 (prog1
	     (setq target-buf (hargs:read-buffer-name
			       (format "Buffer to copy tree text to (default %s): " (other-buffer))))
	   (when (buffer-local-value 'buffer-read-only (get-buffer target-buf))
	     (signal 'buffer-read-only (list target-buf))))
	 (hargs:read (format "Tree number to copy to %s: (0 for whole outline) "
			     target-buf)
		     nil ,label-default nil 'kcell)
	 (y-or-n-p "Include any invisible text? "))))))
  (message "") ;; Erase last interactive prompt, if any.
  (setq target-buf (get-buffer-create target-buf))
  (if (equal cell-ref "0")
      (kotl-mode:copy-region-to-buffer target-buf (point-min) (point-max) nil invisible-flag)
    (let (start end)
      (save-excursion
	(kotl-mode:goto-cell cell-ref t)
	(save-excursion (kfill:forward-line 0) (setq start (point)))
	(kotl-mode:to-valid-position)
	(setq end (kotl-mode:tree-end)))
      (kotl-mode:copy-region-to-buffer target-buf start end nil invisible-flag))))

(defun kotl-mode:copy-tree-or-region-to-buffer ()
  "Copy a Koutline tree to a specified buffer.
If no usable active region, prompt for and copy a Koutline tree
to a specified buffer, otherwise, copy the active region.

Use 0 to copy the whole outline buffer.  Prompt for whether or not
to expand and include any hidden/invisible text within the copied text."
  (interactive)
  (call-interactively 
   (if (use-region-p)
       #'kotl-mode:copy-region-to-buffer
     #'kotl-mode:copy-tree-to-buffer)))

(defun kotl-mode:mail-tree (cell-ref invisible-flag)
  "Mail outline tree rooted at CELL-REF.  Use \"0\" for whole outline buffer.
Invisible text is expanded and included in the mail only if INVISIBLE-FLAG is
non-nil."
  (interactive
   (let ((label-default (kcell-view:label)))
     (hargs:iform-read
      `(interactive
	(list
	 (hargs:read "Mail tree: (0 for whole outline) "
		     nil ,label-default nil 'kcell)
	 (y-or-n-p "Include invisible text? "))))))
  (if (equal cell-ref "0")
      (hmail:buffer nil invisible-flag)
    (let (start end)
      (save-excursion
	(kotl-mode:goto-cell cell-ref t)
	(forward-line 0)
	(setq start (point))
	(kotl-mode:to-valid-position)
	(setq end (kotl-mode:tree-end)))
      (hmail:region start end nil invisible-flag))))

;;; ------------------------------------------------------------------------
;;; Structure Viewing
;;; ------------------------------------------------------------------------

(defun kotl-mode:collapse-tree (&optional all-flag)
  "Collapse to one line each visible cell of the tree rooted at point.
With optional prefix ALL-FLAG non-nil, collapse all cells visible
within the current view."
  (interactive "P")
  (kotl-mode:is-p)
  (let (buffer-read-only)
    (kview:map-tree (lambda (_kview)
		      ;; Use free variable kview-label-sep-len bound in kview:map-tree for speed.
		      (kcell-view:collapse nil kview-label-sep-len))
		    kview all-flag t)))

(defun kotl-mode:expand-tree (&optional all-flag)
  "Expand each visible cell of the tree rooted at point.
With optional prefix ALL-FLAG non-nil, expand all cells visible within
the current view."
  (interactive "P")
  (kotl-mode:is-p)
  (let (buffer-read-only)
    (kview:map-tree
     (lambda (_kview)
       ;; Use free variable kview-label-sep-len bound in kview:map-tree for speed.
       (goto-char (kcell-view:start (point) kview-label-sep-len))
       (outline-flag-region (point) (kcell-view:end-contents) nil))
     kview all-flag t)))

(defun kotl-mode:toggle-tree-expansion (&optional all-flag)
  "Collapse or expand each cell of tree rooted at point.
Act on all visible cells if optional prefix arg ALL-FLAG is given.
If current cell is collapsed, cells will be expanded, otherwise they will be
collapsed."
  (interactive "P")
  (if (kcell-view:collapsed-p)
       ;; expand cells
      (kotl-mode:expand-tree all-flag)
    (kotl-mode:collapse-tree all-flag)))

;;; 
;;;###autoload
(defun kotl-mode:overview (&optional arg)
  "Show the first line of each cell.
With optional prefix ARG, toggle display of blank lines between cells."
  (interactive "P")
  (kotl-mode:show-all)
  (when arg
    (kvspec:toggle-blank-lines))
  (kotl-mode:collapse-tree t))

;;;###autoload
(defun kotl-mode:show-all (&optional arg)
  "Show (expand) all cells in the current view.
With optional prefix ARG, toggle display of blank lines between cells."
  (interactive "P")
  (when (kotl-mode:is-p)
    (kview:set-attr kview 'levels-to-show 0)
    (kview:set-attr kview 'lines-to-show 0)
    (outline-flag-region (point-min) (point-max) nil)
    (when arg
      (kvspec:toggle-blank-lines))
    (when (called-interactively-p 'interactive)
      (kvspec:update t))))

;;;###autoload
(defun kotl-mode:top-cells (&optional arg)
  "Collapse all level 1 cells in view and hide any deeper sublevels.
With optional prefix ARG, toggle display of blank lines between cells."
  (interactive "P")
  (kotl-mode:is-p)
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only))
    (kvspec:levels-to-show 1)
    (kvspec:show-lines-per-cell 1)
    (when arg
      (kvspec:toggle-blank-lines))
    ;; Restore buffer modification status
    (set-buffer-modified-p modified-p)))

;;; 
(defun kotl-mode:hide-sublevels (levels-to-keep)
  "Hide all cells in outline at levels deeper than LEVELS-TO-KEEP (a number).
Shows any hidden cells within LEVELS-TO-KEEP.  1 is the first level.  0 means
display all levels of cells.  Interactively, prompts for LEVELS-TO-KEEP."
  (interactive "P")
  (kvspec:levels-to-show levels-to-keep)
  ;; The prior call might have shown more lines per cell than the current
  ;; viewspec supports, so reset lines per cell.
  (kvspec:lines-to-show)
  (kvspec:update t))

(defun kotl-mode:hide-subtree (&optional cell-ref show-flag)
  "Hide subtree, ignoring root, at optional CELL-REF (defaults to cell at point).
With optional SHOW-FLAG, expand the subtree instead."
  (interactive)
  (kotl-mode:is-p)
  (save-excursion
    ;; Next line ensures point is in the root of the current tree if
    ;; the tree is at all hidden.
    (kotl-mode:beginning-of-line)
    (if cell-ref
	(kotl-mode:goto-cell cell-ref t)
      (kotl-mode:beginning-of-cell))
    (let ((start (kcell-view:end-contents))
	  (end (kotl-mode:tree-end t))
	  (buffer-read-only))
      (outline-flag-region start end (not show-flag)))))

(defun kotl-mode:show-subtree (&optional cell-ref)
  "Show subtree, ignoring root, at optional CELL-REF (defaults to cell at point)."
  (interactive)
  (kotl-mode:hide-subtree cell-ref t))

;;;###autoload
(defun kotl-mode:hide-tree (&optional cell-ref show-flag)
  "Collapse tree rooted at optional CELL-REF (defaults to cell at point).
With optional SHOW-FLAG, expand the tree instead."
  (interactive)
  (kotl-mode:is-p)
  (save-excursion
    ;; Next line ensures point is in the root of the current tree if
    ;; the tree is at all hidden.
    (kotl-mode:beginning-of-line)
    (let ((start (if cell-ref
		     (kotl-mode:goto-cell cell-ref t)
		   (kotl-mode:beginning-of-cell)))
	  (end (kotl-mode:tree-end t))
	  (buffer-read-only))
      (and (not show-flag) (search-forward "\n" nil t)
	   ;; Leave first line visible.
	   (setq start (1- (point))))
      (outline-flag-region start end (not show-flag)))))

;;;###autoload
(defun kotl-mode:show-tree (&optional cell-ref)
  "Display fully expanded tree rooted at CELL-REF."
  (interactive)
  (kotl-mode:hide-tree cell-ref t))

(defun kotl-mode:cell-attributes (all-flag)
  "Print attributes of the current kcell to standard output.
With prefix arg ALL-FLAG non-nil, print the attributes of all visible
kcells from the current buffer to standard output.

See also the documentation for `kotl-mode:cell-help'."
  (interactive "P")
  (save-excursion
    (if (not all-flag)
	(kotl-mode:print-attributes kview)
      (let ((lbl-sep-len (kview:label-separator-length kview)))
	(kotl-mode:beginning-of-buffer)
	(while (progn (kotl-mode:print-attributes kview)
		      (kcell-view:next t lbl-sep-len)))))))

(defun kotl-mode:cell-help (&optional cell-ref cells-flag)
  "Display a temporary buffer with CELL-REF's attributes.
CELL-REF defaults to current cell.
Optional prefix arg CELLS-FLAG selects the cells whose attributes are printed:
  If =  1, print CELL-REF's cell only;
  If >  1, print CELL-REF's visible tree (the tree rooted at CELL-REF);
  If <= 0, print all visible cells in current view (CELL-REF is not used).

See also the documentation for `kotl-mode:cell-attributes'."
  (interactive
   (append
    (let ((arg (prefix-numeric-value current-prefix-arg)))
      (if (< arg 1)
	  '("0")
	(hargs:iform-read
	 (list 'interactive
	       (format "+KDisplay properties of koutline %s: "
		       (if (= arg 1) "cell" "tree"))))))
    (list current-prefix-arg)))
  (unless (integerp cells-flag)
    ;; If cells-flag is nil, this sets it to 1
    (setq cells-flag (prefix-numeric-value cells-flag)))
  (unless (stringp cell-ref)
    (setq cell-ref (kcell-view:label)))
  ;; Ensure these do not invoke with-output-to-temp-buffer a second time.
  (let ((temp-buffer-show-hook)
	(temp-buffer-show-function))
    (with-help-window (hypb:help-buf-name "Koutliner")
      (save-excursion
	(if (or (member cell-ref '("0" 0))
		(<= cells-flag 0))
	    (progn
	      (hattr:report (append '(idstamp 0)
				    (kcell:plist (kview:top-cell kview))))
	      (terpri)
	      (cond ((= cells-flag 1) nil)
		    ((> cells-flag 1)
		     (kview:map-tree #'kotl-mode:print-attributes kview t t))
		    ;; (<= cells-flag 0)
		    (t (kotl-mode:cell-attributes t))))
	  (cond ((= cells-flag 1)
		 (kotl-mode:goto-cell cell-ref)
		 (kotl-mode:print-attributes kview))
		((> cells-flag 1)
		 (kotl-mode:goto-cell cell-ref)
		 (kview:map-tree #'kotl-mode:print-attributes kview nil t))))))))

(defun kotl-mode:get-cell-attribute (attribute &optional pos top-cell-flag)
  "Return ATTRIBUTE's value for the current cell or the cell at optional POS.
With optional prefix arg TOP-CELL-FLAG non-nil, return the hidden top cell's
ATTRIBUTE and ignore any value of POS.

When called interactively, it displays the value in the minibuffer."
  (interactive "SCurrent cell attribute to get: ")
  (let ((value
	 (if (eq attribute 'idstamp)
	     (if top-cell-flag
		 0
	       (kproperty:get (kcell-view:plist-point pos) attribute))
	   (if top-cell-flag
	     (kcell:get-attr (kview:top-cell kview) attribute)
	   (kcell-view:get-attr attribute pos)))))
    (when (called-interactively-p 'interactive)
      (message "Attribute \"%s\" = `%s' in cell <%s>."
	       attribute value (if top-cell-flag
				   "0"
				 (kcell-view:label pos))))
    value))

;;; ------------------------------------------------------------------------
;;; Org mode and Org Table overrides to limit editing to a cell
;;; ------------------------------------------------------------------------

(defun kotl-mode:org-delete-backward-char (n)
  "Like ‘delete-backward-char’, insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (kcell-view:operate
   (lambda () (org-delete-backward-char n))))

(defun kotl-mode:org-delete-char (n)
  "Like ‘delete-char’, but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (kcell-view:operate
   (lambda () (org-delete-char n))))

(defun kotl-mode:org-force-self-insert (n)
  "Needed to enforce self-insert under remapping."
  (interactive "p")
  (kcell-view:operate
   (lambda () (org-force-self-insert n))))

(defun kotl-mode:orgtbl-ctrl-c-ctrl-c (arg)
  "If the cursor is inside a table, realign the table.
If it is a table to be sent away to a receiver, do it.
With prefix arg, also recompute table."
  (interactive "P")
  (kcell-view:operate
   (lambda () (org-ctrl-c-ctrl-c arg))))

(defun kotl-mode:orgtbl-create-or-convert-from-region (arg)
  "Create table or convert region to table, if no conflicting binding.
This installs the table binding `C-c |', but only if there is no
conflicting binding to this key outside orgtbl-mode."
  (interactive "P")
  (kcell-view:operate
   (lambda () (orgtbl-create-or-convert-from-region arg))))

(defun kotl-mode:orgtbl-self-insert-command (n)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (kcell-view:operate
   (lambda () (orgtbl-self-insert-command n))))


;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kotl-mode:add-indent-to-region (&optional indent start end)
  "Add current cell's indent to current region.
Optionally, INDENT and region START and END may be given."
  (or (integerp indent) (setq indent (kcell-view:indent)))
  (save-excursion
    (save-restriction
      (narrow-to-region (or start (point)) (or end (mark t)))
      (goto-char (point-min))
      (let ((indent-str (concat "\n" (make-string indent ?\ ))))
	(while (search-forward "\n" t t)
	  (replace-match indent-str t t))))))

(defun kotl-mode:delete-line (&optional pos)
  "Delete and return contents of cell line at point or optional POS as a string.
Do not delete newline at end of line."
  (save-excursion
    (when pos
      (goto-char pos))
    (if (kview:valid-position-p)
	(let ((bol (kotl-mode:beginning-of-line))
	      (eol (kotl-mode:to-end-of-line)))
	  (prog1
	      (buffer-substring bol eol)
	    (delete-region bol eol)))
      (error "(kotl-mode:delete-line): Invalid position, `%d'" (point)))))

(defun kotl-mode:exchange-point-and-mark ()
  "Put the mark where point is now, and point where the mark is now.
This is like `exchange-point-and-mark', but doesn't activate the mark."
  (goto-char (prog1 (mark t)
	       (set-marker (mark-marker) (point) (current-buffer)))))

(defun kotl-mode:indent-line (&optional arg)
  "Indent line relative to the previous one.
With optional prefix ARG greater than 1, tab forward ARG times.
See the documentation string of `kotl-mode:indent-tabs-mode' for details
on when tabs are used for indenting."
  (interactive "*p")
  (setq arg (prefix-numeric-value arg))
  (let ((indent-tabs-mode kotl-mode:indent-tabs-mode))
    (if (> arg 1)
	(while (> arg 0)
	  (tab-to-tab-stop)
	  (setq arg (1- arg)))
      (indent-relative))))

(defun kotl-mode:indent-region (start end)
  (kotl-mode:maybe-shrink-region-p)
  (indent-region start end nil))

;;;###autoload
(defun kotl-mode:is-p ()
  "Signal an error if current buffer is not a Hyperbole outline, else return t."
  (if (kview:is-p kview)
      t
    (hypb:error
     "(kotl-mode:is-p): Command requires a valid Hyperbole koutline")))

(defun kotl-mode:shrink-region ()
  "Shrink region within visible bounds of a single cell.
If a region is active and outside the visible bounds of a single
cell, shrink it to within those bounds.
The region then falls within the first visible cell that was part of
the region or that followed it.  This prevents editing actions from
removing Koutline structure."
  (interactive)
  ;; Caller has already tested for: (and (not (kotl-mode:valid-region-p)) (region-active-p))
  ;; Reduce the region to within the visible portion of a single cell
  (let ((end (max (point) (mark)))
	(exchange-p (> (point) (mark))))
    (when exchange-p
      (kotl-mode:exchange-point-and-mark))
    (kotl-mode:to-visible-position)
    (setq end (kview:first-invisible-point))
    (set-mark (min end (kcell-view:end-contents (point))))
    (when exchange-p (kotl-mode:exchange-point-and-mark))))

(defun kotl-mode:valid-region-p ()
  "Return t if no active region or the region is within visible bounds of a cell."
  (if (region-active-p)
      (and (kview:valid-position-p (point))
	   (kview:valid-position-p (mark))
	   (>= (region-beginning) (kcell-view:start))
	   (<= (region-end) (kcell-view:end-contents))
	   (not (outline-invisible-in-p (region-beginning) (region-end))))
    t))

(defun kotl-mode:maybe-shrink-region-p ()
  "Shrink active region if `kotl-mode:shrink-region-flag' is non-nil.
Shrink any active region that includes hidden text or crosses
cell boundaries.  The region then falls within the first visible
cell that was part of the region or that followed it

Return t unless any region problem is left uncorrected, notably in
cases where `kotl-mode:shrink-region-flag' is nil."
  ;; This function is called from pre-command-hook, so don't trigger
  ;; any error as that will delete it from pre-command-hook and it
  ;; will no longer be called.
  ;;
  (if (not (eq major-mode 'kotl-mode))
      t
    ;; Deactivate empty region
    (when (eq (point) (when mark-active (mark)))
	(deactivate-mark))
    (if (not (kotl-mode:valid-region-p))
	(if kotl-mode:shrink-region-flag
	    (progn (kotl-mode:shrink-region) t)
	  (message "(Hyperbole): Limit the region to a single visible cell or set `kotl-mode:shrink-region-flag'.")
	  (beep)
	  nil)
      t)))

(defun kotl-mode:tree-end (&optional omit-end-newlines)
  "Return end point of current cell's tree within this view.
If optional OMIT-END-NEWLINES is non-nil, point returned precedes any
newlines at end of tree."
  (let* ((lbl-sep-len (kview:label-separator-length kview))
	 (start-indent (kcell-view:indent nil lbl-sep-len))
	 (next))
    (save-excursion
      (while (and (setq next (kcell-view:next nil lbl-sep-len))
		  (>= (- (kcell-view:indent nil lbl-sep-len) start-indent)
		      (kview:level-indent kview))))
      (cond (next
	     (goto-char (progn (kcell-view:previous nil lbl-sep-len)
			       (kcell-view:end))))
	    (t (goto-char (kcell-view:end))))
      (when omit-end-newlines
	(skip-chars-backward "\n\r"))
      (point))))

(defun kotl-mode:tree-start ()
  "Return beginning of line position preceding current cell's start point."
  (save-excursion (goto-char (kcell-view:start))
		  (line-beginning-position)))

(defun kotl-mode:line-move (arg)
  "Move point ARG visible lines forward within an outline."
  (if (not selective-display)
      (kfill:forward-line arg)
    ;; Move by arg lines, but ignore invisible ones.
    (while (> arg 0)
      (vertical-motion 1)
      (forward-char -1)
      (kfill:forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (vertical-motion -1)
      (beginning-of-line)
      (setq arg (1+ arg))))
  (let ((col (or goal-column (if (consp temporary-goal-column) (car temporary-goal-column)
			       temporary-goal-column))))
    (move-to-column (if (numberp col) (round col) 0) nil)))

;; Consult this: (kotl-mode:to-visible-position)
(defun kotl-mode:pre-self-insert-command ()
  "In a Koutline ensure point is in an editable position before insertion.
Mouse may have moved point outside of an editable area.
`kotl-mode' adds this function to `pre-command-hook'."
  (when (and (memq this-command '(self-insert-command orgtbl-self-insert-command))
	     (eq major-mode 'kotl-mode)
	     (not (kview:valid-position-p))
	     ;; Prevent repeatedly moving point to valid position when moving trees
	     (not (hyperb:stack-frame '(kcell-view:to-label-end))))
    (when (not (kview:valid-position-p))
      (kotl-mode:to-valid-position))))

(defun kotl-mode:print-attributes (_kview)
  "Print to `standard-output' the attributes of the current visible kcell.
Takes argument _KVIEW (so it can be used with `kview:map-tree')
but always operates upon the current view."
  ;; Move to start of visible cell to avoid printing attributes for an
  ;; invisible kcell which point may be over.
  ;; Print first line of cell for reference.
  (save-excursion
    (princ
     (buffer-substring (progn (beginning-of-line) (point))
		       (progn (kview:end-of-actual-line)
			      (point)))))
  (terpri)
  (hattr:report
   (append (list 'idstamp (kcell-view:idstamp-integer))
	   (kcell:plist (kcell-view:cell))))
  (terpri))

(put 'outline 'reveal-toggle-invisible 'kotl-mode:reveal-toggle-invisible)
(defun kotl-mode:isearch-open-invisible (_overlay)
  (kotl-mode:show-tree))
;; Adapted from outline-reveal-toggle-invisible; called by isearch.
(defun kotl-mode:reveal-toggle-invisible (o hidep)
  (if (not (eq major-mode 'kotl-mode))
      (outline-reveal-toggle-invisible o hidep)
    (save-excursion
      (goto-char (overlay-start o))
      (if hidep
	  ;; When hiding the area again, we could just clean it up and let
	  ;; reveal do the rest, by simply doing:
	  ;; (remove-overlays (overlay-start o) (overlay-end o)
	  ;;                  'invisible 'outline)
	  ;;
	  ;; That works fine as long as everything is in sync, but if the
	  ;; structure of the document is changed while revealing parts of it,
	  ;; the resulting behavior can be ugly.  I.e. we need to make
	  ;; sure that we hide exactly a subtree.
	  (let ((end (overlay-end o)))
	    (delete-overlay o)
	    (while (progn
		     (kotl-mode:hide-subtree)
		     (kotl-mode:to-visible-position)
		     (and (not (kotl-mode:eobp)) (< (point) end)))))

	;; When revealing, we just need to reveal sublevels.  If point is
	;; inside one of the sublevels, reveal will call us again.
	;; But we need to preserve the original overlay.
	(let ((o1 (copy-overlay o)))
	  (overlay-put o 'invisible nil) ;Show (most of) the text.
	  (while (progn
		   (kotl-mode:show-tree)
		   ;; Normally just the above is needed.
		   ;; But in odd cases, the above might fail to show anything.
		   ;; To avoid an infinite loop, we have to make sure that
		   ;; *something* gets shown.
		   (and (equal (overlay-start o) (overlay-start o1))
			(< (point) (overlay-end o))
			(= 0 (kfill:forward-line 1)))))
	  ;; If still nothing was shown, just kill the damn thing.
	  (when (equal (overlay-start o) (overlay-start o1))
	    ;; I've seen it happen at the end of buffer.
	    (delete-overlay o1)))))))

(defun kotl-mode:set-temp-goal-column ()
  (if (not (or (eq last-command 'next-line)
	       (eq last-command 'previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (kotl-mode:eolp)
		     ;; Don't count beg of empty line as end of line
		     ;; unless we just did explicit end-of-line.
		     (or (not (bolp)) (eq last-command 'end-of-line)))
		9999
	      (current-column)))))

(defun kotl-mode:to-visible-position (&optional backward-p)
  "Move point to nearest visible and editable position within current koutline.
With optional BACKWARD-P, move backward if possible to get to valid position."
  ;; Empty, visible cell
  (unless (and (kotl-mode:bocp) (kotl-mode:eocp) (not (kcell-view:invisible-p (point))))
    (if (if backward-p
	    (kview:char-invisible-p (1- (point)))
	  (kview:char-invisible-p))
	(goto-char (if backward-p
		       (kview:previous-visible-point)
		     (kview:first-visible-point))))
    (kotl-mode:to-valid-position backward-p)))

(defun kotl-mode:to-valid-position (&optional backward-p)
  "Move point to the nearest editable position within the current koutline view.
With optional BACKWARD-P, move backward if possible to get to valid position."
  (unless (kview:valid-position-p)
    (let ((lbl-sep-len (kview:label-separator-length kview)))
      (cond ((kotl-mode:bobp)
	     (goto-char (kcell-view:start nil lbl-sep-len)))
	    ((kotl-mode:eobp)
	     (skip-chars-backward "\n\r"))
	    (t (when (bolp)
		 (if backward-p
		     (skip-chars-backward "\n\r")
		   (skip-chars-forward "\n\r")))
	       (let ((indent (kcell-view:indent nil lbl-sep-len)))
		 (when (< (current-column) indent)
		   (move-to-column indent))))))))

(defun kotl-mode:transpose-lines-internal (start end)
  "Transpose lines at START and END markers within an outline.
Leave point at end of line now residing at START."
  (if (and start end
	   (kview:valid-position-p start)
	   (kview:valid-position-p end))
      (let* ((pline (kotl-mode:delete-line start))
	     mline)
	(goto-char end)
	(setq mline (kotl-mode:delete-line))
	(insert pline)
	(goto-char start)
	(insert mline))
    ;; Set non-point and non-mark markers to point nowhere before signalling
    ;; an error.
    (or (eq start (point-marker))
	(eq start (mark-marker))
	(set-marker start nil))
    (or (eq end (point-marker))
	(eq end (mark-marker))
	(set-marker start nil))
    (error "(kotl-mode:transpose-lines): Point or mark is at an invalid position")))

(defun kotl-mode:update-buffer ()
  "Update current view buffer in preparation for saving."
  (when (kview:is-p kview)
    (let ((mod-p (buffer-modified-p))
	  (start (window-start)))
      (save-excursion
	(kfile:update)
	(set-buffer-modified-p mod-p))
      (set-window-start nil (max (point-min) start) t)
      nil)))

(defun kotl-mode:maintain-region-highlight ()
  (setq transient-mark-mode t))

;;; ------------------------------------------------------------------------

(unless kotl-mode-map
  (setq kotl-mode-map
	(cond ((fboundp 'copy-keymap)
	       (if (boundp 'indented-text-mode-map)
		   (copy-keymap indented-text-mode-map)
		 (copy-keymap text-mode-map)))
	      (t (make-keymap))))
  ;; Ensure mouse wheel scrolling leaves point in a valid Koutline position
  (when (and (boundp 'mwheel-scroll-up-function)
	     (eq (symbol-value 'mwheel-scroll-up-function) 'scroll-up))
    (setq mwheel-scroll-up-function 'kotl-mode:scroll-up
	  mwheel-scroll-down-function 'kotl-mode:scroll-down))
  ;; Overload edit keys to deal with structure and labels.
  (let (local-cmd)
    (mapc
     (lambda (cmd)
       (setq local-cmd (intern-soft
			(concat "kotl-mode:" (symbol-name cmd))))
       ;; Only bind key locally if kotl-mode local-cmd has already
       ;; been defined and cmd is a valid function.
       (when (and local-cmd (fboundp cmd))
	 ;; Make local-cmd have the same property list as cmd,
	 ;; e.g. so pending-delete property is the same, but delete
	 ;; interactive-only property to suppress byte-compiler warnings.
	 (setplist local-cmd (copy-sequence (symbol-plist cmd)))
	 (cl-remprop local-cmd 'interactive-only)
	 (substitute-key-definition
	  cmd local-cmd kotl-mode-map global-map)))
     '(
       back-to-indentation
       backward-char
       backward-delete-char
       backward-delete-char-untabify
       backward-kill-word
       backward-or-forward-delete-char
       backward-para
       backward-paragraph
       backward-sentence
       backward-word
       beginning-of-buffer
       beginning-of-line
       beginning-of-visual-line
       completion-kill-region
       copy-region-as-kill
       copy-to-register
       delete-blank-lines
       delete-backward-char
       delete-char
       delete-forward-char
       delete-horizontal-space
       delete-indentation
       end-of-buffer
       end-of-line
       end-of-visual-line
       fill-paragraph
       fill-paragraph-or-region
       ;; cursor keys
       fkey-backward-char
       fkey-forward-char
       fkey-next-line
       fkey-previous-line
       backward-char-command
       forward-char-command
       ;;
       forward-char
       forward-word
       forward-para
       forward-paragraph
       forward-sentence
       just-one-space
       kill-word
       kill-line
       kill-visual-line
       kill-whole-line
       kill-region
       kill-ring-save
       kill-sentence
       left-char
       mark-paragraph
       mark-whole-buffer
       move-beginning-of-line
       move-end-of-line
       newline
       newline-and-indent
       next-line
       open-line
       previous-line
       right-char
       scroll-down
       scroll-up
       scroll-down-command
       scroll-up-command
       set-fill-prefix
       transpose-chars
       transpose-lines
       transpose-paragraphs
       transpose-sentences
       transpose-words
       yank
       yank-pop
       zap-to-char
       ;;
       org-delete-backward-char
       org-delete-char
       org-force-self-insert
       orgtbl-ctrl-c-ctrl-c
       orgtbl-create-or-convert-from-region
       orgtbl-self-insert-command)))


  ;; kotl-mode keys
  (define-key kotl-mode-map "\C-c\C-@"  'kotl-mode:mail-tree)
  (define-key kotl-mode-map "\C-c+"     'kotl-mode:append-cell)
  (define-key kotl-mode-map "\C-c,"     'kotl-mode:beginning-of-cell)
  (define-key kotl-mode-map "\C-c."     'kotl-mode:end-of-cell)
  (define-key kotl-mode-map "\C-c<"     'kotl-mode:first-sibling)
  (define-key kotl-mode-map "\C-c>"     'kotl-mode:last-sibling)
  (define-key kotl-mode-map "\C-c^"     'kotl-mode:beginning-of-tree)
  (define-key kotl-mode-map "\C-c$"     'kotl-mode:end-of-tree)
  (define-key kotl-mode-map "\C-ca"     'kotl-mode:add-child)
  (define-key kotl-mode-map "\C-c\C-a"  'kotl-mode:show-all)
  (define-key kotl-mode-map "\C-cb"     'kvspec:toggle-blank-lines)
  (define-key kotl-mode-map "\C-c\C-b"  'kotl-mode:backward-cell)
  (define-key kotl-mode-map "\C-cc"     'kotl-mode:copy-after)
  (define-key kotl-mode-map "\C-c\C-c"  'kotl-mode:copy-before)
  (define-key kotl-mode-map "\C-c\M-c"  'kotl-mode:copy-tree-or-region-to-buffer)
  (define-key kotl-mode-map "\C-cd"     'kotl-mode:down-level)
  (define-key kotl-mode-map "\C-c\C-d"  'kotl-mode:down-level)
  (define-key kotl-mode-map "\C-ce"     'kotl-mode:exchange-cells)
  (define-key kotl-mode-map "\C-c\C-f"  'kotl-mode:forward-cell)
  (define-key kotl-mode-map "\C-cg"     'kotl-mode:goto-cell)
  (define-key kotl-mode-map "\C-ch"     'kotl-mode:cell-help)
  (define-key kotl-mode-map "\C-c\C-h"  'kotl-mode:hide-tree)
  ;; Since the next key binds M-BS, it may already have a local binding,
  ;; in which case we don't want to bind it here.
  (unless (lookup-key kotl-mode-map "\M-\C-h")
    (define-key kotl-mode-map "\M-\C-h"   'kotl-mode:hide-subtree))
  ;; Override this global binding for set-selective-display with a similar
  ;; function appropriate for kotl-mode.
  (define-key kotl-mode-map "\C-x$"     'kotl-mode:hide-sublevels)
  (define-key kotl-mode-map [(tab)]     'kotl-mode:tab-command) ;; TAB
  (define-key kotl-mode-map "\C-i"      'kotl-mode:tab-command) ;; TAB
  (define-key kotl-mode-map [(shift tab)] 'kotl-mode:untab-command) ;; Shift-TAB
  (define-key kotl-mode-map [S-iso-lefttab] 'kotl-mode:untab-command) ;; Shift-TAB
  (define-key kotl-mode-map [backtab]   'kotl-mode:untab-command) ;; Shift-TAB
  (define-key kotl-mode-map [(meta tab)] 'kotl-mode:untab-command) ;; M-TAB
  (define-key kotl-mode-map "\M-\C-i"   'kotl-mode:untab-command) ;; M-TAB
  (define-key kotl-mode-map "\C-c\C-i"  'kotl-mode:set-or-remove-cell-attribute)
  (define-key kotl-mode-map "\C-j"      'kotl-mode:add-cell)
  (define-key kotl-mode-map "\M-j"      'kotl-mode:fill-paragraph)
  (define-key kotl-mode-map "\C-c\M-j"  'kotl-mode:fill-cell)
  (define-key kotl-mode-map "\M-\C-j"   'kotl-mode:fill-tree)
  (define-key kotl-mode-map "\C-c\C-k"  'kotl-mode:kill-tree)
  (define-key kotl-mode-map "\C-ck"     'kotl-mode:kill-contents)
  ;; Force an override of the global {C-x i} insert-file binding
  (define-key kotl-mode-map "\C-xi"     'kimport:insert-file)
  ;; Force an override of the global {C-x r i} insert-register binding
  (define-key kotl-mode-map "\C-xri"    'kimport:insert-register)
  (define-key kotl-mode-map "\C-ck"     'kotl-mode:kill-contents)
  (define-key kotl-mode-map "\C-cl"     'klink:create)
  (define-key kotl-mode-map "\C-c\C-l"  'kview:set-label-type)
  (define-key kotl-mode-map "\C-c\M-l"  'kview:set-label-separator)
  (define-key kotl-mode-map "\C-m"      'kotl-mode:newline)
  (define-key kotl-mode-map "\C-cm"     'kotl-mode:move-after)
  (define-key kotl-mode-map "\C-c\C-m"  'kotl-mode:move-before)
  (define-key kotl-mode-map "\C-c\C-n"  'kotl-mode:next-cell)
  (define-key kotl-mode-map "\C-c\C-o"  'kotl-mode:overview)
  (define-key kotl-mode-map "\C-c\C-p"  'kotl-mode:previous-cell)
  (define-key kotl-mode-map "\C-cp"     'kotl-mode:add-parent)
  (if (memq (global-key-binding "\M-q") '(fill-paragraph
					  fill-paragraph-or-region))
      (progn
	(define-key kotl-mode-map "\C-c\M-q" 'kotl-mode:fill-cell)
	(define-key kotl-mode-map "\M-\C-q"  'kotl-mode:fill-tree)))
  (define-key kotl-mode-map "\C-cs"     'kotl-mode:split-cell)
  (define-key kotl-mode-map "\C-c\C-s"  'kotl-mode:show-tree)
  (define-key kotl-mode-map "\C-c\C-\\" 'kotl-mode:show-tree)
  (define-key kotl-mode-map "\M-s"      'kotl-mode:center-line)
  (define-key kotl-mode-map "\M-S"      'kotl-mode:center-paragraph)
  (define-key kotl-mode-map "\C-ct"     'kotl-mode:transpose-cells)
  (define-key kotl-mode-map "\C-c\C-t"  'kotl-mode:top-cells)
  (define-key kotl-mode-map "\C-cu"     'kotl-mode:up-level)
  (define-key kotl-mode-map "\C-c\C-u"  'kotl-mode:up-level)
  (define-key kotl-mode-map "\C-c\C-v"  'kvspec:activate)
  (define-key kotl-mode-map "\C-x\C-w"  'kfile:write)
  (define-key kotl-mode-map [M-up]              'kotl-mode:move-tree-backward)
  (define-key kotl-mode-map (kbd "ESC <up>")    'kotl-mode:move-tree-backward)
  (define-key kotl-mode-map [M-down]            'kotl-mode:move-tree-forward)
  (define-key kotl-mode-map (kbd "ESC <down>")  'kotl-mode:move-tree-forward)
  (mapc (lambda (key)
	  (define-key kotl-mode-map key         'kotl-mode:promote-tree))
	(list (kbd "M-<left>") (kbd "M-S-<left>") (kbd "ESC <left>")
	      (kbd "ESC S-<left>") (kbd "C-c C-<") (kbd "C-c C-,")))
  (mapc (lambda (key)
	  (define-key kotl-mode-map key         'kotl-mode:demote-tree))
	(list (kbd "M-<right>") (kbd "M-S-<right>") (kbd "ESC <right>")
	      (kbd "ESC S-<right>") (kbd "C-c C->") (kbd "C-c C-."))))

;; When delete-selection-mode (pending-delete-mode) is enabled, make
;; these commands delete the region.
(put 'kotl-mode:quoted-insert 'delete-selection t)

(put 'kotl-mode:yank 'delete-selection 'yank)
(put 'kotl-mode:clipboard-yank 'delete-selection 'yank)
(put 'kimport:insert-register 'delete-selection t)

(put 'kotl-mode:delete-backward-char 'delete-selection 'supersede)
(put 'kotl-mode:delete-forward-char 'delete-selection 'supersede)
(put 'kotl-mode:delete-char 'delete-selection 'supersede)

(put 'kotl-mode:reindent-then-newline-and-indent 'delete-selection t)
(put 'kotl-mode:newline-and-indent 'delete-selection t)
(put 'kotl-mode:newline 'delete-selection t)
(put 'kotl-mode:electric-newline-and-maybe-indent 'delete-selection t)
(put 'kotl-mode:open-line 'delete-selection t)

(defun delete-selection-pre-hook ()
  "Function run before commands that delete selections are executed.
Commands which will delete the selection need a `delete-selection'
property on their symbol; commands which insert text but don't
have this property won't delete the selection.
See `delete-selection-helper'."
  (when (and delete-selection-mode (use-region-p)
	     (not buffer-read-only))
    (let ((deletion-type (and (symbolp this-command)
			      (get this-command 'delete-selection))))
      (when (and deletion-type (kotl-mode:maybe-shrink-region-p))
	(delete-selection-helper deletion-type)))))

(provide 'kotl-mode)

;;; kotl-mode.el ends here
