;;; hproperty.el --- GNU Emacs button highlighting and flashing support -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Aug-92
;; Last-Mod:     26-Apr-25 at 20:25:33 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1992-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Can't use read-only buttons here because then outline-mode
;;   becomes unusable.

;;; Code:

;; (when noninteractive
;;   ;; Don't load this library
;;   (with-current-buffer " *load*"
;;     (goto-char (point-max))))

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hload-path)
(require 'custom) ;; For defface.
(require 'hbut)

;; Comment out next line out because this triggers loads of kview
;; which loads klink which contains a defib whose priority should be set
;; by loading klink from hibtypes.el instead.
;; (eval-when-compile (require 'hyrolo))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hproperty:but-highlight-flag t
  "*Non-nil means highlight named Hyperbole buttons with `hproperty:but-face'."
  :type 'boolean
  :group 'hyperbole-buttons)

(defcustom hproperty:but-emphasize-flag nil
  "*Non-nil means emphasize selectability of Hyperbole button labels.
This is shown when hovering over the button with the mouse."
  :type 'boolean
  :group 'hyperbole-buttons)

(defcustom hproperty:but-flash-time 1000
  "*Emacs button flash delay."
  :type '(integer :match (lambda (_widget value) (and (integerp value) (> value 0))))
  :group 'hyperbole-buttons)
(make-obsolete-variable 'hproperty:but-flash-time "Use `hproperty:but-flash-time-seconds' instead" "8.0")

(defcustom hproperty:but-flash-time-seconds 0.05
  "*Emacs button flash delay."
  :type 'float
  :group 'hyperbole-buttons)

(defface hbut-flash
  '((((class color) (min-colors 88) (background light))
     :background "red3")
    (((class color) (min-colors 88) (background dark))
     :background "red3")
    (((class color) (min-colors 16) (background light))
     :background "red3")
    (((class color) (min-colors 16) (background dark))
     :background "red3")
    (((class color) (min-colors 8))
     :background "red3" :foreground "black")
    (t :inverse-video t))
  "Face for flashing buttons."
  :group 'hyperbole-buttons)

(defcustom hproperty:flash-face 'hbut-flash
  "Hyperbole face for flashing hyper-buttons."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

(defcustom hproperty:highlight-face 'highlight
  "Item highlighting face."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

(defface hbut-face
  '((((min-colors 88) (background dark)) (:foreground "salmon1"))
    (((background dark)) (:background "red" :foreground "black"))
    (((min-colors 88)) (:foreground "salmon4"))
    (t (:background "red")))
  "Face for explicit Hyperbole buttons."
  :group 'hyperbole-buttons)

(defcustom hproperty:but-face 'hbut-face
  "Hyperbole face for explicit buttons."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

(defface hbut-item-face
  '((((class color) (min-colors 88) (background light))
     :background "yellow")
    (((class color) (min-colors 88) (background dark))
     :background "yellow")
    (((class color) (min-colors 16) (background light))
     :background "yellow")
    (((class color) (min-colors 16) (background dark))
     :background "yellow")
    (((class color) (min-colors 8))
     :background "yellow" :foreground "black")
    (t :inverse-video t))
  "Face for Hyperbole list buttons."
  :group 'hyperbole-buttons)

(defcustom hproperty:item-face 'hbut-item-face
  "Hyperbole face for list hyper-buttons."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

(defface ibut-face
  '((((min-colors 88) (background dark)) (:foreground "rosybrown"))
    (((background dark)) (:background "rosybrown" :foreground "black"))
    (((min-colors 88)) (:foreground "rosybrown"))
    (t (:background "rosybrown")))
  "Face for implicit Hyperbole buttons."
  :group 'hyperbole-buttons)

(defcustom hproperty:ibut-face 'ibut-face
  "Hyperbole face for implicit buttons."
  :type 'face
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Support NEXTSTEP and X window systems.
(and (not (fboundp 'display-color-p))
     (fboundp 'x-display-color-p)
     (defalias 'display-color-p 'x-display-color-p))

(defun hproperty:but-add (start end face)
  "Add between START and END a button using FACE in current buffer.
Button is added only if it does not already exist.

If `hproperty:but-emphasize-flag' is non-nil when this is called,
emphasize that button is selectable whenever the mouse cursor
moves over it."
  (let ((but (hproperty:but-get start 'face face)))
    (unless (and but (eq start (hproperty:but-start but))
		 (eq end (hproperty:but-end but)))
      (setq but (make-overlay start end nil t))
      (overlay-put but 'face face)
      (when hproperty:but-emphasize-flag
	(overlay-put but 'mouse-face 'highlight)))))

(defun hproperty:but-clear (&optional pos property value)
  "Remove highlighting from any named Hyperbole button at point or POS.
If optional PROPERTY and VALUE are given, remove highlighting only if
the PROPERTY at the position matches VALUE."
  (let ((but (hproperty:but-get pos property value)))
    (when but
      (delete-overlay but))))

(defun hproperty:but-clear-all (&optional regexp-match)
  "Remove highlighting from all named Hyperbole buttons in buffer.
If REGEXP-MATCH is non-nil, only buttons matching this argument are
de-highlighted."
  (if regexp-match
      (progn (ebut:map (lambda (_lbl start end)
			 (remove-overlays start end 'face hproperty:but-face))
		       regexp-match 'include-delims)
	     (ibut:map (lambda (_lbl start end)
			 (remove-overlays start end 'face hproperty:ibut-face))
		       regexp-match 'include-delims))
    ;; Do across whole buffer
    (remove-overlays nil nil 'face hproperty:but-face)
    (remove-overlays nil nil 'face hproperty:ibut-face)))

(defun hproperty:but-clear-all-in-list (hbut-list)
  "Delete all HBUT-LIST hproperties."
  (mapc #'delete-overlay hbut-list))

(defun hproperty:but-create (&optional regexp-match)
  "Highlight all named Hyperbole buttons in buffer.
De-highlight buttons unless `hproperty:but-highlight-flag' is set.

If REGEXP-MATCH is non-nil, only buttons matching this argument are
highlighted (all others are unhighlighted).

If `hproperty:but-emphasize-flag' is non-nil when this is called,
emphasize that button is selectable whenever the mouse cursor
moves over it."
  (interactive)
  (hproperty:but-clear-all regexp-match)
  (hproperty:but-create-all regexp-match))

(defun hproperty:but-create-all (&optional regexp-match)
  "Highlight all named Hyperbole buttons in buffer.
De-highlight buttons unless `hproperty:but-highlight-flag' is set.

If REGEXP-MATCH is non-nil, only buttons matching this argument are
highlighted.

If `hproperty:but-emphasize-flag' is non-nil when this is called,
emphasize that button is selectable whenever the mouse cursor
moves over it."
  (when hproperty:but-highlight-flag
    (ebut:map (lambda (_lbl start end)
		(hproperty:but-add start end hproperty:but-face))
	      regexp-match 'include-delims)
    (ibut:map (lambda (_lbl start end)
		(hproperty:but-add start end hproperty:ibut-face))
	      regexp-match 'include-delims)))

(defun hproperty:but-create-on-yank (_prop-value start end)
  "Used in `yank-handled-properties' called with START and END pos of the text."
  (save-restriction
    (narrow-to-region start end)
    (hproperty:but-create-all)))

(defun hproperty:but-delete (hproperty-but)
  "Remove HPROPERTY-BUT.  See `hproperty:but-get'."
  (delete-overlay hproperty-but))

(defun hproperty:but-end (hproperty-but)
  "Return the start position of an HPROPERTY-BUT.
See `hproperty:but-get'."
  (overlay-end hproperty-but))

(defun hproperty:but-get-all-in-region (start end &optional property value)
  "Return all buttons in the current buffer between START and END.
If optional PROPERTY and non-nil VALUE are given, return only matching
buttons.

Use `hproperty:but-get-first-in-region' instead if you want only the first
matching button."
  (let ((val-list (if property
		      (list value)
		    (list hproperty:but-face
			  hproperty:ibut-face
			  hproperty:flash-face))))
    (delq nil
	  (mapcar (lambda (overlay)
		    (and (bufferp (overlay-buffer overlay))
			 (memq (overlay-get overlay (or property 'face))
			       val-list)
			 overlay))
		  (overlays-in start end)))))

(defun hproperty:but-get-first-in-region (start end property value)
  "Return list of first button between START & END with PROPERTY & VALUE.
Return nil if none."
  (catch 'first
    (mapc (lambda (overlay)
	    (when (and (bufferp (overlay-buffer overlay))
		       (memq (overlay-get overlay property)
			     (list value)))
	      (throw 'first (list overlay))))
	  (overlays-in start end))))

(defun hproperty:but-get (&optional pos property value)
  "Return button at optional POS or point.
If optional PROPERTY and VALUE are given, return only the first button
with that PROPERTY and VALUE."
  (car (hproperty:but-get-first-in-region pos (1+ pos) property value)))

(defun hproperty:but-move (hproperty-but start end &optional buffer)
  "Set the endpoints of HPROPERTY-BUT to START and END in optional BUFFER.
If BUFFER is nil and HPROPERTY-BUT has no buffer, put it in the current buffer;
otherwise, if BUFFER is omitted, leave HPROPERTY-BUT in the same
buffer it presently inhabits."
  (move-overlay hproperty-but start end buffer))

(defun hproperty:but-start (hproperty-but)
  "Return the end position of an HPROPERTY-BUT.
See `hproperty:but-get'."
  (overlay-start hproperty-but))

(add-to-list 'yank-handled-properties '(hproperty:but-face . hproperty:but-create-on-yank))

;;; char-property and overlay utility functions

(defun hproperty:char-property-contains-p (pos property value)
  "At POS if PROPERTY contains VALUE (a symbol), return VALUE, else nil."
  (let ((val (get-char-property pos property)))
    (when (or (eq val value)
	      ;; `val' may be a list of property values
	      (and (listp val) (memq value val)))
      value)))

(defun hproperty:char-property-start (pos property value)
  "From POS, return the start of text PROPERTY with VALUE overlapping POS.
Otherwise, return nil.  Value must be a symbol."
  (when-let* ((val (hproperty:char-property-contains-p pos property value))
	      (prev pos))
    ;; Can't use `previous-single-char-property-change' below
    ;; because it checks for any change in the property value, not
    ;; just if the property contains the value desired.
    (while (and (setq prev (1- prev))
		(>= prev (point-min))
		(hproperty:char-property-contains-p prev property value)))
    (max (1+ prev) (point-min))))

(defun hproperty:char-property-end (pos property value)
  "From POS, return the end of text PROPERTY with VALUE overlapping POS.
Otherwise, return nil.  Value must be a symbol."
  (when-let* ((val (hproperty:char-property-contains-p pos property value))
	      (next pos))
    ;; Can't use `next-single-char-property-change' below
    ;; because it checks for any change in the property value, not
    ;; just if the property contains the value desired.
    (while (and (setq next (1+ next))
		(<= next (point-max))
		(hproperty:char-property-contains-p next property value)))
    (min next (point-max))))

(defun hproperty:char-property-range (pos property &optional value)
  "Return a char-property range (start . end) at POS where PROPERTY = VALUE.
Return nil if no such range.  If POS is nil, use point.
VALUE is optional; if omitted, use the first char-property at POS with PROPERTY."
  (unless pos (setq pos (point)))
  (let ((start pos)
        (end pos)
	val)
    (when (or (null value)
	      (eq value (setq val (hproperty:char-property-contains-p pos property value))))
      (setq start (hproperty:char-property-start pos property val)
	    end   (hproperty:char-property-end   pos property val)))
    (unless (or (null start) (null end) (= start end))
      (cons start end))))

(defun hproperty:overlay-range (pos property &optional value)
  "Return the first overlay range (start . end) at POS where PROPERTY = VALUE.
Return nil if no such overlay range.  If POS is nil, use point.
VALUE is optional; if omitted, use the first overlay at POS with PROPERTY.

Use `hproperty:char-property-range' for the same capability but for
both text-properties and overlays."
  (unless pos (setq pos (point)))
  (let ((start pos)
        (end pos)
	val)
    (catch 'end
      (dolist (overlay (overlays-at pos t))
	(when (and (setq val (overlay-get overlay property))
		   (or (null value) (eq val value)))
          (setq start (overlay-start overlay)
		end   (overlay-end overlay))
	  (throw 'end nil))))
    (unless (= start end)
      (cons start end))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defsubst hproperty:list-cycle (list-ptr list)
  "Move LIST-PTR to next element in LIST or when at end to first element."
  (or (and list-ptr (setq list-ptr (cdr list-ptr)))
      (setq list-ptr list)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hproperty:color-list
  (when (display-color-p)
    (defined-colors)))

(defvar hproperty:color-ptr nil
  "Pointer to current color name table to use for Hyperbole buttons.")

(defconst hproperty:good-colors
  (if (display-color-p)
      '(
	"medium violet red" "indianred4" "firebrick1" "DarkGoldenrod"
	"NavyBlue" "darkorchid" "tomato3" "mediumseagreen" "deeppink"
	"forestgreen" "mistyrose4" "slategrey" "purple4" "dodgerblue3"
	"mediumvioletred" "lightsalmon3" "orangered2" "turquoise4" "Gray55")
    hproperty:color-list)
  "Good colors for contrast against wheat background and black foreground.")


;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hproperty:cycle-but-color (&optional color)
  "Switch button color.
Set color to optional COLOR name or next item referenced by
hproperty:color-ptr."
  (interactive "sHyperbole button color: ")
  (when (display-color-p)
    (when color (setq hproperty:color-ptr nil))
    (set-face-foreground
     hproperty:but-face (or color (car (hproperty:list-cycle hproperty:color-ptr hproperty:good-colors))))
    (redisplay t)
    t))

(defun hproperty:but-p (&optional pos property value)
  "Return non-nil at point or optional POS iff on a highlighted Hyperbole button."
  (memq t (mapcar (lambda (overlay)
		    (when (memq (overlay-get overlay (or property 'face))
				(if property
				    (list value)
				  (list hproperty:but-face hproperty:ibut-face)))
		      t))
		  (overlays-at (or pos (point))))))

(defun hproperty:set-but-face (pos face)
  "Set button face at POS to FACE."
  (let ((but (hproperty:but-get pos)))
    (when but (overlay-put but 'face face))))

(defun hproperty:but-flash ()
  "Flash a Hyperbole button at or near point to indicate selection."
  (interactive)
  (let* ((categ (hattr:get 'hbut:current 'categ))
	 (start (hattr:get 'hbut:current 'lbl-start))
	 (end   (hattr:get 'hbut:current 'lbl-end))
	 (ibut)
	 (prev)
	 (categ-face))
    (if (eq categ 'explicit)
	(setq categ-face hproperty:but-face)
      (setq categ-face hproperty:ibut-face
	    ibut t))
    (if (and start end)
	(unless (setq prev (hproperty:but-p start))
	  (hproperty:but-add start end categ-face))
      (setq start (point)))
    (when (hproperty:but-p start)
      (unwind-protect
	  (progn
	    (hproperty:set-but-face start hproperty:flash-face)
	    (sit-for hproperty:but-flash-time-seconds)) ;; Force display update
	(hproperty:set-but-face start categ-face)
	(redisplay t)))
    (and ibut (not prev) (hproperty:but-clear start))))

(defun hproperty:select-item (&optional pnt)
  "Select item in current buffer at optional position PNT with hproperty:item-face."
  (when pnt (goto-char pnt))
  (skip-chars-forward " \t")
  (skip-chars-backward "^ \t\n\r")
  (let ((start (point)))
    (save-excursion
      (skip-chars-forward "^ \t\n\r")
      (hproperty:but-add start (point) hproperty:item-face)))
  (sit-for 0))  ;; Force display update

(defun hproperty:select-line (&optional pnt)
  "Select line in current buffer at optional position PNT with hproperty:item-face."
  (when pnt (goto-char pnt))
  (hproperty:but-add (line-beginning-position) (line-end-position) hproperty:item-face)
  ;; Force display update
  (sit-for 0))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hproperty:item-button nil
  "Button used to highlight an item in a listing buffer.")
(make-variable-buffer-local 'hproperty:item-button)

(provide 'hproperty)

;;; hproperty.el ends here
