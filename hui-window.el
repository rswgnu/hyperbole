;;; hui-window.el --- Smart Mouse Key window and modeline depress/release actions.  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Sep-92
;; Last-Mod:      7-Oct-22 at 23:39:57 by Mats Lidell
;;
;; Copyright (C) 1992-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Must be loaded AFTER hmouse-alist has been defined in "hui-mouse.el".
;;
;;   Handles drags in same window or across windows and modeline presses.
;;
;; What drags and modeline presses do.  (Note that a `thing' is a
;; delimited expression, such as a string, list or markup language tag pair).
;; ======================================================================================================
;;                                              Smart Keys
;; Context                         Action Key                  Assist Key
;; ======================================================================================================
;; Drag from thing start or end    Yank thing at release       Kill thing and yank at release
;;
;; Drag from bottom Modeline       Reposition frame as         <- same
;; in frame with non-nil           drag happens
;; drag-with-mode-line param

;; Drag from shared window side
;;   or from left of scroll bar    Resize window width         <- same
;; Modeline vertical drag          Resize window height        <- same
;;
;; Other Modeline drag to          Replace dest. buffer with   Swap window buffers
;;   another window                  source buffer
;;
;; Drag to a Modeline from:
;;   buffer/file menu item         Display buffer/file in      Swap window buffers
;;                                   new window by release
;;   buffer/file menu 1st line     Move buffer/file menu to    Swap window buffers
;;                                   new window by release
;;   anywhere else                 Display buffer in           Swap window buffers
;;                                   new window by release
;;
;; Drag between windows from:
;;   buffer/file menu item         Display buffer/file in      Swap window buffers
;;                                   window of button release
;;   buffer/file menu 1st line     Move buffer/file menu       Swap window buffers
;;   anywhere else                 Create/edit a link button   Swap window buffers
;;
;; Drag outside of Emacs from:
;;   buffer/file menu item         Display buffer/file in      Move window to a new frame
;;                                   a new frame
;;   Modeline or other window      Clone window to a new frame Move window to a new frame
;;
;; Click in modeline
;;     Left modeline edge          Bury buffer                 Unbury bottom buffer
;;     Right modeline edge         Info                        Smart Key Summary
;;     Buffer ID                   Dired on buffer's dir       Next buffer
;;                                   or on parent when a dir
;;     Other blank area            Action Key modeline hook    Assist Key modeline hook
;;                                   Show/Hide Buffer Menu      Popup Jump & Manage Menu
;;
;; Drag in a window, region active Error, not allowed          Error, not allowed
;; Drag horizontally in a window   Split window below          Delete window
;; Drag vertically in a window     Split window side-by-side   Delete window
;; Drag diagonally in a window     Save window-config          Restore window-config from ring
;;
;; Active region exists, click     Yank region at release      Kill region and yank at release
;;   outside of the region

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (defvar assist-flag nil)) ;; Silences free variable compiler warnings
(require 'hycontrol)
;; If installed, use pulse library for momentary highlighting of buffer/file item lines.
(require 'pulse nil t)
(require 'hui-select)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom action-key-minibuffer-function #'hyperbole
  "*Function run by the Action Key after a click in an inactive minibuffer.
Its default value is `hyperbole', which displays the Hyperbole minibuffer menu."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-minibuffer-function #'hui-menu-screen-commands
  "*Function run by the Assist Key after a click in an inactive minibuffer.
Its default value is `hui-menu-screen-commands', which displays a popup menu
of screen control commands."
  :type 'function
  :group 'hyperbole-keys)

;; Set this to `hmouse-context-ibuffer-menu' if you use the ibuffer package.
(defcustom action-key-modeline-function #'hmouse-context-menu
  "Function to call when Action Mouse Key is clicked in the center of a modeline."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-modeline-function #'hui-menu-screen-commands
  "Function to call when Assist Mouse Key is clicked in the center of a modeline."
  :type 'function
  :group 'hyperbole-keys)

(defun hmouse-map-modes-to-form (mode-forms)
  "Map over MODE-FORMS, a sequence of (major-mode(s) form-to-eval) lists.
Return items with a single `major-mode' in the car, (major-mode form-to-eval)."
  (apply 'nconc
	 (mapcar (lambda (modes-form)
		   (if (sequencep (car modes-form))
		       (mapcar (lambda (mode) (cons mode (cdr modes-form)))
			       (car modes-form))
		     (list modes-form)))
		 mode-forms)))

(defvar hmouse-drag-item-mode-forms
  (hmouse-map-modes-to-form
  '((Buffer-menu-mode (Buffer-menu-buffer t))
    (ibuffer-mode (ibuffer-current-buffer t))
    (helm-major-mode (helm-get-selection (current-buffer)))
    ;; Note how multiple major modes may be grouped with a single form for item getting.
    ((dired-mode vc-dired-mode wdired-mode) (or (when (dired-get-filename nil t)
						  (hmouse-dired-display-here-mode 1)
						  (dired-get-filename nil t))
						;; Drag from first line current directory
						;; means move this dired buffer to the
						;; release window.
						(prog1 (current-buffer)
						  (hmouse-pulse-buffer)
						  (bury-buffer))))
     (treemacs-mode (if (fboundp 'treemacs-node-buffer-and-position)
			(treemacs-node-buffer-and-position))
		    (error "(hmouse-item-to-window): %s the treemacs package for item dragging support"
			   (if (fboundp 'treemacs) "Update" "Install")))))
  "List of (major-mode lisp-form) lists.
The car of an item must be a `major-mode' symbol.  The cadr of an item
is a Lisp form to evaluate to get the item name at point (typically a
buffer, file or directory name whose contents will be displayed in the
drag release window.")


(defcustom hmouse-pulse-flag t
  "Non-nil means pulse visually if supported.
When display supports visual pulsing, then pulse lines and
buffers when an Action Key drag is used to place a buffer or file
in a window."
  :type 'boolean
  :group 'hyperbole-keys)

 ;; Mats Lidell says this should be 10 characters for GNU Emacs.
(defvar hmouse-edge-sensitivity 10
  "*Number of chars from window edges in which a click is considered at an edge.")

(defvar hmouse-side-sensitivity 5
  "*Characters from window side within which a click is considered on the side.
Either direction from window side is considered.")

(defvar hmouse-x-drag-sensitivity 5
  "*Number of characters between mouse depress/release for a horizontal drag.
The number of characters mouse must move horizontally between a
depress and release to register as a horizontal drag.")

(defvar hmouse-y-drag-sensitivity 3
  "*Number of lines between mouse depress/release to register a vertical drag.
The number of lines mouse must move vertically between a depress
and release to register a vertical drag")

(defvar hmouse-x-diagonal-sensitivity 4
  "*Number of characters between mouse depress/release to register a diagonal drag.
The number of characters mouse must move horizontally between a
depress and release to register a diagonal drag")
(defvar hmouse-y-diagonal-sensitivity 3
  "*Number of lines between mouse depress/release to register a diagonal drag.
The number of lines mouse must move vertically between a depress
and release to register a diagonal drag.")

;; Ensure any helm item at Action Mouse Key depress point is selected
;; before a drag that ends in another window.
(add-hook 'action-key-depress-hook
	  (lambda () (if (eq major-mode 'helm-major-mode)
			 ;; Select any line with an action.
			 (smart-helm-line-has-action))))

;;;
;;; Add window handling to hmouse-alist dispatch table.

(defvar hmouse-alist)
(defun hmouse-alist-add-window-handlers ()
  (unless (assoc #'(hmouse-inactive-minibuffer-p) hmouse-alist)
    (setq hmouse-alist
	  (append
	   '(
	     ((hmouse-drag-thing) .
	      ((hmouse-yank-region) . (hmouse-kill-and-yank-region)))
	     ((hmouse-drag-window-side) .
	      ((hmouse-resize-window-side) . (hmouse-resize-window-side)))
	     ;;
	     ;; Although Hyperbole can distinguish whether inter-window
	     ;; drags are between frames or not, having different behavior
	     ;; for those 2 cases could be confusing, so treat all
	     ;; modeline drags between windows the same and comment out
	     ;; this next clause.
	     ;;   Modeline drag between frames
	     ;;   ((and (hmouse-modeline-depress) (hmouse-drag-between-frames)) .
	     ;;    ((hmouse-clone-window-to-frame) . (hmouse-move-window-to-frame)))
	     ;;
	     ;; Drag from within a window (not a Modeline) with release on a Modeline
	     ((and (not (hmouse-modeline-depress)) (hmouse-modeline-release)
		   (not (hmouse-modeline-click))) .
	      ((or (hmouse-drag-item-to-display t) (hmouse-buffer-to-window t)) .
	       (hmouse-swap-buffers)))
	     ;; Non-vertical Modeline drag between windows
	     ((and (hmouse-modeline-depress) (hmouse-drag-between-windows)
		   (not (hmouse-drag-vertically-within-emacs))) .
		   ((hmouse-buffer-to-window) . (hmouse-swap-buffers)))
	     ;; Modeline drag that ends outside of Emacs
	     ((and (hmouse-modeline-depress) (hmouse-drag-outside-all-windows)) .
	      ((hycontrol-clone-window-to-new-frame) . (hycontrol-window-to-new-frame)))
	     ;; Other Modeline click or drag
	     ((hmouse-modeline-depress) .
	      ((action-key-modeline) . (assist-key-modeline)))
	     ((hmouse-drag-between-windows) .
	      ;; Note that `hui:link-directly' uses any active
	      ;; region as the label of the button to create.
	      ((or (hmouse-drag-item-to-display) (hui:link-directly)) . (hmouse-swap-buffers)))
	     ((hmouse-drag-region-active) .
	      ((hmouse-drag-not-allowed) . (hmouse-drag-not-allowed)))
	     ((setq hkey-value (hmouse-drag-horizontally)) .
	      ((hmouse-horizontal-action-drag) . (hmouse-horizontal-assist-drag)))
	     ((hmouse-drag-vertically) .
	      ((hmouse-vertical-action-drag) . (hmouse-vertical-assist-drag)))
	     ((setq hkey-value (hmouse-drag-diagonally)) .
	      ((call-interactively #'hywconfig-ring-save) .
	       (call-interactively #'hywconfig-yank-pop)))
	     ;; Window drag that ends outside of Emacs
	     ((hmouse-drag-outside-all-windows) .
	      ((or (hmouse-drag-item-to-display)
		   (hycontrol-clone-window-to-new-frame)) .
		   (hycontrol-window-to-new-frame)))
	     ;; If click in the minibuffer when it is not active (blank),
	     ;; display the Hyperbole minibuffer menu or popup the jump menu.
	     ((hmouse-inactive-minibuffer-p) .
	      ((funcall action-key-minibuffer-function) .
	       (funcall assist-key-minibuffer-function)))
	     ((and (boundp 'ivy-mode) ivy-mode (minibuffer-window-active-p (selected-window))) .
	      ((ivy-mouse-done action-key-release-args) . (ivy-mouse-dispatching-done assist-key-release-args)))
	     ;; Handle widgets in Custom-mode
	     ((eq major-mode 'Custom-mode) .
	      ((smart-custom) . (smart-custom-assist)))
	     ;;
	     ;; Now since this is not a drag and if there was an active
	     ;; region prior to when the Action or Assist Key was
	     ;; pressed, then store point at one end of the region into
	     ;; `hkey-value' and the string value of the region
	     ;; into `hkey-region' which is either yanked, or
	     ;; killed and yanked at the current point.
	     ((hmouse-prior-active-region) .
	      ((hmouse-yank-region) . (hmouse-kill-and-yank-region)))
	     ;;
	     )
	   hmouse-alist))))
(with-eval-after-load 'hui-mouse (hmouse-alist-add-window-handlers))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-at-item-p (start-window)
  "Return t if point is on an item draggable by Hyperbole, otherwise nil."
  (let* ((buf (when (window-live-p start-window)
		(window-buffer start-window)))
	 (mode (when buf
		 (buffer-local-value 'major-mode buf))))
    (and buf (with-current-buffer buf
	       ;; Point must be on an item, not after one
	       (not (looking-at "\\s-*$")))
	 (memq mode (mapcar #'car hmouse-drag-item-mode-forms))
	 t)))

(defun hmouse-context-menu ()
  "If running under a window system, display or hide the buffer menu.
If not running under a window system and Smart Menus are loaded, display the
appropriate Smart Menu for the context at point.  (Smart Menus are a
part of InfoDock and not a part of Hyperbole)."
  (interactive)
  (if (and (fboundp 'smart-menu)
	   (null window-system))
      (smart-menu)
    (let ((wind (get-buffer-window "*Buffer List*"))
	  owind)
      (if wind
	  (unwind-protect
	      (progn (setq owind (selected-window))
		     (select-window wind)
		     (bury-buffer nil))
	    (select-window owind))
	(buffer-menu)))))

(defun hmouse-context-ibuffer-menu ()
  "If running under a window system, display or hide the IBuffer menu.
If not running under a window system and Smart Menus are loaded, display the
appropriate Smart Menu for the context at point.  (Smart Menus are a
part of InfoDock and not a part of Hyperbole)."
  (interactive)
  (if (and (fboundp 'smart-menu)
           (null window-system))
      (smart-menu)
    (let ((wind (get-buffer-window "*Ibuffer*"))
	  owind)
      (if wind
	  (unwind-protect
	      (progn (setq owind (selected-window))
		     (select-window wind)
		     (bury-buffer nil))
	    (select-window owind))
	(ibuffer)))))

(defun hmouse-prior-active-region ()
  "Return t if there is an active region in buffer of last Smart Mouse Key release."
  (when (and (setq hkey-value (if assist-flag assist-key-depress-prev-point action-key-depress-prev-point))
	     (buffer-live-p (marker-buffer hkey-value)))
    (save-excursion
      (with-current-buffer (marker-buffer hkey-value)
	;; Store and goto any prior value of point from the region
	;; prior to the Smart Key depress, so we can return to it later.
	(and (goto-char hkey-value)
	     (hmouse-save-region)
	     t)))))

(defun hmouse-dired-readin-hook ()
  "Remove local `hpath:display-where' setting whenever re-read a dired directory.
See `hmouse-dired-item-dragged' for use."
  (hmouse-dired-display-here-mode 0))

(define-minor-mode hmouse-dired-display-here-mode
  "Display item here on key press after dired item drag.
Once a dired buffer item has been dragged, make next Action Key
press on an item display it in the same dired window.

By default an Action Key press on a dired item displays it in another
window.   But once a Dired item is dragged to another window, the next
Action Key press should display it in the dired window so that the
behavior matches that of Buffer Menu and allows for setting what is
displayed in all windows on screen, including the dired window.

If the directory is re-read into the dired buffer with {g}, then Action
Key behavior reverts to as though no items have been dragged."
  :lighter " DisplayHere"
  (if hmouse-dired-display-here-mode
      (progn (set (make-local-variable 'hpath:display-where) 'this-window)
	     (add-hook 'dired-after-readin-hook 'hmouse-dired-readin-hook nil t))
    (kill-local-variable 'hpath:display-where)
    (remove-hook 'dired-after-readin-hook 'hmouse-dired-readin-hook t)))

(defun hmouse-drag-region-active ()
  "Return non-nil if drag region is active.
If an active region existed in the depress buffer prior to the
depress and a drag motion has occurred, return non-nil ."
  (save-excursion
    (and (hmouse-goto-region-prev-point)
	 (hmouse-use-region-p)
	 (or (hmouse-drag-vertically) (hmouse-drag-horizontally) (hmouse-drag-diagonally))
	 (setq hkey-value (point)))))

(defun hmouse-drag-thing ()
  "Return t if drag began at a thing and ended some other place in the same buffer.
If no region is active and a Smart Key drag began at the
start/end of a delimited construct and ended at some other point
in the same buffer, return t else nil.

Delimited constructs include lists, comments, strings,
 arrays/vectors, sets, and markup pair tags, such as <div>
 </div>.  Point must be on the start or end delimiter or in the
 case of markup pair tags, on the first character of either tag.
 For strings and comments, point must be on the first line."
  ;; Move point back to Smart Key depress location for testing whether at a thing.
  (let ((depress-args (if assist-flag assist-key-depress-args action-key-depress-args))
	(release-args (if assist-flag assist-key-release-args action-key-release-args))
	(marked-thing)
	(ignore-drag))
    (save-excursion
      (hmouse-goto-depress-point)
      (if (and (not (hmouse-use-region-p)) (hui-select-at-delimited-thing-p)
	       (or (markerp depress-args) (markerp release-args)
		   (and (not (or (hmouse-drag-window-side) (hmouse-modeline-depress)))
			(or (hmouse-drag-between-windows) (hmouse-drag-vertically)
			    (hmouse-drag-horizontally) (hmouse-drag-diagonally))))
	       (let ((start-buf (window-buffer (smart-window-of-coords depress-args)))
		     (end-buf (window-buffer (smart-window-of-coords release-args)))
		     (start-point (smart-point-of-coords depress-args))
		     (end-point (smart-point-of-coords release-args)))
		 ;; If it is a click, return nil; if drag end point
		 ;; is within the thing to operate upon, don't set a
		 ;; region, so no operation will be performed but
		 ;; return t (ignore drag).
		 (not (and (eq start-buf end-buf)
 			   start-point
			   end-point
			   (/= start-point end-point)
			   (setq marked-thing (hui-select-delimited-thing))
			   (setq ignore-drag (and (> end-point (min (point) (mark)))
						  (< end-point (max (point) (mark)))))))))
	  (progn (when (not (hmouse-use-region-p)) (hui-select-delimited-thing))
		 ;; Erase any saved location of a region prior to Smart Key depress since now we have a
		 ;; new region location.  This prevents hmouse-kill-and-yank-region from jumping to the
		 ;; old location.
		 (if assist-flag
		     (setq assist-key-depress-prev-point nil)
		   (setq action-key-depress-prev-point nil))
		 ;; Store any new value of point as a result of marking the region, so we can return to it
		 ;; later.
		 (setq hkey-value (point))
		 (hmouse-save-region)
		 t)
	(if marked-thing (deactivate-mark))
	(when ignore-drag (error "(Hyperbole): Smart Key drag of a delimited thing must end outside of the thing"))
	nil))))

(defun hmouse-kill-region ()
  "Kill the marked region near where the Smart Key was depressed.
Signals an error if the depress buffer is read-only."
  ;; Region may be in another buffer, so move there if so.
  (hmouse-goto-region-prev-point)
  (if buffer-read-only
      ;; In this case, we want an error that will terminate execution so that
      ;; hkey-region is not reset to nil.  This allows the user to fix the
      ;; problem and then to try killing again.
      (error "(hmouse-kill-region): Use {%s} to enable killing from this buffer"
	     (hmouse-read-only-toggle-key))
    (kill-region (or hkey-value (point)) (mark))))

(defun hmouse-kill-and-yank-region ()
  "Kill the marked region and yank it at the point of release.
Kill the marked region near where the Smart Key was depressed and
yank it at the point of release.
Signals an error if either depress or release buffer is read-only."
  (when hkey-region
    ;; Move point back to Smart Key depress buffer.
    (hmouse-goto-depress-point)
    (if buffer-read-only
	;; In this case, we want an error that will terminate execution so that
	;; hkey-region is not reset to nil.  This allows the user to fix the
	;; problem and then to try killing again.
	(error "(hmouse-kill-and-yank-region): Use {%s} to enable killing from this buffer"
	       (hmouse-read-only-toggle-key))
      ;; Depress and release may be in the same buffer, in which case,
      ;; save the release point that may change as a result of
      ;; the kill; also, before the kill, restore the point to where it
      ;; was when the region was set.
      (hmouse-goto-release-point)
      (let ((release-point (point-marker))
	    (release-window (if assist-flag assist-key-release-window action-key-release-window)))
	(if buffer-read-only
	    ;; In this case, we want an error that will terminate execution so that
	    ;; hkey-region is not reset to nil.  This allows the user to fix the
	    ;; problem and then to try yanking again.
	    (error "(hmouse-kill-and-yank-region): Use {%s} to enable yanking into this buffer"
		   (hmouse-read-only-toggle-key))
	  ;; Region may be in another buffer, so move there if so.
	  (hmouse-goto-region-prev-point)
	  ;; Now kill and yank the region into the Smart Key release buffer.
	  (kill-region (or hkey-value (point)) (mark))
	  ;; Permanently return to release point
	  (select-frame-set-input-focus (window-frame release-window))
	  (select-window release-window)
	  (goto-char release-point)
	  ;; Protect from indentation errors
	  (condition-case ()
	      (hmouse-insert-region)
	    (error nil)))))))

(defun hmouse-yank-region ()
  "Yank the region of text saved in `hkey-region' into the current buffer.
Signals an error if the buffer is read-only."
  ;; If a region was just selected for yanking, deactivate it as we
  ;; have already copied the region into `hkey-region'.
  (when hkey-region
    (hmouse-goto-region-prev-point)
    (if (region-active-p) (deactivate-mark))
    (hmouse-goto-release-point)
    (if buffer-read-only
	;; In this case, we want an error that will terminate execution so that
	;; hkey-region is not reset to nil.  This allows the user to fix the
	;; problem and then to try yanking again.
	(error "(hmouse-yank-region): Buffer is read-only; use {%s} to enable yanking"
	       (hmouse-read-only-toggle-key))
      ;; Permanently return to release point
      (let ((release-window (if assist-flag assist-key-release-window action-key-release-window)))
	(select-frame-set-input-focus (window-frame release-window))
	(select-window release-window))
      ;; Protect from indentation errors
      (condition-case ()
	  (hmouse-insert-region)
	(error nil)))))

(defun hmouse-drag-between-frames ()
  "Return non-nil if last Action Key depress and release were in different frames.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and (window-live-p assist-key-depress-window)
	   (window-live-p assist-key-release-window)
	   (not (eq (window-frame assist-key-depress-window)
		    (window-frame assist-key-release-window))))
    (and (window-live-p action-key-depress-window)
	 (window-live-p action-key-release-window)
	 (not (eq (window-frame action-key-depress-window)
		  (window-frame action-key-release-window))))))

(defun hmouse-drag-between-windows ()
  "Return non-nil if last Action Key depress and release were in different windows.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and (window-live-p assist-key-depress-window)
	   (window-live-p assist-key-release-window)
	   (not (eq assist-key-depress-window
		    assist-key-release-window)))
    (and (window-live-p action-key-depress-window)
	 (window-live-p action-key-release-window)
	 (not (eq action-key-depress-window action-key-release-window)))))

(defun hmouse-press-release-same-window ()
  "Return non-nil if last Action Key depress and release were in the same window.
If free variable `assist-flag' is non-nil, use Assist Key."
  (if assist-flag
      (and (window-live-p assist-key-depress-window)
	   (window-live-p assist-key-release-window)
	   (eq assist-key-depress-window assist-key-release-window))
    (and (window-live-p action-key-depress-window)
	 (window-live-p action-key-release-window)
	 (eq action-key-depress-window action-key-release-window))))

(defun hmouse-drag-outside-all-windows ()
  "Return non-nil if last Action Key release was outside of an Emacs window.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and (window-live-p assist-key-depress-window) (not assist-key-release-window))
    (and (window-live-p action-key-depress-window) (not action-key-release-window))))

(defun hmouse-drag-item-to-display (&optional new-window)
  "Drag an item and release where it is to be displayed.
Depress on a buffer name in Buffer-menu/ibuffer mode or on a
file/directory in dired mode and release where the item is to be
displayed.

If depress is on an item and release is outside of Emacs, the
item is displayed in a new frame with a single window.  If the
release is inside Emacs and the optional NEW-WINDOW is non-nil,
the release window is sensibly split before the buffer is
displayed.  Otherwise, the buffer is simply displayed in the
release window.

Return t unless source buffer is not one of these modes or point is
not on an item, then nil.

See `hmouse-drag-item-mode-forms' for how to allow for draggable
items in other modes."
  (when (hmouse-at-item-p action-key-depress-window)
    (hmouse-item-to-window new-window)
    t))

(defun hmouse-drag-same-window ()
  "Return non-nil if last Action Key use was a drag in the same window.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (or (hmouse-drag-horizontally) (hmouse-drag-vertically) (hmouse-drag-diagonally)))

(defun hmouse-drag-diagonally ()
  "Return non-nil iff last Action Key use was a diagonal drag in a single window.
If free variable `assist-flag' is non-nil, use Assist Key.
Value returned is nil if not a diagonal drag, or one of the
following symbols depending on the direction of the drag:
southeast, southwest, northwest, northeast."
  (when (hmouse-press-release-same-window)
    (let ((last-depress-x) (last-release-x)
	  (last-depress-y) (last-release-y))
      (if assist-flag
	  (setq last-depress-x (hmouse-x-coord assist-key-depress-args)
		last-release-x (hmouse-x-coord assist-key-release-args)
		last-depress-y (hmouse-y-coord assist-key-depress-args)
		last-release-y (hmouse-y-coord assist-key-release-args))
	(setq last-depress-x (hmouse-x-coord action-key-depress-args)
	      last-release-x (hmouse-x-coord action-key-release-args)
	      last-depress-y (hmouse-y-coord action-key-depress-args)
	      last-release-y (hmouse-y-coord action-key-release-args)))
      (and last-depress-x last-release-x last-depress-y last-release-y
	   (>= (- (max last-depress-x last-release-x)
		  (min last-depress-x last-release-x))
	       hmouse-x-diagonal-sensitivity)
	   (>= (- (max last-depress-y last-release-y)
		  (min last-depress-y last-release-y))
	       hmouse-y-diagonal-sensitivity)
	   (cond
	    ((< last-depress-x last-release-x)
	     (if (< last-depress-y last-release-y)
		 'southeast 'northeast))
	    (t (if (< last-depress-y last-release-y)
		   'southwest 'northwest)))))))

(defun hmouse-drag-horizontally ()
  "Return non-nil iff last Action Key use was a horizontal drag in a single window.
If free variable `assist-flag' is non-nil, use Assist Key.
Value returned is nil if not a horizontal drag, \\='left if drag
moved left or \\='right otherwise."
  (when (hmouse-press-release-same-window)
    (let ((last-depress-x) (last-release-x)
	  (last-depress-y) (last-release-y))
      (if assist-flag
	  (setq last-depress-x (hmouse-x-coord assist-key-depress-args)
		last-release-x (hmouse-x-coord assist-key-release-args)
		last-depress-y (hmouse-y-coord assist-key-depress-args)
		last-release-y (hmouse-y-coord assist-key-release-args))
	(setq last-depress-x (hmouse-x-coord action-key-depress-args)
	      last-release-x (hmouse-x-coord action-key-release-args)
	      last-depress-y (hmouse-y-coord action-key-depress-args)
	      last-release-y (hmouse-y-coord action-key-release-args)))
      (and last-depress-x last-release-x last-depress-y last-release-y
	   (>= (- (max last-depress-x last-release-x)
		  (min last-depress-x last-release-x))
	       hmouse-x-drag-sensitivity)
	   ;; Don't want to register vertical drags here, so ensure any
	   ;; vertical movement was less than the vertical drag sensitivity.
	   (< (- (max last-depress-y last-release-y)
		 (min last-depress-y last-release-y))
	      hmouse-y-drag-sensitivity)
	   (if (< last-depress-x last-release-x) 'right 'left)))))

(defun hmouse-drag-vertically-within-emacs ()
  "Return non-nil iff last Action Key use was a vertical drag in the same frame.
If free variable `assist-flag' is non-nil, use Assist Key.
Value returned is nil if not a vertical line drag, \\='up if drag moved up or
\\='down otherwise."
  (unless (or (hmouse-drag-between-frames) (hmouse-drag-outside-all-windows))
    (let ((last-depress-x) (last-release-x)
	  (last-depress-y) (last-release-y))
      (if assist-flag
	  (setq last-depress-x (hmouse-x-coord assist-key-depress-args)
		last-release-x (hmouse-x-coord assist-key-release-args)
		last-depress-y (hmouse-y-coord assist-key-depress-args)
		last-release-y (hmouse-y-coord assist-key-release-args))
	(setq last-depress-x (hmouse-x-coord action-key-depress-args)
	      last-release-x (hmouse-x-coord action-key-release-args)
	      last-depress-y (hmouse-y-coord action-key-depress-args)
	      last-release-y (hmouse-y-coord action-key-release-args)))
      (and last-depress-x last-release-x last-depress-y last-release-y
	   (>= (- (max last-depress-y last-release-y)
		  (min last-depress-y last-release-y))
	       hmouse-y-drag-sensitivity)
	   ;; Don't want to register horizontal drags here, so ensure any
	   ;; horizontal movement was less than or equal to the horizontal drag
	   ;; sensitivity.
	   (<= (- (max last-depress-x last-release-x)
		  (min last-depress-x last-release-x))
	       hmouse-x-drag-sensitivity)
	   (if (< last-depress-y last-release-y) 'down 'up)))))

(defun hmouse-drag-vertically ()
  "Return non-nil iff last Action Key use was a vertical drag in a single window.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a vertical line drag, \\='up if drag moved up or
\\='down otherwise."
  (when (hmouse-press-release-same-window)
    (hmouse-drag-vertically-within-emacs)))

(defun hmouse-drag-window-side ()
  "Return non-nil if Action Key was dragged, in one window, from a side divider.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (cond ((hyperb:window-system)
	 (let* ((depress-args (if assist-flag assist-key-depress-args
				action-key-depress-args))
		(release-args (if assist-flag assist-key-release-args
				action-key-release-args))
		(wd (smart-window-of-coords depress-args))
		(wr (smart-window-of-coords release-args))
		(right-side-ln (and (window-live-p wd) (1- (nth 2 (window-edges wd)))))
		(last-press-x   (and (window-live-p wd) depress-args (hmouse-x-coord depress-args)))
		(last-release-x (and (window-live-p wr) release-args (hmouse-x-coord release-args))))
	   (and last-press-x last-release-x right-side-ln
		(/= last-press-x last-release-x)
		(not (<= (abs (- right-side-ln (frame-width))) 5))
		(<= (abs (- last-press-x right-side-ln))
		    hmouse-side-sensitivity))))))

(defun hmouse-read-only-toggle-key ()
  "Return the first key binding that toggles read-only mode, or nil if none."
  (key-description (where-is-internal #'read-only-mode nil t)))

(defun hmouse-vertical-action-drag ()
  "Handle an Action Key vertical drag within a window.
Adds a window to the right of this one.
Beep and print message if the window cannot be split further."
  (interactive)
  (condition-case ()
      (split-window-horizontally nil)
    (error (beep)
	   (message "(hmouse-vertical-action-drag): Can't split the window further."))))

(defun hmouse-vertical-assist-drag ()
  "Handle an Assist Key vertical drag within a window: deletes the current window.
Beep and print message if the window cannot be split further."
  (condition-case ()
      (delete-window)
    (error (beep)
	   (message "(hmouse-vertical-assist-drag): A single window cannot be deleted."))))

(defun hmouse-horizontal-action-drag ()
  "Handle an Action Key horizontal drag within a window.
Adds a window below this one.
Beep and print message if the window cannot be split further."
  (interactive)
  (condition-case ()
      (if (fboundp 'split-window-quietly)
	  (split-window-quietly nil)
	(split-window-vertically nil))
    (error (beep)
	   (message "(hmouse-horizontal-action-drag): Can't split the window further."))))

(defun hmouse-horizontal-assist-drag ()
  "Handle an Assist Key horizontal drag within a window: delete the current window.
Beep and print message if the window cannot be split further."
  (condition-case ()
      (delete-window)
    (error (beep)
	   (message "(hmouse-horizontal-assist-drag): A single window cannot be deleted."))))

(defun smart-coords-in-window-p (coords window)
  "Test if COORDS are in WINDOW.  Return WINDOW if they are, nil otherwise."
  (cond ((null coords) nil)
	((eventp coords)
	 (let ((w-or-f (posn-window (event-start coords))))
	   (when (framep w-or-f)
	     (setq w-or-f (frame-selected-window w-or-f)))
	   (when (and (eq w-or-f window) (window-valid-p window))
	     window)))
	((fboundp 'window-edges)
	 (let* ((edges (window-edges window))
		(w-xmin (nth 0 edges))
		(w-ymin (nth 1 edges))
		(w-xmax (nth 2 edges))
		(w-ymax (nth 3 edges))
		(x  (hmouse-x-coord coords))
		(y  (hmouse-y-coord coords)))
	   (and (<= w-xmin x) (<= x w-xmax)
		(<= w-ymin y) (<= y w-ymax)
		(window-valid-p window)
		window)))))

(defun smart-point-of-coords (coords)
  "Return point within window in which COORDS fall or nil if none.
Ignores minibuffer window."
  (cond ((markerp coords)
	 (marker-position coords))
	((eventp coords)
	 (posn-point (event-start coords)))))

(defun smart-window-of-coords (coords)
  "Return window in which COORDS fall or nil if none.
Ignores minibuffer window."
  (cond ((null coords) nil)
        ((markerp coords)
	 (get-buffer-window (marker-buffer coords)))
	((eventp coords)
	 (let ((w-or-f (posn-window (event-start coords))))
	   (when (framep w-or-f) (setq w-or-f (frame-selected-window w-or-f)))
	   w-or-f))
	(t (let ((window-list (hypb:window-list 'no-minibuf))
		 (window)
		 (w))
	     (while (and (not window) window-list)
	       (setq w (car window-list)
		     window-list (cdr window-list)
		     window (smart-coords-in-window-p coords w)))
	     window))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hmouse-split-window ()
  "Split selected window parallel to its shortest dimension."
  (if (>= (window-width) (window-height))
      ;; side-by-side windows
      (split-window-horizontally)
    ;; windows atop the other
    (split-window-vertically)))

(defun hmouse-buffer-to-window (&optional new-window)
  "Replace buffer in the release window with buffer from the depress window.
Invoked via drag, using the Action Key release window and the
Action Key depress window.  With optional boolean NEW-WINDOW
non-nil, sensibly split the release window before replacing the
buffer."
  (when new-window
    (with-selected-window action-key-release-window
      (hmouse-split-window)))
  (set-window-buffer action-key-release-window (window-buffer action-key-depress-window)))

(defun hmouse-drag-not-allowed ()
  "Display an error when a region is active and in-window drags are not allowed."
  ;; Return point to where it was prior to depress so the region does not permanently change.
  (goto-char hkey-value)
  (error "(hmouse-drag-region-active): Region is active; use a Smart Key press/click within a window, not a drag"))

(defun hmouse-set-buffer-and-point (buffer point)
  (when buffer
    (set-buffer buffer)
    (when point (goto-char point))))

(defun hmouse-goto-region-prev-point ()
  "Temporarily set point to where it was prior to the last Smart Key depress.
Return t if such a point is saved, else nil."
  (let* ((prev-point (if assist-flag assist-key-depress-prev-point action-key-depress-prev-point))
	 (buf (and prev-point (marker-buffer prev-point)))
	 (loc (and prev-point (marker-position prev-point))))
    (when (and buf loc)
      (hmouse-set-buffer-and-point buf loc)
      t)))

(defun hmouse-goto-depress-point ()
  "Temporarily set point to where the last Smart Key was depressed."
  (let ((buf (window-buffer (if assist-flag assist-key-depress-window action-key-depress-window)))
	(loc (smart-point-of-coords (if assist-flag assist-key-depress-args action-key-depress-args))))
    (hmouse-set-buffer-and-point buf loc)))

(defun hmouse-goto-release-point ()
  "Temporarily set point to where the last Smart Key was released."
  (let ((buf (window-buffer (if assist-flag assist-key-release-window action-key-release-window)))
	(loc (smart-point-of-coords (if assist-flag assist-key-release-args action-key-release-args))))
    (hmouse-set-buffer-and-point buf loc)))

(defun hmouse-inactive-minibuffer-p ()
  "Non-nil means last event was a press or release in an inactive minibuffer."
  (let ((window (posn-window (event-start last-command-event))))
    (when (framep window) (setq window (frame-selected-window window)))
    (and (window-live-p window)
	 (window-minibuffer-p window)
	 (not (minibuffer-window-active-p window)))))

(defun hmouse-insert-region ()
  "Save a mark, then insert at point the text from `hkey-region' and indent it."
  (indent-for-tab-command)
  (push-mark nil t)
  (if (smart-eobp) (insert "\n"))
  (insert hkey-region)
  (indent-region (point) (mark))
  (message "") ;; Clear any indenting message.
  ;; From par-align.el library; need to think about all the
  ;; possibilities before enabling filling of this region.
  ;; (if (fboundp 'fill-region-and-align) (fill-region-and-align (mark) (point)))
  )

(defun hmouse-pulse-buffer ()
  (when (and hmouse-pulse-flag (featurep 'pulse) (pulse-available-p))
    (recenter)
    (pulse-momentary-highlight-region (window-start) (window-end) 'next-error)))

(defun hmouse-pulse-line ()
  (when (and hmouse-pulse-flag (featurep 'pulse) (pulse-available-p))
    (recenter)
    (pulse-momentary-highlight-one-line (point) 'next-error)))

(defun hmouse-pulse-region (start end)
  (when (and hmouse-pulse-flag (featurep 'pulse) (pulse-available-p))
    (pulse-momentary-highlight-region start end 'next-error)))

(defun hmouse-item-to-window (&optional new-window)
  "Display item of Action Key depress at the location of Action Key release.
Item is a buffer or file menu item.

Release location may be an Emacs window or outside of Emacs, in
which case a new frame with a single window is created to display
the item.  If the release is inside Emacs and the optional
NEW-WINDOW is non-nil, the release window is sensibly split
before the buffer is displayed.  Otherwise, the buffer is simply
displayed in the release window.

If depress is on the top fixed header line or to the right of any
item, this moves the menu buffer itself to the release location."
  (let* ((w1 action-key-depress-window)
	 ;; Release may be outside of an Emacs window in which case,
	 ;; create a new frame and window.
	 (w2 (or action-key-release-window (frame-selected-window (hycontrol-make-frame))))
	 (w1-ref)
	 (pos))
    (when (and (window-live-p w1) (window-live-p w2))
      (unwind-protect
	  (progn (select-window w1)
		 (if (eq (posn-area (event-start action-key-depress-args)) 'header-line)
		     ;; Drag from fixed header-line means move this menu buffer
		     ;; to release window.
		     (progn (setq w1-ref (current-buffer))
			    (hmouse-pulse-buffer)
			    (sit-for 0.05)
			    (bury-buffer))
		   ;; Otherwise, move the current menu item to the release window.
		   (setq w1-ref (eval (cadr (assq major-mode hmouse-drag-item-mode-forms))))
		   (when w1-ref (hmouse-pulse-line) (sit-for 0.05))))
	(hypb:select-window-frame w2)
	(when (and new-window action-key-release-window)
	  (hmouse-split-window))))
    (unwind-protect
	(progn
	  (when (and w1-ref (not (stringp w1-ref)) (sequencep w1-ref))
	    ;; w1-ref is a list or vector of `buffer' and `position' elements.
	    (setq pos (seq-elt w1-ref 1)
		  w1-ref (seq-elt w1-ref 0)))
	  (cond ((not w1-ref)
		 (if (not (window-live-p w1))
		     (error "(hmouse-item-to-window): Action Mouse Key item drag must start in a live window")
		   (error "(hmouse-item-to-window): No item to display at start of Action Mouse Key drag")))
		((buffer-live-p w1-ref) ;; Must be a buffer, not a buffer name
		 (set-window-buffer w2 w1-ref)
		 (set-buffer w1-ref))
		((and (stringp w1-ref) (file-readable-p w1-ref))
		 (set-window-buffer w2 (set-buffer (find-file-noselect w1-ref))))
		(t (error "(hmouse-item-to-window): Cannot find or read `%s'" w1-ref)))
	  (if pos
	      (progn (goto-char pos)
		     (hmouse-pulse-line))
	    (hmouse-pulse-buffer)))
      ;; If helm is active, end in the minibuffer window.
      (if (smart-helm-alive-p)
	  (smart-helm-to-minibuffer)))))

(defun action-key-modeline ()
  "Handles Action Key depresses on a window mode line.
If the Action Key is:
 (1) clicked on the first blank character of a window's modeline,
     the window's buffer is buried (placed at bottom of buffer list);
 (2) clicked on right edge of a window's modeline,
     the Info buffer is displayed, or if already displayed and the
     modeline clicked belongs to a window displaying Info, the Info
     buffer is hidden;
 (3) clicked on the buffer id of a window's modeline, dired is run
     on the current directory, replacing the window's buffer;
     successive clicks walk up the directory tree
 (4) clicked anywhere in the middle of a window's modeline,
     the function given by `action-key-modeline-function' is called;
 (5) dragged vertically from non-bottommost modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors.
 (6) dragged from a bottommost modeline in a frame with a non-nil
     `drag-with-mode-line' parameter (use `frame-set-parameter'),
     moves the frame as the drag occurs."
  ;; Modeline window resizing is now handled in action-key-depress
  ;; via a call to mouse-drag-mode-line, providing live visual
  ;; feedback.
  (let ((w (smart-window-of-coords action-key-depress-args)))
    (if w (select-window w))
    (when (hmouse-modeline-click)
      (cond ((hmouse-emacs-at-modeline-buffer-id-p)
	     (funcall action-key-modeline-buffer-id-function))
	    ((hmouse-release-left-edge) (bury-buffer))
	    ((hmouse-release-right-edge)
	     (if (eq major-mode 'Info-mode)
		 (quit-window)
	       (info)))
	    (t (funcall action-key-modeline-function))))))

(defun assist-key-modeline ()
  "Handles Assist Key depresses on a window mode line.
If the Assist Key is:
 (1) clicked on the first blank character of a window's modeline,
     bottom buffer in buffer list is unburied and placed in window;
 (2) clicked on right edge of a window's modeline,
     the summary of Smart Key behavior is displayed, or if already
     displayed and the modeline clicked belongs to a window displaying
     the summary, the summary buffer is hidden;
 (3) clicked on the buffer id of a window's modeline,
     the next buffer in sequence is displayed in the window
 (4) clicked anywhere in the middle of a window's modeline,
     the function given by `assist-key-modeline-function' is called;
 (5) dragged vertically from non-bottommost modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors.
 (6) dragged from a bottommost modeline in a frame with a non-nil
     `drag-with-mode-line' parameter (use `frame-set-parameter'),
     moves the frame as the drag occurs."
  ;; Modeline window resizing is now handled in assist-key-depress
  ;; via a call to mouse-drag-mode-line, providing live visual
  ;; feedback.
  (let ((buffers)
	(w (smart-window-of-coords assist-key-depress-args)))
    (when w (select-window w))
    (when (hmouse-modeline-click)
	   (cond ((hmouse-emacs-at-modeline-buffer-id-p)
		  (next-buffer))
		 ((hmouse-release-left-edge)
		  (if (fboundp 'last)
		      (switch-to-buffer (car (last (buffer-list))))
		    (setq buffers (buffer-list))
		    (switch-to-buffer (nth (1- (length buffers)) buffers))))
		 ((hmouse-release-right-edge)
		  (if (string-match "Hyperbole Smart Keys" (buffer-name))
		      (hkey-help-hide)
		    (hkey-summarize 'current-window)))
		 (t (funcall assist-key-modeline-function))))))

(defun hmouse-drag-p ()
  "Return non-nil if last Smart Key depress and release locations differ.
Even if the mouse pointer stays within the same position within a
window, its frame may have been moved by a bottommost modeline drag."
  (or (not (eq (if assist-flag
		   assist-key-depress-window
		 action-key-depress-window)
	       (if assist-flag
		   assist-key-release-window
		 action-key-release-window)))
      ;; Depress and release were in the same window; test if there is a drag.
      (hmouse-drag-same-window)))

(defun hmouse-modeline-click ()
  "Non-nil means last Smart Key use were at a single point in a modeline.
Meaning the Smart Key depress and release were at less than drag
tolerance apart in a modeline."
  (and (not (hmouse-drag-p))
       (hmouse-modeline-depress)
       (hmouse-modeline-release)
       (not (or (hmouse-drag-horizontally) (hmouse-drag-vertically) (hmouse-drag-diagonally)))))

(defun hmouse-emacs-modeline-event-p (event)
  "Return non-nil if EVENT happened on a window mode line."
  (or (and (eventp event) (eq (posn-area (event-start event)) 'mode-line))
      ;; If drag release was to an unselected frame mode-line, on
      ;; click-to-focus systems, the release event will not include
      ;; the mode-line area, so manually compute if that was the location.
      (let* ((w (smart-window-of-coords event))
	     ;; Do all calculations in pixels if possible.
	     (line-height (when (and w (window-live-p w)) (frame-char-height (window-frame w))))
	     (mode-ln (when (and w (window-live-p w)) (nth 3 (window-edges w nil t t))))
	     (last-press-y (cdr (posn-x-y (event-start event)))))
	(and (not (eq w (minibuffer-window)))
	     last-press-y mode-ln (< (- mode-ln last-press-y) line-height)))))

(defun hmouse-modeline-event-p (event)
  "Return non-nil if start of EVENT happened on a window mode line."
    (when (and (hyperb:window-system) event
	       (not (posnp event))
	       (not (markerp event)))
      (hmouse-emacs-modeline-event-p event)))

(defun hmouse-modeline-depress ()
  "Return non-nil if Action Key was depressed on a window mode line.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (let ((args (if assist-flag
		  assist-key-depress-args
		action-key-depress-args)))
    (hmouse-modeline-event-p args)))

(defun hmouse-modeline-release ()
  "Return non-nil if Smart Key was released on a window mode line."
  (let ((args (if assist-flag
		  assist-key-release-args
		action-key-release-args)))
    (hmouse-modeline-event-p args)))

(defun hmouse-emacs-at-modeline-buffer-id-p ()
  "Non-nil means mouse is in the buffer name part of the current window's modeline."
  (let* ((coords (hmouse-window-coordinates)) ;; in characters
	 (x-coord (caadr coords))
	 (mode-line-string (and (integerp x-coord) (>= x-coord 0) (format-mode-line mode-line-format)))
	 (keymap (and mode-line-string
		      (<= x-coord (1- (length mode-line-string)))
		      (plist-get (text-properties-at x-coord mode-line-string) 'local-map))))
    (when keymap
      (eq (lookup-key keymap [mode-line mouse-1]) 'mode-line-previous-buffer))))

(defun hmouse-modeline-resize-window ()
  "Resize window whose mode line was depressed upon with the last Smart Key.
Resize amount depends upon the vertical difference between press and release
of the Smart Key."
  (cond ((not (hyperb:window-system)) nil)
	(t ;; Restore position of point prior to Action Key release.
	 (when action-key-release-prev-point
	   (let ((obuf (current-buffer)))
	     (unwind-protect
		 (progn
		   (set-buffer
		    (marker-buffer action-key-release-prev-point))
		   (goto-char
		    (marker-position action-key-release-prev-point)))
	       (set-buffer obuf)))))))

(defun hmouse-clone-window-to-frame (&optional _always-delete-flag)
  (let ((hycontrol-keep-window-flag t))
    (hmouse-move-window-to-frame)))

;; Derived from Emacs mouse.el.
(defun hmouse-move-window-to-frame (&optional always-delete-flag)
  "Move the selected window to the right of the window of Action Key release.
If free variable `assist-flag' is non-nil, uses Assist Key release instead.

If optional ALWAYS-DELETE-FLAG is non-nil, delete the source window
after copying it to the other frame; otherwise, if there is only one
window in the source frame or if free variable `hycontrol-keep-window-flag'
is non-nil, leave the original window and just clone it into the new frame."
  (interactive)
  (let ((depress-window (if assist-flag
			    assist-key-depress-window
			  action-key-depress-window))
	(release-window (if assist-flag
			    assist-key-release-window
			  action-key-release-window))
	buf)
    (cond ((or (window-minibuffer-p depress-window)
	       (window-minibuffer-p release-window))
	   (beep)
	   (minibuffer-message "(Hyperbole): Select a non-minibuffer window"))
	  (t
	   ;; Give temporary modes such as isearch a chance to turn off.
	   (run-hooks 'mouse-leave-buffer-hook)
	   (setq buf (window-buffer depress-window))
	   (with-selected-window release-window
	     (split-window-horizontally)
	     (other-window 1)
	     (switch-to-buffer buf nil t))
	   (with-selected-frame (window-frame depress-window)
	     (unless (or hycontrol-keep-window-flag
			 (and (not always-delete-flag) (one-window-p t)))
	       (delete-window depress-window)))))))

(defun hmouse-release-left-edge ()
  "Return non-nil if last Smart Key release was at left window edge.
`hmouse-edge-sensitivity' value determines how near to actual edge the
release must be.

GNU Emacs mode-line key bindings override the Smart Mouse Key bindings
immediately after the first blank mode-line character position.  Therefore,
mode-line left edge clicks must be on the first blank character of the
mode-line regardless of the setting of `hmouse-edge-sensitivity'."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-left last-release-x)
    (setq window-left (car (window-edges))
	  last-release-x (and args (hmouse-x-coord args)))
    (and last-release-x (< (- last-release-x window-left)
			   hmouse-edge-sensitivity)
	 (>= (- last-release-x window-left) 0))))

(defun hmouse-release-right-edge ()
  "Return non-nil if the last Smart Key release was at right window edge.
`hmouse-edge-sensitivity' value determines how near to actual edge the
release must be."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-right last-release-x)
    (setq window-right (nth 2 (window-edges))
	  last-release-x (and args (hmouse-x-coord args)))
    (and last-release-x (>= (+ last-release-x hmouse-edge-sensitivity)
			    window-right)
	 (>= (- window-right last-release-x) 0))))

(defun hmouse-resize-window-side ()
  "Resizes window whose side was depressed on by the last Smart Key.
Resize amount depends upon the horizontal difference between press and release
of the Smart Key."
  (cond ((hyperb:window-system)
	 (let* ((owind (selected-window))
		(window (smart-window-of-coords
			 (if assist-flag assist-key-depress-args
			   action-key-depress-args)))
		(right-side-ln (and window (1- (nth 2 (window-edges window)))))
		(last-release-x
		 (hmouse-x-coord
		  (if assist-flag assist-key-release-args
		    action-key-release-args)))
		(shrink-amount (- right-side-ln last-release-x)))
	   ;; Restore position of point prior to Action Key release.
	   (if action-key-release-prev-point
	       (let ((obuf (current-buffer)))
		 (unwind-protect
		     (progn
		       (set-buffer (marker-buffer action-key-release-prev-point))
		       (goto-char (marker-position action-key-release-prev-point)))
		   (set-buffer obuf))))
	   (cond
	    ((>= (+ right-side-ln 2) (frame-width))
	     (error
	      "(hmouse-resize-window-side): Can't change width of full frame width window"))
	    ((< (length (hypb:window-list 'no-minibuf)) 2)
	     (error
	      "(hmouse-resize-window-side): Can't resize sole window in frame"))
	    (t (unwind-protect
		   (progn
		     (select-window window)
		     (shrink-window-horizontally shrink-amount))
		 (select-window owind))))))))

(defun hmouse-swap-buffers ()
  "Swaps buffers in windows selected with the last Smart Key depress and release."
  (let* ((w1 (if assist-flag assist-key-depress-window
	       action-key-depress-window))
	 (w2 (if assist-flag assist-key-release-window
	       action-key-release-window))
	 (w1-buf (and w1 (window-buffer w1)))
	 (w2-buf (and w2 (window-buffer w2))))
    (cond ((not (and w1 w2))
	   (error "(hmouse-swap-buffers): Last depress or release was not within a window"))
	  ((eq w1 w2))  ;; Do nothing silently.
	  (t ;; Swap window buffers.
	   (set-window-buffer w1 w2-buf)
	   (set-window-buffer w2 w1-buf)))))

(defun hmouse-swap-windows ()
  "Swaps the sizes of 2 windows selected by the last Smart Key depress and release."
  (let* ((w1 (if assist-flag assist-key-depress-window
	       action-key-depress-window))
	 (w2 (if assist-flag assist-key-release-window
	       action-key-release-window))
	 (w1-width  (and w1 (window-width w1)))
	 (w1-height (and w1 (window-height w1)))
	 (w2-width  (and w2 (window-width w2)))
	 (w2-height (and w2 (window-height w2))))
    (or (and w1 w2)
	(error "(hmouse-swap-windows): Last depress or release was not within a window"))
    (unwind-protect
	(progn
	  (select-window w1)
	  (unless (= w1-height (frame-height))
	    (shrink-window (- w1-height w2-height)))
	  (unless (= w1-width (frame-width))
	    (shrink-window-horizontally (- w1-width w2-width)))
	  (select-window w2)
	  (setq w2-width (window-width w2)
		w2-height (window-height w2))
	  (unless (= w2-height (frame-height))
	    (shrink-window (- w2-height w1-height)))
	  (unless (= w2-width (frame-width))
	    (shrink-window-horizontally (- w2-width w1-width))))
      (select-window w2))))

(defun hmouse-x-coord (args)
  "Return x coordinate in characters from window system dependent ARGS or nil."
  (let ((x (if (markerp args)
	       (save-excursion
		 (hypb:goto-marker args)
		 (current-column))
	     (funcall (cdr (assoc (hyperb:window-system)
				  '(("emacs" . (lambda (args)
						 (when (eventp args) (setq args (event-start args)))
						   (cond
						    ((posnp args)
						     (let ((w-or-f (posn-window args)))
						       (when (framep w-or-f)
							 (setq w-or-f (frame-selected-window w-or-f)))
						       (+ (condition-case ()
							      (car (posn-col-row args))
							    (error 0))
							  (nth 0 (window-edges w-or-f)))))
						    (t (car args)))))
				 ("next"   .  (lambda (args) (nth 1 args))))))
		      args))))
    (when (integerp x)
      x)))

(defun hmouse-y-coord (args)
  "Return y coordinate in frame lines from window system dependent ARGS or nil."
  (let ((y (funcall (cdr (assoc (hyperb:window-system)
				'(("emacs" . (lambda (args)
					       (when (eventp args) (setq args (event-start args)))
						    (cond ((posnp args)
							   (let ((w-or-f (posn-window args)))
							     (when (framep w-or-f)
							       (setq w-or-f (frame-selected-window w-or-f)))
							     (+ (condition-case ()
								    (cdr (posn-col-row args))
								  (error 0))
								(nth 1 (window-edges w-or-f)))))
							  (t (cdr args)))))
				  ("next"   .  (lambda (args) (nth 2 args))))))
		    args)))
    (when (integerp y)
      y)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************


(provide 'hui-window)

;;; hui-window.el ends here
