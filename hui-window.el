;;; hui-window.el --- Smart Mouse Key window and modeline depress/release actions.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Sep-92
;;
;; Copyright (C) 1992-2019  Free Software Foundation, Inc.
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
;;   anywhere else                 Create/modify a link button Swap window buffers
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
;; For momentary highlighting of buffer/file item lines.
(require 'pulse nil t)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom action-key-minibuffer-function #'hyperbole
  "*Function run by the Action Key after a click in an inactive minibuffer.
Its default value is #'hyperbole, which displays the Hyperbole minibuffer menu."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-minibuffer-function #'hui-menu-screen-commands
  "*Function run by the Assist Key after a click in an inactive minibuffer.
Its default value is #'hui-menu-screen-commands, which displays a popup menu
of screen control commands."
  :type 'function
  :group 'hyperbole-keys)

;; Set this to `hmouse-context-ibuffer-menu' if you use the ibuffer package.
(defcustom action-key-modeline-function #'hmouse-context-menu
  "Function to call when the Action Mouse Key is clicked in the center portion of a modeline."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-modeline-function #'hui-menu-screen-commands
  "Function to call when the Assist Mouse Key is clicked in the center portion of a modeline."
  :type 'function
  :group 'hyperbole-keys)

(defun hmouse-map-modes-to-form (mode-forms)
  "Maps over a sequence of (major-mode(s) form-to-eval) lists; returns items with a single major-mode in the car, (major-mode form-to-eval)."
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
The car of an item must be a major-mode symbol.  The cadr of an item
is a Lisp form to evaluate to get the item name at point (typically a
buffer, file or directory name whose contents will be displayed in the
drag release window.")


(defcustom hmouse-pulse-flag t
  "When non-nil (the default) and when display supports visual pulsing, then pulse lines and buffers when an Action Key drag is used to place a buffer or file in a window."
  :type 'boolean
  :group 'hyperbole-keys)

 ;; Mats Lidell says this should be 10 characters for GNU Emacs.
(defvar hmouse-edge-sensitivity 10
  "*Number of characters from window edges within which a click is considered at an edge.")

(defvar hmouse-side-sensitivity 5
  "*Characters in either direction from window side within which a click is considered on the side.")

(defvar hmouse-x-drag-sensitivity 5
  "*Number of characters mouse must move horizontally between depress/release to register a horizontal drag.")

(defvar hmouse-y-drag-sensitivity 3
  "*Number of lines mouse must move vertically between depress/release to register a vertical drag.")

(defvar hmouse-x-diagonal-sensitivity 4
  "*Number of characters mouse must move horizontally between depress/release to register a diagonal drag.")
(defvar hmouse-y-diagonal-sensitivity 3
  "*Number of lines mouse must move vertically between depress/release to register a diagonal drag.")

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
	     ;; If click in the minibuffer when it is not active (blank),
	     ;; display the Hyperbole minibuffer menu or popup the jump menu.
	     ((hmouse-inactive-minibuffer-p) .
	      ((funcall action-key-minibuffer-function) . 
	       (funcall assist-key-minibuffer-function)))
	     ((hmouse-drag-thing) .
	      ((hmouse-yank-region) . (hmouse-kill-and-yank-region)))
	     ((hmouse-drag-window-side) .
	      ((hmouse-resize-window-side) . (hmouse-resize-window-side)))
	     ;;
	     ;; Although Hyperbole can distinguish whether
	     ;; inter-window drags are between frames or not,
	     ;; having different behavior for those 2 cases could
	     ;; be confusing, so treat all modeline drags between
	     ;; windows the same and comment out this next clause.
	     ;;   Modeline drag between frames
	     ;;   ((and (hmouse-modeline-depress) (hmouse-drag-between-frames)) .
	     ;;    ((hmouse-clone-window-to-frame) . (hmouse-move-window-to-frame)))
	     ;;
	     ;; Drag with release on a Modeline
	     ((and (hmouse-modeline-release) (not (hmouse-modeline-click))) .
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

(defun hmouse-at-item-p ()
  "Return t if point is on an item draggable by Hyperbole, otherwise nil."
  (let* ((buf (and (window-live-p action-key-depress-window) (window-buffer action-key-depress-window)))
	 (mode (and buf (buffer-local-value 'major-mode buf))))
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
  "Return t iff there is a non-empty active region in buffer of the last Smart Mouse Key release."
  (when (setq hkey-value (if assist-flag assist-key-depress-prev-point action-key-depress-prev-point))
    (save-excursion
      (set-buffer (marker-buffer hkey-value))
      ;; Store and goto any prior value of point from the region
      ;; prior to the Smart Key depress, so we can return to it later.
      (and (goto-char hkey-value)
	   (hmouse-save-region)
	   t))))

(defun hmouse-dired-readin-hook ()
  "Remove local `hpath:display-where' setting whenever re-read a dired directory.
See `hmouse-dired-item-dragged' for use."
  (hmouse-dired-display-here-mode 0))

(define-minor-mode hmouse-dired-display-here-mode
  "Once a dired buffer item has been dragged, make next Action Key press on an item display it in the same dired window.

By default an Action Key press on a dired item displays it in another
window.   But once a Dired item is dragged to another window, the next
Action Key press should display it in the dired window so that the
behavior matches that of Buffer Menu and allows for setting what is
displayed in all windows on screen, including the dired window.

If the directory is re-read into the dired buffer with {g}, then Action
Key behavior reverts to as though no items have been dragged."
  nil
  " DisplayHere"
  nil
  (if hmouse-dired-display-here-mode
      (progn (set (make-local-variable 'hpath:display-where) 'this-window)
	     (add-hook 'dired-after-readin-hook 'hmouse-dired-readin-hook nil t))
    (kill-local-variable 'hpath:display-where)
    (remove-hook 'dired-after-readin-hook 'hmouse-dired-readin-hook t)))

(defun hmouse-drag-region-active ()
  "Return non-nil if an active region existed in the depress buffer prior to the depress and a drag motion has occurred."
  (save-excursion
    (and (hmouse-goto-region-prev-point)
	 (hmouse-use-region-p)
	 (or (hmouse-drag-vertically) (hmouse-drag-horizontally) (hmouse-drag-diagonally))
	 (setq hkey-value (point)))))

(defun hmouse-drag-thing ()
  "Return t if no region is active and a Smart Key drag began at the start/end of a delimited construct and ended at some other point in the same buffer, else nil.
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
      (error "(hmouse-kill-region): Use {%s} to enable killing from this buffer."
	     (hmouse-read-only-toggle-key))
    (kill-region (or hkey-value (point)) (mark))))

(defun hmouse-kill-and-yank-region ()
  "Kill the marked region near where the Smart Key was depressed and yank it at the point of release.
Signals an error if either depress or release buffer is read-only."
  (when hkey-region
    ;; Move point back to Smart Key depress buffer.
    (hmouse-goto-depress-point)
    (if buffer-read-only
	;; In this case, we want an error that will terminate execution so that
	;; hkey-region is not reset to nil.  This allows the user to fix the
	;; problem and then to try killing again.
	(error "(hmouse-kill-and-yank-region): Use {%s} to enable killing from this buffer."
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
	    (error "(hmouse-kill-and-yank-region): Use {%s} to enable yanking into this buffer."
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
	(error "(hmouse-yank-region): Use {%s} to enable yanking into this buffer."
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
  "Returns non-nil if last Action Key depress and release were in different frames.
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
  "Returns non-nil if last Action Key depress and release were in different windows.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and (window-live-p assist-key-depress-window)
	   (window-live-p assist-key-release-window)
	   (not (eq assist-key-depress-window
		    assist-key-release-window)))
    (and (window-live-p action-key-depress-window)
	 (window-live-p action-key-release-window)
	 (not (eq action-key-depress-window action-key-release-window)))))

(defun hmouse-drag-same-window ()
  "Returns non-nil if last Action Key depress and release were in the same window.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and (window-live-p assist-key-depress-window)
	   (window-live-p assist-key-release-window)
	   (eq assist-key-depress-window assist-key-release-window))
    (and (window-live-p action-key-depress-window)
	 (window-live-p action-key-release-window)
	 (eq action-key-depress-window action-key-release-window))))

(defun hmouse-drag-outside-all-windows ()
  "Returns non-nil if last Action Key release was outside of an Emacs window.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and (window-live-p assist-key-depress-window) (not assist-key-release-window))
    (and (window-live-p action-key-depress-window) (not action-key-release-window))))

(defun hmouse-drag-item-to-display (&optional new-window)
  "Depress on a buffer name in Buffer-menu/ibuffer mode or on a file/directory in dired mode and release where the item is to be displayed.

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
  (when (hmouse-at-item-p)
    (hmouse-item-to-window new-window)
    t))

(defun hmouse-drag-diagonally ()
  "Returns non-nil iff last Action Key use was a diagonal drag within a single window.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a diagonal drag, or one of the following symbols
depending on the direction of the drag: southeast, southwest, northwest, northeast."
  (when (hmouse-drag-same-window)
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
  "Returns non-nil iff last Action Key use was a horizontal drag within a single window.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a horizontal drag, 'left if drag moved left or
'right otherwise."
  (when (hmouse-drag-same-window)
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
  "Returns non-nil iff last Action Key use was a vertical drag that started and ended within the same Emacs frame.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a vertical line drag, 'up if drag moved up or
'down otherwise."
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
  "Returns non-nil iff last Action Key use was a vertical drag within a single window.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a vertical line drag, 'up if drag moved up or
'down otherwise."
  (when (hmouse-drag-same-window)
    (hmouse-drag-vertically-within-emacs)))

(defun hmouse-drag-window-side ()
  "Returns non-nil if Action Key was dragged from a window side divider and released in the same window.
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
  (key-description (or (where-is-internal #'read-only-mode nil t)
		       (where-is-internal #'vc-toggle-read-only nil t)
		       (where-is-internal #'toggle-read-only nil t))))

(defun hmouse-vertical-action-drag ()
  "Handles an Action Key vertical drag within a window: adds a window to the right of this one.
Beeps and prints message if the window cannot be split further."
  (interactive)
  (condition-case ()
      (split-window-horizontally nil)
    (error (beep)
	   (message "(hmouse-vertical-action-drag): Can't split the window further."))))

(defun hmouse-vertical-assist-drag ()
  "Handles an Assist Key vertical drag within a window: deletes the current window.
Beeps and prints message if the window cannot be split further."
  (condition-case ()
      (delete-window)
    (error (beep)
	   (message "(hmouse-vertical-assist-drag): A single window cannot be deleted."))))

(defun hmouse-horizontal-action-drag ()
  "Handles an Action Key horizontal drag within a window: adds a window below this one.
Beeps and prints message if the window cannot be split further."
  (interactive)
  (condition-case ()
      (if (fboundp 'split-window-quietly)
	  (split-window-quietly nil)
	(split-window-vertically nil))
    (error (beep)
	   (message "(hmouse-horizontal-action-drag): Can't split the window further."))))

(defun hmouse-horizontal-assist-drag ()
  "Handles an Assist Key horizontal drag within a window: deletes the current window.
Beeps and prints message if the window cannot be split further."
  (condition-case ()
      (delete-window)
    (error (beep)
	   (message "(hmouse-horizontal-assist-drag): A single window cannot be deleted."))))

(defun smart-coords-in-window-p (coords window)
  "Tests if COORDS are in WINDOW.  Returns WINDOW if they are, nil otherwise."
  (cond ((null coords) nil)
	((eventp coords)
	 (let ((w-or-f (posn-window (event-start coords))))
	   (if (framep w-or-f) (setq w-or-f (frame-selected-window w-or-f)))
	   (eq w-or-f window)))
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
		  window)))))

(defun smart-point-of-coords (coords)
  "Returns point within window in which COORDS fall or nil if none.
Ignores minibuffer window."
  (cond ((markerp coords)
	 (marker-position coords))
	((eventp coords)
	 (posn-point (event-start coords)))))

(defun smart-window-of-coords (coords)
  "Returns window in which COORDS fall or nil if none.
Ignores minibuffer window."
  (cond ((null coords) nil)
        ((markerp coords)
	 (get-buffer-window (marker-buffer coords)))
	((eventp coords)
	 (let ((w-or-f (posn-window (event-start coords))))
	   (if (framep w-or-f) (setq w-or-f (frame-selected-window w-or-f)))
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
  "Invoked via drag, replace the buffer in the Action Key release window with the buffer from the Action Key depress window.
With optional boolean NEW-WINDOW non-nil, sensibly split the release window before replacing the buffer."
  (when new-window
    (with-selected-window action-key-release-window
      (hmouse-split-window)))
  (set-window-buffer action-key-release-window (window-buffer action-key-depress-window)))

(defun hmouse-drag-not-allowed ()
  "Display an error when a region is active and in-window drags are not allowed."
  ;; Return point to where it was prior to depress so the region does not permanently change.
  (goto-char hkey-value)
  (error "(hmouse-drag-region-active): Region is active; use a Smart Key press/click within a window, not a drag."))

(defun hmouse-set-buffer-and-point (buffer point)
  (when buffer
    (set-buffer buffer)
    (when point (goto-char point))))

(defun hmouse-goto-region-prev-point ()
  "Temporarily set point to where it was prior to the last Smart Key depress and return t, else nil if no such point is saved."
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
  "Return t if the last command event was a mouse press or release within an inactive minibuffer, else nil."
  (let ((window (posn-window (event-start last-command-event))))
    (if (framep window) (setq window (frame-selected-window window)))
    (and (window-minibuffer-p window)
	 (not (minibuffer-window-active-p window)))))

(defun hmouse-insert-region ()
  "Save a mark, then insert at point the text from `hkey-region' and indent it."
  (indent-for-tab-command)
  (push-mark nil t)
  (if (eobp) (insert "\n"))
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

(defun hmouse-item-to-window (&optional new-window)
  "Display buffer or file menu item of Action Key depress at the location of Action Key release.

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
	 (buf-name)
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
		((buffer-live-p w1-ref)
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
 (5) dragged vertically from modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors."
  (let ((w (smart-window-of-coords action-key-depress-args)))
    (if w (select-window w))
    (cond ((hmouse-modeline-click)
	   (cond ((hmouse-emacs-at-modeline-buffer-id-p)
		  (funcall action-key-modeline-buffer-id-function))
		 ((hmouse-release-left-edge) (bury-buffer))
		 ((hmouse-release-right-edge)
		  (if (eq major-mode 'Info-mode)
		      (Info-exit)
		    (info)))
		 (t (funcall action-key-modeline-function))))
	  (t (hmouse-modeline-resize-window)))))

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
 (5) dragged vertically from modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors."
  (let ((buffers)
	(w (smart-window-of-coords assist-key-depress-args)))
    (if w (select-window w))
    (cond ((hmouse-modeline-click)
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
		 (t (funcall assist-key-modeline-function))))
	  (t (hmouse-modeline-resize-window)))))

(defun hmouse-modeline-click ()
  "Returns non-nil if last Smart Key depress and release were at a single point (less than drag tolerance apart) in a modeline."
  (and (hmouse-modeline-release) (hmouse-modeline-depress)
       (not (or (hmouse-drag-horizontally) (hmouse-drag-vertically) (hmouse-drag-diagonally)))))

(defun hmouse-emacs-modeline-event-p (event)
  "GNU Emacs: Returns non-nil if EVENT happened on a window mode line."
  (or (and (eventp event) (eq (posn-area (event-start event)) 'mode-line))
      ;; If drag release was to an unselected frame mode-line, on
      ;; click-to-focus systems, the release event will not include
      ;; the mode-line area, so manually compute if that was the location.
      (let* ((w (smart-window-of-coords event))
	     ;; Do all calculations in pixels if possible.
	     (line-height (if w (frame-char-height (window-frame w))))
	     (mode-ln (if w (nth 3 (window-edges w nil t t))))
	     (last-press-y (cdr (posn-x-y (event-start event)))))
	(and (not (eq w (minibuffer-window)))
	     last-press-y mode-ln (< (- mode-ln last-press-y) line-height)))))

(defun hmouse-modeline-event-p (event)
  "Returns non-nil if start of EVENT happened on a window mode line."
    (when (and (hyperb:window-system) event
	       (not (posnp event))
	       (not (markerp event)))
      (cond
       ;; Modern GNU Emacs
       ((fboundp 'posn-area)
	(hmouse-emacs-modeline-event-p event))
       ;; XEmacs
       ((fboundp 'event-over-modeline-p)
	(event-over-modeline-p event))
       ;; Early Emacs
       (t
	(let* ((w (smart-window-of-coords event))
	       (mode-ln (if w (nth 3 (window-edges w))))
	       (last-press-y (hmouse-y-coord event)))
	  ;; Mode-line is always 1 less than the bottom of the window, unless it
	  ;; is a minibuffer window which does not have a modeline.
	  (if (not (eq w (minibuffer-window))) (setq mode-ln (1- mode-ln)))
	  (and last-press-y mode-ln (= last-press-y mode-ln)))))))

(defun hmouse-modeline-depress ()
  "Returns non-nil if Action Key was depressed on a window mode line.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (let ((args (if assist-flag
		  assist-key-depress-args
		action-key-depress-args)))
    (hmouse-modeline-event-p args)))

(defun hmouse-modeline-release ()
  "Returns non-nil if Smart Key was released on a window mode line."
  (let ((args (if assist-flag
		  assist-key-release-args
		action-key-release-args)))
    (hmouse-modeline-event-p args)))

(defun hmouse-emacs-at-modeline-buffer-id-p ()
  "GNU Emacs: Return t if mouse position is within the buffer name field of the current window's mode-line, else nil."
  (let* ((coords (hmouse-window-coordinates)) ;; in characters
	 (x-coord (caadr coords))
	 (mode-line-string (and (integerp x-coord) (>= x-coord 0) (format-mode-line mode-line-format)))
	 (keymap (and mode-line-string
		      (<= x-coord (1- (length mode-line-string)))
		      (plist-get (text-properties-at x-coord mode-line-string) 'local-map))))
    (when keymap
      (eq (lookup-key keymap [mode-line mouse-1]) 'mode-line-previous-buffer))))

(defun hmouse-modeline-resize-window ()
  "Resizes window whose mode line was depressed on by the last Smart Key.
Resize amount depends upon the vertical difference between press and release
of the Smart Key."
  (cond ((not (hyperb:window-system)) nil)
	(t (let* ((owind (selected-window))
		  (window (smart-window-of-coords
			   (if assist-flag assist-key-depress-args
			     action-key-depress-args)))
		  (mode-ln (and window (1- (nth 3 (window-edges window)))))
		  (last-release-y
		   (hmouse-y-coord
		    (if assist-flag assist-key-release-args
		      action-key-release-args)))
		  (shrink-amount (- mode-ln last-release-y)))
	     ;; Restore position of point prior to Action Key release.
	     (if action-key-release-prev-point
		 (let ((obuf (current-buffer)))
		   (unwind-protect
		       (progn
			 (set-buffer
			  (marker-buffer action-key-release-prev-point))
			 (goto-char
			  (marker-position action-key-release-prev-point)))
		     (set-buffer obuf))))
	     (cond
	      ((>= (+ mode-ln 2) (frame-height))
	       (error
		"(hmouse-modeline-resize-window): Can't move bottom window in frame."))
	      ((< (length (hypb:window-list 'no-minibuf)) 2)
	       (error
		"(hmouse-modeline-resize-window): Can't resize sole window in frame."))
	      (t (unwind-protect
		     (progn
		       (select-window window)
		       (shrink-window shrink-amount)
		       ;; Keep redisplay from scrolling other window.
		       (select-window (next-window nil 'no-mini))
		       (condition-case ()
			   (scroll-down shrink-amount)
			 (error nil)))
		   (select-window owind))))))))

(defun hmouse-clone-window-to-frame (&optional always-delete-flag)
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
  "Returns non-nil if last Smart Key release was at left window edge.
`hmouse-edge-sensitivity' value determines how near to actual edge the
release must be.

GNU Emacs mode-line key bindings override the Smart Mouse Key bindings
immediately after the first blank mode-line character position.  Therefore,
mode-line left edge clicks must be on the first blank character of the
mode-line regardless of the setting of `hmouse-edge-sensitivity'."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-left last-release-x)
    (if (fboundp 'window-lowest-p) ;; XEmacs >= 19.12 
	(setq last-release-x (and args (eq (event-window args)
					   (selected-window))
				  (hmouse-x-coord args))
	      window-left 0)
      (setq window-left (car (window-edges))
	    last-release-x (and args (hmouse-x-coord args))))
    (and last-release-x (< (- last-release-x window-left)
			   hmouse-edge-sensitivity)
	 (>= (- last-release-x window-left) 0))))

(defun hmouse-release-right-edge ()
  "Returns non-nil if the last Smart Key release was at right window edge.
`hmouse-edge-sensitivity' value determines how near to actual edge the
release must be."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-right last-release-x)
    (if (fboundp 'window-lowest-p) ;; XEmacs >= 19.12 
	(setq last-release-x (and args (eq (event-window args)
					   (selected-window))
				  (hmouse-x-coord args))
	      window-right (window-width))
      (setq window-right (nth 2 (window-edges))
	    last-release-x (and args (hmouse-x-coord args))))
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
		(shrink-amount (- right-side-ln last-release-x))
		)
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
	      "(hmouse-resize-window-side): Can't change width of full frame width window."))
	    ((< (length (hypb:window-list 'no-minibuf)) 2)
	     (error
	      "(hmouse-resize-window-side): Can't resize sole window in frame."))
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
	 (w2-buf (and w2 (window-buffer w2)))
	 )
    (cond ((not (and w1 w2))
	   (error "(hmouse-swap-buffers): Last depress or release was not within a window."))
	  ((eq w1 w2)
	   ;; Do nothing silently.
	   )
	  (t ;; Swap window buffers.
	   (set-window-buffer w1 w2-buf)
	   (set-window-buffer w2 w1-buf)))))

(defun hmouse-swap-windows ()
  "Swaps the sizes of 2 windows selected with the last Smart Key depress and release."
  (let* ((w1 (if assist-flag assist-key-depress-window
	       action-key-depress-window))
	 (w2 (if assist-flag assist-key-release-window
	       action-key-release-window))
	 (w1-width  (and w1 (window-width w1)))
	 (w1-height (and w1 (window-height w1)))
	 (w2-width  (and w2 (window-width w2)))
	 (w2-height (and w2 (window-height w2)))
	 )
    (or (and w1 w2)
	(error "(hmouse-swap-windows): Last depress or release was not within a window."))
    (unwind-protect
	(progn
	  (select-window w1)
	  (if (not (= w1-height (frame-height)))
	      (shrink-window (- w1-height w2-height)))
	  (if (not (= w1-width (frame-width)))
	      (shrink-window-horizontally (- w1-width w2-width)))
	  (select-window w2)
	  (setq w2-width (window-width w2)
		w2-height (window-height w2))
	  (if (not (= w2-height (frame-height)))
	      (shrink-window (- w2-height w1-height)))
	  (if (not (= w2-width (frame-width)))
	      (shrink-window-horizontally (- w2-width w1-width)))
	  )
      (select-window w2)
      )))

(defun hmouse-x-coord (args)
  "Returns x coordinate in characters from window system dependent ARGS or nil."
  (let ((x (if (markerp args)
	       (save-excursion
		 (hypb:goto-marker args)
		 (current-column))
	     (eval (cdr (assoc (hyperb:window-system)
			       '(("emacs" . (progn (if (eventp args) (setq args (event-start args)))
						   (cond
						    ((posnp args)
						     (let ((w-or-f (posn-window args)))
						       (if (framep w-or-f)
							   (setq w-or-f (frame-selected-window w-or-f)))
						       (+ (car (posn-col-row args))
							  (nth 0 (window-edges w-or-f)))))
						    (t (car args)))))
				 ("next"   .  (nth 1 args))
				 )))))))
    (if (integerp x) x)))

(defun hmouse-y-coord (args)
  "Returns y coordinate in frame lines from window system dependent ARGS or nil."
  (let ((y (eval (cdr (assoc (hyperb:window-system)
			     '(("emacs" . (progn (if (eventp args) (setq args (event-start args)))
						 (cond ((posnp args)
							(let ((w-or-f (posn-window args)))
							  (if (framep w-or-f)
							      (setq w-or-f (frame-selected-window w-or-f)))
							  (+ (cdr (posn-col-row args))
							     (nth 1 (window-edges w-or-f)))))
						       (t (cdr args)))))
			       ("next"   .  (nth 2 args))
			       ))))))
    (if (integerp y) y)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************


(provide 'hui-window)

;;; hui-window.el ends here
