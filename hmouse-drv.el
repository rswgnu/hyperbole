;;; hmouse-drv.el --- Smart Key/Mouse driver functions.  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    04-Feb-90
;; Last-Mod:      7-Oct-22 at 23:30:30 by Mats Lidell
;;
;; Copyright (C) 1989-2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hui-window)
(require 'hypb)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hmouse-verify-release-window-flag t
  "Non-nil means verify Smart Mouse Keys are released in or out of an Emacs frame.
Presently, this does nothing unless Emacs is running under the
macOS window system.  It queries the Mac window manager for the
name of the owner of the top-most window at the point of release,
if any.  Otherwise, if an Emacs frame is below another
application's window at the point of release, Emacs will report
that the release point was in its frame.

See function `hmouse-window-at-absolute-pixel-position' for more details.")

(defvar action-key-depressed-flag nil "t while Action Key is depressed.")
(defvar assist-key-depressed-flag nil "t while Assist Key is depressed.")
(defvar action-key-depress-args nil
  "List of mouse event args from most recent depress of the Action Key.")
(defvar assist-key-depress-args nil
  "List of mouse event args from most recent depress of the Assist Key.")

(defvar action-key-release-args nil
  "List of mouse event args from most recent release of the Action Key.")
(defvar assist-key-release-args nil
  "List of mouse event args from most recent release of the Assist Key.")

(defvar action-key-depress-window nil
  "The last window in which the Action Key was depressed or nil.
This is set to nil when the depress is on an inactive minibuffer.")
(defvar assist-key-depress-window nil
  "The last window in which the Assist Key was depressed or nil.
This is set to nil when the depress is on an inactive minibuffer.")
(defvar action-key-release-window nil
  "The last window in which the Action Key was released or nil.")
(defvar assist-key-release-window nil
  "The last window in which the Assist Key was released or nil.")

;; These store mouse positions and are used only when a mouse is available.
(defvar action-key-depress-position nil
  "The last mouse screen position at which the Action Key was depressed or nil.")
(defvar assist-key-depress-position nil
  "The last mouse screen position at which the Assist Key was depressed or nil.")
(defvar action-key-release-position nil
  "The last mouse screen position at which the Action Key was released or nil.")
(defvar assist-key-release-position nil
  "The last mouse screen position at which the Assist Key was released or nil.")

(defvar action-key-depress-prev-point nil
  "Marker at point prior to last Action Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar assist-key-depress-prev-point nil
  "Marker at point prior to last Assist Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar action-key-release-prev-point nil
  "Marker at point prior to last Action Key release.
Note that this may be a buffer different than where the release occurs.")
(defvar assist-key-release-prev-point nil
  "Marker at point prior to last Assist Key release.
Note that this may be a buffer different than where the release occurs.")

(defvar action-key-cancelled nil
  "When non-nil, cancels last Action Key depress.")
(defvar assist-key-cancelled nil
  "When non-nil, cancels last Assist Key depress.")

(defvar action-key-help-flag nil
  "When non-nil, forces display of help for next Action Key release.")
(defvar assist-key-help-flag nil
  "When non-nil, forces display of help for next Assist Key release.")

(defvar assist-flag nil
  "Non-nil when Hyperbole's Assist Key is in use rather than the Action Key.
Never set directly.  Bound as a parameter when `hkey-execute' is called
and then used as a free variable.")

(defcustom hkey-debug nil
  "Non-nil display a message with the context and values from Smart Key activation.
Default is nil."
  :type 'boolean
  :group 'hyperbole-commands)

(defvar hkey-region nil
  "Used to pass the value of a region between a Smart Key depress and release.
This permits the Smart Keys to behave as paste keys.")

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function br-in-view-window-p "ext:br")
(declare-function br-next-listing-window "ext:br")
(declare-function br-to-view-window "ext:br")

(declare-function ace-window "ext:ace-window")
(declare-function ace-window-display-mode "ext:ace-window")
(declare-function aw-select "ext:ace-window")

(defvar aw-dispatch-alist)
(defvar aw-dispatch-always)
(defvar aw-frame-size)
(defvar aw-keys)

;; window-jump
(declare-function window-jump "ext:window-jump")
(defvar	wj-vec-left)
(defvar	wj-vec-right)
(defvar	wj-vec-down)
(defvar	wj-vec-up)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar action-mouse-key-prev-window nil
  "Window point was in prior to current invocation of `action/assist-mouse-key'.")

(defvar action-mouse-key-prefix-arg nil
  "Prefix argument to pass to `smart-br-cmd-select'.")

(defvar hkey-help-msg "" "Holds last Smart Key help message.")
(defvar hkey--wconfig nil
  "Window configuration within current frame prior to display of a help buffer.")

;;; ************************************************************************
;;; Hyperbole context-sensitive key driver functions
;;; ************************************************************************

(defun hkey-absolute-pixel-position ()
  "Return the absolute pixel position of the mouse or the selected window's point."
  (if (mouse-event-p last-input-event)
      (mouse-absolute-pixel-position)
    (window-absolute-pixel-position)))

;;; Smart Key Depress Functions
(defun action-key-depress (&rest args)
  "Register depress of the Hyperbole Action Mouse Key."
  (interactive)
  (cond (assist-key-depressed-flag
	 (or action-key-help-flag
	     (setq assist-key-help-flag t)))
	((hmouse-save-region)))
  (setq action-key-depress-prev-point (point-marker)
	action-key-depressed-flag t
	action-key-depress-args (hmouse-set-point args)
	action-key-depress-window (or (hmouse-depress-inactive-minibuffer-p args)
				      (selected-window))
	action-key-depress-position (hkey-absolute-pixel-position)
	action-key-release-args nil
	action-key-release-window nil
	action-key-release-prev-point nil)
  (when (and (not assist-key-depressed-flag)
	     (hmouse-modeline-event-p action-key-depress-args))
    (mouse-drag-mode-line action-key-depress-args))
  (when (eq last-command #'org-todo)
    (setq this-command #'org-todo))
  (run-hooks 'action-key-depress-hook))

(defun assist-key-depress (&rest args)
  "Register depress of the Hyperbole Assist Mouse Key."
  (interactive)
  (cond (action-key-depressed-flag
	 (or assist-key-help-flag
	     (setq action-key-help-flag t)))
	((hmouse-save-region)))
  (setq assist-key-depress-prev-point (point-marker)
	assist-key-depressed-flag t
	assist-key-depress-args (hmouse-set-point args)
	assist-key-depress-window (or (hmouse-depress-inactive-minibuffer-p args)
				      (selected-window))
	assist-key-depress-position (hkey-absolute-pixel-position)
	assist-key-release-args nil
	assist-key-release-window nil
	assist-key-release-prev-point nil)
  (when (and (not action-key-depressed-flag)
	     (hmouse-modeline-event-p assist-key-depress-args))
    (mouse-drag-mode-line assist-key-depress-args))
  (when (eq last-command #'org-todo)
    (setq this-command #'org-todo))
  (run-hooks 'assist-key-depress-hook))

(defun action-key-depress-emacs (event)
  "Handle depress event of the Hyperbole Action Mouse Key."
  (interactive "e")
  (action-key-depress event))

(defun assist-key-depress-emacs (event)
  "Handle depress event of the Hyperbole Assist Mouse Key."
  (interactive "e")
  (assist-key-depress event))

;;; Smart Key Release Functions
(defun action-mouse-key-emacs (event)
  "Set point to the current mouse cursor position and execute `action-key'.
EVENT will be passed to `hmouse-function'."
  (interactive "e")
  (apply #'action-mouse-key (hmouse-key-release-args-emacs event)))

(defun assist-mouse-key-emacs (event)
  "Set point to the current mouse cursor position and execute `assist-key'.
EVENT will be passed to `hmouse-function'."
  (interactive "e")
  (apply #'assist-mouse-key (hmouse-key-release-args-emacs event)))

(defun action-mouse-key (&rest args)
  "Set point to the mouse or keyboard cursor position and execute `action-key'.
Any ARGS will be passed to `hmouse-function'."
  (interactive)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; action-key-depress command invocation.
  (when action-key-depressed-flag
    (setq action-key-release-position (hkey-absolute-pixel-position))
    (let ((hkey-alist hmouse-alist))
      (setq action-key-depressed-flag nil)
      (cond (action-key-cancelled
	     (setq action-key-cancelled nil
		   assist-key-depressed-flag nil))
	    (assist-key-depressed-flag
 	     (hmouse-function nil nil args))
	    ((hkey-mouse-help nil args))
	    (t
	     (run-hooks 'action-key-release-hook)
	     (hmouse-function #'action-key-internal nil args)))
      ;; Need to clear these variables so that mouse pasting does
      ;; not occur repeatedly from a single region selection.
      (setq hkey-region nil
	    hkey-value nil))))

(defun assist-mouse-key (&rest args)
  "Set point to the mouse or keyboard cursor position and execute `assist-key'.
Any ARGS will be passed to `hmouse-function'."
  (interactive)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; assist-key-depress command invocation.
  (when assist-key-depressed-flag
    (setq assist-key-release-position (hkey-absolute-pixel-position))
    (let ((hkey-alist hmouse-alist))
      (setq assist-key-depressed-flag nil)
      (cond (assist-key-cancelled
	     (setq assist-key-cancelled nil
		   action-key-depressed-flag nil))
	    (action-key-depressed-flag
	     (hmouse-function nil t args))
	    ((hkey-mouse-help t args))
	    (t
	     (run-hooks 'assist-key-release-hook)
	     (hmouse-function #'assist-key-internal t args)))
      ;; Need to clear this variable so that mouse pasting does
      ;; not occur repeatedly from a single region selection.
      (setq hkey-region nil
	    hkey-value nil))))

;;; Smart Key Commands
(defun action-key-clear-variables ()
  "Clear all Action Key variables."
  ;; Clear all these variables so there can be no confusion between
  ;; mouse presses and keyboard presses.
  (setq action-key-depress-prev-point nil
	action-key-depress-position nil
	action-key-depress-args nil
	action-key-depress-window nil
	action-key-release-position nil
	action-key-release-args nil
	action-key-release-window nil
	action-key-release-prev-point nil))

(defun assist-key-clear-variables ()
  "Clear all Assist Key variables."
  ;; Clear all these variables so there can be no confusion between
  ;; mouse presses and keyboard presses.
  (setq assist-key-depress-prev-point nil
	assist-key-depress-position nil
	assist-key-depress-args nil
	assist-key-depress-window nil
	assist-key-release-position nil
	assist-key-release-args nil
	assist-key-release-window nil
	assist-key-release-prev-point nil))

(defun action-key ()
  "Use one key to perform functions that vary by context.
If no matching context is found, the default function set with
the `action-key-default-function' variable is run.  Return t
unless the `action-key-default-function' variable is not bound to
a valid function."
  (interactive)
  (action-key-clear-variables)
  (prog1 (action-key-internal)
    (run-hooks 'action-key-depress-hook 'action-key-release-hook)))

(defun action-key-internal ()
  (setq action-key-depressed-flag nil)
  (when action-key-cancelled
    (setq action-key-cancelled nil
	  assist-key-depressed-flag nil))
  (or (hkey-execute nil)
      (when (fboundp action-key-default-function)
	(funcall action-key-default-function)
	t)))

(defun assist-key ()
  "Use one key to perform functions that vary by context.
If no matching context is found, the default function set with
the `assist-key-default-function' variable is run.  Return
non-nil unless `assist-key-default-function' variable is not
bound to a valid function."
  (interactive)
  (assist-key-clear-variables)
  (prog1 (assist-key-internal)
    (run-hooks 'assist-key-depress-hook 'assist-key-release-hook)))

(defun assist-key-internal ()
  (setq assist-key-depressed-flag nil)
  (when assist-key-cancelled
    (setq assist-key-cancelled nil
	  action-key-depressed-flag nil))
  (or (hkey-execute t)
      (when (fboundp assist-key-default-function)
	(funcall assist-key-default-function)
	t)))

(defun hkey-either (&optional arg)
  "Execute `action-key' or with non-nil ARG execute `assist-key'."
  (interactive "P")
  (when (and (featurep 'hycontrol)
	     (or hycontrol-windows-mode hycontrol-frames-mode))
      ;; Ignore any prefix arg set by HyControl and use prefix arg
      ;; only if it was given by a user as any number of C-u presses
      ;; and is therefore a list.
    (unless (listp arg) (setq arg nil)))
  (if arg (assist-key) (action-key)))


;;; ************************************************************************
;;; Hyperbole ace-window selection functions
;;; https://github.com/abo-abo/ace-window
;;; ************************************************************************

;; A call to (hkey-ace-window-setup) or (require 'ace-window) must be
;; made prior to calling any other function in this section since
;; Hyperbole does not require ace-window itself.

;;;###autoload
(defun hkey-ace-window-setup (&optional key)
  "Bind optional keyboard KEY and setup display of items specified by short ids.

The ace-window package, (see \"https://elpa.gnu.org/packages/ace-window.html\"),
assigns short ids to each Emacs window and lets you jump to or
operate upon a specific window by giving its letter.  Hyperbole
can insert an operation into ace-window that allows you to
display items such as dired or buffer menu items in a specific
window.

To enable this feature, in your Emacs initialization file after
Hyperbole is initialized, if you already have a key bound for
ace-window, then call:

 (hkey-ace-window-setup)

otherwise, choose a binding like {M-o} and send it to the same
function to bind it:

 (hkey-ace-window-setup \"\M-o\")

Then whenever point is on an item you want displayed in another
window, use {M-o i <id-of-window-to-display-item-in>} and watch the
magic happen."
  (require 'ace-window)
  (when key (hkey-set-key key 'ace-window))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	;; allows {i} operation to work when only 2 windows exist
	aw-dispatch-always t)
  ;; New ace-window frames (window id = z) inherit the size of the
  ;; prior selected frame; same as HyWindow.
  (setq aw-frame-size '(0 . 0)
	aw-dispatch-alist (delq (assq ?i aw-dispatch-alist)
				(delq (assq ?r aw-dispatch-alist)
				      (delq (assq ?t aw-dispatch-alist)
					    (delq (assq ?w aw-dispatch-alist) aw-dispatch-alist)))))
  (push '(?w hkey-window-link "Hyperbole: Window Link") aw-dispatch-alist)
  (push '(?t hkey-throw   "Hyperbole: Throw") aw-dispatch-alist)
  (push '(?r hkey-replace "Hyperbole: Replace Here") aw-dispatch-alist)
  ;; Ace-window includes ?m as the swap windows key, so it is not added here.
  (push '(?i hkey-drag-item "Hyperbole: Drag Item") aw-dispatch-alist)
  (ace-window-display-mode 1))

;;;###autoload
(defun hkey-drag (release-window)
  "Emulate Smart Mouse Key drag from the selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  The drag action determines the final selected
window.

Optional prefix arg non-nil means emulate Assist Key rather than the
Action Key.

Works only when running under a window system, not from a dumb terminal."
  ;; Note: Cannot add free variable start-window as first parameter to this
  ;; function because it is called like many other functions herein with a
  ;; single release-window argument by 'hmouse-choose-windows'.

  ;; Cancel any partial drag that may have been recorded.
  (interactive (list (aw-select " Ace - Hyperbole: Drag")))
  (condition-case nil
      ;; This may trigger a No Action error if starting window
      ;; (window of depress) and release-window are the same; in that
      ;; case: use the error handler to emulate dragging an item if on
      ;; one.
      (progn (if current-prefix-arg
		 (setq assist-key-depressed-flag nil)
	       (setq action-key-depressed-flag nil))
	     (hkey-operate current-prefix-arg)
	     (when (window-live-p release-window)
	       (hypb:select-window-frame release-window))
	     (hkey-operate current-prefix-arg))
    (error (when (eq start-window release-window)
	     (hmouse-drag-item-to-display)))))

;;;###autoload
(defun hkey-drag-stay (release-window)
  "Emulate Smart Mouse Key drag from selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  After the drag, the selected window remains the same
as it was before the drag.

Works only when running under a window system, not from a dumb terminal."
  (let ((start-window (selected-window)))
    (unwind-protect
	(hkey-drag release-window)
      ;; Leave start-window selected
      (when (window-live-p start-window)
	(hypb:select-window-frame start-window)))))

;;;###autoload
(defun hkey-drag-item (release-window)
  "Emulate Smart Mouse Key drag from item in a selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  RELEASE-WINDOW is left selected unless point is not
on an item, in which case, an error is signalled.

Optional prefix arg non-nil means emulate Assist Key rather than the
Action Key.

Works only when running under a window system, not from a dumb terminal."
  (interactive
   (list (let ((mode-line-text (concat " Ace - " (nth 2 (assq ?i aw-dispatch-alist)))))
	   (aw-select mode-line-text))))
  (let ((start-window (if (and (boundp 'start-window) (window-live-p start-window))
			  start-window
			(if current-prefix-arg
			    assist-key-depress-window
			  action-key-depress-window)))
	at-item-flag)
    (unless (window-live-p start-window)
      (setq start-window (selected-window)))
    (cond ((and (setq at-item-flag (hmouse-at-item-p start-window))
		(window-live-p release-window))
	   (hkey-drag release-window)
	   ;; Leave release-window selected
	   (when (window-live-p release-window)
	     (hypb:select-window-frame release-window)))
	  (at-item-flag
	   (error "(hkey-drag-item): No listing item at point"))
	  (t ;; No item at point or selected release is invalid
	   (error "(hkey-drag-item): No item at point or invalid final window, %s" release-window)))))

;;;###autoload
(defun hkey-drag-to (release-window)
  "Emulate Smart Mouse Key drag from a selected window to RELEASE-WINDOW.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  If an item is dragged to RELEASE-WINDOW, then
RELEASE-WINDOW is selected; otherwise, the drag action determines
the selected window.  If no drag has taken place, then the
selected window's buffer is displayed in RELEASE-WINDOW and that
becomes the selected window.

Optional prefix arg non-nil means emulate Assist Key rather than the
Action Key.

Works only when running under a window system, not from a dumb terminal."
  (interactive
   (list (let ((mode-line-text (concat " Ace - " (nth 2 (assq ?i aw-dispatch-alist)))))
	   (aw-select mode-line-text))))
  (let ((start-window (if (and (boundp 'start-window) (window-live-p start-window))
			  start-window
			(if current-prefix-arg
			    assist-key-depress-window
			  action-key-depress-window))))
    (unless (window-live-p start-window)
      (setq start-window (selected-window)))
    (if (and (hmouse-at-item-p start-window) (window-live-p release-window))
	(progn (hkey-drag release-window)
	       ;; Leave release-window selected
	       (when (window-live-p release-window)
		 (hypb:select-window-frame release-window)))
      ;; Leave hkey-drag to choose final selected window
      (hkey-drag release-window)
      ;; (if (eq start-window release-window)
      ;; 	  ;; Leave hkey-drag to choose final selected window
      ;; 	  (hkey-drag release-window)
      ;; 	;; Replace release window's buffer with selected
      ;; 	;; window's buffer.
      ;; 	(hkey-buffer-to start-window release-window)
      ;; 	(when (window-live-p release-window)
      ;; 	  (hypb:select-window-frame release-window)))
      )))

;;;###autoload
(defun hkey-replace (release-window)
  "Grab the buffer from RELEASE-WINDOW and place it into the current window.
When called interactively the RELEASE-WINDOW is chosen via
ace-window.  The selected window does not change."
  (interactive
   (list (let ((mode-line-text (concat " Ace - " (nth 2 (assq ?r aw-dispatch-alist)))))
	   (aw-select mode-line-text))))
  (set-window-buffer (selected-window) (window-buffer release-window)))

;;;###autoload
(defun hkey-swap (to-window)
  "Swap the buffer from the selected window with that of TO-WINDOW.
When called interactively the TO-WINDOW is chosen via ace-window.  Leave
TO-WINDOW as the selected window."
  (interactive
   (list (let ((mode-line-text (concat " Ace - Hyperbole: " (nth 2 (assq ?m aw-dispatch-alist)))))
	   (aw-select mode-line-text))))
  (hkey-swap-buffers (selected-window) to-window))

;; Once the "display-until.el" library is added to Emacs, hkey-throw can be simplified to the following:
;;
;; (defun hkey-throw (release-window)
;;   "Throw either a displayable item at point or the current buffer for display in RELEASE-WINDOW.
;; The selected window does not change."
;;   (interactive
;;    (list (let ((mode-line-text (concat " Ace - " (nth 2 (assq ?t aw-dispatch-alist)))))
;;            (aw-select mode-line-text))))
;;   (if (cadr (assq major-mode hmouse-drag-item-mode-forms))
;;       ;; Throw the item at point
;;       (let ((action-key-depress-window (selected-window))
;;             (action-key-release-window release-window)
;;             (action-key-depress-args))
;;         (hmouse-item-to-window)
;;         (select-window action-key-depress-window)
;;         (display-window-until release-window))
;;     ;; Throw the current buffer
;;     (display-window-until release-window (current-buffer))))

;;;###autoload
(defun hkey-throw (release-window &optional throw-region-flag)
  "Throw for display in RELEASE-WINDOW.
Throw one of:
 - the active (highlighted) region,
 - a displayable item at point or
 - the current buffer.
With optional prefix arg THROW-REGION-FLAG, throw the current region
even if not active.
The selected window does not change."
  (interactive (list (ace-window nil) current-prefix-arg))
  (let ((depress-frame (selected-frame))
	(display-delay (if (boundp 'temp-display-delay)
			   temp-display-delay
			 0.5)))
	;; Throw either the region or the item at point and keep selected-window
	(let ((action-key-depress-window (selected-window))
	      (action-key-release-window release-window)
	      (action-key-depress-args))
	  (hypb:save-selected-window-and-input-focus
	   (unless (hkey-insert-region action-key-depress-window release-window throw-region-flag display-delay)
	     (if (cadr (assq major-mode hmouse-drag-item-mode-forms))
		 (hmouse-item-to-window)
	       (set-window-buffer release-window (current-buffer))))
	   (unless (eq depress-frame (window-frame release-window))
	     ;; Force redisplay or item buffer won't be displayed here.
	     (redisplay t)
	     ;; Show the frame thrown to before it is covered when
	     ;; input-focus is returned to the depress-frame.
	     (raise-frame (window-frame release-window))
	     ;; Don't use sit-for here because it can be interrupted early.
	     (sleep-for display-delay))))))

;;;###autoload
(defun hkey-window-link (release-window)
  "Create an ebut in the selected window, linked to point in RELEASE-WINDOW.
RELEASE-WINDOW is interactively chosen via ace-window.
The selected window does not change."
  (interactive
   (list (let ((mode-line-text (concat " Ace - Hyperbole: " (nth 2 (assq ?w aw-dispatch-alist)))))
	   (aw-select mode-line-text))))
  (let ((start-window (selected-window)))
    (unwind-protect
	(hui:link-directly start-window release-window)
      ;; Leave start-window selected
      (when (window-live-p start-window)
	(hypb:select-window-frame start-window)))))

(defun hkey-insert-region (depress-window release-window throw-region-flag display-delay)
  "Throw any active (highlighted) region from DEPRESS-WINDOW to RELEASE-WINDOW.
If THROW-REGION-FLAG is non-nil, the region is thrown even if not
active, unless the buffers in DEPRESS-WINDOW and RELEASE-WINDOW are
the same, then the region is not thrown.
Highlight the thrown region for DISPLAY-DELAY seconds.

Return t if thrown, else nil."
  (when (or (use-region-p) throw-region-flag)
    (cond ((or (not (window-live-p depress-window))
	       (not (window-live-p release-window)))
	   (user-error "(hkey-insert-region): Invalid window: depress window: '%s'; release window: '%s'"
		       depress-window release-window))
	  ((> (region-end) (region-beginning))
	   ;; Non-empty region
	   (if (and (eq (window-buffer depress-window) (window-buffer release-window))
		    (<= (region-beginning) (window-point release-window))
		    (>= (region-end) (window-point release-window)))
	       (user-error "(hkey-insert-region): Can't throw region to a point within the region")
	     (let* ((orig-buf (current-buffer))
		    (orig-start (region-beginning))
		    (orig-end (region-end))
		    (len (- orig-end orig-start))
		    insert-start
		    insert-end)
	       (select-window release-window 'mark-for-redisplay)
	       (setq insert-start (point)
		     insert-end (+ insert-start len))
	       (insert-buffer-substring orig-buf orig-start orig-end)
	       (hmouse-pulse-region insert-start insert-end)
	       (sit-for display-delay)
	       t)))
	  (t (user-error "(hkey-insert-region): Can't throw an empty region")))))

;;;###autoload
(defun hkey-buffer-to (from-window to-window)
  "Display buffer from FROM-WINDOW in TO-WINDOW.
When interactive use ace-window to choose FROM-WINDOW and
TO-WINDOW.  The selected window does not change."
  (interactive
   (list (aw-select " Ace - Hyperbole: Buffer to Show")
	 (aw-select " Ace - Hyperbole: Show in Window")))
  (with-selected-window from-window
    (set-window-buffer to-window (current-buffer))))

;;;###autoload
(defun hkey-swap-buffers (from-window to-window)
  "Swap buffer from FROM-WINDOW with buffer of TO-WINDOW.
When interactive use ace-window to choose FROM-WINDOW and
TO-WINDOW.  Leave TO-WINDOW as the selected window."
  (interactive
   (list (aw-select " Ace - Hyperbole: Swap from Buffer1...")
	 (aw-select " Ace - Hyperbole: ...to Buffer2")))
  (let ((from-buf (window-buffer from-window))
	(to-buf (window-buffer to-window)))
    (set-window-buffer from-window to-buf)
    (set-window-buffer to-window from-buf)
    (hypb:select-window-frame to-window)))

;;; ************************************************************************
;;; Hyperbole mouse click window selection functions
;;; ************************************************************************

;;;###autoload
(defun hmouse-click-to-drag ()
  "Mouse click on start and end windows for use with `hkey-drag'.
Emulate Smart Mouse Key drag from start window to end window.
The drag action determines the final selected window."
  (interactive)
  (hmouse-choose-windows #'hkey-drag))

;;;###autoload
(defun hmouse-click-to-drag-stay ()
  "Mouse click on start and end windows for use with `hkey-drag-stay'.
Emulate Smart Mouse Key drag from start window to end window.
The selected window does not change."
  (interactive)
  (hmouse-choose-windows #'hkey-drag-stay))

;;;###autoload
(defun hmouse-click-to-drag-item ()
  "Mouse click on start and end windows for use with `hkey-drag-item'.
Emulate {M-o i} from start window to end window.
After the drag, the end window is the selected window."
  (interactive)
  (hmouse-choose-windows #'hkey-drag-item))

;;;###autoload
(defun hmouse-click-to-drag-to ()
  "Mouse click on start and end windows for use with `hkey-drag-to'.
Emulate Smart Mouse Key drag from start window to end window.
After the drag, the end window is the selected window."
  (interactive)
  (hmouse-choose-windows #'hkey-drag-to))

;;;###autoload
(defun hmouse-click-to-replace ()
  "Mouse click on start and end windows for use with `hkey-replace'.
Replace the buffer in start window with the buffer in end window.
The selected window does not change."
  (interactive)
  (hmouse-choose-windows #'hkey-replace))

;; Test this next command
;; (global-set-key [C-down-mouse-1] nil)
;; (global-set-key [C-mouse-1] 'hmouse-click-to-swap)
;;;###autoload
(defun hmouse-click-to-swap ()
  "Mouse click on start and end windows for use with `hkey-swap'.
Swap the buffer in start window with the buffer in end window.
Leave the end window selected."
  (interactive)
  (hmouse-choose-windows #'hkey-swap))

;;;###autoload
(defun hmouse-click-to-throw ()
  "Mouse click on start and end windows for use with `hkey-throw'.
Throw either a displayable item at start window's point or its current
buffer to the end window.  The selected window does not change."
  (interactive)
  (hmouse-choose-windows #'hkey-throw))

(defun hmouse-choose-windows (func)
  "Mouse click on start and end windows for FUNC.
Then with the start window temporarily selected, run FUNC with the
end window as an argument.

Appropriate FUNCs include: hkey-drag, hkey-drag-to, hkey-replace,
hkey-swap and hkey-throw."
  (let* (start-event
	 end-event
	 start-window
	 end-window)
    (message "Click on the %s start window..." func)
    (setq start-window
	  (cl-loop do (setq start-event (read-event))
		   until (and (mouse-event-p start-event)
			      (not (string-match "\\`down-" (symbol-name (car start-event)))))
		   finally return (posn-window (event-start start-event))))
    (message "Now click on the %s end window..." func)
    (setq end-window
	  (cl-loop do (setq end-event (read-event))
		   until (and (mouse-event-p end-event)
			      (not (string-match "\\`down-" (symbol-name (car end-event)))))
		   finally return (posn-window (event-start end-event))))
    (message "Done")
    (with-selected-window start-window
      (funcall func end-window))))

;;; ************************************************************************
;;; Hyperbole Directional Buffer Movement Commands
;;; ************************************************************************

;;;###autoload
(defun hkey-buffer-move-left ()
  "Swap the current buffer with the one on its left, if any; otherwise, do nothing."
  (interactive)
  (hkey-buffer-move 'left))

;;;###autoload
(defun hkey-buffer-move-right ()
  "Swap the current buffer with the one on its right, if any; otherwise do nothing."
  (interactive)
  (hkey-buffer-move 'right))

;;;###autoload
(defun hkey-buffer-move-down ()
  "Swap the current buffer with the one below it, if any; otherwise, do nothing."
  (interactive)
  (hkey-buffer-move 'down))

;;;###autoload
(defun hkey-buffer-move-up ()
  "Swap the current buffer with the one on above it, if any; otherwise, do nothing."
  (interactive)
  (hkey-buffer-move 'up))

(defun hkey-buffer-move (direction &optional arg)
  "Move the current buffer to the next window in DIRECTION.
DIRECTION is a symbol, one of: up, down, left or right.

When the window-jump package is available and `wj-jump-frames' is
non-nil, the buffer may be moved across non-overlapping frames in
the given direction."
  (interactive "SDirection to move buffer (up, down, left or right): \nP")
  ;; Prefer the window-jump package ...
  (if (require 'window-jump nil t)
      (let ((w1 (selected-window)))
	(window-jump
	 (pcase direction
	   ('left wj-vec-left)
	   ('right wj-vec-right)
	   ('down wj-vec-down)
	   ('up wj-vec-up)
	   (_ (error "(hkey-buffer-move): Invalid movement direction, '%s'" direction))))
	(hkey-swap-buffers w1 (selected-window)))
    ;; ... but if not available, use the Emacs builtin windmove package.
    (eval-and-compile
      (require 'windmove))
    (windmove-do-window-select direction arg)))

;;; ************************************************************************
;;; Public support functions
;;; ************************************************************************

;; Next function is redefined from Emacs mouse.el.  The standard
;; version allows moving frames by dragging a bottommost modeline with
;; mouse button1 but only if there is no minibuffer window (a rare
;; configuration) This limitation is so that the minibuffer window
;; can be manually resized.
;;
;; Hyperbole's mouse buttons do not support resizing the minibuffer
;; window so instead this function is modified to allow moving frames
;; that have a minibuffer window.
;;
;; The way this function was written does not allow hooking into it,
;; forcing inclusion of a modified version here.
(defun mouse-drag-mode-line (start-event)
  "Change the height of a window by dragging on its mode line.
START-EVENT is the starting mouse event of the drag action.

If the drag happens in a mode line on the bottom of a frame and
that frame's `drag-with-mode-line' parameter is non-nil, drag the
frame instead."
  (interactive "e")
  (let* ((start (event-start start-event))
	 (window (posn-window start))
         (frame (window-frame window)))
    (cond
     ((not (window-live-p window)))
     ((or (not (window-at-side-p window 'bottom))
          ;; Allow resizing the minibuffer window if it's on the
          ;; same frame as and immediately below `window', and it's
          ;; either active or `resize-mini-windows' is nil.
          (let ((minibuffer-window (minibuffer-window frame)))
            (and (eq (window-frame minibuffer-window) frame)
                 (or (not resize-mini-windows)
                     (eq minibuffer-window
                         (active-minibuffer-window))))))
      (mouse-drag-line start-event 'mode))
     ((and (frame-parameter frame 'drag-with-mode-line)
           (window-at-side-p window 'bottom))
      ;; Drag frame when the window is on the bottom of its frame.
      (mouse-drag-frame start-event 'move)))))

(defun hkey-debug (pred pred-value hkey-action)
  (message "(HyDebug) %sContext: %s; %s: %s; Buf: %s; Mode: %s; MinibufDepth: %s"
	   (cond ((eq pred-value 'hbut:current)
		  (format "ButProps: %S\nButType: %s; ButLabel: %s; "
			  (symbol-plist 'hbut:current)
			  (hattr:get 'hbut:current 'categ)
			  (hypb:format-quote (hbut:label 'hbut:current))))
		 ((functionp pred-value)
		  (format "Selection Func: %s; " pred-value))
		 (t ""))
	   pred
	   (if assist-flag "Assist" "Action")
	   (if (hattr:get  'hbut:current 'actype)
	       (or (hattr:get  'hbut:current 'action)
		   (cons (hattr:get  'hbut:current 'actype)
			 (hattr:get  'hbut:current 'args)))
	     (hypb:format-quote (format "%s" hkey-action)))
	   (current-buffer)
	   major-mode
	   (minibuffer-depth)))

(defun hkey-execute (assisting)
  "Evaluate Action Key form for first non-nil predicate from `hkey-alist'.
Non-nil ASSISTING means evaluate second form (Assist Key form),
otherwise evaluate first form.  Return non-nil iff a non-nil
predicate is found."
  ;; Keep in mind that hkey-alist may be set to hmouse-alist here, with additional predicates.
  (let ((hkey-forms hkey-alist)
	(assist-flag assisting)
	(pred-point (point-marker))
	pred-value hkey-action hkey-form pred)
    (while (and (null pred-value) (setq hkey-form (car hkey-forms)))
      (if (setq hkey-action (if assisting (cddr hkey-form) (cadr hkey-form))
		pred (car hkey-form)
		pred-value (eval pred))
	  (progn
	    ;; Any Smart Key predicate should leave point unchanged.
	    ;; Trigger an error if not.
	    (unless (equal (point-marker) pred-point)
	      (hypb:error "(Hyperbole): `%s' predicate failed to restore point to %s" pred pred-point))
	    (set-marker pred-point nil)
	    ;; Conditionally debug after Smart Key release and evaluation
	    ;; of matching predicate but before hkey-action is executed.
	    (when hkey-debug
	      (hkey-debug pred pred-value hkey-action))
	    (if hkey-debug
		(let ((debug-on-error t)
		      (debug-on-quit t))
		  (eval hkey-action))
	      (eval hkey-action)))
	(setq hkey-forms (cdr hkey-forms))))
    pred-value))

(defun hkey-help (&optional assisting)
  "Display help for the Action Key command in current context.
With optional ASSISTING prefix arg non-nil, display help for the
Assist Key command.  Return non-nil iff associated help
documentation is found."
  (interactive "P")
  (let* ((mouse-flag (when (mouse-event-p last-command-event)
		       (or action-key-depress-position assist-key-depress-position)))
	 (mouse-drag-flag (hmouse-drag-p))
	 (hkey-forms (if mouse-flag hmouse-alist hkey-alist))
	 (hrule:action 'actype:identity)
	 (assist-flag assisting)
	 hkey-form pred-value call calls cmd-sym doc)
    (while (and (null pred-value) (setq hkey-form (car hkey-forms)))
      (or (setq pred-value (eval (car hkey-form)))
	  (setq hkey-forms (cdr hkey-forms))))
    (if pred-value
	(setq call (if assisting (cdr (cdr hkey-form))
		     (cadr hkey-form))
	      cmd-sym (if (eq (car call) #'funcall)
			  (cadr call)
			(car call)))
      (setq cmd-sym (if assisting assist-key-default-function action-key-default-function)
	    call cmd-sym))
    (if (and (consp call) (eq (car call) 'call-interactively))
	(when (consp (cadr call))
	  (setq cmd-sym (if (memq (caadr call) '(function quote))
			    (cadadr call)
			  (caadr call)))))
    (setq calls (if (and (consp call) (eq (car call) 'or))
		    (mapcar #'identity (cdr call))
		  (list cmd-sym)))

    (unless (or action-key-depressed-flag action-key-help-flag)
      (action-key-clear-variables))
    (unless (or assist-key-depressed-flag assist-key-help-flag)
      (assist-key-clear-variables))

    (setq hkey-help-msg
	  (if (and cmd-sym (symbolp cmd-sym))
	      (progn
		(let* ((condition (car hkey-form))
		       (temp-buffer-show-hook
			(lambda (buf)
			  (set-buffer buf)
			  (help-mode)
			  (let ((owind (selected-window)))
			    (if (br-in-browser)
				(save-excursion
				  (br-to-view-window)
				  (select-window (previous-window))
				  (display-buffer buf 'other-win))
			      (display-buffer buf 'other-win))
			    (if (or (and (boundp 'help-window-select)
					 help-window-select)
				    (and (boundp 'help-selects-help-window)
					 help-selects-help-window))
				(select-window (get-buffer-window buf))
			      (select-window owind)))))
		       (temp-buffer-show-function temp-buffer-show-hook))
		  (with-output-to-temp-buffer
		      (hypb:help-buf-name
		       (format "%s %sKey"
			       (if assisting "Assist" "Action")
			       (if mouse-flag "Mouse " "")))
		    (princ (format "A %s of the %s %sKey"
				   (if mouse-flag
				       (if mouse-drag-flag "drag" "click")
				     "press")
				   (if assisting "Assist" "Action")
				   (if mouse-flag "Mouse " "")))
		    (terpri)
		    (princ "WHEN  ")
		    (princ
		     (or condition
			 "there is no matching context"))
		    (terpri)

		    (mapc (lambda (c)
			    (when (and (> (length calls) 1)
				       (not (eq (car calls) c)))
			      ;; Is an 'or' set of calls
			      (princ "OR "))
			    (princ "CALLS ") (princ (if (consp c) c (list c)))
			    (when (and (fboundp (setq call (if (consp c) (car c) c)))
				       (setq doc (documentation call)))
			      (princ " WHICH")
			      (princ (if (string-match "\\`[a-zA-Z]*[a-rt-zA-RT-Z]+s[ [:punct:]]" doc)
					 ":" " WILL:"))
			      (terpri) (terpri)
			      (princ (replace-regexp-in-string "^" "  " doc nil t))
			      (terpri) (terpri)))
			  calls)

		    (when (memq cmd-sym '(hui:hbut-act hui:hbut-help))
		      (princ (format "%s BUTTON SPECIFICS:\n\n%s\n"
				     (htype:def-symbol
				      (if (eq (hattr:get 'hbut:current 'categ)
					      'explicit)
					  (hattr:get 'hbut:current 'actype)
					(hattr:get 'hbut:current 'categ)))
				     (actype:doc 'hbut:current t)))
		      (hattr:report
		       (nthcdr 2 (hattr:list 'hbut:current))))
		    (terpri)))
		"")
	    (message "No %s Key command for current context."
		     (if assisting "Assist" "Action"))))
    doc))

(defun hkey-assist-help ()
  "Display doc associated with Assist Key command in current context.
Return non-nil iff associated documentation is found."
  (interactive)
  (hkey-help t))

;; Overload help-mode quit-window function to support Hyperbole
;; hkey--wconfig window configurations.
(unless (eq (symbol-function #'quit-window) #'hkey-help-hide)
  (defalias 'hkey-quit-window (symbol-function #'quit-window)))

;;;###autoload
(defun hkey-help-hide (&optional kill window)
  "Optionally KILL current buffer (default is bury) and quit WINDOW.
Restore frame to configuration prior to help buffer display.
Point must be in a help buffer.  See `hkey-quit-window' for additional
details."
  (interactive "P")
  (let ((buf (current-buffer)))
    (if (window-configuration-p hkey--wconfig)
	(progn (set-window-configuration hkey--wconfig)
	       (if kill
		   (kill-buffer buf)
		 (bury-buffer buf)))
      (hkey-quit-window kill window)))
  (setq hkey--wconfig nil))

(defalias 'quit-window 'hkey-help-hide)

;; Newer versions of Emacs define this variable but older versions,
;; e.g. Emacs 22, do not.  Calls to the `with-help-buffer' macro
;; compiled in Emacs 25 will fail without this, so conditionally
;; define it here.
(unless (boundp 'help-window-point-marker)
  (defvar help-window-point-marker (make-marker)
    "Marker to override default `window-point' in help windows."))

;;;###autoload
(defun hkey-help-show (&optional buffer current-window)
  "Save prior window configuration if BUFFER displays help.  Display BUFFER.

With optional second arg CURRENT-WINDOW non-nil, force display of buffer within
the current window.  By default, it is displayed according to the setting of
`hpath:display-where'."
  (if (bufferp buffer) (setq buffer (buffer-name buffer)))
  (if (null buffer) (setq buffer (buffer-name (current-buffer))))
  (let ((org-help (and (stringp buffer) (string-match "\\`\\*Org Help\\*" buffer)))
	(owind (selected-window)))
    (and (stringp buffer)
	 (string-match "^\\*Help\\|Help\\*$" buffer)
	 (not (memq t (mapcar (lambda (wind)
				(string-match
				 "^\\*Help\\|Help\\*$"
				 (buffer-name (window-buffer wind))))
			      (hypb:window-list 'no-mini))))
	 (setq hkey--wconfig (current-window-configuration)))
    (unwind-protect
	(let* ((buf (get-buffer-create buffer))
	       ;; Help-mode calls with-temp-buffer which invokes one of these hooks
	       ;; which calls hkey-help-show again, so nullify them before
	       ;; displaying the buffer.
	       (temp-buffer-show-hook)
	       (temp-buffer-show-function)
	       (wind (cond (current-window
			    (switch-to-buffer buf)
			    (selected-window))
			   (t (hpath:display-buffer buf)))))
	  ;; Ignore org-mode's temp help buffers which it handles on its own.
	  (when (and wind (not org-help))
	    (setq minibuffer-scroll-window wind)
	    ;; Don't use help-mode in buffers already set up with a
	    ;; quit-key to bury the buffer, e.g. minibuffer completions,
	    ;; as this will sometimes disable default left mouse key item
	    ;; selection.
	    (unless (or (where-is-internal 'quit-window (current-local-map))
			(where-is-internal 'hkey-help-hide (current-local-map)))
	      (when (string-match "^\\*Help\\|Help\\*$" (buffer-name))
		(help-mode))
	      (when (derived-mode-p 'help-mode)
		(local-set-key "q" #'hkey-help-hide)))))
      ;; If in an *Org Help* buffer, reselect the Org buffer.
      (when org-help
	(select-window owind))
      ;; If in a *Completions* buffer, re-select the window that
      ;; generated the completions.
      (when (buffer-live-p completion-reference-buffer)
	(select-window (get-buffer-window completion-reference-buffer t))))))

(defun hkey-mouse-help (assisting args)
  "If a Smart Key help flag is set and the other Smart Key is not down, show help.
Takes two args: ASSISTING should be non-nil iff command applies
to the Assist Key.  ARGS is a list of arguments passed to
`hmouse-function'.  Return t if help is displayed, nil otherwise."
  (let ((help-shown)
	(other-key-released (not (if assisting
				     action-key-depressed-flag
				   assist-key-depressed-flag))))
    (unwind-protect
	(setq help-shown
	      (cond ((and  action-key-help-flag other-key-released)
		     (setq action-key-help-flag nil)
		     (hmouse-function #'hkey-help assisting args)
		     t)
		    ((and  assist-key-help-flag other-key-released)
		     (setq assist-key-help-flag nil)
		     (hmouse-function #'hkey-assist-help assisting args)
		     t)))
      (when help-shown
	;; Then both Smart Keys have been released.
	(setq action-key-cancelled nil
	      assist-key-cancelled nil)
	t))))

(defun hkey-operate (&optional arg)
  "Use the keyboard to emulate Smart Mouse Key drag actions.
Each invocation alternates between starting a drag and ending it.
Optional prefix ARG non-nil means emulate Assist Key rather than the
Action Key.

Only works when running under a window system, not from a dumb terminal."
  (interactive "P")
  (unless (hyperb:window-system)
    (hypb:error "(hkey-operate): Drag actions require mouse support"))
  (if arg
      (if assist-key-depressed-flag
	  (progn (assist-mouse-key)
		 (when (called-interactively-p 'interactive)
		   (message "Assist Key released.")))
	(assist-key-depress)
	(when (called-interactively-p 'interactive)
	  (message
	   "Assist Key depressed; go to release point and press {%s %s}."
	   (substitute-command-keys "\\[universal-argument]")
	   (substitute-command-keys "\\[hkey-operate]"))))
    (if action-key-depressed-flag
	(progn (action-mouse-key)
	       (when (called-interactively-p 'interactive)
		 (message "Action Key released.")))
      (action-key-depress)
      (when (called-interactively-p 'interactive)
	(message "Action Key depressed; go to release point and press {%s}."
		 (substitute-command-keys "\\[hkey-operate]"))))))

(defun hkey-summarize (&optional current-window)
  "Display smart key operation summary in help buffer.
With optional arg CURRENT-WINDOW non-nil, force display of buffer within
the current window.  By default, it is displayed in another window."
  (interactive)
  (let* ((doc-file (hypb:hkey-help-file))
	 (buf-name (hypb:help-buf-name "Smart Keys"))
	 (wind (get-buffer-window buf-name)))
    (when (file-readable-p doc-file)
      (when (br-in-browser)
	(br-to-view-window))
      (if wind
	  (select-window wind)
	(hkey-help-show buf-name current-window)
	(select-window (get-buffer-window buf-name)))
      (setq buffer-read-only nil) (erase-buffer)
      (insert-file-contents doc-file)
      (goto-char (point-min))
      (set-buffer-modified-p nil))))


(defun hkey-toggle-debug (&optional arg)
  "Toggle whether Hyperbole logs Smart Key events.
Key events can be used later for analysis/submission using {C-h h m c}.
With optional ARG, enable iff ARG is positive."
  (interactive "P")
  (if (or (and arg (<= (prefix-numeric-value arg) 0))
	  (and (not (and arg (> (prefix-numeric-value arg) 0)))
	       hkey-debug))
      (progn (setq hkey-debug nil)
	     (message "Smart Key debugging is off."))
    (setq hkey-debug t)
    (message "Smart Key debugging is on; press a Smart Key to see its context.")))

(defun hmouse-depress-inactive-minibuffer-p (event)
  "Return buffer if last Smart Mouse Key depress was in an inactive minibuffer.
If the last Smart Mouse Key depress EVENT was in the minibuffer
and it was inactive, return it, else nil."
  (let ((window (posn-window (event-start event))))
    (when(framep window)
      (setq window (frame-selected-window window)))
    (and (window-minibuffer-p window)
	 (not (minibuffer-window-active-p window))
	 window)))

;; Based on code from subr.el.
(defun hmouse-vertical-line-spacing (frame)
  "Return any extra vertical spacing in pixels between text lines or 0 if none."
  (let ((spacing (when (display-graphic-p frame)
                   (or (with-current-buffer (window-buffer (frame-selected-window frame))
                         line-spacing)
		       (frame-parameter frame 'line-spacing)))))
    (cond ((floatp spacing)
	   (setq spacing (truncate (* spacing (frame-char-height frame)))))
	  ((null spacing)
	   (setq spacing 0)))
    spacing))

(defun hmouse-window-at-absolute-pixel-position (&optional position release-flag)
  "Return the top-most Emacs window at optional POSITION.
POSTION is ((x . y) in absolute pixels.  If POSITION is nil, use
mouse position if last input event was a mouse event, otherwise,
use the position of point in the selected window.

If the position used is not in a window, return nil.  Considers all windows on
the same display as the selected frame.

If optional RELEASE-FLAG is non-nil, this is part of a Smart Key
release computation, so optimize window selection based on the depress
window already computed.

If the selected frame is a graphical macOS window and
`hmouse-verify-release-window-flag' is non-nil, then return the
top-most Emacs window only if it is the top-most application window at
the position (not below another application's window)."
  (interactive)
  (setq position (or position
		     (if (mouse-event-p last-input-event)
			 (mouse-absolute-pixel-position)
		       (hkey-absolute-pixel-position))))
  ;; Proper top-to-bottom listing of frames is available only in Emacs
  ;; 26 and above.  For prior versions, the ordering of the frames
  ;; returned is not guaranteed, so the frame whose window is returned
  ;; may not be the uppermost.
  (let* ((top-to-bottom-frames (if (fboundp 'frame-list-z-order)
				   (frame-list-z-order)
				 (frame-list)))
	 (pos-x (car position))
	 (pos-y (cdr position))
	 edges left top right bottom
	 frame
	 in-frame
	 window)
    ;; First find top-most frame containing position.
    (while (and (not in-frame) top-to-bottom-frames)
      (setq frame (car top-to-bottom-frames)
	    top-to-bottom-frames (cdr top-to-bottom-frames))
      ;; Check that in-frame is valid with frame-live-p since under macOS
      ;; when position is outside a frame, in-frame could be invalid and
      ;; frame-visible-p would trigger an error in that case.
      (when (and (frame-live-p frame) (frame-visible-p frame))
	(setq edges (frame-edges frame)
	      left   (nth 0 edges)
	      top    (nth 1 edges)
	      right  (nth 2 edges)
	      bottom (nth 3 edges))
	(when (and (>= pos-x left) (<= pos-x right)
		   (>= pos-y top)  (<= pos-y bottom))
	  (setq in-frame frame))))
    ;; If in-frame is found, find which of its windows contains
    ;; position and return that.  The window-at call below requires
    ;; character coordinates relative to in-frame, so compute them.
    (when in-frame
      (let ((depress-position (and release-flag (if assist-flag
						    assist-key-depress-position
						  action-key-depress-position)))
	    (depress-window  (and release-flag (if assist-flag
						   assist-key-depress-window
						 action-key-depress-window))))
	(if (and release-flag depress-window (equal position depress-position))
	    ;; This was a click, so we know that the frame of the click
	    ;; is topmost on screen or the mouse events would not have
	    ;; been routed to Emacs.  Reuse saved window of depress rather
	    ;; then running possibly expensive computation to find the
	    ;; topmost application window.
	    (setq window depress-window)
	  (let ((char-x (/ (- pos-x left) (frame-char-width in-frame)))
		(line-y (/ (- pos-y top) (+ (frame-char-height in-frame)
					    (hmouse-vertical-line-spacing in-frame)))))
	    (setq window (window-at char-x line-y in-frame)))
	  ;;
	  ;; Otherwise, even if in-frame is found, under click-to-focus external window
	  ;; managers, Emacs may have received the drag release event when
	  ;; in-frame was covered by an external application's window.
	  ;; Emacs presently has no way to handle this.  However, for the
	  ;; macOS window system only, Hyperbole has a Python script, topwin.py, which
	  ;; computes the application of the topmost window at the point of release.
	  ;; If that is Emacs, then we have the right window and nothing need be
	  ;; done; otherwise, set window to nil and return.
	  ;;
	  (when (and hmouse-verify-release-window-flag
		     window (eq (window-system) 'ns))
	    ;; If depress and release windows are the same and frame has
	    ;; an auto-raise property, then we know this window was
	    ;; uppermost at the point of release and can skip this computation.
	    (unless (and (eq depress-window window) (frame-parameter nil 'auto-raise))
	      (let ((topwin (expand-file-name "topwin.py" hyperb:dir))
		    (case-fold-search t)
		    topmost-app)
		(when (and topwin (file-executable-p topwin))
		  (setq topmost-app (shell-command-to-string
				     (format "%s %d %d" topwin pos-x pos-y)))
		  (cond ((string-match "emacs" topmost-app)) ; In an Emacs frame, do nothing.
			((or (equal topmost-app "")
			     ;; Any non-Emacs app window
			     (string-match "\\`\\[" topmost-app))
			 ;; Outside of any Emacs frame
			 (setq window nil))
			(t ;; topwin error message
			 ;; Setup of the topwin script is somewhat complicated,
			 ;; so don't trigger an error just because of it.  But
			 ;; display a message so the user knows something happened
			 ;; when topwin encounters an error.
			 (message "(Hyperbole): topwin.py Python script error: %s" topmost-app))))))))))

    (when (called-interactively-p 'interactive)
      (message "%s at absolute pixel position %s"
	       (or window "No Emacs window") position))
    window))

(defun hmouse-window-coordinates (&optional event)
  "Return a list (window (x-chars . y-chars)) for optional EVENT.
Always ignores EVENT coordinates and uses current mouse position.
The area of the EVENT is utilized.  If EVENT is not given and the
free variable `assist-flag' is non-nil, EVENT is set to
`assist-key-release-args', otherwise, `action-key-release-args'.

The coordinates x-chars and y-chars are relative character
coordinates within the window.  If POSITION is not in a live
window, return nil.  Considers all windows on the selected frame's display."
  (interactive)
  (unless (eventp event)
    (setq event (if assist-flag assist-key-release-args action-key-release-args)))
  (let* ((position (mouse-absolute-pixel-position))
	 (pos-x (car position))
	 (pos-y (cdr position))
	 (window (hmouse-window-at-absolute-pixel-position position t))
	 (edges (when (window-live-p window) (window-edges window t t t)))
	 left top right bottom
	 frame)
    (when edges
      (setq left   (nth 0 edges)
	    top    (nth 1 edges)
	    right  (nth 2 edges)
	    bottom (nth 3 edges))
      (when (or (and event (eq (posn-area (event-start event)) 'mode-line))
		(and (>= pos-x left) (<= pos-x right)
		     (>= pos-y top)  (<= pos-y bottom)))
	;; If position is in a live window, compute position's character
	;; coordinates within the window and return the window with these
	;; coordinates.
	(setq frame (window-frame window)
	      pos-x (round (/ (- pos-x left) (frame-char-width frame)))
	      pos-y (round (/ (- pos-y top)  (+ (frame-char-height frame)
						(hmouse-vertical-line-spacing frame)))))))
    (when (called-interactively-p 'interactive)
      (message "%s at %s coordinates (%s . %s)"
	       (if edges window "No live Emacs window")
	       (if frame "character" "absolute pixel")
	       pos-x pos-y))
    (when edges (list window (cons pos-x pos-y)))))

(defun hmouse-key-release-window (release-position)
  "Return the window of last Action/Assist Mouse Key RELEASE-POSITION, if any.
If none return nil."
  (ignore-errors (hmouse-window-at-absolute-pixel-position release-position t)))

(defun hmouse-key-release-args-emacs (event)
  "For GNU Emacs, return a possibly modified version of EVENT as a list.
For mouse drags and double and triple clicks, remove any depress location,
compute the actual release location and include that."
  (if (integerp event)
      (list event)
    (let ((ev-type-str (and (listp event) (symbol-name (car event)))))
      (if (or (and ev-type-str
		   (string-match "\\(double\\|triple\\)-mouse" ev-type-str))
	      (not (= (length event) 3)))
	  event
	(let ((pos (event-end event))
	      coords window window-and-char-coords)
	  (when (and ev-type-str (string-match "drag-mouse" ev-type-str)
		     ;; end of drag event; If drag crossed frames, the location
		     ;; will contain the frame of the depress point and
		     ;; some relative coordinates; change these to the window of
		     ;; release and window's character coordinates if within a window
		     ;; and to nil if outside of Emacs (as best we can tell).
		     (framep (posn-window pos)))
	    (setq window-and-char-coords (hmouse-window-coordinates event)
		  window (car window-and-char-coords)
		  coords (cadr window-and-char-coords))
	    ;; Modify the values in the event-end structure even if no
	    ;; valid window was found.
	    (setcar pos window)
	    (setcar (nthcdr 2 pos) coords)))
	;; Remove depress coordinates and send only original release coordinates.
	(list (car event) (nth 2 event))))))

(defun hmouse-use-region-p ()
  "Return t if there is a non-empty, highlighted region, else nil."
  (let ((use-empty-active-region))
    (use-region-p)))

(defun hmouse-save-region ()
  "Save to `hkey-region' and return any active region within the current buffer.
`transient-mark-mode' must be t or this sets `hkey-region' to nil."
  (setq hkey-region
	(when (hmouse-use-region-p)
	  (buffer-substring (region-beginning) (region-end)))))


;; Save any active region to `hkey-region' when the mouse is moved between frames or buffers.
(add-hook 'mouse-leave-buffer-hook #'hmouse-save-region)

;; BW - Last confirmed in 1999, for some reason, using this next
;; function in byte-compiled form caused the first character
;; after a mouse key depress to be dropped from the input queue when running
;; Emacs under X.  The non-byte-compiled form always worked fine.  We
;; assume this is no longer a problem in 2016 but have this note here
;; in case it is.
(defun hmouse-set-point (args)
  "Set point to Smart Key press/release location given by ARGS.
Return argument list including x and y frame coordinates in characters and
lines or if ARGS is null and there is no graphical window system,
return current point as a marker."
  (and (car args) (listp (car args)) (setq args (car args)))
  (if (and args (hyperb:window-system))
      (progn (hmouse-set-point-at args) args)
    (list 'keyboard-drag (posn-at-point))))

(defun hmouse-set-point-at (set-point-arg-list)
  "Set point to cursor position using SET-POINT-ARG-LIST and return t.
If `hmouse-set-point-command' is not bound to a function, this does nothing
and returns nil."
  (when (fboundp hmouse-set-point-command)
    (or (if set-point-arg-list
	    (funcall hmouse-set-point-command set-point-arg-list)
	  (funcall hmouse-set-point-command))
	t)))

;; "hsettings.el" contains documentation for this variable.
(unless (boundp 'smart-scroll-proportional)
  (defvar smart-scroll-proportional t
    "*Non-nil means Smart Keys should scroll relative to current line.
Smart Keys will scroll relative to current line when pressed at
the end of a line. Action Key moves current line to top of
window.  Assist Key moves current line to bottom of window.
Repeated presses then scroll up or down a windowful.  Nil value
instead ignores current line and always scrolls up or down a
windowful."))

(defun hmouse-function (func assisting set-point-arg-list)
  "Execute FUNC for Action Key and set point from SET-POINT-ARG-LIST.
Use Assist Key with ASSISTING non-nil.  FUNC may be nil in which
case no function is called.  SET-POINT-ARG-LIST is passed to the
call of the command bound to `hmouse-set-point-command'.  Return
nil if `hmouse-set-point-command' variable is not bound to a
valid function."
  (when (fboundp hmouse-set-point-command)
    (if assisting
	(setq assist-key-release-window (hmouse-key-release-window assist-key-release-position)
	      assist-key-release-prev-point (point-marker))
      (setq action-key-release-window (hmouse-key-release-window action-key-release-position)
	    action-key-release-prev-point (point-marker)))
    (and (eq major-mode 'br-mode)
	 (setq action-mouse-key-prev-window
	       (if (br-in-view-window-p)
		   (save-window-excursion
		     (br-next-listing-window)
		     (selected-window))
		 (selected-window))))
    (setq action-mouse-key-prefix-arg current-prefix-arg)
    (let ((release-args (hmouse-set-point set-point-arg-list)))
      (if assisting
	  (setq assist-key-release-args release-args)
	(setq action-key-release-args release-args)))
    (when func
      (funcall func)
      (setq action-mouse-key-prev-window nil
	    action-mouse-key-prefix-arg nil))
    t))

;; The smart keys scroll buffers when pressed at the end of lines.
;; These next two functions do the scrolling and keep point at the end
;; of line to simplify repeated scrolls when using keyboard smart keys.
;;
;; These functions may also be used to test whether the scroll action would
;; be successful: no action is taken if it would fail (because the beginning
;; or end of a buffer is already showing) and nil is returned.
;; t is returned whenever scrolling is performed.

(defun smart-scroll-down ()
  "Scroll down according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the bottom
window line, scroll down (backward) a windowful.  Otherwise, try
to bring current line to the bottom of the window.  Leave point
at end of line and return t if scrolled, nil if not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already last in window, then scroll backward
	;; a windowful, otherwise make it last in window.
	(if (>= (point) (save-excursion
			  (goto-char (1- (window-end)))
			  (beginning-of-line) (point)))
	    (if (pos-visible-in-window-p (point-min))
		(setq rtn nil)
	      (scroll-down))
	  (recenter -1))
      (if (pos-visible-in-window-p (point-min))
	  (setq rtn nil)
	(scroll-down)))
    (end-of-line)
    (or rtn (progn (beep) (message "Beginning of buffer")))
    rtn))

(defun smart-scroll-up ()
  "Scroll up according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the top
window line, scroll up (forward) a windowful.  Otherwise, tyr to
bring current line to the top of the window.  Leave point at end
of line and return t if scrolled, nil if not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already first in window, then scroll forward a
	;; windowful, otherwise make it first in window.
	(if (<= (point) (save-excursion
			  (goto-char (window-start))
			  (end-of-line) (point)))
	    (if (pos-visible-in-window-p (point-max))
		(setq rtn nil)
	      (scroll-up))
	  (recenter 0))
      (if (pos-visible-in-window-p (point-max))
	  (setq rtn nil)
	(scroll-up)))
    (end-of-line)
    (unless rtn
      (beep)
      (message "End of buffer"))
    rtn))

(provide 'hmouse-drv)
;;; hmouse-drv.el ends here
