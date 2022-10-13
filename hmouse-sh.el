;;; hmouse-sh.el --- System-dependent Smart Mouse Key bindings.  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     3-Sep-91 at 21:40:58
;; Last-Mod:     13-Oct-22 at 22:02:30 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   See description in "hmouse-key.el".

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hvar)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************
(defvar br-env-spec)
(defvar br-lang-prefix)
(defvar company-active-map)
(defvar java-class-def-name-grpn)
(defvar java-class-def-regexp)
(defvar jedi-mode)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Other mouse event location prefixes to possibly handle:
;;   vertical-scroll-bar
;;   horizontal-scroll-bar
;;   right-divider
;;   bottom-divider

;; tab-bar sample Smart Mouse setup
;; (define-key tab-prefix-map [down-mouse-2] #'action-key-depress-emacs)
;; (define-key tab-prefix-map [mouse-2]      #'action-mouse-key-emacs)

;; tab-line sample Smart Mouse setup
;; (define-key tab-line-tab-map [tab-line down-mouse-2] #'action-key-depress-emacs)
;; (define-key tab-line-tab-map [tab-line mouse-2]      #'action-mouse-key-emacs)

(defun hmouse-bind-key-emacs (mouse-key-number depress-cmd release-cmd)
  "Ensure MOUSE-KEY-NUMBER (1-5) is bound to DEPRESS-CMD and RELEASE-CMD.
This includes depresses and drags.  Mouse key 1 is [mouse-1], etc.
Use nil as cmd value to unbind a key."
  (hmouse-set-key-list
   depress-cmd
   (nth (1- mouse-key-number)
	'(
	  ([down-mouse-1]
	   [double-down-mouse-1] [triple-down-mouse-1]
	   [header-line down-mouse-1]
	   [left-fringe down-mouse-1]
	   [right-fringe down-mouse-1]
	   [vertical-line down-mouse-1]
	   [mode-line down-mouse-1])

	  ([down-mouse-2]
	   [double-down-mouse-2] [triple-down-mouse-2]
	   [header-line down-mouse-2]
	   [left-fringe down-mouse-2]
	   [right-fringe down-mouse-2]
	   [vertical-line down-mouse-2]
	   [mode-line down-mouse-2])

	  ([down-mouse-3]
	   [double-down-mouse-3] [triple-down-mouse-3]
	   [header-line down-mouse-3]
	   [left-fringe down-mouse-3]
	   [right-fringe down-mouse-3]
	   [vertical-line down-mouse-3]
	   [mode-line down-mouse-3])
	  
	  ([down-mouse-4]
	   [double-down-mouse-4] [triple-down-mouse-4]
	   [header-line down-mouse-4]
	   [left-fringe down-mouse-4]
	   [right-fringe down-mouse-4]
	   [vertical-line down-mouse-4]
	   [mode-line down-mouse-4])

	  ([down-mouse-5]
	   [double-down-mouse-5] [triple-down-mouse-5]
	   [header-line down-mouse-5]
	   [left-fringe down-mouse-5]
	   [right-fringe down-mouse-5]
	   [vertical-line down-mouse-5]
	   [mode-line down-mouse-5]))))
	   
  (hmouse-set-key-list
   release-cmd
   (nth (1- mouse-key-number)
	'(
	  ([drag-mouse-1] [mouse-1]
	   [double-mouse-1] [triple-mouse-1]
	   [header-line drag-mouse-1]
	   [header-line mouse-1]
	   [left-fringe drag-mouse-1]
	   [left-fringe mouse-1]
	   [right-fringe drag-mouse-1]
	   [right-fringe mouse-1]
	   [vertical-line drag-mouse-1]
	   [vertical-line mouse-1]
	   [mode-line drag-mouse-1]
	   [mode-line mouse-1])

	  ([drag-mouse-2] [mouse-2]
	   [double-mouse-2] [triple-mouse-2]
	   [header-line drag-mouse-2]
	   [header-line mouse-2]
	   [left-fringe drag-mouse-2]
	   [left-fringe mouse-2]
	   [right-fringe drag-mouse-2]
	   [right-fringe mouse-2]
	   [vertical-line drag-mouse-2]
	   [vertical-line mouse-2]
	   [mode-line drag-mouse-2]
	   [mode-line mouse-2])

	  ([drag-mouse-3] [mouse-3]
	   [double-mouse-3] [triple-mouse-3]
	   [header-line drag-mouse-3]
	   [header-line mouse-3]
	   [left-fringe drag-mouse-3]
	   [left-fringe mouse-3]
	   [right-fringe drag-mouse-3]
	   [right-fringe mouse-3]
	   [vertical-line drag-mouse-3]
	   [vertical-line mouse-3]
	   [mode-line drag-mouse-3]
	   [mode-line mouse-3])

	  ([drag-mouse-4] [mouse-4]
	   [double-mouse-4] [triple-mouse-4]
	   [header-line drag-mouse-4]
	   [header-line mouse-4]
	   [left-fringe drag-mouse-4]
	   [left-fringe mouse-4]
	   [right-fringe drag-mouse-4]
	   [right-fringe mouse-4]
	   [vertical-line drag-mouse-4]
	   [vertical-line mouse-4]
	   [mode-line drag-mouse-4]
	   [mode-line mouse-4])

	  ([drag-mouse-5] [mouse-5]
	   [double-mouse-5] [triple-mouse-5]
	   [header-line drag-mouse-5]
	   [header-line mouse-5]
	   [left-fringe drag-mouse-5]
	   [left-fringe mouse-5]
	   [right-fringe drag-mouse-5]
	   [right-fringe mouse-5]
	   [vertical-line drag-mouse-5]
	   [vertical-line mouse-5]
	   [mode-line drag-mouse-5]
	   [mode-line mouse-5])))))

(defun hmouse-bind-shifted-key-emacs (shifted-mouse-key-number depress-cmd release-cmd)
  "Ensure SHIFTED-MOUSE-KEY-NUMBER (1-5) is bound to DEPRESS-CMD and RELEASE-CMD.
This includes depresses and drags.  Shifted Mouse Key 1 is
[S-mouse-1], etc.  Use nil as cmd value to unbind the key."
  (hmouse-set-key-list
   depress-cmd
   (nth (1- shifted-mouse-key-number)
	'(
	  ([S-down-mouse-1] [header-line S-down-mouse-1]
	   [left-fringe S-down-mouse-1]
	   [right-fringe S-down-mouse-1]
	   [vertical-line S-down-mouse-1]
	   [mode-line S-down-mouse-1])

	  ([S-down-mouse-2] [header-line S-down-mouse-2]
	   [left-fringe S-down-mouse-2]
	   [right-fringe S-down-mouse-2]
	   [vertical-line S-down-mouse-2]
	   [mode-line S-down-mouse-2])

	  ([S-down-mouse-3] [header-line S-down-mouse-3]
	   [left-fringe S-down-mouse-3]
	   [right-fringe S-down-mouse-3]
	   [vertical-line S-down-mouse-3]
	   [mode-line S-down-mouse-3])
	  
	  ([S-down-mouse-4] [header-line S-down-mouse-4]
	   [left-fringe S-down-mouse-4]
	   [right-fringe S-down-mouse-4]
	   [vertical-line S-down-mouse-4]
	   [mode-line S-down-mouse-4])

	  ([S-down-mouse-5] [header-line S-down-mouse-5]
	   [left-fringe S-down-mouse-5]
	   [right-fringe S-down-mouse-5]
	   [vertical-line S-down-mouse-5]
	   [mode-line S-down-mouse-5]))))
	  
  (hmouse-set-key-list
   release-cmd
   (nth (1- shifted-mouse-key-number)
	'(
	  ([S-drag-mouse-1] [S-mouse-1]
	   [S-double-mouse-1] [S-triple-mouse-1]
	   [header-line S-drag-mouse-1]
	   [header-line S-mouse-1]
	   [left-fringe S-drag-mouse-1]
	   [left-fringe S-mouse-1]
	   [right-fringe S-drag-mouse-1]
	   [right-fringe S-mouse-1]
	   [vertical-line S-drag-mouse-1]
	   [vertical-line S-mouse-1]
	   [mode-line S-drag-mouse-1]
	   [mode-line S-mouse-1])

	  ([S-drag-mouse-2] [S-mouse-2]
	   [S-double-mouse-2] [S-triple-mouse-2]
	   [header-line S-drag-mouse-2]
	   [header-line S-mouse-2]
	   [left-fringe S-drag-mouse-2]
	   [left-fringe S-mouse-2]
	   [right-fringe S-drag-mouse-2]
	   [right-fringe S-mouse-2]
	   [vertical-line S-drag-mouse-2]
	   [vertical-line S-mouse-2]
	   [mode-line S-drag-mouse-2]
	   [mode-line S-mouse-2])

	  ([S-drag-mouse-3] [S-mouse-3]
	   [S-double-mouse-3] [S-triple-mouse-3]
	   [header-line S-drag-mouse-3]
	   [header-line S-mouse-3]
	   [left-fringe S-drag-mouse-3]
	   [left-fringe S-mouse-3]
	   [right-fringe S-drag-mouse-3]
	   [right-fringe S-mouse-3]
	   [vertical-line S-drag-mouse-3]
	   [vertical-line S-mouse-3]
	   [mode-line S-drag-mouse-3]
	   [mode-line S-mouse-3])

	  ([S-drag-mouse-4] [S-mouse-4]
	   [S-double-mouse-4] [S-triple-mouse-4]
	   [header-line S-drag-mouse-4]
	   [header-line S-mouse-4]
	   [left-fringe S-drag-mouse-4]
	   [left-fringe S-mouse-4]
	   [right-fringe S-drag-mouse-4]
	   [right-fringe S-mouse-4]
	   [vertical-line S-drag-mouse-4]
	   [vertical-line S-mouse-4]
	   [mode-line S-drag-mouse-4]
	   [mode-line S-mouse-4])

	  ([S-drag-mouse-5] [S-mouse-5]
	   [S-double-mouse-5] [S-triple-mouse-5]
	   [header-line S-drag-mouse-5]
	   [header-line S-mouse-5]
	   [left-fringe S-drag-mouse-5]
	   [left-fringe S-mouse-5]
	   [right-fringe S-drag-mouse-5]
	   [right-fringe S-mouse-5]
	   [vertical-line S-drag-mouse-5]
	   [vertical-line S-mouse-5]
	   [mode-line S-drag-mouse-5]
	   [mode-line S-mouse-5])))))
	   
(defun hmouse-get-bindings (middle-flag)
  "Return the list of active bindings of mouse keys used by Hyperbole.
If MIDDLE-FLAG is non-nil, include the middle mouse key
binding as well.  These may be the bindings prior to initializing
Hyperbole or the Hyperbole bindings."
  ;; Do nothing when running in batch mode.
  (unless noninteractive
    (nconc
     (when middle-flag (hmouse-get-unshifted-bindings))
     ;; Get mouse bindings under Emacs, even if not under a window
     ;; system, since there can be frames on ttys and windowed
     ;; displays at the same time.
     (mapcar (lambda (key) (cons key (key-binding key)))
	     (if (eq window-system 'dps)
		 ;; NEXTSTEP offers only 2 shift-mouse buttons which we use
		 ;; as the Smart Keys.
		 '([S-down-mouse-1] [S-drag-mouse-1] [S-mouse-1]
		   [S-down-mouse-2] [S-drag-mouse-2] [S-mouse-2]
		   [S-double-mouse-1] [S-triple-mouse-1]
		   [S-double-mouse-2] [S-triple-mouse-2]
		   [header-line S-down-mouse-1] [header-line S-drag-mouse-1]
		   [header-line S-mouse-1]
		   [header-line S-down-mouse-2] [header-line S-drag-mouse-2]
		   [header-line S-mouse-2]
		   [left-fringe S-down-mouse-1] [left-fringe S-drag-mouse-1]
		   [left-fringe S-mouse-1]
		   [left-fringe S-down-mouse-2] [left-fringe S-drag-mouse-2]
		   [left-fringe S-mouse-2]
		   [right-fringe S-down-mouse-1] [right-fringe S-drag-mouse-1]
		   [right-fringe S-mouse-1]
		   [right-fringe S-down-mouse-2] [right-fringe S-drag-mouse-2]
		   [right-fringe S-mouse-2]
		   [vertical-line S-down-mouse-1] [vertical-line S-drag-mouse-1]
		   [vertical-line S-mouse-1]
		   [vertical-line S-down-mouse-2] [vertical-line S-drag-mouse-2]
		   [vertical-line S-mouse-2]
		   [mode-line S-down-mouse-1] [mode-line S-drag-mouse-1]
		   [mode-line S-mouse-1]
		   [mode-line S-down-mouse-2] [mode-line S-drag-mouse-2]
		   [mode-line S-mouse-2])
	       ;; X, macOS or MS Windows
	       '([S-down-mouse-2] [S-drag-mouse-2] [S-mouse-2]
		 [S-down-mouse-3] [S-drag-mouse-3] [S-mouse-3]
		 [S-double-mouse-2] [S-triple-mouse-2]
		 [S-double-mouse-3] [S-triple-mouse-3]
		 [header-line S-down-mouse-2] [header-line S-drag-mouse-2]
		 [header-line S-mouse-2]
		 [header-line S-down-mouse-3] [header-line S-drag-mouse-3]
		 [header-line S-mouse-3]
		 [left-fringe S-down-mouse-2] [left-fringe S-drag-mouse-2]
		 [left-fringe S-mouse-2]
		 [left-fringe S-down-mouse-3] [left-fringe S-drag-mouse-3]
		 [left-fringe S-mouse-3]
		 [right-fringe S-down-mouse-2] [right-fringe S-drag-mouse-2]
		 [right-fringe S-mouse-2]
		 [right-fringe S-down-mouse-3] [right-fringe S-drag-mouse-3]
		 [right-fringe S-mouse-3]
		 [vertical-line S-down-mouse-2] [vertical-line S-drag-mouse-2]
		 [vertical-line S-mouse-2]
		 [vertical-line S-down-mouse-3] [vertical-line S-drag-mouse-3]
		 [vertical-line S-mouse-3]
		 [mode-line S-down-mouse-2] [mode-line S-drag-mouse-2]
		 [mode-line S-mouse-2]
		 [mode-line S-down-mouse-3] [mode-line S-drag-mouse-3]
		 [mode-line S-mouse-3])))
     (nconc
      (mapcar (lambda (key)
		(cons key (key-binding key)))
	      '([(shift button2)] [(shift button2up)]
		[(shift button3)] [(shift button3up)]))
      (when (boundp 'mode-line-map)
	(mapcar (lambda (key)
		  (cons key (lookup-key mode-line-map key)))
		'([(shift button3)] [(shift button3up)])))))))

(defun hmouse-get-unshifted-bindings ()
  "Return the list of middle mouse key bindings prior to their use as Smart Keys."
  ;; Do nothing when running in batch mode.
  (mapc (lambda (key) (cons key (key-binding key)))
	(unless (eq window-system 'dps)
	  ;; X, macOS or MS Windows
	  '([down-mouse-2] [drag-mouse-2] [mouse-2]
	    [down-mouse-3] [drag-mouse-3] [mouse-3]
	    [double-mouse-2] [triple-mouse-2]
	    [double-mouse-3] [triple-mouse-3]
	    [header-line down-mouse-2] [header-line drag-mouse-2]
	    [header-line mouse-2]
	    [left-fringe down-mouse-2] [left-fringe drag-mouse-2]
	    [left-fringe mouse-2]
	    [right-fringe down-mouse-2] [right-fringe drag-mouse-2]
	    [right-fringe mouse-2]
	    [vertical-line down-mouse-2] [vertical-line drag-mouse-2]
	    [vertical-line mouse-2]
	    [left-fringe down-mouse-3] [left-fringe drag-mouse-3]
	    [left-fringe mouse-3]
	    [right-fringe down-mouse-3] [right-fringe drag-mouse-3]
	    [right-fringe mouse-3]
	    [vertical-line down-mouse-3] [vertical-line drag-mouse-3]
	    [vertical-line mouse-3]
	    [mode-line down-mouse-2] [mode-line drag-mouse-2]
	    [mode-line mouse-2]
	    [mode-line down-mouse-3] [mode-line drag-mouse-3]
	    [mode-line mouse-3])))
  (nconc
   (mapcar (lambda (key)
	     (cons key (key-binding key)))
	   '([button2] [button2up]
	     [button3] [button3up]))
   (when (boundp 'mode-line-map)
     (mapcar (function
	      (lambda (key)
		(cons key (lookup-key mode-line-map key))))
	     '([button3] [button3up])))))

;; Based on a function from Emacs mouse.el.
(defun hmouse-posn-set-point (position)
  "Move point to POSITION, an event posn.
Select the corresponding window as well."
  (if (framep (posn-window position))
      (progn (if (not (windowp (frame-selected-window (posn-window position))))
		 (error "Position not in text area of window"))
	     (select-window (frame-selected-window (posn-window position))))
    (unless (windowp (posn-window position))
      (error "Position not in text area of window"))
    (select-window (posn-window position)))
  (when (numberp (posn-point position))
    (goto-char (posn-point position))))

;; Based on mouse-drag-region from Emacs mouse.el.
(defun hmouse-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
If not the start of a region drag-and-drop, then depress the Action Key.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.
In Transient Mark mode, the highlighting remains as long as the mark
remains active.  Otherwise, it remains until the next input event.

When the region already exists and `mouse-drag-and-drop-region'
is non-nil, this moves the entire region of text to where mouse
is dragged over to."
  (interactive "e")
  (if (and mouse-drag-and-drop-region
           (not (member 'triple (event-modifiers start-event)))
           (equal (mouse-posn-property (event-start start-event) 'face) 'region))
      (mouse-drag-and-drop-region start-event)
    ;; Give temporary modes such as isearch a chance to turn off.
    (run-hooks 'mouse-leave-buffer-hook)
    (action-key-depress start-event)
    (mouse-drag-track start-event)))

;; Based on a function from Emacs mouse.el.
(defun hmouse-move-point-emacs (event &optional promote-to-region)
  "Move point to the position clicked on with the mouse.
This should be bound to a mouse click event type.
If PROMOTE-TO-REGION is non-nil and EVENT is a multiple-click,
select the corresponding element around point, with the resulting position of
point determined by `mouse-select-region-move-to-beginning'."
  (interactive "e\np")
  (let ((start-w-or-f (posn-window (event-start event)))
	(end-w-or-f   (posn-window (event-end event))))
    (when (framep start-w-or-f)
      (with-selected-frame start-w-or-f (setq start-w-or-f (selected-window))))
    (when (framep end-w-or-f)
      (with-selected-frame end-w-or-f (setq end-w-or-f (selected-window))))
    (if (and (window-valid-p start-w-or-f)
	     (window-minibuffer-p start-w-or-f)
	     (not (minibuffer-window-active-p start-w-or-f)))
	;; Select the ending frame only, not the window pressed within.
	(select-frame (window-frame end-w-or-f))
      ;; Give temporary modes such as isearch a chance to turn off.
      (run-hooks 'mouse-leave-buffer-hook)
      (if (and promote-to-region (> (event-click-count event) 1))
	  (progn (mouse-set-region event)
		 (when (and (boundp 'mouse-select-region-move-to-beginning)
			    mouse-select-region-move-to-beginning)
		   (when (> (posn-point (event-start event)) (region-beginning))
		     (exchange-point-and-mark))))
	;; Use event-end in case called from mouse-drag-region.
	;; If EVENT is a click, event-end and event-start give same value.
	(if (and (window-valid-p end-w-or-f)
		 (window-minibuffer-p end-w-or-f)
		 (not (minibuffer-window-active-p end-w-or-f)))
	    ;; Select the ending frame only, not the window pressed within.
	    (select-frame (window-frame end-w-or-f))
	  (condition-case ()
	      (hmouse-posn-set-point (event-end event))
	    (error (when (window-valid-p end-w-or-f)
		     (select-frame (window-frame end-w-or-f))))))))))

(defun hmouse-move-point-eterm (arg-list)
  (apply 'mouse-move-point arg-list))

(defun hmouse-set-key-list (binding key-list)
  (mapc (lambda (key) (hkey-set-key key binding)) key-list)
  nil)

(defun hmouse-shifted-setup (middle-flag)
  "Call `hmouse-install' instead of this and see its documentation."
  (interactive)
  ;; Do nothing when running in batch mode.
  (unless noninteractive
    (or hmouse-bindings-flag hmouse-previous-bindings
	(setq hmouse-previous-bindings (hmouse-get-bindings middle-flag)))
    (when middle-flag (hmouse-unshifted-setup middle-flag))
    ;; Ensure Gillespie's Info mouse support is off since
    ;; Hyperbole handles that.
    (when (boundp 'Info-mouse-support) (setq Info-mouse-support nil))
    ;;
    ;; This event setting from the "kmacro.el" library can
    ;; trigger an autoload that binds [S-mouse-3] to 'kmacro-end-call-mouse,
    ;; interfering with the same key used for the Assist Key, so disable
    ;; this.
    (setq kmacro-call-mouse-event nil)
    ;;
    (setq hmouse-set-point-command #'hmouse-move-point-emacs)
    (if (eq window-system 'dps)
	;; NEXTSTEP offers only 2 shift-mouse buttons which we use as the Smart Keys.
	(progn
	  (hmouse-bind-shifted-key-emacs 1 #'action-key-depress-emacs #'action-mouse-key-emacs)
	  (hmouse-bind-shifted-key-emacs 2 #'assist-key-depress-emacs #'assist-mouse-key-emacs))
      ;; X, macOS or MS Windows
      (hmouse-bind-shifted-key-emacs 2 #'action-key-depress-emacs #'action-mouse-key-emacs)
      (hmouse-bind-shifted-key-emacs 3 #'assist-key-depress-emacs #'assist-mouse-key-emacs)
      (with-eval-after-load "company"
	(define-key company-active-map [S-down-mouse-2] 'ignore)
	(define-key company-active-map [S-mouse-2] 'smart-company-to-definition)
	(define-key company-active-map [S-down-mouse-3] 'ignore)
	(define-key company-active-map [S-mouse-3] 'smart-company-help)))
    (setq hmouse-bindings (hmouse-get-bindings middle-flag)
	  hmouse-bindings-flag t)))

(defun hmouse-unshifted-setup (&optional middle-key-only-flag)
  "Bind the middle and right mouse keys as Action and Assist Keys, respectively.
With optional MIDDLE-KEY-ONLY-FLAG non-nil, bind only the middle mouse key."
  (interactive)
  ;; Globally Emacs uses key-translation-map to link mouse-1 to
  ;; do whatever mouse-2 does when 'mouse-1-click-follows-link' is
  ;; non-nil (the default) and point is on an Emacs button.
  ;; Since Hyperbole rebinds mouse-2 here, this does not work by
  ;; default.  We fix that here by overriding the setting of
  ;; down-mouse-1 to include the down Action Key command plus the
  ;; original mouse-1 command.  Then when mouse-1 is released on an
  ;; Emacs button, the Action Key command is run which handles
  ;; activating the button/following a link.
  (global-set-key [down-mouse-1] 'hmouse-drag-region)
  ;;
  ;; Unbind Emacs push-button mouse keys since Hyperbole handles them.
  (define-key button-map [mouse-2] nil)
  (define-key button-map [mode-line mouse-2] nil)
  (define-key button-map [header-line mouse-2] nil)
  ;; Remove push-button help echo string for mouse-2 key.
  (put 'default-button 'help-echo nil)
  ;;
  ;; Remove mouse-2 binding In Info-mode since Hyperbole handles it.
  (var:add-and-run-hook 'Info-mode-hook (lambda () (define-key Info-mode-map [mouse-2] nil)))
  ;;
  (unless (eq window-system 'dps)
    ;; X, macOS or MS Windows
    (hmouse-bind-key-emacs 2 #'action-key-depress-emacs #'action-mouse-key-emacs)
    (unless middle-key-only-flag
      (hmouse-bind-key-emacs 3 #'assist-key-depress-emacs #'assist-mouse-key-emacs))
    `(with-eval-after-load "company"
       (define-key company-active-map [down-mouse-2] 'ignore)
       (define-key company-active-map [mouse-2] 'smart-company-to-definition)
       (unless ,middle-key-only-flag
	 (define-key company-active-map [down-mouse-3] 'ignore)
	 (define-key company-active-map [mouse-3] 'smart-company-help)))))

(provide 'hmouse-sh)

;;; hmouse-sh.el ends here
