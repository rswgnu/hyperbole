;;; hmouse-sh.el --- System-dependent Smart Mouse Key bindings.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     3-Sep-91 at 21:40:58
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
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
;;; Public functions
;;; ************************************************************************

;; Other mouse event location prefixes to possibly handle:
;;   vertical-scroll-bar
;;   horizontal-scroll-bar
;;   right-divider
;;   bottom-divider

(defun hmouse-bind-key-emacs (mouse-key-number depress-cmd release-cmd)
  "Ensure MOUSE-KEY-NUMBER (1-5), e.g. 1 for [mouse-1], is globally bound to DEPRESS-CMD and RELEASE-CMD (includes depresses and drags).
Use nil as cmd values to unbind a key.  Works under GNU Emacs only."
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
	   [mode-line down-mouse-5])
	  )))
	   
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
	   [mode-line mouse-5])
	  ))))

(defun hmouse-bind-shifted-key-emacs (shifted-mouse-key-number depress-cmd release-cmd)
  "Ensure shifted MOUSE-KEY-NUMBER (1-5), e.g. 1 for [Smouse-1], is globally bound to DEPRESS-CMD and RELEASE-CMD (includes depresses and drags).
Use nil as CMD value to unbind the key.  Works under GNU Emacs only."
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
	   [mode-line S-down-mouse-5])
	  )))
	  
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
	   [mode-line S-mouse-5])
	  ))))
	   
(defun hmouse-get-bindings (hmouse-middle-flag)
  "Return the list of current bindings of mouse keys used by Hyperbole.
If HMOUSE-MIDDLE-FLAG is non-nil, includes the middle mouse key binding as well.
These may be the bindings prior to initializing Hyperbole or the Hyperbole bindings."
  ;; Do nothing when running in batch mode.
  (unless noninteractive
    (nconc
     (if hmouse-middle-flag (hmouse-get-unshifted-bindings))
     ;; Get mouse bindings under Emacs or XEmacs, even if not under a
     ;; window system since they can have frames on ttys and windowed
     ;; displays at the same time.
     (if (not (featurep 'xemacs))
	 (mapcar (lambda (key) (cons key (global-key-binding key)))
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
		       [mode-line S-mouse-2]
		       )
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
		     [mode-line S-mouse-3]
		     )))
       (nconc
	(mapcar (lambda (key)
		  (cons key (global-key-binding key)))
		'([(shift button2)] [(shift button2up)]
		  [(shift button3)] [(shift button3up)]))
	(if (boundp 'mode-line-map)
	    (mapcar (lambda (key)
		      (cons key (lookup-key mode-line-map key)))
		    '([(shift button3)] [(shift button3up)]))))))))

(defun hmouse-get-unshifted-bindings ()
  "Return the list of middle mouse key bindings prior to their use as Smart Keys."
  ;; Do nothing when running in batch mode.
  (if (not (featurep 'xemacs))
      (mapcar (lambda (key) (cons key (global-key-binding key)))
	      (if (not (eq window-system 'dps))
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
		    [mode-line mouse-3]
		    )))
    (nconc
     (mapcar (lambda (key)
	       (cons key (global-key-binding key)))
	     '([button2] [button2up]
	       [button3] [button3up]
	       ))
     (if (boundp 'mode-line-map)
	 (mapcar (function
		  (lambda (key)
		    (cons key (lookup-key mode-line-map key))))
		 '([button3] [button3up])))
     )))

;; Based on a function from Emacs mouse.el.
(defun hmouse-posn-set-point (position)
  "Move point to POSITION, an event posn.
Select the corresponding window as well."
  (if (framep (posn-window position))
      (progn (if (not (windowp (frame-selected-window (posn-window position))))
		 (error "Position not in text area of window"))
	     (select-window (frame-selected-window (posn-window position))))
    (if (not (windowp (posn-window position)))
	(error "Position not in text area of window"))
    (select-window (posn-window position)))
  (if (numberp (posn-point position))
      (goto-char (posn-point position))))

;; Based on a function from Emacs mouse.el.
(defun hmouse-move-point-emacs (event &optional promote-to-region)
  "Move point to the position clicked on with the mouse.
This should be bound to a mouse click event type.
If PROMOTE-TO-REGION is non-nil and event is a multiple-click,
select the corresponding element around point, with the resulting position of
point determined by `mouse-select-region-move-to-beginning'."
  (interactive "e\np")
  (let ((start-w-or-f (posn-window (event-start event)))
	(end-w-or-f   (posn-window (event-end event))))
    (if (framep start-w-or-f)
	(with-selected-frame start-w-or-f (setq start-w-or-f (selected-window))))
    (if (framep end-w-or-f)
	(with-selected-frame end-w-or-f (setq end-w-or-f (selected-window))))
    (if (and (window-minibuffer-p start-w-or-f)
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
	(if (and (window-minibuffer-p end-w-or-f)
		 (not (minibuffer-window-active-p end-w-or-f)))
	    ;; Select the ending frame only, not the window pressed within.
	    (select-frame (window-frame end-w-or-f))
	  (condition-case ()
	      (hmouse-posn-set-point (event-end event))
	    (error (select-frame (window-frame end-w-or-f)))))))))

(defun hmouse-move-point-eterm (arg-list)
  (apply 'mouse-move-point arg-list))

(defun hmouse-move-point-xemacs ()
  (condition-case ()
      (mouse-set-point current-mouse-event)
    ;; Catch "not in a window" errors, e.g. on modeline
    (error nil)))

(defun hmouse-set-key-list (binding key-list)
  (mapc (lambda (key) (hkey-global-set-key key binding)) key-list)
  nil)

(defun hmouse-shifted-setup (hmouse-middle-flag)
  "Call `hmouse-install' instead of this and see its documentation."
  (interactive)
  ;; Do nothing when running in batch mode.
  (unless noninteractive
    (or hmouse-bindings-flag hmouse-previous-bindings
	(setq hmouse-previous-bindings (hmouse-get-bindings hmouse-middle-flag)))
    (if hmouse-middle-flag (hmouse-unshifted-setup hmouse-middle-flag))
    ;; Ensure Gillespie's Info mouse support is off since
    ;; Hyperbole handles that.
    (if (boundp 'Info-mouse-support) (setq Info-mouse-support nil))
    ;;
    ;; This event setting from the "kmacro.el" library can
    ;; trigger an autoload that binds [S-mouse-3] to 'kmacro-end-call-mouse,
    ;; interfering with the same key used for the Assist Key, so disable
    ;; this.
    (setq kmacro-call-mouse-event nil)
    ;;
    (cond
     ;; GNU Emacs
     ((not (featurep 'xemacs))
      (setq hmouse-set-point-command 'hmouse-move-point-emacs)
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
	  (define-key company-active-map [S-mouse-3] 'smart-company-help))))
     ;;
     ;; XEmacs
     ((featurep 'xemacs)
      ;; Set mouse bindings under XEmacs, even if not under a window
      ;; system since it can have frames on ttys and windowed displays at
      ;; the same time.
      (setq hmouse-set-point-command 'hmouse-move-point-xemacs)
      (global-set-key '(shift button2)     'action-key-depress)
      (global-set-key '(shift button2up)   'action-mouse-key)
      (if (fboundp 'infodock-set-mouse-bindings)
	  (infodock-set-mouse-bindings)
	(if (boundp 'mode-line-map)
	    (progn (define-key mode-line-map '(shift button3)
		     'assist-key-depress)
		   (define-key mode-line-map '(shift button3up)
		     'assist-mouse-key)
		   ))
	(global-set-key '(shift button3)     'assist-key-depress)
	(global-set-key '(shift button3up)   'assist-mouse-key))))
    (setq hmouse-bindings (hmouse-get-bindings hmouse-middle-flag)
	  hmouse-bindings-flag t)))

(defun hmouse-unshifted-setup (&optional middle-key-only-flag)
  "Binds the middle mouse key as the Action Key and the right mouse key as the Assist Key.
With optional MIDDLE-KEY-ONLY-FLAG non-nil, binds only the middle mouse key."
  (interactive)
  (cond	;; GNU Emacs
   ((not (featurep 'xemacs))
    ;; Unbind Emacs push-button mouse keys since Hyperbole handles them.
    (define-key button-map [mouse-2] nil)
    (define-key button-map [mode-line mouse-2] nil)
    (define-key button-map [header-line mouse-2] nil)
    ;; Remove push-button help echo string for mouse-2 key.
    (put 'default-button 'help-echo nil)
    ;;
    ;; In Info-mode, Emacs uses key-translation-map to link mouse-1 to
    ;; do whatever mouse-2 does but because Hyperbole uses both down
    ;; and up bindings on mouse2, this does not work.  So we rebind
    ;; mouse-1 in Info mode to be an actual Action Mouse Key (which
    ;; makes it follow Info links/cross-references properly, doing a
    ;; superset of what it did before).
    (var:add-and-run-hook 'Info-mode-hook
 			  (lambda ()
			    (define-key Info-mode-map [down-mouse-1] 'action-key-depress-emacs)
			    (define-key Info-mode-map [mouse-1] 'action-mouse-key-emacs)
			    (define-key Info-mode-map [double-down-mouse-1] 'action-key-depress-emacs)
			    (define-key Info-mode-map [double-mouse-1] 'action-mouse-key-emacs)
			    (define-key Info-mode-map [mouse-2] nil)))
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
   ;;
   ;; XEmacs
   ((featurep 'xemacs)
    ;; Set mouse bindings under XEmacs, even if not under a window
    ;; system since it can have frames on ttys and windowed displays at
    ;; the same time.
    ;;
    ;; Get rid of Info-mode button 2 and possibly button 3 bindings since Hyperbole
    ;; handles things in Info.
    (var:add-and-run-hook 'Info-mode-hook
			  (lambda () (define-key Info-mode-map 'button2 nil)))
    ;;
    (global-set-key 'button2     'action-key-depress)
    (global-set-key 'button2up   'action-mouse-key)
    (unless middle-key-only-flag
      (if (and (boundp 'Info-mode-map) (keymapp Info-mode-map))
          (funcall (lambda () (define-key Info-mode-map 'button3 nil)))
	(add-hook 'Info-mode-hook unbind-but3))
      (when (boundp 'mode-line-map)
        (define-key mode-line-map 'button3   'assist-key-depress)
    	(define-key mode-line-map 'button3up 'assist-mouse-key))
      (global-set-key 'button3     'assist-key-depress)
      (global-set-key 'button3up   'assist-mouse-key)))))

(provide 'hmouse-sh)

;;; hmouse-sh.el ends here
