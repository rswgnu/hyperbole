;;; hmouse-key.el --- Setup Smart Key mouse bindings  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    30-May-94 at 00:11:57
;; Last-Mod:     25-Jul-22 at 23:47:01 by Mats Lidell
;;
;; Copyright (C) 1994-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;   Supports macOS, X, NEXTSTEP and MS Windows window systems.
;;
;;   `hmouse-install' binds the Action and Assist Mouse Keys
;;   to shifted mouse buttons and optionally binds the Action Mouse Key
;;   to the middle mouse button.
;;
;;   `hmouse-toggle-bindings' switches between the Hyperbole mouse
;;   bindings and previous mouse key bindings any time after
;;   `hmouse-install' has been called.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (mapc #'require '(hsettings hmouse-drv hmouse-sh)))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-check-action-key ()
  "After Action Mouse Key use, ensure both depress and release events are bound.
Return t iff the key is properly bound, else nil."
  (and (or (and (eventp action-key-depress-args) (eventp action-key-release-args))
	   (not (or action-key-depress-args action-key-release-args)))
       (where-is-internal 'action-key-depress-emacs hyperbole-mode-map t)
       (where-is-internal 'action-mouse-key-emacs hyperbole-mode-map t)))

(defun hmouse-check-assist-key ()
  "After Assist Mouse Key use, ensure both depress and release events are bound.
Return t iff the key is properly bound, else nil."
  (and (or (and (eventp assist-key-depress-args) (eventp assist-key-release-args))
	   (not (or assist-key-depress-args assist-key-release-args)))
       (where-is-internal 'assist-key-depress-emacs hyperbole-mode-map t)
       (where-is-internal 'assist-mouse-key-emacs hyperbole-mode-map t)))

(defun hmouse-set-bindings (key-binding-list)
  "Set mouse keys used as Smart Keys to bindings in KEY-BINDING-LIST.
KEY-BINDING-LIST is the value of either `hmouse-previous-bindings'
\(mouse bindings prior to Smart Key setup) or `hmouse-bindings' (mouse
bindings after Smart Key setup."
  (cond
    ;; Do nothing when running in batch mode.
    (noninteractive)
    ;;
    ;; GNU Emacs or InfoDock
    (t
     (mapcar
       (lambda (key-and-binding)
	 (hkey-set-key (car key-and-binding) (cdr key-and-binding)))
       key-binding-list))))

(defun hmouse-install (&optional arg)
  "Initialize Hyperbole mouse buttons.
Bind the two rightmost shifted mouse keys to the Action and
Assist Keys.  With optional prefix ARG or under InfoDock, also
bind the unshifted middle mouse key to the Action Key.

The standard Hyperbole configuration is Action Key = shift-middle mouse key;
Assist Key = shift-right mouse key."
  (interactive "P")
  (unless hmouse-middle-flag
    (setq hmouse-middle-flag (or arg (and (boundp 'infodock-version)
					  infodock-version))))
  ;; Replace any original mouse bindings before installing Hyperbole bindings and
  ;; then force reinitialization of hmouse-previous-bindings.
  (if (and hmouse-bindings-flag hmouse-previous-bindings)
      (hmouse-set-bindings hmouse-previous-bindings))
  (setq hmouse-bindings-flag nil
	hmouse-previous-bindings nil)
  ;; This function does the actual binding of the Hyperbole mouse keys
  ;; and the setup of the mouse-set-point command via `hmouse-set-point-command'.
  (hmouse-shifted-setup hmouse-middle-flag)
  (if (called-interactively-p 'interactive)
      ;; Assume emacs has support for 3 mouse keys.
      (message "%s the Action Mouse Key; {Shift-Mouse-3} invokes the Assist Mouse Key."
	       (if hmouse-middle-flag "{Mouse-2} and {Shift-Mouse-2} invoke"
		 "{Shift-Mouse-2} invokes"))))

(defun hmouse-add-unshifted-smart-keys ()
  "Bind mouse-2 to the Action Key and mouse-3 to the Assist Key."
  (interactive)
  (require 'hyperbole)
  (hmouse-unshifted-setup))
  
(defun hmouse-toggle-bindings ()
  "Toggle between Smart Mouse Key settings and their prior bindings.
Under InfoDock, the first invocation of this command will make the middle
mouse key the Paste Key instead of the Action Key."
  (interactive)
  (let ((key-binding-list (if hmouse-bindings-flag
			      hmouse-previous-bindings
			    hmouse-bindings))
	(other-bindings-var (if hmouse-bindings-flag
				'hmouse-bindings
			      'hmouse-previous-bindings)))
    (if key-binding-list
	(progn
	  (set other-bindings-var (hmouse-get-bindings nil))
	  (hmouse-set-bindings key-binding-list)
	  (setq hmouse-bindings-flag (not hmouse-bindings-flag))
	  (if (called-interactively-p 'interactive)
	      (message "%s mouse bindings are now in use."
		       (if hmouse-bindings-flag "Hyperbole" "Non-Hyperbole"))))
      (error "(hmouse-toggle-bindings): `%s' is empty"
	     (if hmouse-bindings-flag 'hmouse-previous-bindings 'hmouse-bindings)))))

;; Define function to reload Smart Key bindings and actions after a source code change.
(defun hmouse-update-smart-keys ()
  "Reload the contexts and actions associated with the Smart Keys.
Use after any programmatic change is made."
  (interactive)
  (hkey-initialize)
  (makunbound 'hkey-alist)
  (makunbound 'hmouse-alist)
  (let ((load-prefer-newer t))
    ;; This also reloads "hui-window" where mouse-only actions are defined.
    (mapc #'load '("hui-mouse" "hibtypes" "hactypes")))
  (message "Hyperbole Smart Key and Smart Mouse Key actions have been updated."))


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hmouse-bindings nil
  "List of (key . binding) pairs for Hyperbole mouse keys.")

(defvar hmouse-bindings-flag nil
  "True if Hyperbole mouse bindings are in use, else nil.")

(defvar hmouse-previous-bindings nil
  "List of prior (key . binding) pairs for mouse keys rebound by Hyperbole.")

(provide 'hmouse-key)

;;; hmouse-key.el ends here
