;;; hywconfig.el --- Save ring of window configurations  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Mar-89
;; Last-Mod:      6-Aug-22 at 23:12:26 by Mats Lidell
;;
;; Copyright (C) 1989-2022  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This library provides two unrelated ways of managing window
;;   configurations, (the set of windows and associated buffers within a
;;   specific frame).  The first way associates a name with each
;;   stored window configuration.  The name can then be used to
;;   retrieve the window configuration later.  The following functions
;;   provide this behavior:
;;
;;      hywconfig-add-by-name
;;      hywconfig-delete-by-name
;;      hywconfig-restore-by-name
;;
;;   The second way utilizes a ring structure, just like the Emacs
;;   kill ring, except the elements stored are window configurations
;;   instead of textual regions.  The following functions support
;;   storage and sequential retrieval of window configurations:
;;
;;      hywconfig-ring-save
;;      hywconfig-yank-pop
;;      hywconfig-delete-pop
;;
;;   None of this information is stored between Emacs sessions, so your
;;   window configurations will last only through a single session of use.
;;
;;   Based in part on kill-ring code from simple.el.

;;; Code:
;;; ************************************************************************
;;; Recommended key bindings
;;; ************************************************************************

;;; Set up in "hyperbole.el".

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hargs)
(require 'ring)
(require 'set)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(define-obsolete-variable-alias 'wconfig-ring-max 'hywconfig-ring-max "06.00")
(defcustom hywconfig-ring-max 10
  "*Max length of Hyperbole window config ring before oldest elements are deleted."
  :type `(integer
          :match ,(lambda (_widget value) (and (integerp value) (> value 0))))
  :group 'hyperbole-screen)


;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; Handling of name associations with each stored window configuration.
;;;###autoload
(defun hywconfig-add-by-name (name)
  "Save the current window configuration under the string NAME.
When called interactively and a window configuration already exists under
NAME, confirms whether or not to replace it."
  (interactive "sName for current window configuration: ")
  (or (stringp name)
      (error "(hywconfig-add-by-name): `name' argument is not a string: %s" name))
  (let ((set:equal-op  (lambda (key elt) (equal key (car elt))))
	(wconfig-names (hywconfig-get-names)))
    (if (or (not (called-interactively-p 'interactive))
	    (not (set:member name wconfig-names))
	    (y-or-n-p
	     (format "Replace existing `%s' window configuration? " name)))
	(progn (hywconfig-set-names (set:replace name (current-window-configuration)
						 wconfig-names))
	       (if (called-interactively-p 'interactive)
		   (message
		    (substitute-command-keys
		     (format "Window configuration `%s' saved.  Use {\\[hywconfig-restore-by-name]} to restore." name))))))))

;;;###autoload
(defun hywconfig-delete-by-name (name)
  "Deletes frame-specific window configuration saved under NAME."
  (interactive (list (hargs:read-match "Delete window configuration named: "
				       (hywconfig-get-names) nil t)))
  (cond ((null name)
	 (message "There is no named window configuration to delete."))
	((not (stringp name))
	 (error "(hywconfig-delete-by-name): `name' argument is not a string: %s" name))
	(t (let ((set:equal-op (lambda (key elt) (equal key (car elt)))))
	     (hywconfig-set-names (set:remove name (hywconfig-get-names)))
	     (if (called-interactively-p 'interactive)
		 (message "Window configuration `%s' has been deleted." name))))))

;;;###autoload
(defun hywconfig-restore-by-name (name)
  "Restore frame-specific window configuration saved under NAME."
  (interactive (list (hargs:read-match "Restore window configuration named: "
				       (hywconfig-get-names) nil t)))
  (cond ((null name)
	 (message "There is no named window configuration to restore."))
	((not (stringp name))
	 (error "(hywconfig-restore-by-name): `name' argument is not a string: %s" name))
	(t (let ((wconfig (set:get name (hywconfig-get-names))))
	     (if wconfig
		 (progn (hywconfig-set-window-configuration wconfig)
			(if (called-interactively-p 'interactive)
			    (message "Window configuration `%s' is now active." name)))
	       (error "(hywconfig-restore-by-name): No window configuration for this frame named `%s'" name))))))

;;; Window configuration ring management (like text kill ring).
;;;###autoload
(defun hywconfig-delete-pop ()
  "Replace the current frame's window configuration with the most recently saved.
Then deletes this new configuration from the ring."
  (interactive)
  (let ((ring (hywconfig-get-ring)))
    (if (ring-empty-p ring)
	(error "(hywconfig-delete-pop): Window configuration save ring is empty")
      (if (ring-empty-p ring)
	  (message "Window configuration save ring is now empty.")
	(hywconfig-set-window-configuration (ring-ref ring 0))
	(ring-remove ring 0)))))

;;;###autoload
(defun hywconfig-ring-empty-p ()
  "Return t if the wconfig ring for the current frame is empty; nil otherwise."
  (ring-empty-p (hywconfig-get-ring)))

;;;###autoload
(defun hywconfig-ring-save ()
  "Save the current frame's window configuration onto the save ring.
Use {\\[hywconfig-yank-pop]} to restore it at a later time."
  (interactive)
  (ring-insert (hywconfig-get-ring) (current-window-configuration))
  (if (called-interactively-p 'interactive)
      (message
       (substitute-command-keys
	"Window configuration saved.  Use {\\[hywconfig-yank-pop]} to restore."))))

;;;###autoload
(defun hywconfig-yank-pop (n)
  "Replace current frame's window config with prefix arg Nth prior one in ring.
Interactively, default value of N = 1, means the last saved window
configuration is displayed.

The sequence of window configurations wraps around, so that after the
oldest one comes the newest one."
  (interactive "p")
  (let ((ring (hywconfig-get-ring))
	prev)
    (if (ring-empty-p ring)
	(error "(hywconfig-yank-pop): Window configuration save ring is empty")
      (setq prev (ring-remove ring (- 1 n)))
      (ring-insert-at-beginning ring prev)
      (hywconfig-set-window-configuration (ring-ref ring 0)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hywconfig-get-names ()
  "Return the current frame's list of named window configurations."
  (let* ((frame (selected-frame))
	 (names (frame-parameter frame 'hywconfig-names)))
    (if (not names)
	(set-frame-parameter frame 'hywconfig-names (setq names (set:create))))
    names))

(defun hywconfig-set-names (names)
  "Set the current frame's list of named window configurations."
  (set-frame-parameter (selected-frame) 'hywconfig-names names))

(defun hywconfig-get-ring ()
  "Return the current frame's window configuration ring."
  (let* ((frame (selected-frame))
	 (ring (frame-parameter frame 'hywconfig-ring)))
    (if (not ring)
	(set-frame-parameter frame 'hywconfig-ring (setq ring (make-ring hywconfig-ring-max))))
    ring))

(defun hywconfig-set-window-configuration (wconfig)
  (if (window-configuration-p wconfig)
      (condition-case nil
	  (progn (set-window-configuration wconfig) t)
	(error (message "(HyWconfig): Invalid window configuration, `%s'" wconfig)
	       (beep)
	       (sit-for 2)
	       nil))))

(run-hooks 'hywconfig-load-hook)

(provide 'hywconfig)

;;; hywconfig.el ends here
