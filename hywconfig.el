;;; hywconfig.el --- Save ring of window configurations  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Mar-89
;; Last-Mod:     20-Jan-24 at 20:20:58 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1989-2024  Free Software Foundation, Inc.
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

(require 'hargs) ;; this requires 'hypb
(require 'ring)

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
NAME, confirm whether or not to replace it.

Return t if name is added or replaced, else nil."
  (interactive "sName for current window configuration: ")
  (unless (stringp name)
    (error "(hywconfig-add-by-name): `name' argument is not a string: %s" name))
  (when (or (not (called-interactively-p 'interactive))
	    (not (member name (hywconfig-named-get-names)))
	    (y-or-n-p
	     (format "Replace existing `%s' window configuration? " name)))
    (hywconfig-named-put name (current-window-configuration))
    (when (called-interactively-p 'interactive)
      (message
       (substitute-command-keys
	(format "Window configuration `%s' saved.  Use {\\[hywconfig-restore-by-name]} to restore." name))))
    t))

;;;###autoload
(defun hywconfig-delete-by-name (name)
  "Delete frame-specific window configuration saved with NAME.

Return t if name exists and is deleted, else nil."
  (interactive (list (hargs:read-match "Delete window configuration named: "
				       (hywconfig-named-get-names) nil t)))
  (cond ((null name)
	 (message "There is no named window configuration to delete."))
	((not (stringp name))
	 (error "(hywconfig-delete-by-name): `name' argument is not a string: %s" name))
	(t (let ((removed-flag (hywconfig-named-remove name)))
	     (when (and removed-flag (called-interactively-p 'interactive))
	       (message "Window configuration `%s' has been deleted." name))
	     removed-flag))))

;;;###autoload
(defun hywconfig-restore-by-name (name)
  "Restore frame-specific window configuration saved with NAME.

Return t if name exists and is restored, else nil."
  (interactive (list (hargs:read-match "Restore window configuration named: "
				       (hywconfig-named-get-names) nil t)))
  (cond ((null name)
	 (message "There is no named window configuration to restore."))
	((not (stringp name))
	 (error "(hywconfig-restore-by-name): `name' argument is not a string: %s" name))
	(t (let ((wconfig (hywconfig-named-get name)))
	     (if wconfig
		 (progn (hywconfig-set-window-configuration wconfig)
			(when (called-interactively-p 'interactive)
			  (message "Window configuration `%s' is now active." name))
			t)
	       (error "(hywconfig-restore-by-name): No window configuration for this frame named `%s'" name))))))

;;; Window configuration ring management (like text kill ring).
;;;###autoload
(defun hywconfig-delete-pop ()
  "Replace the selected frame's window configuration with the most recently saved.
Then delete this new configuration from the ring."
  (interactive)
  (let ((ring (hywconfig-get-ring)))
    (if (ring-empty-p ring)
	(error "(hywconfig-delete-pop): Window configuration save ring is empty")
      (if (ring-empty-p ring)
	  (message "Window configuration save ring is now empty.")
	(hywconfig-set-window-configuration (ring-ref ring 0))
	(ring-remove ring 0)))))

;;;###autoload
(defun hywconfig-delete-pop-continue ()
  "Replace selected frame's window configuration with the most recently saved.
Then delete this new configuration from the ring.  If the hywconfig
ring is not empty, then stay in the hywconfig menu."
  (interactive)
  (hywconfig-delete-pop)
  (unless (hywconfig-ring-empty-p)
    (hyperbole 'win)))

;;;###autoload
(defun hywconfig-ring-empty-p ()
  "Return t if the wconfig ring for the selected frame is empty; nil otherwise."
  (ring-empty-p (hywconfig-get-ring)))

;;;###autoload
(defun hywconfig-ring-save ()
  "Save the selected frame's window configuration onto the save ring.
Use {\\[hywconfig-yank-pop]} to restore it at a later time."
  (interactive)
  (ring-insert (hywconfig-get-ring) (current-window-configuration))
  (when (called-interactively-p 'interactive)
    (message
     (substitute-command-keys
      "Window configuration saved.  Use {\\[hywconfig-yank-pop]} to restore."))))

;;;###autoload
(defun hywconfig-yank-pop (n)
  "Replace selected frame's window config with prefix arg Nth prior one in ring.
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

;;;###autoload
(defun hywconfig-yank-pop-continue (n)
  "Replace selected frame's window config with prefix arg Nth prior one in ring.
If there are more than one entries in the ring, then stay in the hywconfig menu.

Interactively, default value of N = 1, means the last saved window
configuration is displayed.

The sequence of window configurations wraps around, so that after the
oldest one comes the newest one."
  (interactive "p")
  (hywconfig-yank-pop n)
  (when (> (ring-length (hywconfig-get-ring)) 1)
    (hyperbole 'win)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hywconfig-named-get (name)
  "Return the selected frame's window configuration with NAME."
  (with-suppressed-warnings ((obsolete lax-plist-get)) ;; Obsolete since 29.1, use plist-get
    (lax-plist-get (hywconfig-named-get-entries) name)))

(defun hywconfig-named-get-names ()
  "Return the selected frame's list of window configuration names."
  (hypb:map-plist (lambda (name _wconfig) name) (hywconfig-named-get-entries)))

(defun hywconfig-named-get-entries ()
  "Get the selected frame's plist of named window configurations."
  (frame-parameter (selected-frame) 'named-hywconfigs))

(defun hywconfig-named-put (name wconfig)
  "Add WCONFIG with NAME to selected frame's plist of named window configurations."
  (hywconfig-named-set-entries
   (with-suppressed-warnings ((obsolete lax-plist-put)) ;; Obsolete since 29.1, use plist-get
     (lax-plist-put (hywconfig-named-get-entries) name wconfig))))

(defun hywconfig-named-remove (name)
  "Remove the selected frame's stored window configuration with NAME."
  (let ((plist (hywconfig-named-get-entries)))
    (prog1 (hypb:remove-from-plist plist name)
      (hywconfig-named-set-entries plist))))

(defun hywconfig-named-set-entries (entries)
  "Set the selected frame's plist of named window configuration ENTRIES."
  (set-frame-parameter (selected-frame) 'named-hywconfigs entries))

(defun hywconfig-get-ring ()
  "Return the selected frame's window configuration ring."
  (let* ((frame (selected-frame))
	 (ring (frame-parameter frame 'hywconfig-ring)))
    (unless ring
      (set-frame-parameter frame 'hywconfig-ring (setq ring (make-ring hywconfig-ring-max))))
    ring))

(defun hywconfig-set-window-configuration (wconfig)
  "Return window configureation WCONFIG within the selected frame."
  (when (window-configuration-p wconfig)
    (condition-case nil
	(progn (set-window-configuration wconfig) t)
      (error (message "(HyWconfig): Invalid window configuration, `%s'" wconfig)
	     (beep)
	     (sit-for 2)
	     nil))))

(run-hooks 'hywconfig-load-hook)

(provide 'hywconfig)

;;; hywconfig.el ends here
