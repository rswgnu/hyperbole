;;; hhist.el --- History of Hyperbole buttons selected  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    24-Apr-91 at 03:36:23
;; Last-Mod:     21-Jul-22 at 11:17:46 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;   This implements a last-in-first-out stack of traversed locations
;;   for use as a locational history in Hyperbole.  Each location stores
;;   the frame and window locations of all buffers.  When restoring a location,
;;   if new frames are created after the location has been stored, they are
;;   minimized, not deleted.  Use the Emacs Buffer menu to return to them.
;;   Frames which have been deleted are not restored.

;;; Code:
;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hhist:add (elt)
  "Add ELT to hyper-history list.
Do not add if the same as current or prior location (frame configuration).
ELT must have been created via a call to `hhist:element' prior to
changing the current frame configuration somehow."
  ;; Even though this next line looks useless, it cures a problem with
  ;; window buffer correspondences on startup, so don't remove it.
  (set-buffer (window-buffer (selected-window)))
  (when (not (frame-configuration-p elt))
    (error "(hhist:add): 'elt' must be a frame configuration, not %s" elt))
  (unless (or (equal elt (car *hhist*))
	      (equal elt (current-frame-configuration)))
    (setq *hhist* (cons elt *hhist*))))

(defun hhist:element ()
  "Return a history element for current point location."
  (current-frame-configuration))

(defun hhist:remove (&optional arg)
  "Remove optional prefix ARG entries from history, return to ARGth location.
The command is ignored with ARG < 1."
  (interactive "p")
  (setq arg (cond ((or (null arg)
		       (and (listp arg) (not (integerp (car arg))))
		       (not (integerp arg)))
		   1)
		  ((and (listp arg) (integerp (car arg)))
		   (car arg))
		  ((listp arg) 1)
		  (t arg)))
  (let ((prev-config))
    (when *hhist*
      (when (< arg 1)
	(message "(hhist:remove): No previous location to which to return.")
	(beep))
      (while (and (> arg 0) *hhist*)
	(setq prev-config (car *hhist*)
	      *hhist* (cdr *hhist*)
	      arg (1- arg)))
      (when (frame-configuration-p prev-config)
	;; Minify but keep any frames created after this frame configuration was saved.
	(set-frame-configuration prev-config t)))))

(defun hhist:init ()
  "Reset history list."
  (interactive)
  (setq *hhist* nil))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst *hhist* nil
  "List of previously visited Hyperbole button source locations.
Car of list is most recent.")

(provide 'hhist)

;;; hhist.el ends here
