;;; hsys-org.el --- GNU Hyperbole support for Emacs Org mode links
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     2-Jul-16 at 14:54:14
;;
;; Copyright (C) 2016-2019  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This defines a context-sensitive implicit button type, org-mode,
;;   triggered when the major mode is org-mode or is derived from
;;   org-mode and point is anywhere other than at the end of a line.
;;
;;   When:
;;     on an Org mode link - displays the link referent
;;     on an Org mode heading - cycles through the available display
;;       views for that heading
;;     anywhere else - executes `org-meta-return'.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hbut)
(require 'org)

(defun hsys-org-cycle ()
  "Calls org-cycle and forces it to be set as this-command to cycle through all states."
  (setq last-command 'org-cycle
	this-command 'org-cycle)
  (org-cycle))

(defun hsys-org-global-cycle ()
  "Calls org-global-cycle and forces it to be set as this-command to cycle through all states."
  (setq last-command 'org-cycle
	this-command 'org-cycle)
  (org-global-cycle nil))

;;; ************************************************************************
;;; Public Button Types
;;; ************************************************************************

(defib org-mode ()
  "Follows any Org mode link at point or cycles through views of the outline subtree at point."
  (when (derived-mode-p 'org-mode)
    (cond ((org-link-at-p)
	   (hact 'org-link nil))
	  ((org-at-heading-p)
	   (hact 'hsys-org-cycle))
	  (t (hact 'org-meta-return)))))

(defun org-mode:help (&optional _but)
  "If on an Org mode heading, cycles through views of the whole buffer outline.
If on an Org mode link, displays standard Hyperbole help."
  (when (derived-mode-p 'org-mode)
    (cond ((org-link-at-p)
	   (hkey-help current-prefix-arg)
	   t)
	  ((org-at-heading-p)
	   (hact 'hsys-org-global-cycle)
	   t))))

(defact org-link (link)
  "Follows an Org mode LINK.  If LINK is nil, follows the link at point."
  (if (stringp link)
      (org-open-link-from-string link) ;; autoloaded
    (org-open-at-point-global))) ;; autoloaded

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Assumes caller has already checked that the current buffer is in org-mode.
(defun org-link-at-p ()
  "Returns non-nil iff point is on an Org mode link."
  (let ((face-prop (get-text-property (point) 'face)))
    (or (eq face-prop 'org-link)
	(and (listp face-prop) (memq 'org-link face-prop)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(provide 'hsys-org)

;;; hsys-org.el ends here
