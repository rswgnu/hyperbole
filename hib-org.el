;;; hib-org.el --- Implicit button type for Org mode
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     2-Jul-16 at 14:54:14
;;
;; Copyright (C) 2016-2020  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This defines a context-sensitive implicit button type, org-mode,
;;   triggered when the major mode is org-mode or is derived from
;;   org-mode and point is anywhere other than at the end of a line.
;;
;;   See the doc for ibtypes::org-mode for details of what it does and
;;   its compatibility with org-mode.
;;
;;   For a good tutorial on basic use of Org-mode, see:
;;     https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (require 'hmouse-drv))

(require 'hsys-org)

;;; ************************************************************************
;;; Public Button Types
;;; ************************************************************************

(defib org-mode ()
  "Follow Org mode references, cycles outline visibility and executes code blocks.

First, this follows internal links in Org mode files.  When pressed on a
link referent/target, the link definition is displayed, allowing two-way
navigation between definitions and targets.

Second, this follows Org mode external links.

Third, within a radio target definition, this jumps to the first
occurrence of an associated radio target.

Fourth, when point is on an outline heading in Org mode, this
cycles the view of the subtree at point.

Fifth, with point on the first line of a code block definition, this
executes the code block via the Org mode standard binding of {C-c C-c},
\(org-ctrl-c-ctrl-c).

In any other context besides the end of a line, the Action Key invokes the
Org mode standard binding of {M-RET}, (org-meta-return).

To disable ALL Hyperbole support within Org major and minor modes, set the
custom option `hsys-org-enable-smart-keys' to nil.  Then in Org modes, this
will simply invoke `org-meta-return'.

Org links may be used outside of Org mode buffers.  Such links are
handled by the separate implicit button type, `org-link-outside-org-mode'."
  (when (and (funcall hsys-org-mode-function)
	     ;; Prevent infinite recursion if ever called via org-metareturn-hook
	     ;; from org-meta-return invocation.
	     (not (hyperb:stack-frame '(ibtypes::debugger-source org-meta-return))))
    (cond ((not hsys-org-enable-smart-keys)
	   (hact 'org-meta-return))
	  ((and (not (hyperb:stack-frame '(hbut:at-p))) (hbut:at-p))
	   ;; Activate any Hyperbole button at point
	   (hbut:act))
	  ((eq hsys-org-enable-smart-keys t)
	   (let (start-end)
	     (cond ((hsys-org-agenda-item-at-p)
		    (hsys-org-set-ibut-label (cons (line-beginning-position) (line-end-position)))
		    (hact 'org-agenda-show-and-scroll-up current-prefix-arg))
		   ((setq start-end (hsys-org-internal-link-target-at-p))
		    (hsys-org-set-ibut-label start-end)
		    (hact 'org-internal-link-target))
		   ((hsys-org-radio-target-def-at-p)
		    (hact 'org-radio-target))
		   ((setq start-end (hsys-org-link-at-p))
		    (hsys-org-set-ibut-label start-end)
		    (hact 'org-link))
		   ((org-at-heading-p)
		    (hact 'hsys-org-cycle))
		   ((hsys-org-block-start-at-p)
		    (org-ctrl-c-ctrl-c))
		   (t
		    ;; no operation
		    t))))
	  ;; default fallback cmd when (eq hsys-org-enable-smart-keys 'button)
	  (t
	   (hact 'org-meta-return)))))

(defun org-mode:help (&optional _but)
  "If on an Org mode heading, cycles through views of the whole buffer outline.
If on an Org mode link, displays standard Hyperbole help."
  (cond ((or (hsys-org-link-at-p) (hsys-org-agenda-item-at-p))
	 (hkey-help current-prefix-arg)
	 t)
	((and (funcall hsys-org-mode-function)
	      (org-at-heading-p))
	 (hact 'hsys-org-global-cycle)
	 t)))

(defact org-link (&optional link)
  "Follows an optional Org mode LINK to its target.
If LINK is nil, follows any link at point.  Otherwise, triggers an error."
  (if (stringp link)
      (cond ((fboundp #'org-link-open-from-string)
	     (org-link-open-from-string link))
            ((fboundp #'org-open-link-from-string)
	     (org-open-link-from-string link))) ;; autoloaded
    (org-open-at-point))) ;; autoloaded

(defact org-internal-link-target (&optional link-target)
  "Follows an optional Org mode LINK-TARGET back to its link definition.
If LINK-TARGET is nil, follows any link target at point.  Otherwise, triggers an error."
  (let (start-end)
    (cond ((stringp link-target)
	   (setq start-end t)
	   (hsys-org-search-internal-link-p link-target))
	  ((null link-target)
	   (when (setq start-end (hsys-org-internal-link-target-at-p))
	     (hsys-org-search-internal-link-p (buffer-substring-no-properties
					       (car start-end) (cdr start-end))))))
    (unless start-end
      (error "(org-internal-link-target): Point must be on a link target (not the link itself)"))))


(defact org-radio-target (&optional target)
  "Jump to the next occurrence of an optional Org mode radio TARGET link.
If TARGET is nil and point is on a radio target definition or link, it
uses that one.  Otherwise, triggers an error."
  (let (start-end)
    (cond ((stringp target)
	   (setq start-end t)
	   (hsys-org-to-next-radio-target-link target))
	  ((null target)
	   (when (setq start-end (hsys-org-radio-target-at-p))
	     (hsys-org-to-next-radio-target-link (buffer-substring-no-properties
					          (car start-end) (cdr start-end))))))
    (unless start-end
      (error "(org-radio-target): Point must be on a radio target definition or link"))))

(provide 'hib-org)
