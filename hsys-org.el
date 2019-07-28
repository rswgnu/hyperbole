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

(defvar hsys-org-mode-function #'hsys-org-mode-p
  "*Boolean function of no arguments that determines whether hsys-org actions are triggered or not.")

(defun hsys-org-mode-p ()
  "Returns non-nil if an Org-related major or minor mode is active in the current buffer."
  (or (derived-mode-p 'org-mode)
      (and (boundp 'outshine-mode) outshine-mode)
      (and (boundp 'poporg-mode) poporg-mode)))


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
  "Follows Org mode references, cycles outline visibility and executes code blocks.

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
(org-ctrl-c-ctrl-c).

In any other context besides the end of a line, the Action Key invokes the
Org mode standard binding of {M-RET}, (org-meta-return)."
  (when (funcall hsys-org-mode-function)
    (let (start-end)
      (cond ((setq start-end (hsys-org-internal-link-target-at-p))
	     (hsys-org-set-ibut-label start-end)
	     (hact 'org-internal-link-target))
	    ((hsys-org-radio-target-def-at-p)
	     (hact 'org-radio-target))
	    ((setq start-end (hsys-org-link-at-p))
	     (hsys-org-set-ibut-label start-end)
	     (hact 'org-link))
	    ((org-at-heading-p)
	     (hact 'hsys-org-cycle))
	    ((hsys-org-at-block-start-p)
	     (org-ctrl-c-ctrl-c))
	    (t
	     (hact 'org-meta-return))))))

(defun org-mode:help (&optional _but)
  "If on an Org mode heading, cycles through views of the whole buffer outline.
If on an Org mode link, displays standard Hyperbole help."
  (when (derived-mode-p 'org-mode)
    (cond ((hsys-org-link-at-p)
	   (hkey-help current-prefix-arg)
	   t)
	  ((org-at-heading-p)
	   (hact 'hsys-org-global-cycle)
	   t))))

(defact org-link (&optional link)
  "Follows an optional Org mode LINK to its target.
If LINK is nil, follows any link at point.  Otherwise, triggers an error."
  (if (stringp link)
      (org-open-link-from-string link) ;; autoloaded
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
  "Jumps to the next occurrence of an optional Org mode radio TARGET link.
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

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hsys-org-region-with-text-property-value (pos property)
  "Returns (start . end) buffer positions of the region around POS that shares its non-nil text PROPERTY value, else nil."
  (if (null pos) (setq pos (point)))
  (let ((property-value (get-text-property pos property))
	(start-point pos))
    (when property-value
	;; Can't use previous-single-property-change here because it
	;; ignores characters that lack the property, i.e. have nil values.
	(if (bobp)
	    (setq start-point (point-min))
	  (while (equal (get-text-property (1- start-point) property) property-value)
	    (setq start-point (1- start-point))))
	(cons start-point (next-single-property-change start-point property)))))

(defun hsys-org-at-block-start-p ()
  "Returns non-nil if point is on the first line of an Org block definition, else nil."
  (save-excursion
    (forward-line 0)
    (or (looking-at org-block-regexp)
	(looking-at org-dblock-start-re))))

(defun hsys-org-link-at-p ()
  "Returns non-nil iff point is on an Org mode link.
Assumes caller has already checked that the current buffer is in org-mode."
  (or (org-in-regexp org-any-link-re)
      (hsys-org-face-at-p 'org-link)))

;; Assumes caller has already checked that the current buffer is in org-mode.
(defun hsys-org-target-at-p ()
  "Returns non-nil iff point is on an Org mode radio target (definition) or link target (referent).
Assumes caller has already checked that the current buffer is in org-mode."
  (hsys-org-face-at-p 'org-target))

(defun hsys-org-radio-target-link-at-p ()
  "Returns (target-start . target-end) positions iff point is on an Org mode radio target link (referent), else nil."
  (and (get-text-property (point) 'org-linked-text)
       (hsys-org-link-at-p)
       (hsys-org-region-with-text-property-value (point) 'org-linked-text)))

(defun hsys-org-radio-target-def-at-p ()
  "Returns (target-start . target-end) positions iff point is on an Org mode radio target (definition), including any delimiter characters, else nil."
  (when (hsys-org-target-at-p)
    (save-excursion
      (if (not (looking-at "<<<"))
	  (goto-char (or (previous-single-property-change (point) 'face) (point-min))))
      (if (looking-at "<<<")
	  (goto-char (match-end 0)))
      (and (get-text-property (point) 'org-linked-text)
	   (hsys-org-region-with-text-property-value (point) 'face)))))

(defun hsys-org-radio-target-at-p ()
  "Returns (target-start . target-end) positions iff point is on an Org mode <<<radio target definition>>> or radio target link (referent), including any delimiter characters, else nil."
  (or (hsys-org-radio-target-def-at-p)
      (hsys-org-radio-target-link-at-p)))

(defun hsys-org-internal-link-target-at-p ()
  "Returns (target-start . target-end) positions iff point is on an Org mode <<link target>>, including any delimiter characters, else nil."
  (when (hsys-org-target-at-p)
    (save-excursion
      (if (not (looking-at "<<"))
	  (goto-char (or (previous-single-property-change (point) 'face) (point-min))))
      (if (looking-at "<<<?")
	  (goto-char (match-end 0)))
      (and (not (get-text-property (point) 'org-linked-text))
	   (hsys-org-region-with-text-property-value (point) 'face)))))

(defun hsys-org-face-at-p (org-face-type)
  "Returns `org-face-type` iff point is on a character with face `org-face-type', a symbol, else nil."
  (let ((face-prop (get-text-property (point) 'face)))
    (when (or (eq face-prop org-face-type)
	      (and (listp face-prop) (memq org-face-type face-prop)))
      org-face-type)))

(defun hsys-org-search-internal-link-p (target)
  "Searches from buffer start for an Org internal link definition matching TARGET.
White spaces are insignificant.  Returns t if a link is found, else nil."
  (if (string-match "<<.+>>" target)
      (setq target (substring target 2 -2)))
  (let ((re (format "%s"
		    (mapconcat #'regexp-quote
			       (split-string target)
			       "[ \t]+\\(?:\n[ \t]*\\)?")))
	(origin (point)))
    (goto-char (point-min))
    (catch :link-match
      (while (re-search-forward re nil t)
	(backward-char)
	(let ((object (org-element-context)))
	  (when (eq (org-element-type object) 'link)
	    (org-show-context 'link-search)
	    (throw :link-match t))))
      (goto-char origin)
      nil)))

(defun hsys-org-search-radio-target-link-p (target)
  "Searches from point for a radio target link matching TARGET.
White spaces are insignificant.  Returns t if a target link is found, else nil."
  (if (string-match "<<<.+>>>" target)
      (setq target (substring target 3 -3)))
  (let ((re (format "%s"
		    (mapconcat #'regexp-quote
			       (split-string target)
			       "[ \t]+\\(?:\n[ \t]*\\)?")))
	(origin (point)))
    (catch :radio-match
      (while (re-search-forward re nil t)
	(backward-char)
	(let ((object (org-element-context)))
	  (when (eq (org-element-type object) 'link)
	    (org-show-context 'link-search)
	    (throw :radio-match t))))
      (goto-char origin)
      nil)))

(defun hsys-org-set-ibut-label (start-end)
  "Record the label and START-END positions of any implicit button at point."
  (when (consp start-end)
    (ibut:label-set (ibut:key-to-label
		     (ibut:label-to-key
		      (buffer-substring-no-properties (car start-end) (cdr start-end))))
		    (car start-end) (cdr start-end))))


(defun hsys-org-to-next-radio-target-link (target)
  "Moves to the start of the next radio TARGET link if found.  TARGET must be a string."
  (if (string-match "<<<.+>>>" target)
      (setq target (substring target 3 -3)))
  (let ((opoint (point))
	(start-end (hsys-org-radio-target-at-p))
	found)
    (if start-end
	;; Move past any current target link
	(goto-char (cdr start-end)))
    (while (and (hsys-org-search-radio-target-link-p target)
		(setq found t)
		(not (hsys-org-radio-target-link-at-p))))
    (when found
      (if (hsys-org-radio-target-link-at-p)
	  (goto-char (or (previous-single-property-change (point) 'face) (point-min)))
	(goto-char opoint)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(provide 'hsys-org)

;;; hsys-org.el ends here
