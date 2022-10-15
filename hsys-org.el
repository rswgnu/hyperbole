;;; hsys-org.el --- GNU Hyperbole support functions for Org mode  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     2-Jul-16 at 14:54:14
;; Last-Mod:     10-Oct-22 at 22:55:01 by Mats Lidell
;;
;; Copyright (C) 2016-2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Support functions for "hui-mouse.el#smart-org".
;;
;;   smart-org is triggered when the major mode is org-mode or is derived
;;   from org-mode.
;;
;;   See the doc for smart-org for details of what it does and
;;   its compatibility with org-mode.
;;
;;   For a good tutorial on basic use of Org-mode, see:
;;     https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (require 'hmouse-drv))
(require 'hbut)
(require 'org)
(require 'org-element)
;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))

;;;###autoload
(defun hsys-org-meta-return-shared-p ()
  "Return non-nil if hyperbole-mode is active and shares the org-meta-return key."
  (let ((org-meta-return-keys (where-is-internal #'org-meta-return org-mode-map)))
    (when (or (set:intersection org-meta-return-keys
				(where-is-internal #'hkey-either hyperbole-mode-map))
	      (set:intersection org-meta-return-keys
				(where-is-internal #'action-key hyperbole-mode-map)))
      t)))

;;;###autoload
(defun hsys-org-meta-return ()
  "Call `org-meta-return' with the numeric value of any prefix arg when given."
  (interactive "P")
  (if current-prefix-arg
      (org-meta-return (prefix-numeric-value current-prefix-arg))
    (org-meta-return)))

;;;###autoload
(defcustom hsys-org-enable-smart-keys 'unset
  "This applies in Org major/minor modes only when `hyperbole-mode' is active.
If set to \\='unset prior to loading Hyperbole, then Hyperbole
initialization will set its value.

The following table shows what the Smart Keys do in various contexts
with different settings of this option.  For example, a nil value makes
{M-RET} operate as it normally does within Org mode contexts.

|---------+-------------------+------------------+----------+------------------|
| Setting | Smart Key Context | Hyperbole Button | Org Link | Fallback Command |
|---------+-------------------+------------------+----------+------------------|
| buttons | Ignore            | Activate         | Activate | org-meta-return  |
| nil     | Ignore            | Ignore           | Ignore   | org-meta-return  |
| t       | Activate          | Activate         | Activate | None             |
|---------+-------------------+------------------+----------+------------------|"
  :type '(choice (const :tag "buttons - In Org, enable Smart Keys within Hyperbole buttons only" buttons)
		 (const :tag "nil     - In Org, disable Smart Keys entirely" nil)
		 (const :tag "t       - In Org, enable all Smart Key contexts" t))
  :initialize #'custom-initialize-default
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;;###autoload
(defvar hsys-org-mode-function #'hsys-org-mode-p
"*Zero arg bool func that returns non-nil if point is in an Org-related buffer.")

;;; ************************************************************************
;;; Public Action Types
;;; ************************************************************************

(defact org-link (&optional link)
  "Follows an optional Org mode LINK to its target.
If LINK is nil, follows any link at point.  Otherwise, triggers an error."
  (if (stringp link)
      (org-link-open-from-string link)
    (org-open-at-point))) ;; autoloaded

(defact org-internal-link-target (&optional link-target)
  "Follows an optional Org mode LINK-TARGET back to its link definition.
If LINK-TARGET is nil, follow any link target at point.
Otherwise, trigger an error."
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

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hsys-org-mode-p ()
  "Return non-nil if point is within an Org major or minor-mode buffer."
  (or (derived-mode-p 'org-mode)
      (derived-mode-p 'org-agenda-mode)
      (and (boundp 'outshine-mode) outshine-mode)
      (and (boundp 'poporg-mode) poporg-mode)))

(defun hsys-org-cycle ()
  "Call `org-cycle' and set as `this-command' to cycle through all states."
  (setq this-command 'org-cycle)
  (org-cycle))

(defun hsys-org-global-cycle ()
  "Call `org-global-cycle' and set as `this-command' to cycle through all states."
  (setq this-command 'org-cycle)
  (org-global-cycle nil))

(defun hsys-org-todo-cycle ()
  "Call `org-todo' and set as `this-command' to cycle through all states."
  (setq this-command 'org-todo)
  (org-todo))

(defun hsys-org-todo-set-cycle ()
  "Call `org-todo' to switch to the next set of keywords.
Force it to be set as `this-command'."
  (setq this-command 'org-todo)
  (org-call-with-arg 'org-todo 'nextset))

(defun hsys-org-todo-occur (&optional keyword)
  "Filter to a tree of todos matching optional KEYWORD.
The tree will include all higher headlines above each match.
Match to all todos if `keyword' is nil or the empty string."
  (interactive
   (list (hargs:read-match "List todos matching keyword: " org-todo-keywords-1)))
  (unless keyword (setq keyword ""))
  (message "%d '%s' TODO entries found"
	   (org-occur (concat "^" org-outline-regexp " *" (regexp-quote keyword)))
	   keyword))

(defun hsys-org-region-with-text-property-value (pos property)
  "Return region around POS that shares its text PROPERTY value, else nil.
Return the (start . end) buffer positions of the region."
  (when (null pos) (setq pos (point)))
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

(defun hsys-org-agenda-item-at-p ()
  "Return non-nil if point is on an Org Agenda item line, else nil."
  (and (derived-mode-p 'org-agenda-mode)
       (org-get-at-bol 'org-marker)))

(defun hsys-org-block-start-at-p ()
  "Return non-nil if point is on the first line of an Org block definition."
  (save-excursion
    (forward-line 0)
    (or (looking-at org-block-regexp)
	(looking-at org-dblock-start-re))))

(defun hsys-org-link-at-p ()
  "Return non-nil iff point is on an Org mode link.
Assume caller has already checked that the current buffer is in `org-mode'
or are looking for an Org link in another buffer type."
  ;; If any Org test fails, just return nil
  (unless (or (smart-eolp) (smart-eobp))
    (condition-case ()
	(let* ((context
		;; Only consider supported types, even if they are not
		;; the closest one.
		(org-element-lineage
		 ;; Next line can trigger an error when `looking-at' is called
		 ;; with a `nil' value of `org-complex-heading-regexp'.
		 (org-element-context)
		 '(clock footnote-definition footnote-reference headline
			 inlinetask link timestamp)
		 t))
	       (type (org-element-type context)))
	  (or (eq type 'link)
	      (and (boundp 'org-link-bracket-re)
		   (org-in-regexp org-link-bracket-re))
	      (and (boundp 'org-bracket-link-regexp)
		   (org-in-regexp org-bracket-link-regexp))
	      (and (boundp 'org-target-link-regexp)
		   (not (null org-target-link-regexp))
		   (org-in-regexp org-target-link-regexp))))
      (error nil))))

;; Assume caller has already checked that the current buffer is in org-mode.
(defun hsys-org-heading-at-p (&optional _)
  "Non-nil when on a headline."
  (unless (or (smart-eolp) (smart-eobp))
    (outline-on-heading-p t)))

;; Assume caller has already checked that the current buffer is in org-mode.
(defun hsys-org-target-at-p ()
  "Return non-nil iff point is on an Org radio target or radio target link.
The radio target is the definition and the radio target link is
the referent.  Assume caller has already checked that the current
buffer is in `org-mode'."
  (hsys-org-face-at-p 'org-target))

;; Assume caller has already checked that the current buffer is in org-mode.
(defun hsys-org-todo-at-p ()
  "Return non-nil iff point is on an Org mode todo keyword.
Assume caller has already checked that the current buffer is in `org-mode'."
  (when (assq :todo-keyword (org-context))
    t))

(defun hsys-org-radio-target-link-at-p ()
  "Return target region iff point is on an Org mode radio target referent.
Target region is (target-start . target-end) iff point is on
an Org mode radio target link (referent), else nil."
  (and (hsys-org-face-at-p 'org-link)
       (hsys-org-link-at-p)
       (hsys-org-region-with-text-property-value (point) 'face)))

(defun hsys-org-radio-target-def-at-p ()
  "Return target-start region iff point is on an Org mode radio definition.
Target region is (target-start . target-end) iff point is on
an Org mode radio target (definition), including any delimiter
characters, else nil."
  (when (hsys-org-target-at-p)
    (save-excursion
      (unless (looking-at "<<<")
	(goto-char (or (previous-single-property-change (point) 'face) (point-min))))
      (when (looking-at "<<<")
	(goto-char (match-end 0)))
      (and (hsys-org-face-at-p 'org-target)
	   (hsys-org-region-with-text-property-value (point) 'face)))))

(defun hsys-org-radio-target-at-p ()
  "Return target region iff point is on an Org mode definition or referent.
Target region is (target-start . target-end).  It is returned
iff point is on an Org mode <<<radio target definition>>> or
radio target link (referent), including any delimiter characters,
else nil."
  (and (or (hsys-org-radio-target-def-at-p)
	   (hsys-org-radio-target-link-at-p))
       (hsys-org-region-with-text-property-value (point) 'face)))

(defun hsys-org-internal-link-target-at-p ()
  "Return target region iff point is on an Org mode <<link target>>.
This is including any delimiter characters.  Target region
is (target-start . target-end)."
  (when (hsys-org-target-at-p)
    (save-excursion
      (unless (looking-at "<<")
	(goto-char (or (previous-single-property-change (point) 'face) (point-min))))
      (when (looking-at "<<<?")
	(goto-char (match-end 0)))
      (and (get-text-property (point) 'org-linked-text)
           (hsys-org-region-with-text-property-value (point) 'face)))))

(defun hsys-org-face-at-p (org-face-type)
  "Return ORG-FACE-TYPE iff point is on a character with that face, else nil.
  ORG-FACE-TYPE must be a symbol, not a symbol name."
  
  (let ((face-prop (get-text-property (point) 'face)))
    (when (or (eq face-prop org-face-type)
	      (and (listp face-prop) (memq org-face-type face-prop)))
      org-face-type)))

(defun hsys-org-search-internal-link-p (target)
  "Search from buffer start for an Org internal link definition matching TARGET.
White spaces are insignificant.  Returns t if a link is found, else nil."
  (when (string-match "<<.+>>" target)
    (setq target (substring target 2 -2)))
  (let ((re (format "%s" (mapconcat #'regexp-quote
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
  "Search from point for a radio target link matching TARGET.
White spaces are insignificant.  Returns t if a target link is found, else nil."
  (when (string-match "<<<.+>>>" target)
    (setq target (substring target 3 -3)))
  (let ((re (format "%s" (mapconcat #'regexp-quote
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
  "Move to the start of the next radio TARGET link if found.
TARGET must be a string."
  (when (string-match "<<<.+>>>" target)
    (setq target (substring target 3 -3)))
  (let ((opoint (point))
	(start-end (hsys-org-radio-target-at-p))
	found)
    (when start-end
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
