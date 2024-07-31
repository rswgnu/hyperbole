;;; kotl-orgtbl.el --- Allow use of Org minor-mode table editing in koutlines  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    10/18/2020
;; Last-Mod:     14-Jul-24 at 23:32:40 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2020-2021  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;   - Org Table Support: Org table editing now automatically works in the
;;     Koutliner via Org table minor mode.  Use {M-x orgtbl-mode RET} to
;;     toggle this on and off or simply press the Action Key on a pipe |
;;     character to do the same thing.  See "(Org)Tables" for details.

;;   - New Tree Demotion/Promotion Keys: Tree promotion and demotion keys now
;;     match the defaults in Org mode and Outline mode, plus some easier to
;;     type ones.  The tables below summarize which keys work whether inside
;;     an Org table or outside.

;;     |----------------------------+-----------------------------|
;;     | Demotion Inside Org Table  | Promotion Inside Org Table  |
;;     |----------------------------+-----------------------------|
;;     | C-c C-,                    | C-c C-.                     |
;;     | C-c C-<                    | C-c C->                     |
;;     |----------------------------+-----------------------------|

;;     |----------------------------+-----------------------------|
;;     | Demotion Outside Org Table | Promotion Outside Org Table |
;;     |----------------------------+-----------------------------|
;;     | TAB                        | Shift-TAB or M-TAB          |
;;     | M-left-arrow               | M-right-arrow               |
;;     | C-c C-,                    | C-c C-.                     |
;;     | C-c C-<                    | C-c C->                     |
;;     |----------------------------+-----------------------------|

;;  If in an Org table, the kotl-mode {TAB} binding operates only
;;  when invoked with an explicit prefix argument; otherwise, {TAB}
;;  performs its normal table-based alignment and movement.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'org-table)
(require 'hmouse-drv)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function kotl-mode:next-line "kotl-mode")
(declare-function kotl-mode:transpose-lines "kotl-mode")
(declare-function kotl-mode:tab-command "kotl-mode")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kotl-mode-overriding-orgtbl-mode-map nil
  "Keymap to override Org Table minor mode keys within kotl-mode major mode.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Redefine this Org Table function to handle Koutlines as well.
(defun orgtbl-tab (arg)
  "Justification and field motion for `orgtbl-mode' with Koutline support."
  (interactive "P")
  (cond ((and (derived-mode-p 'kotl-mode) arg)
	 (kotl-mode:tab-command (if (= (prefix-numeric-value arg) 1) nil arg)))
	(arg
	 (org-table-edit-field t))
	(t (org-table-justify-field-maybe)
	   (org-table-next-field))))

(defun kotl-mode:transpose-lines-up ()
  "Exchange current line and previous line, maintaining point location.
If no previous line, exchange current with next line."
  (interactive)
  (let ((opoint (point)))
    (kotl-mode:transpose-lines 1)
    (goto-char opoint)))

(defun kotl-mode:transpose-lines-down ()
  (interactive)
  (let ((opoint (point)))
    (kotl-mode:next-line 1)
    (kotl-mode:transpose-lines 1)
    (goto-char opoint)))

(defun kotl-mode:orgtbl-meta-return (arg)
  "Let Action Key handle tables in kotl-mode."
  (interactive "P")
  (hkey-either arg))

(defun kotl-mode:setup-overriding-orgtbl-keymap ()
  (condition-case err
      (progn
	(setq kotl-mode-overriding-orgtbl-mode-map
	      (if (fboundp 'copy-keymap)
		  (copy-keymap orgtbl-mode-map)
		(make-sparse-keymap)))

	;; Override org-tbl {M-RET} binding since Action Key provides the
	;; same functionality when in a table, but may also be invoked from
	;; a mouse button.
	(org-defkey kotl-mode-overriding-orgtbl-mode-map "\M-\C-m"
		    (orgtbl-make-binding 'kotl-mode:orgtbl-meta-return 105
        				 "\M-\C-m" [(meta return)]))
	(org-defkey kotl-mode-overriding-orgtbl-mode-map [(meta return)]
		    (orgtbl-make-binding 'kotl-mode:orgtbl-meta-return 106
        				 [(meta return)] "\M-\C-m"))
	(org-defkey kotl-mode-overriding-orgtbl-mode-map "\C-d"
		    (orgtbl-make-binding 'kotl-mode:delete-char 201 "\C-d"))
	(org-defkey kotl-mode-overriding-orgtbl-mode-map [S-iso-lefttab]
		    (orgtbl-make-binding 'org-shifttab 107
        				 [S-iso-lefttab] [backtab] [(shift tab)]))
	
	;; Overload edit keys to deal with structure and labels.
	(let ((local-cmd)
	      (cmds '(org-delete-char
		      org-delete-backward-char
		      org-force-self-insert
		      orgtbl-create-or-convert-from-region
		      orgtbl-ctrl-c-ctrl-c
		      orgtbl-self-insert-command)))
	  (mapc
	   (lambda (cmd)
	     (setq local-cmd (intern-soft
			      (concat "kotl-mode:" (symbol-name cmd))))
	     ;; Only bind key locally if kotl-mode local-cmd has already
	     ;; been defined and cmd is a valid function.
	     (when (and local-cmd (fboundp cmd))
	       ;; Make local-cmd have the same property list as cmd,
	       ;; e.g. so pending-delete property is the same, but delete
	       ;; interactive-only property to suppress byte-compiler warnings.
	       (setplist local-cmd (copy-sequence (symbol-plist cmd)))
	       (cl-remprop local-cmd 'interactive-only)
	       (org-remap kotl-mode-overriding-orgtbl-mode-map cmd local-cmd)))
	   (setq cmds
		 (if orgtbl-optimized
		     (append '(delete-backward-char
			       delete-char
			       delete-forward-char
			       ;; self-insert-command
			       )
			     cmds)
		   cmds)))))
    (error
     (setq kotl-mode-overriding-orgtbl-mode-map nil)
     (error "(kotl-mode:setup-overriding-orgtbl-keymap): Setup error: %s" err))))

(provide 'kotl-orgtbl)

;;; kotl-orgtbl.el ends here
