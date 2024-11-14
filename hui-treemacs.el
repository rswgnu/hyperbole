;;; hui-treemacs.el --- GNU Hyperbole Smart Key support for the Treemacs file manager package  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Nov-17
;; Last-Mod:     13-Nov-24 at 13:09:22 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2017-2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;; Ignore loading of this file unless the Treemacs package v2 or
;; greater has been installed.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'package)
(require 'seq)

(or (require 'treemacs nil t)
    (and (package-installed-p 'treemacs)
	 (package-activate 'treemacs))
    (hypb:require-package 'treemacs))

(defvar treemacs-version)

(unless (string-greaterp treemacs-version "v2")
  (error "(hui-treemacs): Hyperbole requires Treemacs package version 2.0 or greater, not %s" treemacs-version))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************
(defvar action-key-depress-window)      ; "hmouse-drv.el"
(defvar action-key-eol-function)        ; "hmouse-drv.el"
(defvar assist-flag)                    ; "hmouse-drv.el"
(defvar assist-key-eol-function)        ; "hmouse-drv.el"
(defvar aw-ignored-buffers)

(declare-function first-line-p "hui-mouse")
(declare-function hact "hact")
(declare-function hypb:require-package "hypb")
(declare-function last-line-p "hui-mouse")
(declare-function package-activate "package")
(declare-function treemacs "ext:treemacs")
(declare-function treemacs-add-and-display-current-project-exclusively "ext:treemacs")
(declare-function treemacs-current-button "ext:treemacs-core-utils")
(declare-function treemacs-current-visibility "ext:treemacs-scope")
(declare-function treemacs-display-current-project-exclusively "ext:treemacs")
(declare-function treemacs-get-local-window "ext:treemacs-scope")
(declare-function treemacs-is-treemacs-window? "ext:treemacs-core-utils")
(declare-function treemacs-node-buffer-and-position "ext:treemacs-mouse-interface")
(declare-function treemacs-quit "ext:treemacs-interface")
(declare-function treemacs-toggle-node "ext:treemacs-interface")

;;; ************************************************************************
;;; smart-treemacs functions
;;; ************************************************************************

;; Want to be able to select Treemacs window with ace-window.
;; This also averts window labeling problems with ace-window.
(eval-after-load "ace-window"
  '(setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers)))

(unless (fboundp 'treemacs-quit)
  (fset 'treemacs-quit #'bury-buffer))

;;;###autoload
(defun smart-treemacs-edit (&optional dir)
  "Use `treemacs' to edit optional DIR or the `default-directory'."
  (let ((default-directory (if (stringp dir) dir default-directory)))
    (cond ((fboundp #'treemacs-add-and-display-current-project-exclusively)
	   (treemacs-add-and-display-current-project-exclusively))
	  ;; Older obsoleted function
	  ((fboundp #'treemacs-display-current-project-exclusively)
	   (treemacs-display-current-project-exclusively))
	  (t (treemacs)))))

(defun smart-treemacs-quit (&optional arg)
  "Quit treemacs visible in current frame with `bury-buffer'.
With a prefix ARG call `treemacs-kill-buffer' instead."
  (interactive "P")
  (when (eq (treemacs-current-visibility) 'visible)
    (with-selected-window (treemacs-get-local-window)
      (treemacs-quit arg))))

;;;###autoload
(defun smart-treemacs ()
  "Use a single key or mouse key to manipulate directory entries.

Invoked via a key press when in treemacs-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) on or to the left of an entry icon, run the treemacs TAB command
     to expand or collapse the entry;
 (2) elsewhere within an entry line, display the item, which may be a
     directory, for editing, normally in another window;
 (3) at the end of an entry line: if an Action Key press, invoke
     `action-key-eol-function', typically to scroll up proportionally;
     if an Asisst Key press, invoke `assist-key-eol-function', typically
     to scroll down proportionally;
 (4) at the end of the first or last line of the buffer, quit this
     Treemacs invocation."

  (interactive)
  (cond ((and (eolp) (or (first-line-p) (last-line-p)))
	 (hact 'smart-treemacs-quit))
	((eolp)
	 (hact 'funcall (if assist-flag assist-key-eol-function action-key-eol-function)))
	(t (if (and (treemacs-current-button)
		    (= (point) (- (button-start (treemacs-current-button)) 2)))
	       ;; Before or on the entry's icon
	       (hact 'treemacs-TAB-action current-prefix-arg)
	     ;; On the entry, handles dirs, files and tag entries
	     (hact 'treemacs-RET-action current-prefix-arg)))))

;;;###autoload
(defun smart-treemacs-modeline ()
  "Toggle display of Treemacs from Smart Action Key click on a modeline.

When pressed on the Treemacs buffer modeline or Treemacs is displaying
the default directory of the buffer modeline clicked upon, then
quit/hide the Treemacs window.  Otherwise, display the Treemacs window
with the default directory of the buffer modeline clicked upon.

Suitable for use as a value of `action-key-modeline-buffer-id-function'."
  (cond
   ;; Clicked on Treemacs buffer id
   ((if action-key-depress-window
	(treemacs-is-treemacs-window? action-key-depress-window)
      (hact 'string-match " Treemacs " (format-mode-line mode-line-format)))
    ;; Quit/hide treemacs.
    (hact 'treemacs-quit))
   ;;
   ;; Treemacs is visible and displaying the same dir as
   ;; the default dir of the clicked on modeline.
   ((and (eq (treemacs-current-visibility) 'visible)
	 (string-equal (expand-file-name default-directory)
		       (with-selected-window (treemacs-get-local-window)
			 (save-excursion
			   (goto-char (point-min))
			   default-directory))))
    ;; Quit/hide treemacs.
    (hact 'smart-treemacs-quit))
   ;;
   ;; Otherwise, invoke treemacs on the default dir of the clicked on modeline.
   (t (hact 'smart-treemacs-edit))))

(provide 'hui-treemacs)
;;; hui-treemacs.el ends here

