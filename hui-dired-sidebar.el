;;; hui-dired-sidebar.el --- Hyperbole Smart Key support for dired sidebar  -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    25-Jul-20
;; Last-Mod:     24-Jan-22 at 00:18:47 by Bob Weiner
;;
;; Copyright (C) 2020-2021 Free Software Foundation, Inc.  See the
;; "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (require 'dired-sidebar nil t))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************
(declare-function dired-sidebar-toggle-sidebar "ext:dired-sidebar")
(defvar dired-sidebar-cycle-subtree-on-click)

;;; ************************************************************************
;;; smart-dired-sidebar functions
;;; ************************************************************************

;;;###autoload
(defun smart-dired-sidebar ()
  "Use a single key or mouse key to manipulate directory entries.

Invoked via a key press when in dired-sidebar-mode.  It assumes
that its caller has already checked that the key was pressed in
an appropriate buffer and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the item is displayed for editing,
     normally in another window, or if it is a directory and
     `dired-sidebar-cycle-subtree-on-click' is t it will expand
     and collapse the entry
 (2) at the end of an entry line: invoke `action-key-eol-function',
     typically to scroll up proportionally, if an Action Key press; invoke
     `assist-key-eol-function', typically to scroll down proportionally,
     if an Asisst Key press;
 (3) on the first line of the buffer (other than the end of line),
     dired is run on the current directory of this dired-sidebar;
 (4) at the end of the first or last line of the buffer,
     this dired-sidebar invocation is hidden."

  (interactive)
  (cond ((first-line-p)
	 (if (eolp)
	     (dired-sidebar-toggle-sidebar)
	   (hact 'link-to-directory default-directory)))
	((and (last-line-p) (eolp))
	 (dired-sidebar-toggle-sidebar))
	((eolp)
	 (funcall (if assist-flag assist-key-eol-function action-key-eol-function)))
	(t (let ((file (dired-get-file-for-visit)))
	     (if (and dired-sidebar-cycle-subtree-on-click
		      (file-directory-p file)
		      (not (string-suffix-p "." file)))
		 (hact 'dired-sidebar-subtree-toggle)
               (hact 'dired-sidebar-find-file file))))))

(provide 'hui-dired-sidebar)
;;; hui-dired-sidebar.el ends here
