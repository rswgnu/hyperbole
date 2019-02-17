;;; hui-treemacs.el --- Hyperbole Smart Key support for the Treemacs file manager package
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Nov-17
;;
;; Copyright (C) 2017-2019  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (require 'treemacs nil t))

(unless (and (featurep 'treemacs) (string-greaterp treemacs-version "v2"))
  (error "(hui-treemacs): Hyperbole requires Treemacs package version 2.0 or greater"))

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
(defun smart-treemacs ()
  "Uses a single key or mouse key to manipulate directory entries.

Invoked via a key press when in treemacs-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) on an entry icon, the treemacs TAB command is run to expand and
     collapse the entry;
 (2) elsewhere within an entry line, the item is displayed for editing,
     normally in another window;
 (3) at the end of an entry line: invoke `action-key-eol-function',
     typically to scroll up proportionally, if an Action Key press; invoke
     `assist-key-eol-function', typically to scroll down proportionally,
     if an Asisst Key press;
 (4) on the first line of the buffer (other than the end of line),
     dired is run on the current directory of this Treemacs;
 (5) at the end of the first or last line of the buffer,
     this Treemacs invocation is quit."

  (interactive)
  (cond ((first-line-p)
	 (if (eolp)
	     (treemacs-quit)
	   (hact 'link-to-directory default-directory)))
	((and (last-line-p) (eolp))
	 (treemacs-quit))
	((eolp)
	 (funcall (if assist-flag assist-key-eol-function action-key-eol-function)))
	(t (let ((over-icon (and (treemacs-current-button)
				 (= (point) (- (button-start (treemacs-current-button)) 2))))
		 (result (treemacs-node-buffer-and-position)))
	     (if (and (not over-icon) result (or (bufferp result) (listp result)))
		 (if (listp result)
		     (hact 'link-to-buffer-tmp (seq-elt result 0) (seq-elt result 1))
		   ;; (bufferp result)
		   (hact 'link-to-buffer-tmp result))
	       (treemacs-toggle-node current-prefix-arg))))))

;;;###autoload
(defun smart-treemacs-modeline ()
  "Toggle display of Treemacs file viewer based on Smart Action Key click on a modeline.

When pressed on the Treemacs buffer modeline or Treemacs is displaying
the default directory of the buffer modeline clicked upon, then
quit/hide the Treemacs window.  Otherwise, display the Treemacs window
with the default directory of the buffer modeline clicked upon.

Suitable for use as a value of `action-key-modeline-buffer-id-function'."
  (cond
   ;; Clicked on Treemacs buffer id
   ((if action-key-depress-window
	(treemacs-is-treemacs-window? action-key-depress-window)
      (string-match " Treemacs " (format-mode-line mode-line-format)))
    ;; Quit/hide treemacs.
    (treemacs-quit))
   ;;
   ;; Treemacs is visible and displaying the same dir as
   ;; the default dir of the clicked on modeline.
   ((and (eq (treemacs-current-visibility) 'visible)
	 (string-equal (expand-file-name default-directory)
		       (with-current-buffer (treemacs-get-local-buffer)
			 default-directory)))
    ;; Quit/hide treemacs.
    (treemacs-quit))
   ;;
   ;; Otherwise, invoke treemacs on the default dir of the clicked on modeline.
   (t (treemacs))))

(provide 'hui-treemacs)
