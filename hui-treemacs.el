;;; hui-treemacs.el --- Hyperbole Smart Key support for the Treemacs file manager package
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Nov-17
;;
;; Copyright (C) 2017  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (require 'treemacs nil t))

(eval-when (load)
  (unless (>= (string-to-number treemacs-version) 1.14)
    (error "(hui-treemacs): Hyperbole requires Treemacs package version 1.14 or greater")))

;;; ************************************************************************
;;; smart-treemacs functions
;;; ************************************************************************

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
	     (treemacs-toggle)
	   (hact 'link-to-directory default-directory)))
	((and (last-line-p) (eolp))
	 (treemacs-toggle))
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
	       (treemacs-push-button current-prefix-arg))))))

;;;###autoload
(defun smart-treemacs-modeline ()
  "Toggle display of Treemacs file viewer based on Smart Action Key click on a modeline.

When pressed on the Treemacs buffer modeline or Treemacs is displaying
the default directory of the buffer modeline clicked upon, then
quit/hide the Treemacs window.  Otherwise, display the Treemacs window
with the default directory of the buffer modeline clicked upon.

Suitable for use as a value of `action-key-modeline-buffer-id-function'."
  (if (fboundp 'treemacs)
      (progn
	(require 'treemacs)
	(cond
	 ;; Clicked on Treemacs buffer id
	 ((if action-key-depress-window
	      (treemacs-is-treemacs-window? action-key-depress-window)
	    (string-match " Treemacs " (format-mode-line mode-line-format)))
	  ;; Quit/hide treemacs.
	  (treemacs-toggle))
	 ;;
	 ;; Treemacs is visible and displaying the same dir as
	 ;; the default dir of the clicked on modeline.
	 ((and (treemacs-buffer-exists?)
	       (string-equal (expand-file-name default-directory)
			     (with-current-buffer (treemacs-buffer-exists?)
			       default-directory)))
	  ;; Quit/hide treemacs.
	  (treemacs-toggle))
	 ;;
	 ;; Otherwise, invoke treemacs on the default dir of the clicked on modeline.
	 (t (treemacs))))
    (error "(smart-treemacs-modeline): Treemacs package is not installed")))

(provide 'hui-treemacs)
