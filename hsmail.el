;;; hsmail.el --- GNU Hyperbole buttons in mail composer: mail and mh-letter -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     9-May-91 at 04:50:20
;; Last-Mod:      7-Oct-22 at 23:52:10 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;    Works with both "message.el" and older "sendmail.el" settings.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'message)
(require 'hypb)                         ;For `hypb:supercite-p'.
(require 'sendmail)

(defvar inhibit-hyperbole-messaging) ;; From "hsettings.el".

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar smail:comment nil
  "Default comment form to evaluate and add to outgoing mail and Gnus postings.
Default is nil for no comment.  Set to:

    (format \"Comments: GNU Hyperbole mail buttons accepted, v%s.\n\"
                hyperb:version)

for a comment.")

;; Used by 'message-send' in Emacs "message.el".
(add-hook 'message-send-hook  #'smail:widen)
;; Used by 'mail-send' in Emacs "sendmail.el".
(add-hook 'mail-send-hook  #'smail:widen)

;; For compatibility with Supercite and GNU Emacs.
;; message-mode defines message-yank-prefix - mail-yank-prefix removed
;; message-mode defines message-indentation-spaces - mail-indentation-spaces removed
;; message-mode defines message-reply-buffer - mail-reply-buffer removed

;;; ************************************************************************
;;; Overloaded functions
;;; ************************************************************************

(defun smail:comment-add (&optional comment-form)
  "Add a comment to the current outgoing message.
Only if Hyperbole has been loaded and `inhibit-hyperbole-messaging' is nil.
Optional COMMENT-FORM is evaluated to obtain the string to add to the
message.  If not given, `smail:comment' is evaluated by default."
  (if (and (featurep 'hyperbole) (not inhibit-hyperbole-messaging))
      (let ((comment (eval (or comment-form smail:comment) t)))
	(if comment
	    (save-excursion
	      (goto-char (point-min))
	      (and (or (search-forward mail-header-separator nil t)
		       (if (derived-mode-p 'mh-letter-mode)
			   (search-forward "\n--------" nil t)))
		   (not (search-backward comment nil t))
		   (progn (beginning-of-line) (insert comment))))))))

(defun smail:widen ()
  "Widens outgoing mail buffer to include Hyperbole button data."
  (if (fboundp #'mail+narrow) (mail+narrow) (widen)))

;; Redefine this function from "message.el" to include Hyperbole button
;; data when yanking in a message and to highlight buttons if possible.
(defun message--yank-original-internal (arg)
  "See `message-yank-original'.

If supercite is in use, header fields are never deleted.
Use (setq sc-nuke-mail-headers \\='all) to have them removed."
  (let ((modified (buffer-modified-p))
	body-text)
	(when (and message-reply-buffer
		   message-cite-function)
	  (when (equal message-cite-reply-position 'above)
	    (save-excursion
	      (setq body-text
		    (buffer-substring (message-goto-body)
				      (point-max)))
	      (delete-region (message-goto-body) (point-max))))
	  (when (bufferp message-reply-buffer)
	    (delete-windows-on message-reply-buffer t)
	    (with-current-buffer message-reply-buffer
	      ;; Hyperbole addition: Might be called from newsreader
	      ;; before any Hyperbole mail reader support has been autoloaded.
	      (cond ((fboundp 'rmail:msg-widen) (rmail:msg-widen))
		    ((eq major-mode 'news-reply-mode) (widen)))
	      (hmail:msg-narrow)
	      (when (fboundp 'hproperty:but-create)
		(hproperty:but-create))))
	  (push-mark (save-excursion
		       (cond
			((bufferp message-reply-buffer)
			 (insert-buffer-substring message-reply-buffer))
			((and (consp message-reply-buffer)
			      (functionp (car message-reply-buffer)))
			 (apply (car message-reply-buffer)
				(cdr message-reply-buffer))))
		       (unless (bolp)
			 (insert ?\n))
		       (point)))
	  (unless arg
	    (funcall message-cite-function)
	    (unless (eq (char-before (mark t)) ?\n)
	      (let ((pt (point)))
		(goto-char (mark t))
		(insert-before-markers ?\n)
		(goto-char pt))))
	  (pcase message-cite-reply-position
	    ('above
	     (message-goto-body)
	     (insert body-text)
	     (insert (if (bolp) "\n" "\n\n"))
	     (message-goto-body))
	    ('below
	     (message-goto-signature)))
	  ;; Hyperbole addition
	  (when (bufferp message-reply-buffer)
	    (with-current-buffer message-reply-buffer
	      (hmail:msg-narrow)))
	  ;; Add a `message-setup-very-last-hook' here?
	  ;; Add `gnus-article-highlight-citation' here?
	  (unless modified
            (setq message-checksum (message-checksum))))))


;; Redefine this function from "sendmail.el" to include Hyperbole button
;; data when yanking in a message and to highlight buttons if possible.
(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in Rmail).
Put point before the text and mark after.

Normally indent each nonblank line ARG spaces (default 3).
However, if `mail-yank-prefix’ is non-nil, insert that prefix
on each line when `mail-indent-citation' is called.

Apply `mail-citation-hook', `mail-yank-hook' or `mail-yank-hooks'
to text (in decreasing order of precedence).

Just \\[universal-argument] as argument means don't indent,
insert no prefix, and don't delete any header fields.

If supercite is in use, header fields are never deleted.
Use (setq sc-nuke-mail-headers \\='all) to have them removed."
  (interactive "P")
  (if mail-reply-action
      (let ((start (point))
	    (original mail-reply-action)
	    (opoint (point))
	    (omark (mark t)))
	(and (consp original) (eq (car original) 'insert-buffer)
	     (setq original (nth 1 original)))
	(unwind-protect
	    (progn
	      (if (consp original)
		  (progn
		    ;; Call yank function, and set the mark if it doesn't.
		    (apply (car original) (cdr original))
		    (if (eq omark (mark t))
			(push-mark (point))))
		;; If the original message is in another window in the same
		;; frame, delete that window to save space.
		(delete-windows-on original t)
		(with-current-buffer original
		  ;; Hyperbole addition: Might be called from newsreader
		  ;; before any Hyperbole mail reader support has been autoloaded.
		  (cond ((fboundp 'rmail:msg-widen) (rmail:msg-widen))
			((eq major-mode 'news-reply-mode) (widen))))
		(with-no-warnings
		  ;; We really want this to set mark.
		  (insert-buffer original)
		  ;; If they yank the original text, the encoding of the
		  ;; original message is a better default than
		  ;; the default buffer-file-coding-system.
		  (and (coding-system-equal
			(default-value 'buffer-file-coding-system)
			buffer-file-coding-system)
		       (setq buffer-file-coding-system
			     (coding-system-change-text-conversion
			      buffer-file-coding-system
			      (coding-system-base
			       (with-current-buffer original
				 buffer-file-coding-system))))))
		(set-text-properties (point) (mark t) nil))
	      ;; Hyperbole addition
	      (hmail:msg-narrow)
	      (when (fboundp 'hproperty:but-create)
		(hproperty:but-create))
	      (unless (consp arg)
		;; Don't ever remove headers if user uses Supercite package,
		;; since he can set an option in that package to do
		;; the removal.
		(or (hypb:supercite-p)
		    (mail-yank-clear-headers
		     start (marker-position (mark-marker))))
		(goto-char start)
		(let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
						 mail-indentation-spaces))
		      ;; Avoid error in Transient Mark mode
		      ;; on account of mark's being inactive.
		      (mark-even-if-inactive t))
		  (cond ((and (boundp 'mail-citation-hook) mail-citation-hook)
			 ;; Bind mail-citation-header to the inserted message's header.
			 (let ((mail-citation-header
				(buffer-substring-no-properties
				 start
				 (save-excursion
				   (save-restriction
				     (narrow-to-region start (point-max))
				     (goto-char start)
				     (rfc822-goto-eoh)
				     (point))))))
			   (run-hooks 'mail-citation-hook)))
			((and (boundp 'mail-yank-hook) mail-yank-hook)
			 (run-hooks 'mail-yank-hook))
			((and (boundp 'mail-yank-hooks) mail-yank-hooks)
			 (run-hooks 'mail-yank-hooks))
			(t (mail-indent-citation))))
		(goto-char (min (point-max) (mark t)))
		(set-mark opoint)
		(delete-region (point)	; Remove trailing blank lines.
			       (progn (re-search-backward "[^ \t\n\r\f]")
				      (end-of-line)
				      (point))))
	      (unless (eq major-mode 'news-reply-mode)
		;; This is like exchange-point-and-mark, but doesn't activate the mark.
		;; It is cleaner to avoid activation, even though the command
		;; loop would deactivate the mark because we inserted text.
		(goto-char (prog1 (mark t)
			     (set-marker (mark-marker)
					 (point) (current-buffer))))
		(unless (eolp)
		  (insert ?\n))))
	  ;; Hyperbole addition
	  (hmail:msg-narrow)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;;; Try to setup comment addition as the first element of these hooks.
(add-hook 'message-setup-hook  #'smail:comment-add)
(add-hook 'mh-letter-mode-hook #'smail:comment-add)

(provide 'hsmail)

;;; hsmail.el ends here
