;;; hgnus-mail.el --- Gnus mailer support                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    17-Dec-22 at 22:04:19
;; Last-Mod:     18-Feb-23 at 10:31:45 by Mats Lidell
;;
;; Copyright (C) 2023  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'gnus-sum)
(require 'gnus-art)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun Gnus-mail-init ()
  "Initialize Hyperbole support for Gnus mail reading."
  (interactive)
  (setq hmail:compose-mail-func 'hgnus-mail--message-mail-other-window
        hmail:composer  'message-mode
	hmail:lister    'gnus-summary-mode
	hmail:modifier  'message-mode
	hmail:reader    'gnus-article-mode)
  ;;
  ;; Setup public abstract interface to Hyperbole defined mail
  ;; reader-specific functions used in "hmail.el".
  ;;
  (rmail:init)
  ;;
  ;; Setup private abstract interface to mail reader-specific functions
  ;; used in "hmail.el".
  ;;
  (defalias 'rmail:get-new       #'gnus-group-get-new-news)
  (defalias 'rmail:msg-forward   #'gnus-summary-mail-forward)
  (defalias 'rmail:summ-msg-to   #'gnus-summary-show-article)
  (defalias 'rmail:summ-new      #'gnus-summary-rescan-group)
  (when (called-interactively-p 'interactive)
      (message "Hyperbole Gnus mail reader support initialized.")))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hgnus-mail--message-mail-other-window (_noerase to)
  "Open mail composer in other window with field TO set.
_NOERASE is for compatibility with `mail-other-window' type of
mail functions."
  (gnus-msg-mail to nil nil nil #'switch-to-buffer-other-window))

(defalias 'Gnus-Summ-goto #'gnus-summary-show-article)
(defalias 'Gnus-msg-next #'gnus-article-goto-next-page)

(defun Gnus-Summ-delete ()
  "Mark article for process so it can be expunged later."
  (gnus-summary-mark-as-processable 1))

(defun Gnus-Summ-expunge ()
  "Delete all articles with a process mark."
  (let ((gnus-novice-user t))
    (gnus-summary-delete-article)))

(defun Gnus-Summ-undelete-all ()
  "Undelete all messages."
  (error "Sorry.  Deleted messages can't be undeleted"))

(provide 'hgnus-mail)
;;; hgnus-mail.el ends here
