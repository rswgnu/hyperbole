;;; hgnus-mail.el --- Gnus mailer support                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    17-Dec-22 at 22:04:19
;; Last-Mod:     17-Dec-22 at 23:45:46 by Mats Lidell
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; FIXME - Start to use common prefix such as hypb for file name and
;; functions?
;;
;; Would be:
;;   File: hypb-gnus-email.el
;;   Function: hypb-gnus-email-init

;;;###autoload
(defun Gnus-mail-init ()
  "Initialize Hyperbole support for Gnus mail reading."
  (interactive)
  (setq hmail:compose-mail-other-window 'hgnus-mail--message-mail-other-window
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
  (defalias 'rmail:get-new       'gnus-group-get-new-news)
  (defalias 'rmail:msg-forward   'gnus-summary-mail-forward)
  (defalias 'rmail:summ-msg-to   nil)   ;FIXME - What is this for?
  (defalias 'rmail:summ-new      nil)   ;FIXME - What is this for?
  (if (called-interactively-p 'interactive)
      (message "Hyperbole Gnus mail reader support initialized.")))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

;; FIXME - noerase arg is nil in the other invocation so really not
;; needed. We can use a local defun in the other case as well and so
;; the noerase can go away.
(defun hgnus-mail--message-mail-other-window (_noerase to)
  "Open mail composer in other window with field TO set."
  (gnus-msg-mail to nil nil nil #'switch-to-buffer-other-window))

(provide 'hgnus-mail.el)
;;; hgnus-mail.el ends here
