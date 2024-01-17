;;; hsys-www.el --- GNU Hyperbole support for Emacs World-Wide Web (WWW) browsing  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     7-Apr-94 at 17:17:39 by Bob Weiner
;; Last-Mod:     18-Jan-24 at 23:59:15 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1994-2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This module defines an implicit button type and associated action and
;;   help types.  A press of the Action Key on a unified resource locator
;;   (URL) displays the referent for the URL.  A press of the Help Key on a
;;   URL displays what action the Action Key will take when pressed.
;;
;;   Customize the web browser used by setting, `browse-url-browser-function'
;;   to a function that invokes the desired browser on the URL.  It
;;   may be set from the Hyperbole Customization menu.  This menu also
;;   includes a setting for whether the browser reuses windows or
;;   generates new ones.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hload-path)
;;; This does not require any particular web browser.
(require 'browse-url)
(require 'eww) ;; Must load to override it's function `eww-browse-url' below.
(require 'hbut)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar hpath:display-where-alist)      ; "hpath.el"

(declare-function hpath:remote-available-p "hpath")
(declare-function hpath:remote-p "hpath")
(declare-function hpath:remote-at-p "hpath")
(declare-function hpath:www-at-p "hpath")

;; Forward declare conditionally defined functions
(declare-function eww-history-property "hsys-www")
(declare-function eww-bookmark-property "hsys-www")
(declare-function eww-link-at-point "hsys-www")
(declare-function shr-link-at-point "hsys-www")

;;; ************************************************************************
;;; Public functions and types
;;; ************************************************************************

;; eww-mode should define these next functions but presently does not,
;; so define them here when needed.
(unless (fboundp 'eww-link-at-point)
  (defun shr-link-at-point ()
    "Return any shr hyperlink url at point or nil if none."
    (get-text-property (point) 'shr-url))
  (defun eww-link-at-point ()
    "Return any eww web page hyperlink url at point or nil if none."
    (shr-link-at-point))
  (defun eww-bookmark-property (property)
    "Return value of PROPERTY, a symbol, for current eww bookmark line or nil."
    (if (eq major-mode 'eww-bookmark-mode)
	(plist-get (get-text-property (line-beginning-position) 'eww-bookmark)
		   property)))
  (defun eww-history-property (property)
    "Return value of PROPERTY, a symbol, for current eww history line or nil."
    (if (eq major-mode 'eww-history-mode)
	(plist-get (get-text-property (line-beginning-position) 'eww-history)
		   property))))

(defib www-url ()
  "Follow any non-ftp url (link) at point.
The variable, `browse-url-browser-function', customizes the url browser that
is used.
Valid values of this variable include `browse-url-default-browser' and
`browse-url-generic'."
  (cond ((looking-at "\\s-*\\'")
	 ;; Don't match if at the end of the buffer; end of line is
	 ;; handled elsewhere.
	 nil)
	((and (eq major-mode 'eww-mode) (eww-link-at-point))
	 (ibut:label-set (eww-link-at-point))
	 (hact 'eww-follow-link))
	((eq major-mode 'eww-bookmark-mode)
	 (ibut:label-set (concat (eww-bookmark-property :title)
				 (if (eww-bookmark-property :url)
				     (concat " <" (eww-bookmark-property :url) ">"))))
	 (hact 'eww-bookmark-browse))
	((eq major-mode 'eww-history-mode)
	 (ibut:label-set (concat (eww-history-property :title)
				 (if (eww-history-property :url)
				     (concat " <" (eww-history-property :url) ">"))))
	 (hact 'eww-history-browse))
	(t (let ((link-and-pos (hpath:www-at-p t)))
	     ;; Skip ftp URLs which are handled elsewhere.
	     (if (and link-and-pos (not (hpath:remote-at-p)))
		 (progn (ibut:label-set link-and-pos)
			(hact 'www-url (car link-and-pos))))))))

(defact www-url (url)
  "Follow a link given by URL.
The variable, `browse-url-browser-function', customizes the url browser that
is used.  Valid values of this variable include `browse-url-default-browser' and
`browse-url-generic'."
  (interactive "sURL to follow: ")
  (or (stringp url)
      (error "(www-url): URL = `%s' but must be a string" url))
  (if (or (functionp browse-url-browser-function)
	  ;; May be a predicate alist of functions from which to select
	  (consp browse-url-browser-function))
      (let (browse-function-name
	    browser)
	(if (symbolp browse-url-browser-function)
	    (setq browse-function-name (symbol-name browse-url-browser-function)
		  browser (and (string-match
				"-\\([^-]+\\)\\'"
				browse-function-name)
			       (capitalize (substring browse-function-name
						      (match-beginning 1)
						      (match-end 1)))))
	  (setq browser "default browser"))
	(message "Sending %s to %s..." url browser)
	(browse-url url)
	(message "Sending %s to %s...done" url browser))
    (error "(www-url): `browse-url-browser-function' must be set to a web browser invoking function")))

;;;###autoload
(defun www-url-expand-file-name (path &optional dir)
  "Expand and return non-url and non-remote PATH in DIR.
Return http urls unchanged.  Normalize remote paths."
  (when (listp path)
    (setq path (car path)
	  dir  (car (cdr path))))
  (if (string-match "\\`www\\.\\|\\`https?:" path)
      path
    (require 'hpath)
    (or (hpath:remote-p path)
	(expand-file-name path dir))))

;;;###autoload
(defun www-url-find-file-noselect (path &rest args)
  "Find PATH without selecting its buffer.
Handle http urls.  ARGS is the optional arguments to
`find-file-noselect'.  If PATH is a list ARGS is set to remainder
after that the first element is extracted as the PATH."
  (if (listp path)
      (setq args (cdr path)
	    path (car path)))
  (let* ((remote-sym (hpath:remote-available-p))
	 (inhibit-file-name-handlers
	  (if remote-sym
	      (append (list 'dired-handler-fn
		       (intern-soft (concat (symbol-name remote-sym)
					    "-file-handler-function")))
		      (and (eq inhibit-file-name-operation 'find-file-noselect)
			   inhibit-file-name-handlers))
	    inhibit-file-name-handlers))
	 (inhibit-file-name-operation 'find-file-noselect))
    (if (string-match "\\`www\\.\\|\\`https?:" path)
	(progn (require 'hyperbole)
	       ;; Display url.
	       (hact 'www-url path)
	       ;; return same buffer
	       (current-buffer))
      (apply #'find-file-noselect path args))))

;;;###autoload
(defun eww-browse-url (url &optional new-window)
  "Ask the eww browser to load URL.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new buffer tab on the window tab-line.  A non-nil
prefix argument reverses the effect of `browse-url-new-window-flag'.

If `tab-bar-mode' is enabled, then whenever a document would
otherwise be loaded in a new buffer, it is loaded in a new tab
in the tab-bar on an existing frame.  See more options in
`eww-browse-url-new-window-is-tab'.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (when (or (eq eww-browse-url-new-window-is-tab t)
            (and (eq eww-browse-url-new-window-is-tab 'tab-bar)
                 tab-bar-mode))
    (let ((tab-bar-new-tab-choice t))
      (tab-new)))
  (let ((hpath:display-where-alist
	 (if new-window 'other-window hpath:display-where-alist)))
    (hpath:display-buffer
     (generate-new-buffer
      (format "*eww-%s*" (url-host (url-generic-parse-url
                                    (eww--dwim-expand-url url)))))))
  (eww-mode)
  (eww url))


(provide 'hsys-www)

;;; hsys-www.el ends here
