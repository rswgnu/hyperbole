;;; hsettings.el --- GNU Hyperbole settings which may require customization  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Apr-91 at 00:48:49
;; Last-Mod:      1-Aug-22 at 21:34:56 by Mats Lidell
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   See the "INSTALL" file for installation instructions and the
;;   "README" file for general information.
;;
;;   Be sure to have users load any personal mail/news personalizations
;;   before they load Hyperbole so that Hyperbole's mail or news
;;   support features work as desired.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hversion)
(require 'hvar)
(require 'browse-url)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************
(declare-function hproperty:but-create "hui-em-but")

;;; Read the comments and modify as desired.

;;; ************************************************************************
;;; TIMEZONE SETTING
;;; ************************************************************************

;; The following section applies only to MS-DOS and MS-Windows OSs.
;; Users of other OSs may simply ignore this section.

;; Some versions of Microsoft OSs don't automatically set the
;; timezone so that Hyperbole can read it.  Nor do they include a
;; UNIX-style date program.  So follow the commented instructions in
;; the code below here.

;; If Hyperbole loads without error, then your system sets the
;; timezone properly and you need not do anything.  If you receive a
;; timezone error, simply follow the instructions below to set the
;; timezone manually and then reload Hyperbole.
(if (and hyperb:microsoft-os-p
	 (require 'htz)
	 (not (stringp htz:local)))
    (progn
      ;; Comment out the following `error' line...
      (error "(hsettings.el): Configure the TIMEZONE SETTING section in this file")
      ;; ... and uncomment the following line, substituting an appropriate
      ;;     timezone from the list in the variable, `htz:world-timezones'
      ;;     in the file, "htz.el".
      ;;   (setenv "TZ" "your-3char-timezone")
      ))

;;; ************************************************************************
;;; SMART SETTINGS FOR THE ACTION AND ASSIST KEYS AND SMART MENUS
;;; ************************************************************************

;; The following setting allows direct selection of Helm completion
;; entries with the Smart Mouse Keys.  Otherwise, by default, helm
;; disables all mouse keys while completing.  This setting requires
;; the 'global_mouse' branch of the Helm git development tree because
;; it has many changes for Hyperbole mouse support.  Otherwise, the
;; setting will do nothing.
;; Find it at: "https://github.com/rswgnu/helm/tree/global_mouse".
;;
;; If you change the value of this setting (e.g. simply comment it out),
;;  you must restart Emacs for it to take full effect.  Setting this to
;; 't' or 'nil' will not provide Hyperbole support.
(eval-after-load "helm" '(if (boundp 'helm--disable-mouse-mode)
			     ;; Using Helm version heavily modified for
			     ;; Hyperbole Smart Key use.
			     (setq helm-allow-mouse 'global-mouse-bindings)))

;; The Smart Menu system is an attractive in-buffer menu system that
;; predates Emacs menu systems; it is included in InfoDock.
(defvar hkey-always-display-menu nil
  "*Non-nil means a Smart Key press pops up the Smart Menu window.
The Smart Menu system must have already been loaded.  If a Smart
Menu is already displayed, perform another Action or Assist Key function.")

(defcustom hmouse-middle-flag (and (boundp 'infodock-version) infodock-version t)
  "*Under InfoDock or when t, additionally bind the middle mouse button as an
Action Key."
  :type 'boolean
  :group 'hyperbole-keys)

(defcustom smart-scroll-proportional t
  "*Non-nil means Smart Key scroll behavior is relative to current line.
Smart Keys will scroll relative to current line when pressed at
the end of a line.  Action Key moves current line to top of the
window.  Assist Key moves current line to bottom of the window.
Repeated presses then scroll up or down a windowful.  Nil value
ignores current line and always scrolls up or down a windowful."
  :type 'boolean
  :group 'hyperbole-keys)

;;; ************************************************************************
;;; INTERNET SETTINGS
;;; ************************************************************************

;; String to be used in the call: (hpath:rfc rfc-num) to create a remote
;; path to the RFC document for `rfc-num'.  Uncomment and alter this setting
;; if another site is closer for you.
;; (setq hpath:rfc "/anonymous@ftp.ietf.org:rfc/rfc%s.txt")

;; When a user creates an explicit button or edits a Koutline, Hyperbole
;; tries to store her Internet e-mail address from the variable,
;; `user-mail-address'.  This should be set in your personal Emacs
;; initialization file, "~/.emacs" with a line like so:
;;
;;   (setq user-mail-address "your-email@address.com")
;;
;; and Hyperbole should be loaded after this setting is made.

;; Web search setttings for Hyperbole Find/Web menu.
(require 'browse-url)

(defun hyperbole-update-menus ()
  "Rebuild all Hyperbole menus with any updated settings."
  (hyperbole-menubar-menu)
  (hyperbole-minibuffer-menu))

(defcustom hyperbole-default-web-search-term-max-lines 2
  "Provide a default search term using the selected text if the
active region contains less than or equal to this number of
lines"
  :type 'integer
  :group 'hyperbole-commands)

(defun hyperbole-default-web-search-term ()
  "Return a default search term if region is active and not too large."
  (and (region-active-p)
       (<= (count-lines (region-beginning) (region-end))
	   hyperbole-default-web-search-term-max-lines)
       (buffer-substring-no-properties (region-beginning) (region-end))))

(defun hyperbole-read-web-search-arguments (&optional service-name search-term)
  "Read from the keyboard a list of (web-search-service-string search-term-string).
With optional non-empty SERVICE-NAME and SEARCH-TERM arguments,
use those instead of reading from the keyboard."
  (let ((completion-ignore-case t))
    (while (or (not (stringp service-name)) (equal service-name ""))
      (setq service-name (completing-read "Search service: " hyperbole-web-search-alist
					  nil t)))
    (while (or (not (stringp search-term)) (equal search-term ""))
      (setq search-term (read-string (format "Search %s for: " service-name)
				     (hyperbole-default-web-search-term))))
    (list service-name search-term)))

(defun hyperbole-web-search (&optional service-name search-term return-search-expr-flag)
  "Search web SERVICE-NAME for SEARCH-TERM.
Both arguments are optional and are prompted for when not given or when null.
Uses `hyperbole-web-search-alist' to match each service to its search url.
Uses `hyperbole-web-search-browser-function' and the `browse-url'
package to display search results."
  (interactive)
  (cl-multiple-value-bind (service-name search-term)
      (hyperbole-read-web-search-arguments service-name search-term)
    (let ((browse-url-browser-function hyperbole-web-search-browser-function)
	  (search-pat (cdr (assoc service-name hyperbole-web-search-alist
				  (lambda (service1 service2)
				    (equal (downcase service1) (downcase service2)))))))
      (setq search-term (browse-url-url-encode-chars search-term "[*\"()',=;?% ]"))
      (if return-search-expr-flag
	  (cond ((stringp search-pat)
		 (format search-pat search-term))
		((functionp search-pat)
		 (list search-pat search-term))
		(t (user-error "(Hyperbole): Invalid web search service `%s'" service-name)))
	(cond ((stringp search-pat)
	       (browse-url (format search-pat search-term)))
	      ((functionp search-pat)
	       (funcall search-pat search-term))
	      (t (user-error "(Hyperbole): Invalid web search service `%s'" service-name)))))))

;; This must be defined before the defcustom `inhbit-hyperbole-messaging'.
;;;###autoload
(defun hyperbole-toggle-messaging (&optional arg)
  "Toggle Hyperbole support for explicit buttons in mail and news buffers.
Toggle the boolean variable `inhibit-hyperbole-messaging’ and either
add hooks (nil value) or remove them (t value).

With optional prefix ARG > 0, enable support.  If ARG <= 0,
disable/inhibit support."
  (interactive "P")
  (setq inhibit-hyperbole-messaging (if (null arg)
					(not inhibit-hyperbole-messaging)
				      (<= (prefix-numeric-value arg) 0)))
  (if inhibit-hyperbole-messaging
      (var:remove-all)
    (var:append-all)
    ;; Add any hooks that were skipped when inhibit-hyperbole-messaging
    ;; was nil.
    (cond ((boundp 'hyperbole-loading))
	  ((not after-init-time)
	   (add-hook 'after-init-hook (lambda () (load "hyperbole"))))
	  (t (load "hyperbole"))))
  (if (called-interactively-p 'interactive)
      (message "Hyperbole messaging button support is %s"
	       (if inhibit-hyperbole-messaging "disabled" "enabled"))))

(defcustom inhibit-hyperbole-messaging t
  "*Determine whether Hyperbole supports explicit buttons in mail and news buffers.
The default of t means disable such support (work remains to
modernize these features).  When t, Hyperbole will not alter
messaging mode hooks nor overload functions from these packages,
preventing potential incompatibilities.

If you want to use Hyperbole buttons in mail and news buffers, set
this variable to nil by adding (hyperbole-toggle-messaging 1)
to your personal Emacs initialization file, prior to loading
Hyperbole, and then restart Emacs."
  :type 'boolean
  :initialize #'custom-initialize-set
  :set (lambda (_symbol value)
	 ;; Invert value to produce ARG for hyperbole-toggle-messaging.
	 (hyperbole-toggle-messaging (if value 0 1)))
  :group 'hyperbole-buttons)

(defcustom hyperbole-web-search-browser-function browse-url-browser-function
  "*Function of one url argument called by any Hyperbole Find/Web search."
  :type 'function
  :group 'hyperbole-commands)

(defcustom hyperbole-web-search-alist
  '(("Amazon" . "http://www.amazon.com/s/field-keywords=%s")
    ("Bing" . "http://www.bing.com/search?q=%s")
    ;; Wikipedia Dictionary
    ("Dictionary" . "https://en.wiktionary.org/wiki/%s")
    ("Elisp" . "http://www.google.com/search?q=%s+filetype:el")
    ;; Facebook Hashtags
    ("Facebook" . "https://www.facebook.com/hashtag/%s")
    ;; To search for a Facebook user, use "https://www.facebook.com/%s".
    ("Google" . "http://www.google.com/search?q=%s")
    ("gitHub" . "https://github.com/search?ref=simplesearch&q=%s")
    ("Images" . "http://www.google.com/images?hl=en&q=%s")
    ("Jump"   . webjump)
    ("Maps" . "http://maps.google.com/maps?q=%s")
    ("RFCs" . "https://tools.ietf.org/html/rfc%s")
    ("StackOverflow" . "https://stackoverflow.com/search?q=%s")
    ("Twitter" . "https://twitter.com/search?q=%s")
    ("Wikipedia" . "https://en.wikipedia.org/wiki/%s")
    ("Youtube" . "https://www.youtube.com/results?search_query=%s"))
  "*Alist of (web-service-name . emacs-cmd-or-url-with-%s-parameter) elements.
The first capitalized character of each web-service-name must be unique.
This custom option is used in the Hyperbole Find/Web menu where
the %s in the url-with-%s-parameter is replaced with an interactively
obtained search term; if second argument is a command instead, then
it is called interactively to prompt for the search term with which it
then runs the search."
  :initialize #'custom-initialize-default
  :set (lambda (option value)
	 (set option value)
	 (hyperbole-update-menus))
  :type '(alist :key-type string :value-type (choice string symbol))
  :group 'hyperbole-commands)

;;; ************************************************************************
;;; GNU EMACS CONFIGURATION
;;; ************************************************************************

;; No-op unless set by one of the conditionals below.
(defun hui:but-flash ())

(cond ((not noninteractive)
       (require 'hui-em-but)
       ;; Highlight explicit buttons whenever a file is read in.
       (add-hook 'find-file-hook #'hproperty:but-create t)
       (defalias 'hui:but-flash #'hproperty:but-flash)))

;;; ************************************************************************
;;; ONLINE LIBRARY CONFIGURATION
;;; ************************************************************************

;; Support for online library document id references is loaded here but
;; requires some additional configuration before use.  See the DESCRIPTION
;; section in "hib-doc-id.el" for complete installation and use information.
;;
(add-hook 'hibtypes-end-load-hook (lambda () (require 'hib-doc-id)))

;;; ************************************************************************
;;; SITE-SPECIFIC ADDITIONS - Add your Hyperbole configuration additions here.
;;; ************************************************************************

;;; ************************************************************************
;;; END OF HYPERBOLE SITE-SPECIFIC CUSTOMIZATIONS
;;; ************************************************************************

(provide 'hsettings)

;;; hsettings.el ends here
