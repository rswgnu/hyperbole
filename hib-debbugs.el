;;; hib-debbugs.el --- Implicit button type for browsing GNU debbugs issues.  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Jun-16 at 14:24:53
;; Last-Mod:     24-Jul-22 at 10:41:16 by Bob Weiner
;;
;; Copyright (C) 2016, 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of Hyperbole.  It requires the Emacs package
;; Debbugs 0.9.7 or higher; there were bugs in earlier versions
;; that made it incompatible with the queries Hyperbole issues.
;;
;;; Commentary:
;;
;;   NOTE: Although Emacs now comes with the library "bug-reference-mode.el"
;;         which displays bug numbers, this library provides a much broader
;;         set of facilities and is activated via the Smart Keys, so no
;;         new key bindings are necessary.
;;
;;   Debbugs is a client-server issue tracker used by Gnu projects
;;   to manage issues and maintain threads of discussion around them.
;;
;;   This does nothing unless the third-party Emacs package debbugs
;;   0.9.7 or higher is installed.  Once it is installed
;;   and Hyperbole is loaded, the Smart Keys will generate debbugs
;;   queries when pressed within any of the following buffer text
;;   formats (with point prior to any attribute):
;;
;;      bug#id-number, bug# id-number, bug #id-number, or bug id-number
;;      bug?attr1=val1&attr2=val2&attr3=val3
;;      bug#id-number?attr1=val1&attr2=val2&attr3=val3
;;
;;   where the attr and val fields are sent as part of the debbugs query.
;;   Note that `issue' or `debbugs' may be used as well in place of `bug'.
;;
;;   A press of the Action Key on a Gnu debbugs string, will generate
;;   a debbugs query and display the result.  If the string represents a
;;   single debbugs id, Hyperbole will display the original submission
;;   message for that issue and will allow further browsing of the
;;   discussion.
;;
;;   A press of the Assist Key on a Gnu debbugs id, displays the subject
;;   and current status of the issue.  When within another type of Gnu
;;   debbugs query, it shows standard Hyperbole button help for the query.
;;
;;   Additionally, two query functions are provided:
;;
;;      (debbugs-gnu-query:list query-attribute-list) displays issues
;;   matching the set of (attribute . attribute-value) pairs in its argument.
;;
;;   For example:
;;      (debbugs-gnu-query:list '((status . "open") (package . "hyperbole")))
;;      (debbugs-gnu-query:list '((package . "hyperbole") (severity . "normal")))
;;
;;      (debbugs-gnu-query:string url-query-string) parses and applies
;;   attributes from a string of the form:
;;      bug[#id-number]?attr1=val1&attr2=val2&attr3=val3
;;   and uses it to display matching issues.
;;
;;   For example:
;;      (debbugs-gnu-query:string "bug?package=hyperbole&status=open")
;;      (debbugs-gnu-query:string "package=hyperbole&severity=normal")

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(hactypes)))
(eval-when-compile (require 'debbugs-gnu nil t))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(eval-after-load "debbugs-gnu"
  #'(progn (push "hyperbole"  debbugs-gnu-all-packages)
	   (push "oo-browser" debbugs-gnu-all-packages)))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function debbugs-get-status "ext:debbugs")
(declare-function debbugs-gnu-bugs "ext:debbugs")
(declare-function debbugs-gnu-current-id "ext:debbugs")
(declare-function debbugs-gnu-select-report "ext:debbugs")
(declare-function debbugs-gnu-show-reports "ext:debbugs")
(defvar debbugs-gnu-current-query)
(defvar debbugs-port)

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************

(defib debbugs-gnu-query ()
  "Display the results of a Debbugs query from a bug reference string around point.
This works in most types of buffers.  If the query includes a
single id number, display the original message submission for
that id and allow browsing of the followup discussion.  Accepts
the following buffer text formats (with point prior to any
attribute):

   bug#id-number or bug# id-number or bug #id-number
   bug?attr1=val1&attr2=val2&attr3=val3
   bug#id-number?attr1=val1&attr2=val2&attr3=val3

Note that `issue' or `debbugs' may be used as well in place of `bug'."
  (when (debbugs-version-sufficient-p)
    (when (debbugs-query:at-p)
      (if (and (match-beginning 3) (string-equal "?" (match-string 3)))
	  (hact 'debbugs-gnu-query:string (buffer-substring-no-properties
					   (or (match-beginning 1) (match-beginning 2))
					   (match-end 4)))
	(hact 'debbugs-gnu-query (string-to-number (match-string 2)))))))

(defact debbugs-gnu-query (id)
  "Display the discussion of Gnu debbugs ID (a positive integer)."
  (require 'debbugs-gnu)
  (when (debbugs-get-status id)
    (debbugs-gnu-bugs id)
    (debbugs-gnu-show-discussion)))

(defun debbugs-gnu-query:help (_but)
  "Make a Gnu debbugs id number at point display the pretty-printed bug status.
The id number can optionally be prefixed with a # sign.
Ignore other types of GNU debbugs query strings."
  (if (and (debbugs-version-sufficient-p)
	   (debbugs-query:at-p)
	   (match-beginning 2))
      (debbugs-query:status (string-to-number (match-string 2)))
    ;; Non-single issue query, show standard button help.
    (hkey-help t)))

(defib debbugs-gnu-mode ()
  "Make a Gnu Debbugs listing entry at point display the discussion on the issue."
  (if (eq major-mode 'debbugs-gnu-mode)
      (hact 'smart-debbugs-gnu)))

(defun debbugs-gnu-mode:help (&optional _but)
  "Make a Gnu debbugs listing at point pretty-print its status to a window below."
  (condition-case ()
      (let ((display-buffer-overriding-action
	     '(display-buffer-below-selected . nil)))
	(debbugs-query:status (debbugs-gnu-current-id))
	(hypb:maximize-window-height))
    (error nil)))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun debbugs-gnu-show-discussion ()
    (debbugs-gnu-select-report)
    ;; Display the 2nd message which is the initial bug report.  This
    ;; may be in Gnus or Rmail summary mode.
    (goto-char (point-min))
    (forward-line 1)
    (call-interactively (key-binding "\C-m")))

(defun debbugs-gnu-query:string (url-query-string)
  "Show the results of a Gnu debbugs query with attributes from URL-QUERY-STRING.
URL-QUERY-STRING must be a valid URL query string (part after the
question mark) of debbugs attributes and values,
i.e. \"attr1=val1&attr2=val2&attr3=val3\" URL encoded characters
are decoded.  An optional prefix of \"bug#<id-number>?\" may also
be included at the front of the string to limit the query to a
particular issue number.  Note that `issue' or `debbugs' may be
used as well in place of `bug'."
  (let* ((case-fold-search t)
	 (id (when (string-match "\\`\\(bug\\|debbugs\\|issue\\)\\s-?#?\\s-?\\(\\([1-9][0-9]*\\)\\|\\?\\)+"
				 url-query-string)
	       (prog1 (match-string 3 url-query-string)
		 (setq url-query-string (substring url-query-string (match-end 0))))))
	 attr-pair-list)
    ;; Change elements from lists to cons pairs.
    (setq attr-pair-list
	  (mapcar (lambda (elt) (cons (car elt) (cadr elt)))
		  (url-parse-query-string url-query-string))) ;; autoloaded
    (when id
      (push (cons 'bugs (list (string-to-number id))) attr-pair-list))
    (debbugs-gnu-query:list attr-pair-list)))

(defun debbugs-gnu-query:list (query-attribute-list)
  "Show the results of a Gnu debbugs query with QUERY-ATTRIBUTE-LIST attributes.
Each element of the list should be of the form (attribute . attribute-value).
Attribute may be a symbol or a string.  Common attributes
include: status, severity, and package."
  (require 'debbugs-gnu)
  (setq debbugs-gnu-current-query nil)
  (dolist (attr query-attribute-list)
    (add-to-list 'debbugs-gnu-current-query
		 (cons (if (symbolp (car attr))
			   (car attr)
			 (intern (car attr)))
		       (cdr attr))))
  (debbugs-gnu-show-reports))

(defun smart-debbugs-gnu ()
  "Display the discussion on the issue at point.
When the Action Key is pressed on a Gnu Debbugs listing entry."
  (debbugs-gnu-show-discussion))

;; (let ((entries (cdar tabulated-list-entries)))
;;   (cond ((= (length entries) 1)
;; 	 (hact 'debbugs-gnu-query
;; 	       (string-to-number (aref (nth (1- (line-number-at-pos (point))) entries) 0))))))
;; Each listed entry can be retrieved as a list of dotted pair attributes with:
;;   (tabulated-list-get-id (point))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun debbugs-query:at-p ()
  "Return t if point appears to be within a debbugs id.
Id number is (match-string 2).  If this is a query with attributes,
then (match-string 3) = \"?\" and (match-string 4) is the query
attributes." 
  ;; Point must be before one of the bug#222 characters to match.
  (let ((case-fold-search t))
    (when (string-match "[bugise#0-9]" (char-to-string (following-char)))
	(save-excursion
	  (skip-chars-backward "#0-9")
	  (skip-chars-backward " \t\n\r\f")
	  (skip-chars-backward "bugdiseBUGDISE#") ;; bug, debbugs or issue
	  ;; Allow for bug#222?package=hyperbole&severity=high as well as bug222, or bug#222.
	  (or (looking-at "[ \t\n\r\f]*\\(bug#?\\|debbugs#?\\|issue#?\\)[ \t\n\r\f]*#?\\([1-9][0-9]*\\)?\\(\\?\\)\\([a-z=&0-9%;()]+\\)")
	      (looking-at "[ \t\n\r\f]*\\(bug#?\\|debbugs#?\\|issue#?\\)[ \t\n\r\f]*#?\\([1-9][0-9]*\\)[\].,;?:!\)\>\}]?\\([ \t\n\r\f]\\|\\'\\)"))))))
	      ;; Ignore matches like  #222, so this is not confused with "hib-social.el" social references.
	      ;; (looking-at "[ \t\n\r\f]*\\(bug\\|debbugs\\|issue\\)?[ \t\n\r\f]*#\\([1-9][0-9]*\\)[\].,;?!\)\>\}]?\\([ \t\n\r\f]\\|\\'\\)")

(defun debbugs-query:status (id)
  "Pretty print to `standard-output' the status attributes of debbugs ID.
Debbugs ID is a positive integer.  Ignore nil valued attributes.
Return t unless no attributes are printed."
  (require 'debbugs-gnu)
  ;; The (car (debbugs-get-status id)) is a list of (attribute . value) pairs which we sort below.
  (let ((attrib-list
	 (sort (delq nil (mapcar (lambda (elt) (when (cdr elt) elt)) (car (debbugs-get-status id))))
	       (lambda (a b) (string-lessp (car a) (car b)))))
	(has-attr) attr len val)
    (unless (or (null attrib-list) (not (listp attrib-list)))
      (with-help-window "*Debbugs Help*"
	(princ (format "Status of %s %s package issue #%d (%s):\n"
		       (capitalize (substring debbugs-port 0 (string-match "\\." debbugs-port)))
		       (or (cadr (assq 'package attrib-list)) "")
		       id
		       (or (cdr (assq 'subject attrib-list)) "no subject")))
	(while (setq attr (car (car attrib-list)))
	  (setq val (cdr (car attrib-list))
		attrib-list (cdr attrib-list))
	  (when val
	    (setq has-attr t
		  len (number-to-string (max (- 16 (length (symbol-name attr))) 1)))
	    (princ (format (concat "   %s:%" len "s%S\n") attr " " val))))
	has-attr))))

(defun debbugs-version-sufficient-p ()
  "Return t iff debbugs version is sufficient for use with Hyperbole.
Must be greater than equal to 0.9.7."
  (save-excursion
    (let* ((debbugs-src (locate-file "debbugs" load-path '(".el")))
	   (visiting-debbugs-src (when debbugs-src (get-file-buffer debbugs-src)))
	   debbugs-src-buffer
	   version)
      (when debbugs-src
	(unwind-protect
	    (progn (set-buffer (setq debbugs-src-buffer (find-file-noselect debbugs-src)))
		   (widen)
		   (goto-char (point-min))
		   (when (re-search-forward "^;; Version: \\([.0-9]+\\)" nil t)
		     (setq version (match-string 1))))
	  (unless visiting-debbugs-src
	    (kill-buffer debbugs-src-buffer)))
	(when (and version (not (equal version "")))
	  (version-list-<= (version-to-list "0.9.7") (version-to-list version)))))))

(provide 'hib-debbugs)

;;; hib-debbugs.el ends here
