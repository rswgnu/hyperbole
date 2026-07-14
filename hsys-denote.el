;;; hsys-denote.el --- GNU Hyperbole support functions for denote note-taking -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     2-Jul-16 at 14:54:14
;; Last-Mod:     13-Jul-26 at 23:57:36 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  Denote is a note-taking package with an efficient file-naming scheme.
;;  See <describe-package 'denote>.  This library together with "hywiki.el"
;;  interfaces denote to Hyperbole via a link action and support functions.

;;; Code:

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hywiki) ;; requires "hpath" and "hypb".

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function denote-extract-id-from-string "ext:denote")
(declare-function denote-file-prompt "ext:denote")
(declare-function denote-get-link-description "ext:denote")
(declare-function denote-get-path-by-id "ext:denote")

(defvar denote-date-identifier-regexp)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defib denote-link ()
  "If on a denote id with optional #section, jump to its associated note.
The id must be prefixed with \"denote:\" and can be in double quotes if the
section contains spaces (though '-' is preferred instead of space so that
Org mode links are highlighted and selected properly).

Two forms of denote links are accepted:

  1. The hyperbole link form with syntax:
        \\(<[Description]>\\)? \"denote:<file-id>\\(#section-name-or-uuid\\)?\"

  2. The org link form with syntax:
        [[denote:<file-id>\\(::*?section-name\\)?][Description]]
     or [[denote:<file-id>\\(::#?section-id\\)?][Description]]"
  (let* ((id-start-end (hsys-denote-link-at-p))
         (id-reference (nth 0 id-start-end))
         (start (nth 1 id-start-end))
         (end (nth 2 id-start-end)))
    (when id-start-end
      (ibut:label-set id-reference start end)
      (hact 'link-to-denote id-reference))))

(defun hys-denote-get-link-regexp ()
  "Return regexp matching a denote id plus optional #section implicit button.
Fail with an error when the denote library has not been required
by the caller.

Group 1 is the denote file id;
Group 3 is the optional #section or ::[#*]?section referring to a section
heading within the file.
Group 4 is the '#' or '::[#*]?' section prefix.
Group 5 is the section without the prefix."
  ;; Note: this must stay as a function and not a constant since the denote
  ;; variable used herein may not be defined until some other function is
  ;; called.
    (concat
     "denote:" denote-date-identifier-regexp
     "\\(\\(::[#*]?\\|#\\)\\([^][(){}<>'`\"*#:; \t\n\r\f][^][ \t\n\r\f\"]*\\)\\)?"))

(defun hsys-denote-link-at-p (&optional org-link-start org-link-end)
  "If in a denote link, return (link-string link-start link-end), else nil.
Start and end positions exclude any delimiters and any description part of a
link.  Optional ORG-LINK-START and ORG-LINK-END include any delimiters;
this means point is within an Org link but still need to check if it is a
denote link."
  (when (require 'denote nil t)
    (let (link-start-end
          start
          end)
      (cond
       ;; Previously detected Org link
       ((and (integerp org-link-start) (integerp org-link-end)
             (save-excursion
               (goto-char org-link-start)
               (looking-at "\\(\\[\\[\\)?\\(denote:[^][\n\r\f\"]+\\)")))
        (setq start (match-beginning 2)
              end (min (match-end 2) org-link-end))
        (list (buffer-substring-no-properties start end) start end))

       ;; String delimited
       ((setq link-start-end (hypb:in-string-p 1 :range))
        (when (string-match-p "\\`denote:" (car link-start-end))
          link-start-end))

       ;; Org link (may or may not have square bracket delimiters); if does
       ;; not, then can't allow spaces in the section name
       ((setq link-start-end (hsys-org-link-at-p)
              org-link-start (nth 0 link-start-end)
              org-link-end (nth 0 link-start-end))
        (when (save-excursion
                (goto-char org-link-start)
                (or
                 ;; Don't allow section spaces
                 (looking-at "\\(\\)\\(denote:[^][ \t\n\r\f\"]+\\)")
                 ;; Do allow section spaces
                 (looking-at "\\(\\[\\[\\)\\(denote:[^][\n\r\f\"]+\\)")))
          (list (match-string-no-properties 2) (match-beginning 2) (match-end 2))))

       ;; Non-delimited
       (t (let ((id-regexp (hys-denote-get-link-regexp))
                (eol (line-end-position))
                (opoint (point))
                found)
            (save-excursion
              (goto-char (line-beginning-position))
              (while (and (not found) (re-search-forward id-regexp eol t))
                (when (and (<= (match-beginning 0) opoint)
                           (> (point) opoint))
                  (setq found t)))
              found)
            (when found
              ;; Return (id-reference ref-start ref-end)
              (list (substring-no-properties (match-string-no-properties 0))
                    (match-beginning 0) (match-end 0)))))))))

;;;###autoload
(defact link-to-denote (id-and-section &optional file)
  "Display a denote entry given by ID-AND-SECTION and optional FILE.
ID-AND-SECTION optionally may be prefixed with \"denote:\" or \"dn:\" and
may be suffixed with:

  1. #section or ::*section, where section is any exact match to a denote
     in-file heading;
or
  2. ::#section-id, where section-id is an Org id linking to a section.

Optional FILE is the denote file to display.  If given, the file is
displayed ignoring any information in ID-AND-SECTION.

This does nothing if denote has not been installed.  If installed
and ID-AND-SECTION or FILE is not found, trigger an error."
  (interactive
   (if (require 'denote nil t)
       (let ((file (denote-file-prompt
		    nil ;; for default: (denote-get-identifier-at-point)
		    "Get denote entry id" nil t)))
	 (if (stringp file)
	     (list (denote-extract-id-from-string file) file)
	   '(nil nil)))
     '(nil nil)))
  (when (require 'denote nil t)
    (if (and (stringp file) (not (string-empty-p file)))
        (if (file-readable-p file)
	    (hpath:find file)
	  (hypb:error "(link-to-denote): File is unreadable: \"%s\""
		      file))
      (if (stringp id-and-section)
	  ;; Remove any "denote:" or "dn:" prefix
	  (let ((file-id (denote-extract-id-from-string id-and-section))
                (section (when (and (string-match (hys-denote-get-link-regexp)
                                                  id-and-section)
                                    ;; section start, if any
                                    (match-beginning 5))
                           ;; Don't use `match-string-no-properties' here
                           ;; because it doesn't allow spaces in the section
                           ;; name
                           (substring-no-properties id-and-section
                                                    (match-beginning 5))))
                (section-prefix (match-string-no-properties 4 id-and-section)))
            (if (and section-prefix
                     ;; An Org section/heading id
                     (or (string-equal "::#" section-prefix)
                         (hsys-org-uuid-is-p section)))
                (hact 'link-to-org-id section)
	      (setq file (denote-get-path-by-id file-id))
              (if (and file (file-readable-p file))
                  ;; Change the section prefix to # since `hpath:find'
                  ;; only understands the # syntax
		  (hpath:find (if section (concat file "#" section) file))
	        (hypb:error "(link-to-denote): File from ID \"%s\" is unreadable: \"%s\""
			    id-and-section file))))
        (hypb:error "(link-to-denote): Invalid file ID and optional section: \"%s\""
                    id-and-section)))))

(defun hsys-denote-file-at-p ()
  "In a denote file or on a dired denote filename, return a link to it, else nil.
If in the file, add any section that point is within, plus relative line and
column number."
  (when (require 'denote nil t)
    (let ((file (or buffer-file-name
                    (and (derived-mode-p 'dired-mode)
                         (dired-get-filename nil t)))))
      (when (and file
                 ;; This ensures has a file-id
                 (denote-file-has-denoted-filename-p file))
        (let* ((file-id (denote-retrieve-filename-identifier file))
               (org-flag (derived-mode-p 'org-mode))
               (heading (when org-flag (org-get-heading)))
               (section-id (when org-flag (org-id-get))))
          (concat
           (when section-id
             (concat ibut:label-start heading ibut:label-end " "))
           "denote:" file-id
           (cond (section-id
                  (concat "#" section-id))
                 (heading
                  (concat "#" heading)))
           (when buffer-file-name
             (let ((line-num (if org-flag
                                 (count-lines
                                  (save-excursion
                                    (condition-case nil
                                        (progn (org-back-to-heading t)
                                               (point))
                                      (error (point-min))))
                                  ;; Without 1+, line count is often off by one
                                  (min (1+ (point)) (point-max)))
                               (current-line)))
                   (col-num (current-column)))
               (concat (unless (<= line-num 1)
                         (format ":L%d" line-num))
                       (unless (= col-num 0)
                         (format ":C%d" col-num)))))))))))

;;;###autoload
(defun hsys-denote-get-description-from-id (id)
  "Given a denote entry id, return its description string or nil if none."
  (denote-get-link-description
   (denote-get-path-by-id
    (denote-extract-id-from-string id))))

;;;###autoload
(defun hsys-denote-get-id-from-description (id)
  "Given a denote entry description, return its id or nil if none."
  (denote-get-link-description
   (denote-get-path-by-id
    (denote-extract-id-from-string id))))

(provide 'hsys-denote)
