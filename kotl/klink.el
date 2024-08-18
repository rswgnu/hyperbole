;;; klink.el --- Implicit reference to a Koutline kcell  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Nov-93 at 12:15:16
;; Last-Mod:     18-Aug-24 at 09:42:48 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1993-2022  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;; link =
;;    < pathname [, cell-ref] [, position] >
;;    < @ cell-ref >  ;; In same buffer
;;    < journal-name, journal-item-number [, cell-ref] [, position] >
;;
;;; pathname =
;;    path   ;; display path in Emacs buffer
;;    !path  ;; execute pathname within a shell
;;    &path  ;; execute path as a windowed program
;;    -path  ;; Load as an Emacs Lisp program
;;
;;; cell-ref =
;;    cell - 1a, 012, 1.2, 1a=012 (both relative and absolute ids separated
;;                                 by an equal sign)
;;    range - 1a-5c, 1a-+3 (include 3 cells past 1a)  (not yet implemented)
;;    tree  - 1a+  (not yet implemented)
;;
;;   optionally followed by a period and 1 or more relative position specs
;;   (not yet implemented):
;;
;;    previous-cell - .b
;;    down-a-level - .d
;;    end-of-branch - .e
;;    follow-next-link - .l
;;    return-to-prev-location - .r
;;    return-to-prev-buffer - .rf
;;    sibling - .s, .2s for 2 siblings forward
;;    tail-of-tree - .t
;;    up-a-level - .u
;;    last char of cell - .f
;;
;;   and then optionally followed by any amount of whitespace, a pipe `|'
;;   character and then one or more view specification characters.  (Augment
;;   viewspec characters may be given instead, preceded by a colon.  They are
;;   ignored for now.)
;;
;;; position (relative to cell start) = (not yet implemented)
;;    char-pos, e.g. 28 or C28
;;    word-num, e.g. W5
;;    line-num, e.g. L2
;;    paragraph-num, e.g. P3
;;    regexp-match, e.g. "regexp"
;;

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'subr-x) ;; For string-trim
(require 'hmouse-tag) ;; For smart-c-include-regexp
(require 'hbut) ;; For defib.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom klink:ignore-modes
  '(amoccur-mode bash-ts-mode moccur-mode occur-mode shell-mode ssh-mode
                 telnet-mode term-mode)
  "Major modes in which to ignore potential klinks to avoid false positives."
  :type '(list function)
  :group 'hyperbole-koutliner)

(defcustom klink:c-style-modes
  '(c++-mode c++-ts-mode c-mode c-ts-mode java-mode java-ts-mode objc-mode)
  "C-related major modes with where klinks appear only within comments."
  :type '(list function)
  :group 'hyperbole-koutliner)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function kcell-view:label "kview")
(declare-function hbut:get-key-src "hbut")
(declare-function hbut:label-p "hbut")
(declare-function hargs:iform-read "hargs")
(declare-function hattr:set "hbut")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar klink:cell-ref-regexp
  (concat "[0-9a-zA-Z][.*~=0-9a-zA-Z \t\n\r]*\\s-*,\\s-*"
	  "[|:.*~=0-9a-zA-Z \t\n\r]+"
	  "\\|[|: 0-9a-zA-Z][|:.*~=0-9a-zA-Z \t\n\r]*")
  "Regexp matching a cell reference including relative and view specs.
Contains no groupings.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun klink:absolute (label-and-pos)
  "Return an absolute klink string from LABEL-AND-POS list.
With point in a klink's source buffer and LABEL-AND-POS a list
of (klink-label, klink-start, klink-end) including delimiters,
return an absolute klink string.  Klink returned is of the form:
\"<absolute-file-name, cell-ref>\".  See documentation for
`kcell:ref-to-id' for valid cell-ref formats."
  (when (and (derived-mode-p 'kotl-mode) label-and-pos (listp label-and-pos))
    (let* ((file-and-cell-ref (klink:parse (car label-and-pos)))
	   (file (or (car file-and-cell-ref) buffer-file-name))
	   (cell-ref (nth 1 file-and-cell-ref)))
      (klink:set-yank-handler
       (format "<%s#%s>" (expand-file-name file) cell-ref)))))

;;;###autoload
(defun klink:create (reference)
  "Insert at point an implicit link to REFERENCE.
REFERENCE should be a cell-ref or a string containing \"filename#cell-ref\".
See documentation for `kcell:ref-to-id' for valid cell-ref formats."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     ;; This `default-directory' setting is referenced in "hargs.el" for argument getting.
     (hattr:set 'hbut:current 'dir default-directory)
     (save-excursion
       (hargs:iform-read
	'(interactive "*+LInsert link to <[file]#cell-id[|vspecs]>: ")))))
  (barf-if-buffer-read-only)
  ;; Reference generally is a string.  It may be a list as a string, e.g.
  ;; "(\"file\" \"cell\")", in which case, we remove the unneeded internal
  ;; double quotes and then parse it with pattern matching.
  (and (stringp reference) (> (length reference) 0)
       (eq (aref reference 0) ?\()
       (setq reference (replace-regexp-in-string "\\\"" "" reference nil t)))
  ;; This `default-directory' setting is referenced in "hargs.el" for
  ;; getting arguments.
  (hattr:set 'hbut:current 'dir default-directory)
  (let (file-ref cell-ref)
    (setq reference (klink:parse reference)
	  file-ref  (car reference)
	  cell-ref  (nth 1 reference))
    ;; Don't need filename if link is to a cell in current buffer.
    (when (and file-ref (equal buffer-file-name
			       (expand-file-name file-ref default-directory)))
      (setq file-ref nil))
    (cond (file-ref
	   (setq file-ref (hpath:relative-to file-ref))
	   ;; Remove "./" prefix, if any.
	   (when (string-match "^\\./" file-ref)
	     (setq file-ref (substring file-ref (match-end 0))))
	   (insert "<" file-ref)
	   (when cell-ref
	     (insert "#" cell-ref))
	   (insert ">"))
	  (cell-ref (insert "<#" cell-ref ">"))
	  (t  (error "(klink:create) Invalid reference, `%s'" reference)))))

;;;###autoload
(defun klink:at-p ()
  "Return non-nil iff point is within a klink.
See documentation for the `actypes::link-to-kotl' function for valid klink
formats.  Value returned is a list of: link-label, link-start-position, and
link-end-position, (including delimiters)."
  (let (bol label-and-pos referent path)
    (when (and
	   ;; Avoid false matches in certain modes.
	   (not (memq major-mode klink:ignore-modes))
 	   ;; If this is an OO-Browser listing buffer, ignore anything that
	   ;; looks like a klink, e.g. a C++ <template> class.
	   (if (fboundp 'br-browser-buffer-p)
	       (not (br-browser-buffer-p))
	     t)
	   ;; If in a programming mode, Klinks can occur only within comments.
	   (if (and (derived-mode-p 'prog-mode)
		    (not (derived-mode-p 'lisp-interaction-mode))
		    (not (memq major-mode hui-select-markup-modes)))
	       ;; Next line means point is within a comment
	       (nth 4 (syntax-ppss))
	     t)
	   ;; If in a C-based mode, Klinks can occur only within comments.
	   (if (and (memq major-mode klink:c-style-modes)
		    (fboundp 'c-within-comment-p))
	       (or (c-within-comment-p)
		   (save-excursion
		     (and (re-search-backward "//\\|\n" nil t) (looking-at "//"))))
	     t)
	   ;; Don't match to C-style lines like:  #include < path >,
	   ;; even if inside a comment.
	   (if (memq major-mode klink:c-style-modes)
	       (save-excursion
		 (beginning-of-line)
		 (setq bol (point))
		 (require 'hmouse-tag)
		 (not (looking-at smart-c-include-regexp)))
	     t)
	   (save-excursion
	     ;; Don't match Elisp print objects such as #<buffer>
	     ;; even if inside a comment
	     (and (search-backward "<" bol t)
		  (not (eq (preceding-char) ?#))
		  ;; Don't match to \<(explicit)> or <[implicit]> Hyperbole
                  ;; buttons or message attachments such as <#part ...>
		  (not (memq (char-after (1+ (point))) '(?\( ?\[)))
		  (not (looking-at "<#part\\s-"))))
	   (setq label-and-pos (hbut:label-p t "<" ">" t))
	   (stringp (setq referent (car label-and-pos)))
	   (setq referent (string-trim referent))
	   ;; Ensure it conforms to some klink specification.
	   (or (string-match "\\`[ \t]*[-#@|!&]" referent)
	       (and (or (when (string-match "\\s-*,\\|#[0-9a-z.=]+\\'" referent)
			  (setq path (substring referent 0 (match-beginning 0)))
			  (hpath:is-p (expand-file-name path (hbut:get-key-src t t))))
			(hpath:is-p (expand-file-name referent (hbut:get-key-src t t))))
		    (string-match "\\.kot" referent)))
	   ;; Eliminate matches to e-mail addresses like, <user@domain>
	   (not (string-match "[^<> \t\n\r\f][!&@]" referent))
	   ;; Eliminate matches to URLs but allow for single char Windows path drive prefixes
	   (not (string-match "\\`[a-zA-Z][a-zA-Z]+:" referent))
	   ;; Don't match to <HTML> and </SGML> type tags
	   (not (and (memq major-mode hui-select-markup-modes)
		     ;; Assume , followed by a number is a klink.
		     (not (string-match ",\\s-*[0-9]" referent))
		     (string-match "\\`[a-zA-Z!/]" referent))))
      label-and-pos)))

(defun klink:set-yank-handler (klink)
  "Add yank-handler to KLINK and return the modified KLINK.
Link is made relative when yanked into the same koutline or the
same directory."
  (add-text-properties 0 (length klink)
		       (list 'yank-handler '(klink:yank-handler)
			     'yank-excluded-properties (cons 'yank-handler (get-text-property 0 'yank-excluded-properties klink)))
		       klink)
  klink)

;;; ************************************************************************
;;; Hyperbole type definitions
;;; ************************************************************************

(defib klink ()
  "Follow a link delimited by <> to a koutline cell.
See documentation for the `link-to-kotl' function for valid klink formats."
  (let* ((link-and-pos (klink:at-p))
	 (link (car link-and-pos))
	 (start (nth 1 link-and-pos))
	 (end   (nth 2 link-and-pos)))
    (when link
      (ibut:label-set link start end)
      (hact 'klink:act link start))))

(defact link-to-kotl (link)
  "Display at the top of another window the referent pointed to by LINK.
LINK may be of any of the following forms; the <> are optional:
  < [-!&] pathname >
  < pathname [#cell-ref] > or < pathname [, cell-ref] >
  < #cell-ref > or < @ cell-ref >
  < |viewspec >
  < :augment-viewspec >

See documentation for `kcell:ref-to-id' for valid cell-ref formats."

  (interactive "sKotl link specifier: ")
  (unless (stringp link)
    (error "(link-to-kotl): Non-string link argument, %s" link))
  (cond
   ((or (string-match (format "\\`<?\\s-*[#@]\\s-*\\(%s\\)\\s-*>?\\'"
			      klink:cell-ref-regexp) link)
	(string-match (format "\\`<?\\s-*\\([|:]%s\\)\\s-*>?\\'"
			      klink:cell-ref-regexp) link))
    ;; < @ cell-ref > or < |viewspec > or < :augment-viewspec >
    (hact 'link-to-kcell nil (match-string 1 link)))
   ((and (string-match
	  ;; The path part of this next regexp must be non-greedy (+?)
	  ;; so that an anchored # expression if any is not considered
	  ;; part of the path.
	  (format "\\`<?\\s-*\\([^ \t\n\r\f,<>]*?\\)\\s-*\\([#,]\\s-*\\(%s\\)\\)?\\s-*>?\\'"
		  klink:cell-ref-regexp)
	  link)
	 (match-end 3))
    ;; < pathname, cell-ref > or < pathname#cell-ref > or < pathname >
    (hact 'link-to-kcell (match-string 1 link) (match-string 3 link)))
   ((string-match
     "\\`<?\\s-*\\(\\([-!&]\\)?\\s-*[^ \t\n\r\f,<>]+\\)\\s-*>?\\'" link)
    ;; < [-!&] pathname >
    (hpath:find (match-string 1 link)))
   (t (error "(link-to-kotl): Invalid link specifier, %s" link))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun klink:act (klink start-pos)
  "Update relative part of KLINK at START-POS if it has moved and jump to KLINK.

See `actypes::link-to-kotl' for valid KLINK formats."
  (let ((obuf (current-buffer)))
    ;; Perform klink's action which is to jump to klink referent.
    (prog1 (hact 'link-to-kotl klink)
      (when (derived-mode-p 'kotl-mode)
	(save-excursion
	  ;; Update klink label if need be, which might be in a different buffer
	  ;; than the current one.
	  (klink:update-label klink start-pos obuf))))))

(defun klink:parse (reference)
  "Return (file-ref cell-ref) list parsed from REFERENCE string.
Either element of the list may be nil if REFERENCE does not contain that
element.  REFERENCE must be one of the following forms (and may include an
optional pair of <> delimiters) or an error is triggered:
  (pathname#cell-ref) or pathname#cell-ref
  (pathname, cell-ref) or pathname, cell-ref
  cell-ref
  |viewspec
  :augment-viewspec (ignored for now)

See documentation for `kcell:ref-to-id' for valid cell-ref formats."

  (or (stringp reference)
      (error "(klink:parse): Non-string reference argument, %s"
	     reference))
  (cond
   ((string-match
     (format
      "\\`\\s-*[<\(]?\\s-*\\([^|: \t\n\r,<>][^ \t\n\r,<>]*\\)\\s-*[#,]\\s-*\\(%s\\)\\s-*[\)>]?\\s-*\\'"
      klink:cell-ref-regexp)
     reference)
    ;; <pathname#cell-ref> or <pathname, cell-ref>
    (list (match-string 1 reference) (match-string 2 reference)))
   ((string-match (format "\\`\\s-*<?[#@]?\\s-*\\(%s\\)\\s-*>?\\s-*\\'"
			  klink:cell-ref-regexp)
		  reference)
    ;; <#cell-ref> or <@ cell-ref> or cell-ref
    (list nil (match-string 1 reference)))
   (t (error "(klink:parse): Invalid reference specifier, %s" reference))))

(defun klink:replace-label (klink link-buf start new-label)
  "Replace out of date relative id in a link reference of the form, relid=idstamp."
  (with-current-buffer link-buf
    (if buffer-read-only
	(message "Relative label should be `%s' in klink <%s>."
		 new-label klink)
      (goto-char start)
      (cond ((or (looking-at "<\\s-*[#@]\\s-*")
		 (looking-at "[^,]+?[#,]\\s-*"))
	     (goto-char (match-end 0))
	     (zap-to-char 1 ?=)
	     (insert new-label ?=))
	    (t nil)))))

(defun klink:update-label (klink start link-buf)
  "Update label of KLINK if its relative cell id has changed.
Assume point is in klink referent buffer, where the klink points."
  (and (stringp klink)
       (string-match "[#@,]\\s-*\\([*0-9][*.0-9a-zA-Z]*\\)\\s-*=\\s-*0[0-9]*"
		     klink)
       ;; Then klink has both relative and permanent ids.
       (let* ((label (match-string 1 klink))
	      (new-label (kcell-view:label)))
	 (and new-label (not (equal label new-label))
	      (klink:replace-label klink link-buf start new-label)))))

(defun klink:yank-handler (klink)
  (if (string-match "<\\([^,]+?\\)[#,][ \t]*\\(.+\\)" klink)
      (let* ((file (match-string 1 klink))
	     (rest (match-string 2 klink))
	     (dir (file-name-directory file)))
	(cond ((equal file buffer-file-name)
	       ;; Remove the klink filename since yanking into the
	       ;; same file
	       (insert (format "<#%s" rest)))
	      ((and buffer-file-name (equal dir (file-name-directory buffer-file-name)))
	       ;; Use filename without dir since yanking into same directory
	       (insert (format "<%s#%s" (file-name-nondirectory file) rest)))
	      (t (insert klink))))
    (insert klink)))
				 
(provide 'klink)

;;; klink.el ends here
