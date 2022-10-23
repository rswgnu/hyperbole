;;; hyrolo-logic.el --- Logic functions for GNU Hyperbole Rolo files  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    13-Jun-89 at 22:57:33
;; Last-Mod:      9-Oct-22 at 18:01:03 by Bob Weiner
;;
;; Copyright (C) 1989-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;  INSTALLATION:
;;
;;   See also hyrolo.el.  These functions are separated from hyrolo.el since many
;;   users may never want or need them.  They can be automatically loaded when
;;   desired by adding the following to one of your Emacs init files:
;;
;;    (autoload 'hyrolo-fgrep-logical "hyrolo-logic" "Rolo search with logical operators." t)
;;
;;  FEATURES:
;;
;;   1.  One command, `hyrolo-fgrep-logical' which prompts for a logical search
;;       expression string and displays any matching entries.  A sample expression
;;       might be:
;;        (and (or (not time card) (xor "french balloons" spanish)) teacher pet)
;;
;;       By default, only sub-entries with matches are shown, not entire
;;       hierarchies of entries, for more intuitive results.  Use a prefix argument
;;
;;       Either double quotes or parentheses may be used to group multiple
;;       words as a single argument.
;;
;;   2.  Logical `hyrolo-and', `hyrolo-or', `hyrolo-not', and `hyrolo-xor' rolo
;;       entry string filter functions.  They take any number of string or
;;       boolean arguments and may be nested.  NOTE THAT THESE FUNCTIONS
;;       SHOULD NEVER BE CALLED DIRECTLY UNLESS THE FREE VARIABLES `start'
;;       and `end' ARE BOUND BEFOREHAND.
;;
;;   3.  Logical `hyrolo-r-and', `hyrolo-r-or', `hyrolo-r-not', and `hyrolo-r-xor'
;;       rolo entry regexp filter functions.  They take any number of string or
;;       boolean arguments and may be nested.  NOTE THAT THESE FUNCTIONS
;;       SHOULD NEVER BE CALLED DIRECTLY UNLESS THE FREE VARIABLES `start'
;;       and `end' ARE BOUND BEFOREHAND.
;;
;;  EXAMPLE PROGRAMMATIC USAGE:
;;
;;     (hyrolo-logic (hyrolo-and (hyrolo-not "Tool-And-Die") "secretary"))
;;
;;   would find all non-Tool-And-Die Corp. secretaries in your rolo.
;;
;;   The logical matching routines are not at all optimal, but then most
;;   rolo files are not terribly lengthy either, so results are often
;;   displayed quickly.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hyrolo)

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (defvar next-entry-exists nil))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hyrolo-fgrep-logical (expr &optional count-only include-sub-entries no-sub-entries-out
				  whole-buffer-flag)
  "Display rolo entries matching EXPR.
EXPR is a string that may contain sexpression logical prefix operators.
If optional COUNT-ONLY is non-nil, don't display entries, return
count of matching entries only.  If optional INCLUDE-SUB-ENTRIES
flag is non-nil, SEXP will be applied across all sub-entries at
once.  Default is to apply SEXP to each entry and sub-entry
separately.  Entries are displayed with all of their sub-entries
unless INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT
flag is non-nil.

A complex example of EXPR might be:
  (and (or (not time card) (xor (and french balloons) spanish)) teacher pet)
which means:
  Match neither `time' nor `card'
    or
  Match exactly one of (`french' and `balloons') or (`spanish').
    and
  Match `teacher' and `pet'.

Either double quotes or parentheses may be used to group multiple words as a
single argument."
  (interactive "sLogical rolo search: \nP\nP")
  (when (called-interactively-p 'any)
    (setq no-sub-entries-out (not no-sub-entries-out)))
  (let* ((case-fold-search t)
	 (total-matches))
    (cond (whole-buffer-flag
	   (setq expr (format "(hyrolo-logic (quote %S) nil %s %s %s %s)"
			      expr count-only include-sub-entries
			      no-sub-entries-out t))
	   (setq total-matches (eval (read expr))))
	  ((string-match "\(\\(and\\|or\\|xor\\|not\\)\\>" expr)
	   (setq expr (replace-regexp-in-string "\(or " "\(| " expr nil t))
	   (setq expr (replace-regexp-in-string "\(xor " "\(@ " expr nil t))
	   (setq expr (replace-regexp-in-string "\(not " "\(! " expr nil t))
	   (setq expr (replace-regexp-in-string "\(and " "\(& " expr nil t))

	   (setq expr (replace-regexp-in-string "\(r-or " "\(r-| " expr nil t))
	   (setq expr (replace-regexp-in-string "\(r-xor " "\(r-@ " expr nil t))
	   (setq expr (replace-regexp-in-string "\(r-not " "\(r-! " expr nil t))
	   (setq expr (replace-regexp-in-string "\(r-and " "\(r-& " expr nil t))

	   (setq expr (replace-regexp-in-string
		       "\"\\([^\"]*\\)\"" "{\\1}" expr nil nil))
	   (setq expr (replace-regexp-in-string
		       "\(\\([^@|!&()][^()\"]*\\)\)" "{\\1}" expr nil nil))
	   (let ((saved-expr expr))
	     (while
		 (not (equal
		       saved-expr
		       (setq expr (replace-regexp-in-string
				   "\\(\\s-\\)\\([^{}()\" \t\n\r]+\\)\\([^{}()]*[()]\\)"
				   "\\1\"\\2\"\\3" expr nil nil))))
	       (setq saved-expr expr)))
	   (setq expr (replace-regexp-in-string
		       "{\\([^{}]+\\)}" "\"\\1\"" expr nil nil))
	   (setq expr (replace-regexp-in-string "\(| " "\(hyrolo-or start end  " expr nil t))
	   (setq expr (replace-regexp-in-string "\(@ " "\(hyrolo-xor start end " expr nil t))
	   (setq expr (replace-regexp-in-string "\(! " "\(hyrolo-not start end " expr nil t))
	   (setq expr (replace-regexp-in-string "\(& " "\(hyrolo-and start end " expr nil t))

	   (setq expr (replace-regexp-in-string "\(r-| " "\(hyrolo-r-or start end  " expr nil t))
	   (setq expr (replace-regexp-in-string "\(r-@ " "\(hyrolo-r-xor start end " expr nil t))
	   (setq expr (replace-regexp-in-string "\(r-! " "\(hyrolo-r-not start end " expr nil t))
	   (setq expr (replace-regexp-in-string "\(r-& " "\(hyrolo-r-and start end " expr nil t))

	   (setq expr (format "(hyrolo-logic (quote %S) nil %s %s %s %s)"
			      expr count-only include-sub-entries
			      no-sub-entries-out whole-buffer-flag))
	   (setq total-matches (eval (read expr))))
	  (t
	   ;; Search string does not contain embedded logic
	   ;; operators; do a string search instead.
	   (setq total-matches (hyrolo-fgrep expr))))

    (if (called-interactively-p 'interactive)
	(message "%s matching entr%s found in rolo."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

(defun hyrolo-logic (sexp &optional in-bufs count-only include-sub-entries no-sub-entries-out
			  whole-buffer-flag)
  "Apply SEXP to all entries in optional IN-BUFS.
Display entries where SEXP is non-nil.
If IN-BUFS is nil, `hyrolo-file-list' is used.  If optional COUNT-ONLY is
non-nil, don't display entries, return count of matching entries only.  If
optional INCLUDE-SUB-ENTRIES flag is non-nil, apply SEXP across all sub-entries
at once.  Default is to apply SEXP to each entry and sub-entry separately.
Entries are displayed with all of their sub-entries unless INCLUDE-SUB-ENTRIES
is nil and optional NO-SUB-ENTRIES-OUT flag is non-nil.  SEXP should utilize the
free variables `start' and `end' as the region on which to operate.
Return the number of evaluations of SEXP that match entries."
  (let* ((display-buf (unless count-only
		       (prog1 (hyrolo-set-display-buffer)
			 (setq buffer-read-only nil)
			 (erase-buffer))))
	 (result
	  (mapcar
	   (lambda (buf-or-file)
	     (setq buf-or-file (or (get-buffer buf-or-file)
				   (funcall hyrolo-find-file-noselect-function buf-or-file)))
	     (hyrolo-map-logic sexp buf-or-file count-only include-sub-entries
			       no-sub-entries-out whole-buffer-flag))
	   (cond ((null in-bufs) hyrolo-file-list)
		 ((listp in-bufs) in-bufs)
		 ((list in-bufs)))))
	 (total-matches (apply '+ result)))
    (unless (or count-only (= total-matches 0))
      (hyrolo-display-matches display-buf))
    total-matches))

(defun hyrolo-map-logic (sexp hyrolo-buf &optional count-only
			 include-sub-entries _no-sub-entries-out
			 whole-buffer-flag)
  "Apply logical SEXP to each entry in HYROLO-BUF.
Write out matching entries to `hyrolo-display-buffer'.  If
optional COUNT-ONLY is non-nil, don't display entries, return
count of matching entries only.  If optional INCLUDE-SUB-ENTRIES
flag is non-nil, apply SEXP across all sub-entries at once.
Default is to apply SEXP to each entry and sub-entry separately.
Entries are displayed with all of their sub-entries unless
INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT flag
is non-nil.  SEXP should utilize the free variables `start' and
`end' as the region on which to operate.  Return the number of
evaluations of SEXP that match entries."
  (setq hyrolo-buf (or (get-buffer hyrolo-buf) hyrolo-buf))
  (if (or (bufferp hyrolo-buf)
	  (when (file-exists-p hyrolo-buf)
	    (setq hyrolo-buf (find-file-noselect hyrolo-buf t))))
      (let* ((display-buf (hyrolo-set-display-buffer))
	     (buffer-read-only)
	     (hdr-pos)
	     (num-found 0))
	  (set-buffer hyrolo-buf)
	  (save-excursion
	    (save-restriction
	      (hyrolo-widen)
	      (goto-char (point-min))
	      ;; Ensure no entries in outline mode are hidden.
	      (outline-show-all)
	      (when (re-search-forward hyrolo-hdr-regexp nil t 2)
		(forward-line)
		(setq hdr-pos (cons (point-min) (point))))
	      (setq num-found (if whole-buffer-flag
				  (hyrolo-map-kotl
				   sexp hyrolo-buf display-buf hdr-pos count-only include-sub-entries)
				(hyrolo-map-entries
				 sexp hyrolo-buf display-buf hdr-pos count-only include-sub-entries)))))
	  (hyrolo-kill-buffer hyrolo-buf)
	  num-found)
    0))

;;
;; INTERNAL FUNCTIONS.
;;

(defun hyrolo-map-entries (sexp hyrolo-buf display-buf hdr-pos &optional count-only include-sub-entries)
  (let* ((start)
	 (end)
	 (end-entry-hdr)
	 (curr-entry-level-len)
	 (num-found 0))
    (while (re-search-forward hyrolo-entry-regexp nil t)
      (setq end-entry-hdr (match-end hyrolo-entry-group-number)
	    start (match-beginning hyrolo-entry-group-number)
	    next-entry-exists nil
	    curr-entry-level-len (length (match-string-no-properties hyrolo-entry-group-number))
	    end (hyrolo-to-entry-end include-sub-entries curr-entry-level-len))
      (let ((result (eval sexp `((start . ,start) (end . ,end)))))
	(unless count-only
	  (and result (= num-found 0)
	       (let* ((src (or (buffer-file-name hyrolo-buf)
			       hyrolo-buf))
		      (src-line
		       (format
			(concat (if (boundp 'hbut:source-prefix)
				    hbut:source-prefix
				  "@loc> ")
				"%s")
			(prin1-to-string src))))
		 (set-buffer display-buf)
		 (goto-char (point-max))
		 (if hdr-pos
		     (progn
		       (insert-buffer-substring
			hyrolo-buf (car hdr-pos) (cdr hdr-pos))
		       (insert src-line "\n\n"))
		   (insert (format hyrolo-hdr-format src-line)))
		 (set-buffer hyrolo-buf))))
	(if result
	    (progn (goto-char end)
		   (setq num-found (1+ num-found))
		   (or count-only
		       (append-to-buffer display-buf start end)))
	  (goto-char end-entry-hdr))))
    num-found))

(defun hyrolo-map-kotl (sexp hyrolo-buf display-buf hdr-pos &optional count-only include-sub-entries)
  (let* ((start)
	 (end)
	 (end-entry-hdr)
	 (curr-entry-level-len)
	 (num-found 0)
	 result)
    (mapc (lambda (cell-ref)
	    (when (setq result (kotl-mode:goto-cell cell-ref))
	      (setq end-entry-hdr (point)
		    start (line-beginning-position)
		    next-entry-exists nil
		    curr-entry-level-len (- result start)
		    end (hyrolo-to-entry-end include-sub-entries curr-entry-level-len))
	      (unless count-only
		(and result (= num-found 0)
		     (let* ((src (or (buffer-file-name hyrolo-buf)
				     hyrolo-buf))
			    (src-line
			     (format
			      (concat (if (boundp 'hbut:source-prefix)
					  hbut:source-prefix
					"@loc> ")
				      "%s")
			      (prin1-to-string src))))
		       (set-buffer display-buf)
		       (goto-char (point-max))
		       (if hdr-pos
			   (progn
			     (insert-buffer-substring
			      hyrolo-buf (car hdr-pos) (cdr hdr-pos))
			     (insert src-line "\n\n"))
			 (insert (format hyrolo-hdr-format src-line)))
		       (set-buffer hyrolo-buf))))
	      (if result
		  (progn (goto-char end)
			 (setq num-found (1+ num-found))
			 (or count-only
			     (append-to-buffer display-buf start end)))
		(goto-char end-entry-hdr))))
	  sexp)
    num-found))

;; Do NOT call the following functions directly.
;; Send them as parts of an expression to `hyrolo-logic'.

(defun hyrolo-not (start end &rest pat-list)
  "Logical <not> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (save-restriction
    (narrow-to-region start end)
    (let ((pat))
      (while (and pat-list
		  (or (null (setq pat (car pat-list)))
		      (and (stringp pat)
			   (goto-char start)
			   (not (funcall hyrolo-next-match-function (regexp-quote pat) nil)))))
	(setq pat-list (cdr pat-list)))
      (not pat-list))))

(defun hyrolo-or (start end &rest pat-list)
  "Logical <or> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (if (memq t pat-list)
      t
    (save-restriction
      (narrow-to-region start end)
      (let ((pat))
	(while (and pat-list
		    (or (null (setq pat (car pat-list)))
			(and (stringp pat)
			     (goto-char start)
			     (not (funcall hyrolo-next-match-function (regexp-quote pat) nil)))))
	  (setq pat-list (cdr pat-list)))
	(if pat-list t nil)))))

(defun hyrolo-xor (start end &rest pat-list)
  "Logical <xor> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (save-restriction
    (narrow-to-region start end)
    (let ((pat)
	  (matches 0))
      (while (and pat-list
		  (or (not (setq pat (car pat-list)))
		      (and (or (eq pat t)
			       (not (goto-char start))
			       (funcall hyrolo-next-match-function (regexp-quote pat) nil))
			   (setq matches (1+ matches)))
		      t)
		  (< matches 2))
	(setq pat-list (cdr pat-list)))
      (= matches 1))))

(defun hyrolo-and (start end &rest pat-list)
  "Logical <and> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (unless (memq nil pat-list)
    (save-restriction
      (narrow-to-region start end)
      (let ((pat))
	(while (and pat-list
		    (setq pat (car pat-list))
		    (or (eq pat t)
			(not (goto-char start))
			(funcall hyrolo-next-match-function (regexp-quote pat) nil)))
	  (setq pat-list (cdr pat-list)))
	(not pat-list)))))

;; Work with regular expression patterns rather than strings

(defun hyrolo-r-not (start end &rest pat-list)
  "Logical <not> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a regular expression."
  (save-restriction
    (narrow-to-region start end)
    (let ((pat))
      (while (and pat-list
		  (or (null (setq pat (car pat-list)))
		      (and (stringp pat)
			   (goto-char start)
			   (not (funcall hyrolo-next-match-function pat nil)))))
	(setq pat-list (cdr pat-list)))
      (not pat-list))))

(defun hyrolo-r-or (start end &rest pat-list)
  "Logical <or> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a regular expression."
  (if (memq t pat-list)
      t
    (save-restriction
      (narrow-to-region start end)
      (let ((pat))
	(while (and pat-list
		    (or (null (setq pat (car pat-list)))
			(and (stringp pat)
			     (goto-char start)
			     (not (funcall hyrolo-next-match-function pat nil)))))
	  (setq pat-list (cdr pat-list)))
	(if pat-list t nil)))))

(defun hyrolo-r-xor (start end &rest pat-list)
  "Logical <xor> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a regular expression."
  (save-restriction
    (narrow-to-region start end)
    (let ((pat)
	  (matches 0))
      (while (and pat-list
		  (or (not (setq pat (car pat-list)))
		      (and (or (eq pat t)
			       (not (goto-char start))
			       (funcall hyrolo-next-match-function pat nil))
			   (setq matches (1+ matches)))
		      t)
		  (< matches 2))
	(setq pat-list (cdr pat-list)))
      (= matches 1))))

(defun hyrolo-r-and (start end &rest pat-list)
  "Logical <and> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a regular expression."
  (unless (memq nil pat-list)
    (save-restriction
      (narrow-to-region start end)
      (let ((pat))
	(while (and pat-list
		    (setq pat (car pat-list))
		    (or (eq pat t)
			(not (goto-char start))
			(funcall hyrolo-next-match-function pat nil)))
	  (setq pat-list (cdr pat-list)))
	(not pat-list)))))

(provide 'hyrolo-logic)

;;; hyrolo-logic.el ends here
