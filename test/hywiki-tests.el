;;; hywiki-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    18-May-24 at 23:59:48
;; Last-Mod:      2-Feb-26 at 00:32:30 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024-2026  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'ert-x)
(require 'hy-test-helpers)
(require 'hywiki)
(require 'hsys-org)
(require 'ox-publish)
(require 'seq) ;; for `seq-take-while' and `seq-uniq'

(defconst hywiki-tests--edit-string-pairs
   [
    ;; !! TODO: These tests fail
    ;; ("Hi#a<insert-char ?b> cd" "{Hi#ab} cd")
    ;; ("\"WikiWord#section with spaces\"<backward-delete-char 1>" "\"{WikiWord#section} with spaces") ;; shrink highlight to "{WikiWord#section}
    ;; ("\"WikiWord#a b c<backward-delete-char 2>" "\"{WikiWord#a} b")

    ;; These tests pass
    ("Hi" "{Hi}")
    ("HyWikiW<kill-word 1>ord<yank 1> HyW<kill-word 1>ikiWord<yank 1>"
     "{HyWikiWord} {HyWikiWord}")
    ("HyWikiWord" "{HyWikiWord}")
    ("HyWiki<delete-region>Word" "{HyWikiWord}")
    ("HyWiki<insert \"Word\">" "{HyWikiWord}")
    ("HyWikiW<kill-word 1>ord<yank 1>" "{HyWikiWord}")
    ("Wiki<zap-to-char 1 ?n>#sectionWord" "{WikiWord}"
     "zap-to-WikiWord" "Delete section chars to form  a WikiWord") ;; highlight
    ("Wiki#sec<tion>Word" "{Wiki#sec}<tion>Word")
    ("<HyWikiWord>" "<{HyWikiWord}>")
    ("<delete-char 1>\"WikiWord#section with spaces\"" "{WikiWord#section} with spaces\"") ;; shrink highlight to {WikiWord#section}
    ("\"WikiWord#section\"<backward-delete-char 1>" "\"{WikiWord#section}") ;; no highlight change 
    ("FAI AI" "{FAI} {AI}")
    ("WikiWord#a b c<backward-delete-char 1>" "{WikiWord#a} b ")
    ("HiHo#s " "{HiHo#s} ")
    ("HiHo#s<insert-char ? >" "{HiHo#s} ")
    ("(Non#s n)<backward-delete-char 1>" "({Non#s} n")
    ("<kill-word 1>WikiWord unhighlighted" " unhighlighted") ;; dehighlight
    ;; WikiWord below does not highlight since could be an Info node
    ;; ibut, like "(hyperbole)WikiWord", that we don't want to trigger
    ;; as a wiki word.
    ("(MyWikiWord)WikiWord" "({MyWikiWord})WikiWord")
    ]
   "Vector of (pre-test-cmd-str post-test-str-result [test-name] [doc]) elements.
Last two elements are optional.")

(ert-deftest hywiki-tests--edit ()
  (hywiki-tests--preserve-hywiki-mode
    (let ((test-num 0)
	  before
	  after
	  name
	  doc
	  markedup-before
	  markedup-after
	  start
	  end
	  hywiki-ref-positions)
      (unwind-protect
	  (progn
	    (org-mode)
	    (mapc
	     (lambda (before-after)
	       (condition-case err
	           (progn
		     (setq before (nth 0 before-after)
		           after  (nth 1 before-after)
		           name   (nth 2 before-after)
		           doc    (nth 3 before-after))
		     ;; Ensure all brace delimited HyWikiWords have their pages
		     ;; created so their references will be highlighted.
		     (mapc #'hywiki-add-page
		           (delq nil
				 (mapcar #'hywiki-get-singular-wikiword
					 (seq-remove #'string-empty-p
						     (mapcar #'string-trim
							     (hywiki-tests--get-brace-strings after))))))
		     (unwind-protect
			 (progn
		           (pop-to-buffer (current-buffer))
		           (erase-buffer)
		           (hywiki-tests--insert-by-char before)
		           (hywiki-tests--interpolate-buffer)
		           ;; Markup before string in temp buffer
		           ;; Surround any HyWikiWord refs with braces to match after string.
		           (setq hywiki-ref-positions (hywiki-get-reference-positions))
		           (dolist (start-end hywiki-ref-positions)
			     (setq start (car start-end)
			           end (cdr start-end))
			     (goto-char end)
			     (hywiki-tests--insert "}")
			     (goto-char start)
			     (hywiki-tests--insert "{"))
		           ;; Store the buffer string for comparison
		           (setq markedup-before (buffer-string))
		           ;; Markup after string
		           (erase-buffer)
		           (hywiki-tests--insert after)
		           (hywiki-tests--interpolate-buffer)
		           (setq markedup-after (buffer-string))
		           ;; Compare markedup-before to markedup-after
		           (if (or name doc)
			       (should (equal (list :test-num test-num
						    :markedup (format "%S" markedup-before)
						    :test-name name :doc doc
						    :before before :after after)
					      (list :test-num test-num
						    :markedup (format "%S" markedup-after)
						    :test-name name :doc doc
						    :before before :after after)))
			     (should (equal (list :test-num test-num
					          :markedup (format "%S" markedup-before)
					          :before before :after after)
					    (list :test-num test-num
					          :markedup (format "%S" markedup-after)
					          :before before :after after))))
		           (cl-incf test-num))
		       (goto-char (point-min))))
		 (error (error "%s ---- %S" err (list :markedup markedup-before
					              :test-num test-num
					              :before before :after after)))))
	     hywiki-tests--edit-string-pairs))
	(let ((default-directory hywiki-directory))
          (hy-delete-files-and-buffers
          '("AI.org" "FAI.org" "Hi.org" "HiHo.org" "HyWiki.org" "HyWikiW.org" "HyWikiWord.org" "MyWikiWord.org" "Non.org" "Wiki.org")))))))

(defun hywiki-tests--get-brace-strings (s)
  "Return the substrings in S delimited by curly braces {…}, excluding braces.
Assume no nesting of braces, nor any quoting of braces."
  (let ((pos 0)
        (result '()))
    (while (string-match "{\\([^}]*\\)}" s pos)
      (push (match-string 1 s) result)
      (setq pos (match-end 0)))
    (nreverse result)))

(defun hywiki-tests--insert (&rest args)
  (apply #'hywiki-tests--command-execute #'insert args))

(defun hywiki-tests--insert-by-char (str)
  "Interactively insert the characters from STR."
  (interactive "sInsert string: ")
  (mapc (lambda (c)
	  (setq unread-command-events (nconc unread-command-events
					     (listify-key-sequence
					      (char-to-string c)))))
	str)
  (hywiki-tests--execute-commands))

(defun hywiki-tests--only-file-in-dir-p (file)
  "Check if FILE is single in its directory."
  (let* ((dir (file-name-directory file))
         (files (seq-remove (lambda (f) (member f '("." "..")))
                            (directory-files dir))))
    (and (= (length files) 1)
         (string= (car files) (file-name-nondirectory file)))))

(defun hywiki-tests--delete-hywiki-cache (dir)
  "Delete the hywiki cache if it is the only file in DIR."
  (let ((hywiki-cache (expand-file-name hywiki-cache-default-file dir)))
    (when (and hywiki-cache
	       (file-readable-p hywiki-cache)
	       (file-writable-p hywiki-cache)
               (hywiki-tests--only-file-in-dir-p hywiki-cache))
      (delete-file hywiki-cache))))

(defun hywiki-tests--delete-hywiki-dir-and-buffer (dir)
  "Remove DIR and optional `hywiki-cache' file."
  (hywiki-tests--delete-hywiki-cache dir)
  (hy-delete-dir-and-buffer dir))

(defun hywiki-tests--execute-commands ()
  "Process all events from `unread-command-events'."
  (interactive)
  (while unread-command-events
    (let ((event (pop unread-command-events))
	  (noninteractive nil))
      ;; Execute this event as if typed
      (setq this-command (key-binding (vector event) t)
	    last-command-event event)
      ;; (message "Event = %s; last-command-event = %s; cmd = %s"
      ;;          event last-command-event this-command)
      (when this-command
	(run-hooks 'pre-command-hook)
	(command-execute this-command)
	(when (and (symbolp this-command)
		   (string-suffix-p "self-insert-command" (symbol-name this-command)))
	    ;; Force execution of `post-self-insert-hook' since is not
	    ;; run automatically when not in top-level command processing
	  (run-hooks 'post-self-insert-hook))
	(run-hooks 'post-command-hook)))))

(defun hywiki-tests--interpolate-buffer ()
  "Replace action buttons and Hyperbole variable markup in buffer."
  (let ((str (buffer-string)))
    ;; Replace env and lisp variable references
    (hpath:substitute-value str)
    ;; Replace action buttons with resulting values
    (goto-char (point-min))
    (when (hbut:at-p)
      (condition-case err
	  ;; Force HyWikiWord highlighting at point
	  (hywiki-tests--command-execute 'hbut:act 'hbut:current)
	(error (message "(hywiki-tests--interpolate-buffer) Error: %s"
			err))))
    (while (and (search-forward "<" nil t)
		(/= (preceding-char) ?\\))
      (when (and (hargs:delimited-p "<" ">")
		 ;; This creates the 'hbut:current in-memory ibut
		 (ibut:at-type-p 'action))
	;; Force HyWikiWord highlighting at point
	(condition-case err
	    ;; Force HyWikiWord highlighting at point
	    (hywiki-tests--command-execute 'hbut:act 'hbut:current)
	  (error (message "(hywiki-tests--interpolate-buffer) Error: %s"
			  err)))))))

(defun hywiki-tests--command-execute (sexp &rest rest)
  "Apply SEXP to REST of arguments as a HyWiki command and return the result.
Run pre, post-self-insert (when appropriate)  and post command hooks
around the call.  This is for simulating the command loop."
  (let ((buf (current-buffer))
	(cmd (cond ((symbolp sexp)
		    sexp)
		   ((listp sexp)
		    (when (symbolp (car sexp))
		      (car sexp))))))
    (setq last-command this-command
	  this-command cmd)
    (run-hooks 'pre-command-hook)
    (unwind-protect
	(command-execute
	 (lambda () (interactive)
	   (if rest
	       (apply sexp rest)
	     (eval sexp t))))
      ;; Ensure point remains in the same buffer before and after SEXP
      ;; evaluation.  This prevents false switching to the *ert* test
      ;; buffer when debugging.
      (set-buffer buf)
      (when (memq this-command (list 'self-insert-command
				     (key-binding [remap self-insert-command])))
	(run-hooks 'post-self-insert-hook))
      (run-hooks 'post-command-hook))))

(defmacro hywiki-tests--preserve-hywiki-mode (&rest body)
  "Restore hywiki-mode after running BODY."
  (declare (indent 0) (debug t))
  `(let* ((prior-hywiki-mode hywiki-mode)
	  (hywiki-directory (make-temp-file "hywiki" t))
          (wiki-page (cdr (hywiki-add-page "WikiWord"))))
     (unwind-protect
         (save-window-excursion
           (with-temp-buffer
	     (set-window-buffer (selected-window) (current-buffer))
             (hywiki-mode :all)
	     (sit-for 0.01)
	     ,@body))
       (hywiki-mode prior-hywiki-mode)
       (hy-delete-file-and-buffer wiki-page)
       (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--verify-preserve-hywiki-mode ()
  "Verify `hywiki-tests--preserve-hywiki-mode' restores `hywiki-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (hywiki-mode :all)
    (hywiki-tests--preserve-hywiki-mode
      (should (eq hywiki-mode :all))
      (hywiki-mode nil)
      (should-not hywiki-mode))
    (should (eq hywiki-mode :all))))

(ert-deftest hywiki-tests--hywiki-create-page--adds-file-in-wiki-folder ()
  "Verify add page creates file in wiki folder and sets hash table."
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
	 (hywiki-page-file (expand-file-name "WikiWord.org" hywiki-directory)))
    (unwind-protect
	(progn
          (should (string= hywiki-page-file
                           (cdr (hywiki-add-page "WikiWord"))))
          ;; Verify hash table is updated
          (with-mock
            (not-called hywiki-create-page)
            (should (string= (file-name-nondirectory hywiki-page-file)
			     (cdr (hywiki-get-referent "WikiWord"))))))
      (hy-delete-file-and-buffer hywiki-page-file)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--hywiki-create-page--adds-no-wiki-word-fails ()
  "Verify add page requires a WikiWord."
  ;; Should not leave erroneously created file after test but leaving
  ;; added error cleanup till later if it is even needed!? No file
  ;; should be created so only happens on error!? (If this is
  ;; considered an error case that is.)
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (should-not (hywiki-add-page "notawikiword"))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--action-key-on-hywikiword-displays-page ()
  "Verify `action-key' on a prefixed WikiWord, outside of hywiki-directory, creates a new page."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t))
          (wikifile "WikiWord.org"))
      (unwind-protect
          (with-temp-buffer
	    (hywiki-mode :all)
            (hywiki-tests--insert "[[hy:WikiWord]]")
            (goto-char 4)
            (action-key)
	    (should (equal (cons 'page wikifile) (hywiki-get-referent "WikiWord"))))
        (hy-delete-file-and-buffer (expand-file-name wikifile hywiki-directory))
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--assist-key-on-hywikiword-displays-help ()
  "Verify `assist-key' on a prefixed WikiWord, outside of hywiki-directory, displays help for the WikiWord link."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (with-temp-buffer
	    (hywiki-mode :all)
            (hywiki-tests--insert "[[hy:WikiWord]]")
            (goto-char 6)
            (should (string= "WikiWord" (hywiki-word-at)))
            (delete-other-windows)
            (assist-key)
            (should (get-window-with-predicate
		     (lambda (win) (string-prefix-p "*Help: Hyperbole "
						    (buffer-name
						     (window-buffer win)))))))
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--action-key-on-wikiword-displays-page ()
  "Verify `action-key' on a WikiWord, outside of hywiki-directory, creates a new page."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t))
           (hywiki-page-file (expand-file-name "WikiWord.org" hywiki-directory)))
      (unwind-protect
          (dolist (v '(nil t)) ;; Verify the file exists the second time
            (if v
                (should (file-exists-p hywiki-page-file))
              (should-not (file-exists-p hywiki-page-file)))
            (with-temp-buffer
	      (hywiki-mode :all)
              (hywiki-tests--insert "WikiWord\n")
              (goto-char 4)
              (action-key)
              (should (string= hywiki-page-file (buffer-file-name)))
              (should (equal (cons 'page (file-name-nondirectory hywiki-page-file))
                             (hywiki-get-referent "WikiWord")))
              (if v
                  (should (looking-at-p "WikiWord page"))
                (hywiki-tests--insert "WikiWord page")
                (goto-char (point-min)))))
        (hy-delete-file-and-buffer hywiki-page-file)
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--action-key-on-wikiword-and-section-displays-page ()
  "Verify `action-key' on a WikiWord with section moves to the section."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t))
	   (hywiki-page-file (expand-file-name "WikiWord.org" hywiki-directory))
           (sections '("* Header" "** SubHeader" "*** SubSubHeader")))
      (unwind-protect
          (progn
            (find-file hywiki-page-file)
            (dolist (v sections)
              (hywiki-tests--insert (format "%s\nbody\n" v)))
            (save-buffer)
	    (hywiki-mode :all)
            (dolist (v sections)
              (with-temp-buffer
                (hywiki-tests--insert "WikiWord#" (cadr (split-string v " ")))
                (goto-char 4)
                (action-key)
                (should (string= hywiki-page-file (buffer-file-name)))
	        (should (looking-at-p (regexp-quote v))))))
        (hy-delete-file-and-buffer hywiki-page-file)
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--action-key-on-wikiword-and-line-column-displays-page ()
  "Verify `action-key' on a WikiWord with line and column specifications goes to expected point."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t))
	   (hywiki-page-file (expand-file-name "WikiWord.org" hywiki-directory)))
      (unwind-protect
          (progn
            (find-file hywiki-page-file)
            (hywiki-tests--insert "\
line 1
line 2
")
            (save-buffer)
	    (hywiki-mode :all)
            (dolist (l '(1 2))
              (dolist (c '("" ":C0" ":C5"))
                (with-temp-buffer
                  (hywiki-tests--insert (format "WikiWord:L%s%s" l c))
                  (goto-char 4)
                  (action-key)
                  (should (string= hywiki-page-file (buffer-file-name)))
                  (if (string= c ":C5")
	              (should (looking-at-p (format "%s$" l)))
	            (should (looking-at-p (format "line %s$" l))))))))
        (hy-delete-file-and-buffer hywiki-page-file)
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--not-a-wikiword-unless-in-hywiki-mode ()
  "Verify WikiWord is not a WikiWord unless in `hywiki-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (with-temp-buffer
            (hywiki-mode nil)
            (hywiki-tests--insert "WikiWord")
            (goto-char 4)
            (should-not (hywiki-word-at))
            (hywiki-mode :all)
            (should (string= "WikiWord" (hywiki-word-at))))
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--a-wikiword-in-hywiki-directory ()
  "Verify WikiWord is identified if in `hywiki-directory'."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t))
           (referent (hywiki-add-page "WikiWord"))
	   (wiki-page (cdr referent)))
      (unwind-protect
          (with-current-buffer (find-file-noselect wiki-page)
            (hywiki-mode :pages)
            (hywiki-tests--insert "PotentialWikiWord")
	    (newline nil t)
            (goto-char 4)
            (should (hywiki-word-at)))
        (hy-delete-file-and-buffer wiki-page)
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--wikiword-identified-with-delimiters ()
  "Verify WikiWord is identified when surrounded by delimiters."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
	  (hywiki-org-link-type-required nil)
          (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (hywiki-mode :all)

          ;; Matches a WikiWord
          (dolist (v '("WikiWord" "[WikiWord]" "[[WikiWord]]" "{WikiWord}" "(WikiWord)"
                       "<WikiWord>" "<<WikiWord>>" "{[[WikiWord]]}" "([[WikiWord]])"
                       "[WikiWord AnotherWord WikiWord WikiWord]"
                       ))
            (with-temp-buffer
              (org-mode)
              (hywiki-tests--insert v)
	      (newline nil t)
              (goto-char 6)
              (if (string= "WikiWord" (hywiki-word-at))
		  (should t)
		(should-not v))))

          ;; Does not match as a WikiWord
          (dolist (v '("WikiWord#"))
            (with-temp-buffer
              (org-mode)
              (hywiki-tests--insert v)
	      (newline nil t)
              (goto-char 6)
	      (if (string= "WikiWord" (hywiki-word-at))
		  (should-not v)
		(should t))))

          ;; Identifies as org link (Note: Not checked if target
          ;; exists.) AND matches WikiWord
          (dolist (v '("[[hy:WikiWord]]" "[[hy:WikiWord\\]]]"))
            (with-temp-buffer
              (org-mode)
              (hywiki-tests--insert v)
	      (newline nil t)
              (goto-char 6)
              (font-lock-ensure)
              (should (hsys-org-face-at-p 'org-link))
	      (if (string= "WikiWord" (hywiki-word-at))
		  (should t)
		(should-not v))))

          ;; Identifies as org link (Note: Not checked if target
          ;; exists.) AND DOES NOT match WikiWord
          (dolist (v '("[[WikiWord AnotherWord]]"))
            (with-temp-buffer
              (org-mode)
              (hywiki-tests--insert v)
	      (newline nil t)
              (goto-char 6)
              (font-lock-ensure)
              (should (hsys-org-face-at-p 'org-link))
              (if (string= "WikiWord" (hywiki-word-at))
		  (should v)
		(should-not v)))))
      (hywiki-mode nil)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--at-wikiword-finds-word-and-section ()
  "Verify `hywiki-word-at' finds WikiWord and section if available."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hywiki-directory (make-temp-file "hywiki" t))
          (words '("WikiWord" "WikiWord:L1" "WikiWord:L1:C2"
                   "WikiWord#section" "WikiWord#section:L1" "WikiWord#section:L1:C2"
                   "WikiWord#section-subsection" "WikiWord#section-subsection:L1" "WikiWord#section-subsection:L1:C2"
                   ("(WikiWord#section with spaces)" . "WikiWord#section with spaces")
                   ("(WikiWord#section)" . "WikiWord#section")
                   )))
      (unwind-protect
          (with-temp-buffer
            (hywiki-mode :all)
            (dolist (w words)
              (let ((in (if (stringp w) w (car w)))
                    (expect (if (stringp w) w (cdr w))))
                (erase-buffer)
                (hywiki-tests--insert in)
                (goto-char 4)
                (should (string= expect (hywiki-word-at))))))
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--sections-with-dash-space ()
  "Verify `hywiki-word-at' finds sections with dash and space."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (hywiki-mode :all)
            (with-temp-buffer
              (hywiki-tests--insert "WikiWord#section rest is ignored")
              (goto-char 4)
              (should (string= "WikiWord#section" (hywiki-word-at))))
            (with-temp-buffer
              (hywiki-tests--insert "WikiWord#section-with-dash")
              (goto-char 4)
              (should (string= "WikiWord#section-with-dash" (hywiki-word-at))))
            (with-temp-buffer
              (hywiki-tests--insert "WikiWord#section-with#hash")
              (goto-char 4)
              (should-not (hywiki-word-at)))
            (with-temp-buffer
              (hywiki-tests--insert "WikiWord#\"section-within-quotes\"")
              (goto-char 4)
              (should-not (hywiki-word-at))))
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--sections-with-space-and-delimited-string ()
  "Verify `hywiki-word-at' with space and delimited string.
Only allow spaces in #section if the delimited string is a single
HyWikiWord reference."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (hywiki-mode :all)
            (with-temp-buffer           ; Delimited string allow space
              (hywiki-tests--insert "\"WikiWord#section rest\"")
              (goto-char 4)
              (should (string= "WikiWord#section rest" (hywiki-word-at))))
            (with-temp-buffer           ; Not a single WikiWord reference so no space
              (hywiki-tests--insert "\"WikiPage WikiWord#section rest\"")
              (goto-char 4)
              (should (string= "WikiPage" (hywiki-word-at)))
              (goto-char 20)
              (should (string= "WikiWord#section" (hywiki-word-at))))
            (with-temp-buffer           ; Single WikiWord reference (WikiPage is part of section)
              (hywiki-tests--insert "\"WikiWord#section rest WikiPage\"")
              (goto-char 4)
              (should (string= "WikiWord#section rest WikiPage" (hywiki-word-at)))))
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--word-is-p ()
  "Verify `hywiki-word-is-p' identifies WikiWords."
  (should (hywiki-word-is-p "WikiWord"))
  (should (hywiki-word-is-p "WikiWord#section"))
  (should-not (hywiki-word-is-p "hy:WikiWord"))
  (should-not (hywiki-word-is-p "wikiWord")))

(ert-deftest hywiki-tests--maybe-at-wikiword-beginning ()
  "Verify `hywiki-maybe-at-wikiword-beginning' identifies if maybe at beginning of WikiWord."
  (with-temp-buffer
    (hywiki-tests--insert "WikiWord")
    (goto-char 1)
    (should (hywiki-maybe-at-wikiword-beginning))
    (goto-char 2)
    (should-not (hywiki-maybe-at-wikiword-beginning)))
  (dolist (acceptable-char '("(" "{" "<" "\"" "'" "`" "\t" "\n" "\r" "\f" " "))
    (with-temp-buffer
      (hywiki-tests--insert (format "%sWikiWord" acceptable-char))
      (goto-char 2)
      (should (hywiki-maybe-at-wikiword-beginning)))))

(ert-deftest hywiki-tests--in-page-p ()
  "Verify `hywiki-in-page-p' identifies a page in and outside of the wiki directory."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (cdr (hywiki-add-page "WikiWord")))
         (no-wiki-page (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect no-wiki-page)
            (should-not (hywiki-in-page-p)))
          (with-current-buffer (find-file-noselect wiki-page)
            (should (hywiki-in-page-p))))
      (hy-delete-files-and-buffers (list no-wiki-page wiki-page))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--active-in-current-buffer-p ()
  "Verify `hywiki-active-in-current-buffer-p'."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (cdr (hywiki-add-page "WikiWord")))
         (hywiki-mode :all))
    (unwind-protect
        (with-current-buffer (find-file-noselect wiki-page)
          (should (hywiki-active-in-current-buffer-p))
          (hywiki-mode nil)
          (should-not (hywiki-active-in-current-buffer-p))
          (let ((hywiki-exclude-major-modes (list 'org-mode)))
            (should-not (hywiki-active-in-current-buffer-p)))
	  (hywiki-mode nil)
          (mocklet ((hywiki-in-page-p => nil))
            (should-not (hywiki-active-in-current-buffer-p)))
          (dired-mode)
          (should-not (hywiki-active-in-current-buffer-p)))
      (hy-delete-file-and-buffer wiki-page)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--directory-get-mod-time ()
  "Verify `hywiki-directory-get-mod-time'."
  (mocklet ((file-readable-p => nil))
    (should (null (hywiki-directory-get-mod-time))))
  (mocklet ((file-readable-p => t)
            (file-attributes => '(t 0 0 0 nil (100 100 100 100) nil 0 "drwxr-xr-x" t 0 0)))
    (should (equal '(100 100 100 100) (hywiki-directory-get-mod-time)))))

(ert-deftest hywiki-tests--directory-modified-p ()
  "Verify `hywiki-directory-modified-p'.
Both mod-time and checksum must be changed for a test to return true."
  (let ((hywiki--directory-mod-time nil))
    (should (hywiki-directory-modified-p)))
  (let ((hywiki--directory-mod-time '(100 100 100 100))
	(hywiki--directory-checksum "3"))

    ;; Verify not modified checksum and mod-time.
    (mocklet ((hywiki-directory-get-mod-time => '(100 100 100 100))
	      (hywiki-directory-get-checksum => "3"))
      (should-not (hywiki-directory-modified-p)))

    ;; Next test case should never be able to occur since a change in
    ;; the filenames in the checksum necessitates a change in the
    ;; mod-time variable.  So the modified test should return nil.
    ;; Verify modified checksum.
    (mocklet ((hywiki-directory-get-mod-time => '(100 100 100 100))
	      (hywiki-directory-get-checksum => "4"))
      (should-not (hywiki-directory-modified-p)))

    ;; Verify modified mode-time.
    (mocklet ((hywiki-directory-get-mod-time => '(10 10 10 10))
	      (hywiki-directory-get-checksum => "3"))
      (should-not (hywiki-directory-modified-p)))

    ;; Verify both mod-time and checksum modified.
    (mocklet ((hywiki-directory-get-mod-time => '(10 10 10 10))
	      (hywiki-directory-get-checksum => "4"))
      (should (hywiki-directory-modified-p)))))

(ert-deftest hywiki-tests--get-page-list ()
  "Verify `hywiki-get-wikiword-list' returns one WikiWord."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (cdr (hywiki-add-page "WikiWord"))))
    (unwind-protect
        (progn
          (should (equal '("WikiWord") (hywiki-get-wikiword-list)))
          (should (equal wiki-page (cdr (hywiki-add-page "WikiWord"))))
          (should (equal '("WikiWord") (hywiki-get-wikiword-list))))
      (hy-delete-file-and-buffer wiki-page)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-after-add-and-delete ()
  "Verify `hywiki-get-wikiword-list' is updated when a page is added and removed."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (cdr (hywiki-add-page "WordOne"))))
    (unwind-protect
        (let ((wiki-page2 (cdr (hywiki-add-page "WordTwo"))))
	  (unwind-protect
              (should (set:equal '("WordOne" "WordTwo")
				 (hywiki-get-wikiword-list)))
	    ;; This delay is necessary or the test can fail sporadically
	    (sit-for 0.01)
            (hy-delete-file-and-buffer wiki-page2))
	  (should (set:equal '("WordOne") (hywiki-get-wikiword-list))))
      (hy-delete-file-and-buffer wiki-page)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-multiple-words ()
  "Verify `hywiki-get-wikiword-list' returns multiple WikiWords."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (basename "WikiWord")
         (wiki-page-list nil))
    (unwind-protect
        (progn
          (dolist (char '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"))
            (push (cdr (hywiki-add-page (format "%s%s" basename char)))
		  wiki-page-list))
          (should (= 10 (length wiki-page-list)))
          (should (= 10 (length (hywiki-get-wikiword-list))))
          (should (= 10 (length (seq-uniq (hywiki-get-wikiword-list))))))
      (hy-delete-files-and-buffers wiki-page-list)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-when-new-wiki-directory ()
  "Verify `hywiki-get-wikiword-list' is empty for new `hywiki-directory'."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (cdr (hywiki-add-page "WikiWord"))))
    (unwind-protect
        (progn
          (should (= 1 (length (hywiki-get-wikiword-list))))
          (let ((hywiki-directory (make-temp-file "hywiki" t)))
            (unwind-protect
                (progn
                  (should (= 0 (length (hywiki-get-wikiword-list)))))
              (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))
      (hy-delete-file-and-buffer wikipage)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-for-new-wiki-directory-after-added-referent ()
  "Verify `hywiki-get-wikiword-list' is empty for new `hywiki-directory'."
  (defvar hywiki-add-referent-hook)
  (let ((hywiki-directory (make-temp-file "hywiki" t))
	(referent '(page . "/tmp/a.org"))
        (hywiki-add-referent-hook 'test-func))
    (unwind-protect
        (progn
          (mocklet (((test-func) => t))
            (should (equal referent (hywiki-add-referent "WikiWord" referent))))
          (should (= 1 (length (hywiki-get-wikiword-list))))
          (let ((hywiki-directory (make-temp-file "hywiki" t)))
            (unwind-protect
                (should (zerop (length (hywiki-get-wikiword-list))))
              (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

;; Following test cases for verifying proper face is some what
;; experimental. They need to be run in interactive mode.

(ert-deftest hywiki-tests--face-property-for-wikiword-with-wikipage ()
  "Verify WikiWord for a wiki page gets face property hywiki-word-face."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
	   str)
      (hywiki-tests--insert (setq str "WikiWor"))
      (hywiki-tests--command-execute #'self-insert-command 1 ?d)
      (goto-char 4)
      (ert-info ((format "str = \"%s\"" str))
	(should (hywiki-word-face-at-p)))

      (erase-buffer)
      (hywiki-tests--insert (setq str "WikiWord"))
      (hywiki-tests--command-execute #'newline 1 'interactive)
      (goto-char 4)
      (ert-info ((format "str = \"%s\"" str))
	(should (hywiki-word-face-at-p))))))

(ert-deftest hywiki-tests--no-face-property-for-no-wikipage ()
  "Verify WikiWord for no wiki page does not get face property hywiki-word-face."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (hywiki-mode nil)
              (hywiki-tests--insert "WikiWor")
	      (hywiki-tests--command-execute #'self-insert-command 1 ?d)
              (goto-char 4)
              (should-not (hywiki-word-face-at-p))))
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--verify-face-property-when-editing-wikiword ()
  "Verify face property changes when WikiWord is edited."
  (hywiki-tests--preserve-hywiki-mode
    (hywiki-tests--insert "Wikiord ")
    (goto-char 5)
    (should (looking-at-p "ord "))
    (should-not (hywiki-word-face-at-p))

    (hywiki-tests--command-execute #'self-insert-command 1 ?W)
    (goto-char 5)
    (should (looking-at-p "Word "))
    (should (hywiki-word-face-at-p))

    (hywiki-tests--command-execute #'delete-char 1)
    (should (looking-at-p "ord "))
    (should-not (hywiki-word-face-at-p))))

(ert-deftest hywiki-tests--verify-face-property-when-editing-wikiword-first-char ()
  "Verify face property changes when WikiWord is edited in the first char position."
  (hywiki-tests--preserve-hywiki-mode
    (hywiki-tests--insert "WikiWord")
    (hywiki-tests--command-execute #'self-insert-command 1 ? )
    (goto-char 1)
    (should (looking-at-p "WikiWord"))
    (should (hywiki-word-face-at-p))

    (hywiki-tests--command-execute #'delete-char 1)
    (should (looking-at-p "ikiWord"))
    (should-not (hywiki-word-face-at-p))

    (hywiki-tests--command-execute #'self-insert-command 1 ?W)
    (goto-char 1)
    (should (looking-at-p "WikiWord"))
    (should (hywiki-word-face-at-p))))

(ert-deftest hywiki-tests--references-to-org-link ()
  "Verify `hywiki-references-to-org-links' converts WikiWords to org links."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikipage (cdr (hywiki-add-page "WikiWord"))))
      (unwind-protect
          (progn
            (hywiki-mode :all)
            (with-temp-buffer
	      (setq default-directory hywiki-directory)
              (hywiki-tests--insert "WikiWord#section:L2:C4")
              (hywiki-tests--command-execute #'self-insert-command 1 ? )
              (goto-char 4)
              (hywiki-references-to-org-links)
              (should (string= "[[hy:WikiWord#section:L2:C4]] "
                               (buffer-substring-no-properties (point-min) (point-max)))))
            (with-temp-buffer
              (hywiki-tests--insert "WikiWor")
	      (hywiki-tests--command-execute #'self-insert-command 1 ?d)
              (goto-char 4)
              (hywiki-references-to-org-links)
              (should (string= "[[hy:WikiWord]]"
                               (buffer-substring-no-properties (point-min) (point-max))))))
        (hy-delete-file-and-buffer wikipage)
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--at-tags-p ()
  "Verify `hywiki-at-tags-p'."
  (mocklet ((hsys-org-at-tags-p => nil))
    (should-not (hywiki-at-tags-p)))
  (mocklet ((hsys-org-at-tags-p not-called)
            (hywiki-in-page-p => nil))
    (should-not (hywiki-at-tags-p t)))
  (mocklet ((hsys-org-at-tags-p => t)
            (hywiki-in-page-p => t))
    (should (hywiki-at-tags-p)))
  (mocklet ((hsys-org-at-tags-p => t)
            (hywiki-in-page-p => nil))
    (mocklet ((buffer-name => "*HyWiki Tags*"))
      (should (hywiki-at-tags-p)))
    (mocklet ((buffer-name => "*Other Tags*"))
      (should-not (hywiki-at-tags-p)))))

(ert-deftest hywiki-tests--org-set-publish-project ()
  "Verify `hywiki-org-set-publish-project' sets publish vars with hywiki."
  (let (hywiki-org-publish-project-alist
        org-publish-project-alist)
    (should (hywiki-org-set-publish-project))
    (should (assoc "hywiki" org-publish-project-alist))
    (should (string= "hywiki" (car hywiki-org-publish-project-alist)))))

(ert-deftest hywiki-tests--org-get-publish-project ()
  "Verify `hywiki-org-get-publish-project' gets project or creates one if not found."
  ;; Gets project that exists
  (let ((org-publish-project-alist '(("hywiki"))) ; hywiki exists
        (hywiki-org-publish-project-alist 'var-has-a-value))
    (should (equal '("hywiki") (hywiki-org-get-publish-project))))
  ;; Created project if not exists
  (let (org-publish-project-alist)      ; hywiki does not exist
    (mocklet (((hywiki-org-set-publish-project) => '("project")))
      (should (equal '("project") (hywiki-org-get-publish-project))))))

(ert-deftest hywiki-tests--reference-to-referent ()
  "Verify `hywiki-reference-to-referent' resolves a reference to a referent."
  (should-not (hywiki-reference-to-referent 88)) ; Number
  (should-not (hywiki-reference-to-referent '("string"))) ; List
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (cdr (hywiki-add-page "WikiWord"))))
    (unwind-protect
        (progn
          (should-not (hywiki-reference-to-referent "NoWikiWord"))
          (should (when wikipage (string= wikipage (hywiki-reference-to-referent "WikiWord"))))
          (should (when wikipage (string= wikipage (hywiki-reference-to-referent "hy:WikiWord"))))
          (should (when wikipage (string= (concat wikipage "::section")
					  (hywiki-reference-to-referent "WikiWord#section")))))
      (hy-delete-file-and-buffer wikipage)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--org-link-export ()
  "Verify `hywiki-org-link-export' output for different formats."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (cdr (hywiki-add-page "WikiWord")))
	 (filename (when wikipage (file-name-nondirectory wikipage)))
	 (filename-stem (when filename (file-name-sans-extension filename)))
         (info "environment"))
    (unwind-protect
        (progn
	  (find-file wikipage)
          (should (string-match-p
                   (format "\\[hy\\] <doc:.*%s>" filename)
                   (hywiki-org-link-export "WikiWord" "doc" 'ascii info)))
          ;; FIXME: Solving this case with a mock for now.
          (mocklet (((hywiki--org-link-html-format "WikiWord" "" "doc" "environment") => (format "<a href=\".*%s.html\">doc</a>" "WikiWord")))
            (should (string-match-p
                     (format "<a href=\".*%s.html\">doc</a>" filename-stem)
                     (hywiki-org-link-export "WikiWord" "doc" 'html info))))
          (should (string-match-p
                   (format "\\[doc\\](.*%s.md)" filename-stem)
                   (hywiki-org-link-export "WikiWord" "doc" 'md info)))
          (should (string-match-p
                   (format "\\href{.*%s.latex}{doc}" filename-stem)
                   (hywiki-org-link-export "WikiWord" "doc" 'latex info)))
          (should (string-match-p
                   (format "@uref{.*%s.texi,doc}" filename-stem)
                   (hywiki-org-link-export "WikiWord" "doc" 'texinfo info)))
          (should (string-match-p
                   (format ".*%s" filename)
                   (hywiki-org-link-export "WikiWord" "doc" 'unknown info)))
          (should (string= "NotAWikiPage" (hywiki-org-link-export "NotAWikiPage" "doc" 'ascii nil))))
      (hy-delete-file-and-buffer wikipage)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--action-key-moves-to-word-and-section ()
  "Verify action key on a WikiWord with section, line and column works."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikipage (cdr (hywiki-add-page "WikiWord")))
           (words '(("WikiWord:L1" . "First line")
                    ("WikiWord:L1:C2" . "rst line")
                    ("WikiWord#Asection" . "* Asection")
                    ("WikiWord#Asection:L1" . "* Asection")
                    ("WikiWord#Asection:L2" . "body A")
                    ("WikiWord#Asection:L2:C2" . "dy A")
                    ("WikiWord#Bsection-subsection" . "** Bsection subsection")
                    ("WikiWord#Bsection-subsection:L2" . "body B")
                    ("WikiWord#Bsection-subsection:L2:C2" . "dy B")
                    ("(WikiWord#Bsection subsection)" . "** Bsection subsection")
                    ("(WikiWord#Asection)" . "* Asection")
                    )))
      (unwind-protect
          (progn
            ;; Setup target WikiWord
            (with-current-buffer (find-file-noselect wikipage)
              (hywiki-tests--insert "\
First line
* Asection
body A
** Bsection subsection
body B
")
              (save-buffer))
            ;; Create temp buffers with WikiWord links to the target
            ;; WikiWord page and verify they work.
            (with-temp-buffer
              (hywiki-mode :all)
              (let (wiki-link
		    expected-str-at-pos)
		(condition-case err-msg
		    (dolist (w words)
                      (setq wiki-link (car w)
			    expected-str-at-pos (cdr w))
		      (erase-buffer)
		      (hywiki-tests--insert wiki-link)
		      (goto-char 4)
		      (save-excursion
			(action-key)
			(should (looking-at-p expected-str-at-pos))))
		  (error (error "'%s', '%s' - Error: %s"
				wiki-link expected-str-at-pos err-msg))))))
        (hy-delete-file-and-buffer wikipage)
        (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory)))))

(defun hywiki-tests--search-section (section)
  "Find SECTION in current buffer and return the id string.
Search for elements of type <h?>...</h?> for the id string.  Example:
<h4 id=\"org7c18f23\"><span class=\"section-number-4\">1.1.1.</span> section</h4>
would return org7c18f23."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "<h. id=\"\\(.*?\\)\">.*</span> %s</h.>" section) nil t)
      (match-string-no-properties 1))))

(ert-deftest hywiki-tests--published-html-links-to-word-and-section ()
  "Verify published html links to WikiWord and section."
  ;; org-publish does not work properly to support HyWiki export prior
  ;; to version 9.7, so this will be skipped for Emacs 28 and 29.
  (skip-unless (string-greaterp org-version "9.6.999"))
  (hywiki-tests--preserve-hywiki-mode
    (let* (org-publish-project-alist
	   (hywiki-org-publishing-directory (make-temp-file "public_html_" t))
	   (wikipage (cdr (hywiki-add-page "WikiPage")))
	   (wikipage-html (expand-file-name "WikiPage.html" hywiki-org-publishing-directory))
	   (wikiword (cdr (hywiki-add-page "WikiWord")))
	   (wikiword-html (expand-file-name "WikiWord.html" hywiki-org-publishing-directory)))
      (unwind-protect
	  (progn
	    (hywiki-org-set-publish-project)
	    (should (file-exists-p hywiki-directory))
	    (should (file-exists-p wikipage))
	    (should (file-exists-p wikiword))

	    ;; Setup wiki pages for WikiWord and WikiPage.
	    (find-file wikiword)
	    (hywiki-tests--insert "\
First line
* Asection
body A
** Bsection subsection
body B
*** Csection-subsection
body C
")
	    (save-buffer)
	    (find-file wikipage)
	    (hywiki-tests--insert "\
WikiWord
WikiWord#Asection
\"WikiWord#Bsection subsection\"
WikiWord#Csection-subsection
")
	    (save-buffer)

	    ;; Export the wiki
	    (hywiki-publish-to-html t)

	    ;; Verify files and folder are generated
	    (should (file-exists-p hywiki-org-publishing-directory))
	    (should (file-exists-p wikipage-html))
	    (should (file-exists-p wikiword-html))

            (let (idA idB idC)
              ;; Verify anchors are generated and fetch their ids
              (with-current-buffer (find-file-noselect wikiword-html)
		(setq idA (should (hywiki-tests--search-section "Asection")))
		(setq idB (should (hywiki-tests--search-section "Bsection subsection")))
		(setq idC (should (hywiki-tests--search-section "Csection-subsection"))))

	      ;; Verify links are generated
	      (find-file wikipage-html)
	      ;; (First check we even get the wikipage with sections)
	      (should (<= 1 (count-matches (regexp-quote "WikiWord") (point-min) (point-max))))
	      (should (= 1 (count-matches (regexp-quote "WikiWord#Asection") (point-min) (point-max))))
	      (should (= 1 (count-matches (regexp-quote "WikiWord#Bsection subsection") (point-min) (point-max))))
	      (should (= 1 (count-matches (regexp-quote "WikiWord#Csection-subsection") (point-min) (point-max))))

	      ;; Then verify the href links are generated
	      (should (= 1 (count-matches (regexp-quote "<a href=\"WikiWord.html\">WikiWord</a>") (point-min) (point-max))))
	      (should (= 1 (count-matches
                            (format "<a href=\"WikiWord.html#%s\">WikiWord#Asection</a>" idA) (point-min) (point-max))))
	      (should (= 1 (count-matches
                            (format "<a href=\"WikiWord.html#%s\">WikiWord#Bsection subsection</a>" idB) (point-min) (point-max))))
	      (should (= 1 (count-matches
                            (format "<a href=\"WikiWord.html#%s\">WikiWord#Csection-subsection</a>" idC) (point-min) (point-max))))))
	(hy-delete-files-and-buffers (list wikipage wikiword wikipage-html wikiword-html
					   (expand-file-name "index.org" hywiki-directory)
					   (expand-file-name "index.html" hywiki-org-publishing-directory)))
	(hy-delete-dir-and-buffer hywiki-org-publishing-directory)))))

(ert-deftest hywiki-tests--publish-special-cases ()
  "Verify different special cases."
  ;; org-publish does not work properly to support HyWiki export prior
  ;; to version 9.7, so this will be skipped for Emacs 28 and 29.
  (skip-unless (string-greaterp org-version "9.6.999"))
  (hywiki-tests--preserve-hywiki-mode
    (let* (org-publish-project-alist
	   (hywiki-org-publishing-directory (make-temp-file "public_html_" t))
	   (wikipage (cdr (hywiki-add-page "WikiPage")))
	   (wikipage-html (expand-file-name "WikiPage.html" hywiki-org-publishing-directory))
	   (wikiword (cdr (hywiki-add-page "WikiWord")))
	   (wikiword-html (expand-file-name "WikiWord.html" hywiki-org-publishing-directory))
           (href "<a href=\"WikiWord.html\">WikiWord</a>"))
      (unwind-protect
	  (progn
	    (hywiki-org-set-publish-project)

            (find-file wikiword)
            (erase-buffer)
            (hywiki-tests--insert "Text\n")
	    (save-buffer)

            (should (file-exists-p hywiki-directory))
	    (should (file-exists-p wikipage))
	    (should (file-exists-p wikiword))

            (dolist (v `(("WikiWord WikiWord" . ,(format "%s %s" href href))
			 ("\"WikiWord WikiWord\"" . ,(format "\"%s %s\"" href href))
			 ;;                                       ^ Missing a space!?
			 ("WikiWord Text WikiWord" . ,(format "%s Text %s" href href))
			 ("\"WikiWord Text WikiWord\"" . ,(format "\"%s Text %s\"" href href))
			 ;;                                            ^ Missing " Text "
			 ("WikiWord WikiWord WikiWord" . ,(format "%s %s %s" href href href))
			 ;; !! TODO FIXME
			 ;; (cons "\"WikiWord WikiWord WikiWord\"" (format "\"%s %s %s\"" href href href))
			 ;; ^ Crashes due to (wrong-type-argument integer-or-marker-p nil) caused by buffer-substring-no-properties(nil nil)
			 ))
              (let ((input (car v))
                    (regex-output (cdr v))
                    (revert-without-query '(".*")))
		;; Setup WikiPage.
		(find-file wikipage)
                (erase-buffer)
                (hywiki-tests--insert input)
	        (save-buffer)

		;; Export the wiki
		(hywiki-publish-to-html t)

		;; Verify Export
		(ert-info ((format "Publish '%s' => Expect '%s'"
				   input
				   regex-output))
	          (find-file wikipage-html t)
                  (revert-buffer t t)
                  (should (= 1 (count-matches (regexp-quote regex-output)
					      (point-min) (point-max))))))))
	;; Unwind
	(hy-delete-files-and-buffers (list wikipage wikiword wikipage-html wikiword-html
					   (expand-file-name "index.org" hywiki-directory)
					   (expand-file-name "index.html" hywiki-org-publishing-directory)))
	(hy-delete-dir-and-buffer hywiki-org-publishing-directory)))))

(ert-deftest hywiki-tests--get-singular-wikiword ()
  "Verify plural WikiWord is converted to singular.
Note special meaning of `hywiki-allow-plurals-flag'."
  (let ((hywiki-allow-plurals-flag t))
    (should (string= "WikiWord" (hywiki-get-singular-wikiword "WikiWord")))
    (should (string= "WikiWord" (hywiki-get-singular-wikiword "WikiWords"))))
  (let ((hywiki-allow-plurals-flag nil))
    (should (string= "WikiWord" (hywiki-get-singular-wikiword "WikiWord")))
    (should (string= "WikiWords" (hywiki-get-singular-wikiword "WikiWords")))))

(ert-deftest hywiki-tests--get-plural-wikiword ()
  "Verify singular WikiWord is converted to plural."
  ;; Note: Should not this also respect `hywiki-allow-plurals-flag' so
  ;; this can be disabled completely?
  (should (string= "WikiWords" (hywiki-get-plural-wikiword "WikiWord")))
  (should (string= "Taxes" (hywiki-get-plural-wikiword "Tax")))
  ;; Exceptions
  (should (string= "Monarchs" (hywiki-get-plural-wikiword "Monarchs"))))

(ert-deftest hywiki-tests--add-referent ()
  "Verify `hywiki-add-referent'."
  (defvar hywiki-add-referent-hook)
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
	 (file "/tmp/a.org")
	 (referent (cons 'page file)))
    (unwind-protect
        (progn
          (should-not (hywiki-add-referent "notawikiword" referent))
          (should (hywiki-add-referent "WikiWord" referent))
          (should (equal referent (hywiki-get-referent "WikiWord"))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-activity ()
  "Verify `hywiki-add-activity'."
  (let ((hywiki-directory (make-temp-file "hywiki" t))
	(wikiword "WikiWord"))
    (unwind-protect
        (mocklet (((hypb:require-package 'activities) => t)
                  (activities-completing-read => "activity"))
          (hywiki-add-activity wikiword)
          (should (equal '(activity . "activity")
			 (hywiki-get-referent wikiword))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-bookmark ()
  "Verify `hywiki-add-bookmark'."
  (require 'bookmark)
  (let ((hywiki-directory (make-temp-file "hywiki" t))
        (bookmark-alist nil)
        (file (make-temp-file "hypb.txt")))
    (unwind-protect
        (progn
          (find-file file)
          (hy-test-helpers:ert-simulate-keys "\r"
            (should-error (hywiki-add-bookmark "")))
          (hy-test-helpers:ert-simulate-keys "WikiWord\r"
            (hywiki-add-bookmark "WikiWord")
            (should (equal '(bookmark . "WikiWord")
			   (hywiki-get-referent "WikiWord")))))
      (hy-delete-file-and-buffer file)
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-command ()
  "Verify `hywiki-add-command'."
  (let ((hywiki-directory (make-temp-file "hywiki" t))
	(wikiword "WikiWord"))
    (unwind-protect
	(hy-test-helpers:ert-simulate-keys "hpath:find\r"
	  (hywiki-add-command wikiword)
	  (should (equal '(command . hpath:find)
			 (hywiki-get-referent wikiword))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-find ()
  "Verify `hywiki-add-find'."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikiword "WikiWord")
	 (referent '(find . hywiki-word-grep)))
    (unwind-protect
        (progn
          (hywiki-add-find wikiword)
          (should (equal referent (hywiki-get-referent wikiword))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-global-button ()
  "Verify `hywiki-add-global-button'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(mocklet ((hargs:read-match => "gbtn"))
	  (should (equal '(global-button . "gbtn") (hywiki-add-global-button "WikiWord"))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-hyrolo ()
  "Verify `hywiki-add-hyrolo'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(progn
	  (hywiki-add-hyrolo "WikiWord")
	  (should (equal '(hyrolo . hyrolo-fgrep) (hywiki-get-referent "WikiWord"))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-info-index ()
  "Verify `hywiki-add-info-index'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (hy-test-helpers:ert-simulate-keys "files\r"
          (info "emacs")
	  (hywiki-add-info-index "WikiWord")
	  (should (equal '(info-index . "(emacs)files") (hywiki-get-referent "WikiWord"))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-info-node ()
  "Verify `hywiki-add-info-node'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(hy-test-helpers:ert-simulate-keys "(emacs)\r"
	  (hywiki-add-info-node "WikiWord")
	  (should (equal '(info-node . "(emacs)") (hywiki-get-referent "WikiWord"))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-key-series ()
  "Verify `hywiki-add-key-series'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(progn
	  (hy-test-helpers:ert-simulate-keys "ABC\r"
	    (hywiki-add-key-series "WikiWord")
	    (should (equal '(key-series . "{ABC}") (hywiki-get-referent "WikiWord"))))
	  (hy-test-helpers:ert-simulate-keys "{ABC}\r"
	    (hywiki-add-key-series "WikiWord")
	    (should (equal '(key-series . "{ABC}") (hywiki-get-referent "WikiWord")))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-path-link ()
  "Verify `hywiki-add-path-link'."
  (let ((hywiki-directory (make-temp-file "hywiki" t))
	(wikiword "WikiWord"))
    (unwind-protect
	(progn (hywiki-add-path-link wikiword "file" 20)
	       (should (equal '(path-link . "file:L1")
			      (hywiki-get-referent wikiword))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-org-id ()
  "Verify `hywiki-add-org-id'."
  ;; Error case - Non org-mode buffer
  (let ((wikiword "WikiWord")
        (hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (progn
          (let ((filea (make-temp-file "hypb" nil ".txt")))
            (unwind-protect
                (with-current-buffer (find-file filea)
                  (mocklet (((hmouse-choose-link-and-referent-windows) => (list nil (get-buffer-window))))
                    (should-error (hywiki-add-org-id wikiword) :type '(error))))
	      (hy-delete-file-and-buffer filea)))

          (let ((filea (make-temp-file "hypb" nil ".org")))
            (unwind-protect
                (with-current-buffer (find-file filea)
                  (hywiki-tests--insert "* header\n")

                  ;; Error-case - No Org ID and read only
                  (setq buffer-read-only t)
                  (mocklet (((hmouse-choose-link-and-referent-windows) => (list nil (get-buffer-window))))
	            (should-error (hywiki-add-org-id wikiword) :type '(error))

                    ;; Normal case - Org-mode with Org ID
                    (goto-char (point-max))
                    (setq buffer-read-only nil)
	            (let ((referent-value (cdr (hywiki-add-org-id wikiword))))
		      (if (stringp referent-value)
		          (should (string-prefix-p "ID: " referent-value))
		        (error "(hywiki-tests--add-org-id): referent value is a non-string: %s" referent-value)))))
	      (hy-delete-file-and-buffer filea))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-org-roam-node ()
  "Verify `hywiki-add-org-roam-node'."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
	 (wikiword (hy-make-random-wikiword)))
    (unwind-protect
        (mocklet ((cl-struct-org-roam-node-tags => nil)
		  ((hypb:require-package 'org-roam) => t)
		  ((org-roam-node-read) => "node")
		  (org-roam-node-title => "node-title"))
	  (hywiki-add-org-roam-node wikiword)
          (should (equal '(org-roam-node . "node-title")
			 (hywiki-get-referent wikiword))))
      (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(defmacro hywiki-tests--referent-test (expected-referent &rest prepare)
  "Template macro for generating a non-page HyWikiWord referent.
EXPECTED-REFERENT is the result expected from `hywiki-get-referent'.
The template runs the PREPARE body, and that must add the HyWikiWord
named WikiReferent with a non-page referent type."
  (declare (indent 0) (debug t))
  `(let* ((hsys-consult-flag nil)
	  (hywiki-directory (make-temp-file "hywiki" t))
	  (wiki-word-non-page "WikiReferent")
          (mode-require-final-newline nil))
     (unwind-protect
         (save-excursion
           (should (equal '() (hywiki-get-wikiword-list)))

           ,@prepare

	   ;; Stop checking existence of cache file since there may be
	   ;; a race condition that makes it not exist yet.
           ;; (should (file-exists-p (hywiki-cache-default-file)))

           (should (equal ,expected-referent (hywiki-get-referent wiki-word-non-page)))

           ;; Simulate reload from cache
           (hywiki-cache-save)
           (setq hywiki--referent-hasht nil)
           (hywiki-make-referent-hasht)

           (should (equal ,expected-referent (hywiki-get-referent wiki-word-non-page))))
       (hy-delete-files-and-buffers (list (hywiki-cache-default-file)))
       (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--save-referent-keyseries ()
  "Verify saving and loading a referent keyseries works ."
  (hywiki-tests--referent-test
    (cons 'key-series "{ABC}")
    (hy-test-helpers:ert-simulate-keys "ABC\r"
      (hywiki-add-key-series wiki-word-non-page))))

(ert-deftest hywiki-tests--save-referent-keyseries-use-menu ()
  "Verify saving and loading a referent keyseries works using Hyperbole's menu."
  ; The failure is intermittent. See expanded test case below.
  (skip-unless (not noninteractive))
  `(let* ((hywiki-directory (make-temp-file "hywiki" t))
          (wiki-page (cdr (hywiki-add-page "WikiPage")))
          (mode-require-final-newline nil)
	  wiki-page-buffer)
     (unwind-protect
         (save-excursion
	   (setq wiki-page-buffer (find-file wiki-page))
	   (erase-buffer)
	   (hywiki-tests--insert "WikiWord")
           (save-buffer)
           (goto-char 4)
	   (should (hact 'kbd-key "C-u C-h hhck {C-e SPC ABC} RET"))
	   (hy-test-helpers:consume-input-events)
	   (should (equal (cons 'key-series "C-e SPC {ABC}")
			  (hywiki-get-referent "WikiWord")))
	   (should (string-equal "Wiki{C-e ABC}Referent"
				 (buffer-substring-no-properties
				  (point-min)
				  (point-max)))))
       (hy-delete-files-and-buffers (list wiki-page (hywiki-cache-default-file)))
       (hywiki-tests--delete-hywiki-dir-and-buffer hywiki-directory))))

;; Bookmark
(ert-deftest hywiki-tests--save-referent-bookmark ()
  "Verify saving and loading a referent bookmark works."
  (hywiki-tests--referent-test
    (cons 'bookmark wiki-word-non-page)
    (let ((file (make-temp-file "hypb")))
      (unwind-protect
          (progn
            (find-file file)
            (hy-test-helpers:ert-simulate-keys (concat wiki-word-non-page "\r")
              (hywiki-add-bookmark wiki-word-non-page)))
        (hy-delete-file-and-buffer file)))))

;; Command
(defun hywiki-tests--command (wikiword)
  "Verify WIKIWORD is WikiReferent."
  (interactive)
  (should (string= "WikiReferent" wikiword)))

(ert-deftest hywiki-tests--save-referent-command ()
  "Verify saving and loading a referent command works."
  (hywiki-tests--referent-test
    (cons 'command #'hywiki-tests--command)
    (hy-test-helpers:ert-simulate-keys "hywiki-tests--command\r"
      (hywiki-add-command wiki-word-non-page))))

(ert-deftest hywiki-tests--save-referent-command-use-menu ()
  "Verify saving and loading a referent command works using Hyperbole's menu.."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
    (progn
      (sit-for 0.2)
      (cons 'command #'hywiki-tests--command))
    (let ((vertico-mode 0))
      (should (hact 'kbd-key "C-u C-h hhc WikiReferent RET c hywiki-tests--command RET"))
      (hy-test-helpers:consume-input-events))))

;; Find
(ert-deftest hywiki-tests--save-referent-find ()
  "Verify saving and loading a referent find works."
  (hywiki-tests--referent-test
    (cons 'find #'hywiki-word-grep)
    (hywiki-add-find wiki-word-non-page)))

(ert-deftest hywiki-tests--save-referent-find-use-menu ()
  "Verify saving and loading a referent find works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (hywiki-tests--referent-test
      (progn
        (sit-for 0.2)
        (cons 'find #'hywiki-word-grep))
      (let ((vertico-mode 0))
        (find-file wiki-page)
        (hywiki-tests--insert "\nWikiReferent\n")
        (save-buffer)
        (goto-char (point-min))
        (should (hact 'kbd-key "C-u C-h hhc WikiReferent RET f RET"))
        (hy-test-helpers:consume-input-events)))))

;; Global-button
(ert-deftest hywiki-tests--save-referent-global-button ()
  "Verify saving and loading a referent global-button works."
  (hywiki-tests--referent-test
    (cons 'global-button "gbtn")
    (mocklet ((hargs:read-match => "gbtn"))
      (hywiki-add-global-button wiki-word-non-page))))

(ert-deftest hywiki-tests--save-referent-global-button-use-menu ()
  "Verify saving and loading a referent global-button works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
    (progn
      (sit-for 0.2)
      (cons 'global-button "global"))
    (defvar test-buffer)
    (let* ((test-file (make-temp-file "gbut" nil ".txt"))
           (test-buffer (find-file-noselect test-file)))
      (unwind-protect
          (with-mock
            (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
            (stub gbut:label-list => (list "global"))
            (mock (gbut:act "global") => t)
            (gbut:ebut-program "global" 'link-to-file test-file)
            (should (hact 'kbd-key "C-u C-h hhc WikiReferent RET g global RET"))
            (hy-test-helpers:consume-input-events))
        (hy-delete-file-and-buffer test-file)))))

;; HyRolo
(ert-deftest hywiki-tests--save-referent-hyrolo ()
  "Verify saving and loading a referent hyrolo works."
  (hywiki-tests--referent-test
    (cons 'hyrolo #'hyrolo-fgrep)
    (hywiki-add-hyrolo wiki-word-non-page)))

;; Info index
(ert-deftest hywiki-tests--save-referent-info-index ()
  "Verify saving and loading a referent info index works."
  (hywiki-tests--referent-test
    (cons 'info-index "(emacs)files")
    (save-excursion
      (hy-test-helpers:ert-simulate-keys "files\r"
        (info "emacs")
        (hywiki-add-info-index wiki-word-non-page)))))

(ert-deftest hywiki-tests--save-referent-info-index-use-menu ()
  "Verify saving and loading a referent info index works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (ert-skip "The menu key sequence works when used manually but fails here for unknown reasons. Skip this for now.")
  (hywiki-tests--referent-test
    (cons 'info-index "(emacs)files")
    (save-excursion
      (unwind-protect
          (progn
            (should (hact 'kbd-key "C-u C-h hhc WikiReferent RET i (emacs)files RET"))
            (hy-test-helpers:consume-input-events))
        (kill-buffer "*info*")))))

;; Info node
(ert-deftest hywiki-tests--save-referent-info-node ()
  "Verify saving and loading a referent info node works."
  (hywiki-tests--referent-test
    (cons 'info-node "(emacs)")
    (save-excursion
      (unwind-protect
          (hy-test-helpers:ert-simulate-keys "(emacs)\r"
            (hywiki-add-info-node wiki-word-non-page))
        (kill-buffer "*info*")))))

(ert-deftest hywiki-tests--save-referent-info-node-use-menu ()
  "Verify saving and loading a referent info node works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
    (progn
      (sit-for 0.2)
      (cons 'info-node "(emacs)"))
    (save-excursion
      (unwind-protect
          (progn
            (should (hact 'kbd-key "C-u C-h hhc WikiReferent RET n (emacs) RET"))
            (hy-test-helpers:consume-input-events))
        (kill-buffer "*info*")))))

;; Path link
(ert-deftest hywiki-tests--save-referent-path-link ()
  "Verify saving and loading a referent path link works."
  (hywiki-tests--referent-test
    (cons 'path-link "file:L1")
    (hywiki-add-path-link wiki-word-non-page "file" 1)))

;; Org id
(ert-deftest hywiki-tests--save-referent-org-id ()
  "Verify saving and loading a referent org id works."
  (hywiki-tests--referent-test
    (cons 'org-id "ID: generated-org-id")
    (save-excursion
      (let ((filea (make-temp-file "hypb" nil ".org")))
        (unwind-protect
            (with-current-buffer (find-file filea)
              (hywiki-tests--insert "* header\n")
              (mocklet (((hmouse-choose-link-and-referent-windows) => (list nil (get-buffer-window)))
                        ((org-id-get-create) => "generated-org-id"))
                (goto-char (point-max))
	        (hywiki-add-org-id wiki-word-non-page)))
	  (hy-delete-file-and-buffer filea))))))

;; FIXME: Add Org-id links tests.

;; Org roam
(ert-deftest hywiki-tests--save-referent-org-roam-node ()
  "Verify saving and loading a referent org roam node works."
  (hywiki-tests--referent-test
    (cons 'org-roam-node "node-title")
    (mocklet (((hypb:require-package 'org-roam) => t)
	      ((org-roam-node-read) => "node")
	      ((org-roam-node-title "node") => "node-title"))
      (hywiki-add-org-roam-node wiki-word-non-page))))

(ert-deftest hywiki-tests--save-referent-org-roam-node-use-menu ()
  "Verify saving and loading a referent org roam node works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
    (progn
      (sit-for 0.2)
      (cons 'org-roam-node "node-title"))
    (mocklet (((hypb:require-package 'org-roam) => t)
	      ((org-roam-node-read) => "node")
	      ((org-roam-node-title "node") => "node-title")
              ((hywiki-display-org-roam-node "WikiReferent" "node-title") => t))
      (should (hact 'kbd-key "C-u C-h hhc WikiReferent RET r"))
      (hy-test-helpers:consume-input-events))))

(ert-deftest hywiki-tests--delete-parenthesised-char ()
  "Verify removing a char between parentheses only removes the char.
See gh#rswgnu/hyperbole/669."
  (with-temp-buffer
    (hywiki-tests--insert "(a)")
    (goto-char 2)
    (hywiki-tests--command-execute #'delete-char 1)
    (should (string= "()" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hywiki-tests--word-face-at-p ()
  "Verify `hywiki-word-face-at-p'."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
   (hywiki-mode nil)
   (hywiki-tests--insert "WikiWor")
   (hywiki-tests--command-execute #'self-insert-command 1 ?d)
   (goto-char 4)
   (should-not (hywiki-word-face-at-p))

   (erase-buffer)
   (hywiki-mode :all)
   (hywiki-tests--insert "WikiWor")
   (hywiki-tests--command-execute #'self-insert-command 1 ?d)
   (goto-char 4)
   (should (hywiki-word-face-at-p))))

(defun hywiki-tests--hywiki-face-regions ()
  "Return (start . end) for all hywiki--word-face overlays in buffer.
The result is returned as a lexicographical sorted list to make
comparison with expected overlays stable."
  (let (overlay-list)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (equal (overlay-get overlay 'face) 'hywiki--word-face)
        (push (cons (overlay-start overlay) (overlay-end overlay)) overlay-list)))
    (sort overlay-list
          (lambda (x y)
            (or (< (car x) (car y))
                (and (= (car x) (car y))
                     (< (cdr x) (cdr y))))))))

(defvar hywiki-tests--with-face-test t
  "Non-nil to perform face validation of WikiWord.")

(defun hywiki-tests--word-at ()
  "Test if there is a HyWikiWord reference at point with a referent.
Choose what test to perform based on value of `hywiki-tests--with-face-test'."
  (when (hywiki-referent-exists-p)
    (if hywiki-tests--with-face-test
	(hywiki-highlighted-word-at)
      (hywiki-word-at))))

(defun hywiki-tests--verify-hywiki-word (expected)
  "Verify that `hywiki-word-at' returns t if a wikiword is EXPECTED.
If EXPECTED is a string, also verify that the wikiword matches the
string."
  (ert-info ((format (concat "buffer name = \"%s\"\n"
			     "major-mode = %s\n"
			     "(hywiki-active-in-current-buffer-p) = %s\n"
			     "pre-command-hook = %s\n"
			     "buffer contents = \"%s\"\n"
			     "hywiki-tests--with-face-test = %s\n"
			     "expected = \"%s\"")
		     (buffer-name)
		     major-mode
		     (hywiki-active-in-current-buffer-p)
		     pre-command-hook
		     (buffer-substring-no-properties (point-min) (point-max))
		     hywiki-tests--with-face-test
		     expected))
    (if (not expected)
	(should-not (hywiki-tests--word-at))
      (let ((hywiki-reference (hywiki-tests--word-at)))
	(if (stringp expected)
            (should (string= expected hywiki-reference))
          (should hywiki-reference))
	(should (hywiki-word-is-p hywiki-reference))))))

(defun hywiki-tests--run-test-case (test-case)
  "Run the TEST-CASE from point.
Each test case consists of cons cells with an operation and the
expected state of the highlighted WikiWord reference being
constructed.  Operations are either a string to be inserted, a
number of chars to be deleted or a symbol p<number> for where to
move point.  The expected state is either nil for not a wikiword
or non-nil for a wikiword.  The state is checked after all chars
of the string are inserted.  If equal to a string it is checked
for match with the wikiword.  Movement of point is relative to
point when the function is called."
  (let ((origin (point)))
    (ert-info ((format "Test case => '%s'" test-case))
      (dolist (steps test-case)
	(let ((step (car steps))
              (vfy (cdr steps)))
          (cond ((stringp step)
		 (dolist (ch (string-to-list step))
                   (hywiki-tests--command-execute #'self-insert-command 1 ch))
		 (save-excursion
                   (goto-char (1- (point)))
                   (hywiki-tests--verify-hywiki-word vfy)))
		((integerp step)
		 (let ((forward (> step 0)))
                   (dotimes (_ (abs step))
                     (if forward
			 (hywiki-tests--command-execute #'delete-forward-char 1)
                       (hywiki-tests--command-execute #'backward-delete-char 1)))
                   (hywiki-tests--verify-hywiki-word vfy)))
		((and (symbolp step) (string-prefix-p "p" (symbol-name step)))
		 (let* ((pos (string-to-number (substring (symbol-name step) 1)))
			(newpos (max (min (+ origin (1- pos)) (point-max))
				     (point-min))))
                   (when (or (> (point-min) newpos) (< (point-max) newpos))
                     (ert-fail (format "New point: '%s' is outside of buffer" newpos)))
                   (goto-char newpos))
		 (hywiki-tests--verify-hywiki-word vfy))
		(t (ert-fail (format "Unknown step: '%s' in WikiWord verification" step)))))))))

(defconst hywiki-tests--wikiword-step-check
  '(
    (("Hi" . "Hi"))
    (("HiHo" . t) ("#"))
    (("HiHo" . t) ("#s " . "HiHo#s"))
    (("HiHo" . t) ("#s" . t) (-2 . "HiHo"))
    (("HiHo#s" . t) (-4 . t) (-1) ("i" . "Hi"))
    (("HiHo#s " . t) ("n"))
    (("HiHo#s " . t) (" n"))
    ;; With delimiters
    (("(HiHo#s" . "HiHo#s") (" " . "HiHo#s"))
    (("(HiHo#s" . "HiHo#s") (")" . "HiHo#s")) ; Delimiter part of WikiWord. See below too.
    (("(HiHo#s" . "HiHo#s") ("-" . "HiHo#s-") ("n" . "HiHo#s-n") (")" . "HiHo#s-n"))
    ;; Insert and delete between WikiWords and non WikiWords
    (("HiHo" . t) (p3 . t) (" " . "Hi") (p4 . "Ho") (-1 . "HiHo"))
    (("Hiho" . t) (p3 . t) (" " . "Hi") (p4) (-1 . "Hiho"))
    (("hiHo") (p3) (" ") (p4 . "Ho") (-1))
    ;; With double quotes
    (("\"HiHo\"" . t) (p4 . t) (" " . "Hi") (p5 . "Ho") (-1 . "HiHo"))
    (("\"Hiho\"" . t) (p4 . t) (" " . "Hi") (p5) (-1 . "Hiho"))
    (("\"hiHo\"") (p4) (" ") (p5 . "Ho") (-1))
    (("\"Hi\"Ho" . t) (p5 . "Ho") (" " . "Ho") (p4 . "Hi"))
    (("Hi\"Ho\"" . t) (p3 . "Hi") (" " . "Hi") (p4) (p5 . "Ho"))
    )
  "List of test cases for WikiWords.")

(ert-deftest hywiki-tests--wikiword-step-check-verification ()
  "Run the step check to verify WikiWord is identified under change.
Performs each operation from the step check and verifies if the
resulting state at point is a WikiWord or not."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiHiHo (cdr (hywiki-add-page "HiHo")))
           (wikiHiho (cdr (hywiki-add-page "Hiho")))
           (wikiHi (cdr (hywiki-add-page "Hi")))
           (wikiHo (cdr (hywiki-add-page "Ho")))
           (wiki-page-list (list wikiHiHo wikiHiho wikiHi wikiHo))
           (hywiki-tests--with-face-test nil))
      (unwind-protect
          (dolist (testcase hywiki-tests--wikiword-step-check)
	    (erase-buffer)
            (hywiki-tests--run-test-case testcase))
        (hy-delete-files-and-buffers wiki-page-list)))))

(ert-deftest hywiki-tests--wikiword-step-check-verification-with-faces ()
  "Run the step check to verify WikiWord is identified under change.
Perform each operation from the step check and verify whether there
is a WikiWord at point or not."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiHiHo (cdr (hywiki-add-page "HiHo")))
           (wikiHiho (cdr (hywiki-add-page "Hiho")))
           (wikiHi (cdr (hywiki-add-page "Hi")))
           (wikiHo (cdr (hywiki-add-page "Ho")))
           (wiki-page-list (list wikiHiHo wikiHiho wikiHi wikiHo)))
      (unwind-protect
          (dolist (testcase hywiki-tests--wikiword-step-check)
	    (erase-buffer)
            (hywiki-tests--run-test-case testcase))
        (hy-delete-files-and-buffers wiki-page-list)))))

(defconst hywiki-tests--lorem-ipsum "\
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse
aliquet diam euismod turpis ultricies, et porta sem blandit. Sed vitae."
  "Bulk text for in the middle of text tests.")

(ert-deftest hywiki-tests--wikiword-step-check-verification-with-surrounding-text ()
  "Run the step check to verify WikiWord is identified under change.
Insert test in the middle of other text."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiHiHo (cdr (hywiki-add-page "HiHo")))
           (wikiHiho (cdr (hywiki-add-page "Hiho")))
           (wikiHi (cdr (hywiki-add-page "Hi")))
           (wikiHo (cdr (hywiki-add-page "Ho")))
           (wiki-page-list (list wikiHiHo wikiHiho wikiHi wikiHo)))
      (unwind-protect
          (progn
            (insert hywiki-tests--lorem-ipsum)
            (goto-char (/ (point-max) 2))
            (let ((pos (point)))
              (hywiki-tests--insert " HiHo ")
              (goto-char (1+ pos))
              (should (looking-at-p "HiHo ")))
            (hywiki-tests--run-test-case
             '((p3 . t)
               (" " . "Hi")
               (p1 . t) (p4 . t) (-1 . t)))

            (erase-buffer)
            (insert hywiki-tests--lorem-ipsum)
            (goto-char (/ (point-max) 2))
            (let ((pos (point)))
              (hywiki-tests--insert " Hiho ")
              (goto-char (1+ pos))
              (should (looking-at-p "Hiho ")))
            (hywiki-tests--run-test-case
             '((p3 . t)
               (" " . "Hi")
               (p1 . t) (p4) (-1 . "Hiho"))))
        (hy-delete-files-and-buffers wiki-page-list)))))

(ert-deftest hywiki-tests--wikiword-step-check-edit-wikiword-in-emacs-lisp-mode ()
  "Run the step check to verify WikiWord is identified under change in a docstring.
A WikiWord is completed, then last char is deleted and reinserted.  The
face is verified during the change."
  (hywiki-tests--preserve-hywiki-mode
    (emacs-lisp-mode)
    (hywiki-tests--insert "\
(defun func ()
  \"WikiWor\"
")
    ;; Set point after WikiWor
    (goto-char 1)
    (should (search-forward "WikiWor"))

    ;; Complete WikiWord and verify highlighting
    (hywiki-tests--run-test-case
     '(("d" . "WikiWord") ("\"" . "WikiWord") (p2 . t) (-1) ("d" . "WikiWord")))))

(ert-deftest hywiki-tests--wikiword-identified-in-emacs-lisp-mode ()
  "Verify WikiWord is identified when surrounded by delimiters in `emacs-lisp-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (emacs-lisp-mode)
    (let* ((hsys-org-enable-smart-keys t)
	   str)
      ;; Matches a WikiWord
      (dolist (v '("WikiWord" "[WikiWord]" "{WikiWord}" "(WikiWord)"
                   "<WikiWord>" "[WikiWord AnotherWord]"
                   ))
	(erase-buffer)
        (hywiki-tests--insert (setq str (format ";; %s" v)))
        (hywiki-tests--command-execute #'newline 1 'interactive)
        (goto-char 9)
	(ert-info ((format "str = \"%s\"" str))
          (should (string= "WikiWord" (hywiki-tests--word-at))))

	(erase-buffer)
        (hywiki-tests--insert (setq str (format "(setq var \"%s\")" v)))
        (hywiki-tests--command-execute #'newline 1 'interactive)
        (goto-char 16)
	(ert-info ((format "str = \"%s\"" str))
          (should (string= "WikiWord" (hywiki-tests--word-at)))))

      ;; Does not highlight as a WikiWord
      (dolist (v '("WikiWord#" "[[WikiWord]]" "<<WikiWord>>"
		   "{[[WikiWord]]}" "([[WikiWord]])"))
	(erase-buffer)
        (hywiki-tests--insert (setq str (format ";; %s" v)))
        (hywiki-tests--command-execute #'newline 1 'interactive)
        (goto-char 9)
	(ert-info ((format "str = \"%s\"" str))
          (should-not (hywiki-tests--word-at)))

	(erase-buffer)
        (hywiki-tests--insert (setq str (format  "(setq var \"%s\")" v)))
        (hywiki-tests--command-execute #'newline 1 'interactive)
        (goto-char 16)
	(ert-info ((format "str = \"%s\"" str))
          (should-not (hywiki-tests--word-at)))))))

(ert-deftest hywiki-tests--wikiword-identified-in-strings-in-emacs-lisp-mode ()
  "Verify WikiWord is identified when in strings in `emacs-lisp-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (let ((words '("Foo" "Bar" "Baz" "Qux")))
      (emacs-lisp-mode)
      (insert
       (format "\
(defun a ()
  \"%s.\"
  nil)
" (mapconcat 'identity words " ")))
      (goto-char (point-min))
      (forward-line 1)
      (dolist (v words)
        (should (search-forward v))
        (should (string= v (hywiki-word-at)))))))

(ert-deftest hywiki-tests--filename-same-as-wiki-word ()
  "Regular files should not be WikiWords even when hywiki-mode is active."
  (hywiki-tests--preserve-hywiki-mode
    (let ((default-directory hywiki-directory))
      (hywiki-tests--insert "\"WikiWord\" \"WikiWord.org\"\n")
      (goto-char 2)
      (should (looking-at-p "WikiWord\" "))
      (hywiki-mode nil)
      (should-not (ibtype:test-p 'hywiki-existing-word))
      (hywiki-mode :all)
      (should (ibtype:test-p 'hywiki-existing-word))
      (goto-char 13)
      ;; Verify that using the org extension selects the WikiWord.
      (should (looking-at-p "WikiWord\\.org\""))
      (should (ibtype:test-p 'pathname)))))

(ert-deftest hywiki-tests--nonexistent-wikiword-with-section-should-create-wikiword ()
  "Verify action-key on a new WikiWord#section creates proper page filename."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiword "WikiWd")
	   (section "#section")
	   (wikifile (expand-file-name
		      (concat wikiword hywiki-file-suffix)
		      hywiki-directory)))
      (unwind-protect
	  (progn
	    (hywiki-tests--insert (concat wikiword section))
	    (goto-char 4)
	    (action-key)
	    (sit-for 0.01)
	    (should-not (file-exists-p (expand-file-name
					(concat wikiword hywiki-file-suffix section)
					hywiki-directory)))
	    (should (file-exists-p wikifile)))
	(hy-delete-file-and-buffer wikifile)))))

(ert-deftest hywiki-tests--verify-removal-of-delimiter-updates-face ()
  "Verify WikiWord highlight face change when adding/removing a delimiter."
  :expected-result :failed
  (hywiki-tests--preserve-hywiki-mode
    (let ((hywiki-tests--with-face-test t))
      (setq wiki-page (cdr (hywiki-add-page "Hi")))
      (dolist (testcase
               '((("\"Hi#a b c\"") (p3 . "Hi#a b c") (p11) (-1) (p3 . "Hi#a") (p10) ("\"") (p3 . "Hi#a b c"))
                 (("(Hi#s n)" . "Hi#s n") (-1) (p3 . "Hi#s") (p8) (")" . "Hi#s n"))))
	(erase-buffer)
        (hywiki-tests--run-test-case testcase)))))

(ert-deftest hywiki-tests--wikiword-yanked-with-extra-words ()
  "Verify that a yanked in WikiWord highlights properly."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiHi (cdr (hywiki-add-page "Hi")))
           (wikiHo (cdr (hywiki-add-page "Ho")))
           (hywiki-tests--with-face-test t))
      (unwind-protect
          (progn
            ;; Left part of WikiWord yanked in.
            (hywiki-tests--insert "i#s")
            (goto-char 1)
            (let ((kill-ring (list "H"))
                  interprogram-paste-function)
              (yank))
            (hywiki-tests--verify-hywiki-word "Hi#s")

            ;; Right part of WikiWord yanked in.
	    (erase-buffer)
            (hywiki-tests--insert "H")
            (let ((kill-ring (list "i#s"))
                  interprogram-paste-function)
              (yank))
            (goto-char 2)
            (hywiki-tests--verify-hywiki-word "Hi#s")

            ;; Non WikiWords in front of WikiWord.
	    (erase-buffer)
            (let ((kill-ring (list "a b Hi#c"))
                  interprogram-paste-function)
              (yank))
            (goto-char 1)
            (hywiki-tests--verify-hywiki-word nil)
            (goto-char 6)
            (hywiki-tests--verify-hywiki-word "Hi#c")

            ;; Non WikiWords after WikiWord.
	    (erase-buffer)
            (let ((kill-ring (list "Hi#a b c"))
                  interprogram-paste-function)
              (yank))
            (goto-char 2)
            (hywiki-tests--verify-hywiki-word "Hi#a")

            ;; Multiple WikiWords with non WikiWords.
	    (erase-buffer)
            (let ((kill-ring (list "a Hi#b c Ho#d e"))
                  interprogram-paste-function)
              (yank))
            (goto-char 4)
            (hywiki-tests--verify-hywiki-word "Hi#b")
            (goto-char 11)
            (hywiki-tests--verify-hywiki-word "Ho#d"))
        (hy-delete-files-and-buffers (list wikiHi wikiHo))))))

(ert-deftest hywiki-tests--create-wikiword-file-highlights-wikiword ()
  "Verify creating a WikiWord-file highlights the WikiWord in another file."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiHi (cdr (hywiki-add-page "Hi")))
           (hywiki-tests--with-face-test t)
           wikiHo)
      (unwind-protect
          (progn
            (with-current-buffer (find-file wikiHi)
              (hywiki-tests--insert "Ho")
              (save-buffer)
              (setq wikiHo (cdr (hywiki-add-page "Ho")))
              (goto-char 2)
              (hywiki-tests--verify-hywiki-word "Ho")))
        (hy-delete-files-and-buffers (list wikiHi wikiHo))))))

(ert-deftest hywiki-tests--maybe-highlight-page-names ()
  "Verify `hywiki-maybe-highlight-references'.
Start and stop point of all highlighted regions in the buffer, as
computed by `hywiki-tests--hywiki-face-regions', are compared to the
expected result."
  :expected-result :failed
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiword (cdr (hywiki-add-page "WiWo")))
	   input
	   overlay-regions)
      (unwind-protect
          (dolist (v `(("WiWo" . ((1 . 5)))
                       ("WiWo text" . ((1 . 5)))
                       ("WiWo WiWo" . ((1 . 5) (6 . 10)))
                       ("WiWo text WiWo" . ((1 . 5) (11 . 15)))
                       ("\"WiWo\"" . ((2 . 6)))
                       ("\"WiWo text\"" . ((2 . 6)))
                       ;; Failing tests below.
                       ("\"WiWo WiWo\"" . ((2 . 6) (7 . 11)))
                       ("\"WiWo text WiWo\"" . ((2 . 6) (12 . 16)))
                       ("\"WiWo WiWo WiWo\"" . ((2 . 6) (7 . 11) (12 . 16)))))
            (setq input (car v)
                  overlay-regions (cdr v))
            (hywiki-tests--insert input)
	    (hywiki-maybe-highlight-references (point-min) (point-max))
	    ;; Verify Overlays
            (ert-info ((format "Text '%s' => Expected overlays '%s'" input overlay-regions))
              (should (equal (hywiki-tests--hywiki-face-regions) overlay-regions))))
        ;; Unwind
        (hy-delete-file-and-buffer wikiword)))))

(ert-deftest hywiki-tests--consult-grep ()
  "Verify `hywiki-consult-grep' calls `hsys-consult-grep'."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-consult-flag nil))
      ;; No path list
      (mocklet (((hsys-consult-grep "--include *.org" "--glob *.org" "regexp" 0 (list hywiki-directory) "prompt") => "match"))
        (should (string= (hywiki-consult-grep "regexp" 0 nil "prompt") "match")))
      ;; Path list
      (mocklet (((hsys-consult-grep "--include *.org" "--glob *.org" "regexp" 0 '("path") "prompt") => "match"))
        (should (string= (hywiki-consult-grep "regexp" 0 '("path") "prompt") "match")))
      ;; No Prompt, max-matches = 0
      (mocklet (((hsys-consult-grep "--include *.org" "--glob *.org" "regexp" 0 '("path") "Grep HyWiki dir headlines") => "match"))
        (should (string= (hywiki-consult-grep "regexp" 0 '("path")) "match")))
      ;; No Prompt, max-matches != 0
      (mocklet (((hsys-consult-grep "--include *.org" "--glob *.org" "regexp" 1 '("path") "Grep HyWiki dir") => "match"))
        (should (string= (hywiki-consult-grep "regexp" 1 '("path")) "match"))))))

(ert-deftest hywiki-tests--hywiki-help ()
  "Verify `hywiki-help'."
  (mocklet (((hkey-actions) => t)
            ((hkey-help) => "hkey-help"))
    (should (string= (hywiki-help) "hkey-help")))
  (let ((help-bn "*Help: HyWikiWords*"))
    (unwind-protect
        (progn
          (when (get-buffer help-bn)
            (kill-buffer help-bn))
          (mocklet (((hkey-actions) => nil)
                    (hkey-help not-called))
            (hywiki-help)
            (should (get-buffer help-bn))
            (with-current-buffer help-bn
              (should (string-prefix-p "On a HyWikiWord" (buffer-string))))))
      (kill-buffer help-bn))))

(ert-deftest hywiki-tests--add-path-link-v2 ()
  "Verify path links."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikiHi (cdr (hywiki-add-page "Hi")))
           (wikiHo (cdr (hywiki-add-page "Ho"))))
      (unwind-protect
          (progn
            (hywiki-mode :all)
            (with-current-buffer (find-file wikiHo)
              (hywiki-tests--insert "123"))
            (with-current-buffer (find-file wikiHi)
              (hywiki-add-path-link "HoRef" wikiHo 3)
              (should (string= (concat (file-name-nondirectory wikiHo) ":L1:C2")
			       (cdr (hywiki-get-referent "HoRef"))))))
        (hy-delete-files-and-buffers (list wikiHi wikiHo))))))

(ert-deftest hywiki-test--hywiki-mode ()
  "Verify activating local and global `hywiki-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (should (eq nil (hywiki-mode 0)))
    (should (eq nil (hywiki-mode -1)))
    (should (eq nil (hywiki-mode nil)))
    (should (eq :pages (hywiki-mode 2)))
    (should (eq :all (hywiki-mode 1)))
    (should (eq :all (hywiki-mode t)))
    (should (eq :all (hywiki-mode :all)))

    ;; Toggle
    (should (eq nil (call-interactively #'hywiki-mode)))
    (should (eq :all (call-interactively #'hywiki-mode)))
    (should (eq nil (hywiki-mode 'toggle)))
    (should (eq :all (hywiki-mode 'toggle)))))

(ert-deftest hywiki-tests--interactive-hywiki-mode-toggles ()
  "Verify `hywiki-mode' called interactively toggles mode."
  (hywiki-tests--preserve-hywiki-mode
    (should hywiki-mode)
    ;; Toggle
    (call-interactively #'hywiki-mode)
    (should-not hywiki-mode)
    (call-interactively #'hywiki-mode)
    (should hywiki-mode)))

(ert-deftest hywiki-tests--directory-dired-edit ()
  "Verify Dired is activated."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((wikiHi (cdr (hywiki-add-page "Hi")))
           (action-key-modeline-buffer-id-function nil)) ; Avoid treemacs.
      (unwind-protect
          (progn
            (hywiki-directory-edit)
            (should (equal 'dired-mode major-mode))
            (should (string= default-directory (file-name-as-directory hywiki-directory))))
        (hy-delete-files-and-buffers (list wikiHi))))))

(ert-deftest hywiki-tests--tags-view ()
  "Verify `hywiki-tag-view' calls `org-tags-view' and sets up `org-redo-cmd'."
  (hywiki-tests--preserve-hywiki-mode
    (hywiki-tests--insert "1\n2\n3\n4\n5\n")
    (goto-char 1)
    (let ((bn (buffer-name)))
      (mocklet (((org-tags-view nil "match") => t))
        (hywiki-tags-view nil "match" bn)
        (should (equal (get-text-property 1 'org-redo-cmd)
                       (list #'hywiki-tags-view nil nil bn)))
        (should (= (line-number-at-pos) 3)))

      ;; todo-only
      (erase-buffer)
      (hywiki-tests--insert "1\n2\n3\n4\n5\n")
      (goto-char 1)
      (mocklet (((org-tags-view t "match") => t))
        (hywiki-tags-view t "match" bn)
        (should (equal (get-text-property 1 'org-redo-cmd)
		       (list #'hywiki-tags-view t nil bn)))
        (should (= (line-number-at-pos) 3))))))

(provide 'hywiki-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hywiki-tests.el ends here
