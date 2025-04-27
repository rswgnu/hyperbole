;;; hywiki-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    18-May-24 at 23:59:48
;; Last-Mod:     27-Apr-25 at 10:11:50 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024-2025  Free Software Foundation, Inc.
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

(defun hywiki-tests--command-execute (cmd &rest rest)
  "Run CMD, with optional REST params, between calls to pre and post hooks.
This is for simulating the command loop."
  (setq last-command this-command)
  (setq this-command cmd)
  (run-hooks 'pre-command-hook)
  (if rest
      (apply cmd rest)
    (command-execute cmd))
  (run-hooks 'post-command-hook))

(defmacro hywiki-tests--preserve-hywiki-mode (&rest body)
  "Restore hywiki-mode after running BODY."
  (declare (indent 0) (debug t))
  `(let ((current-hywiki-mode hywiki-mode))
     (unwind-protect
	 (progn ,@body)
       (hywiki-mode (if current-hywiki-mode 1 0)))))

(ert-deftest hywiki-tests--verify-preserve-hywiki-mode ()
  "Verify `hywiki-tests--preserve-hywiki-mode' restores `hywiki-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (hywiki-mode 1)
    (hywiki-tests--preserve-hywiki-mode
      (should hywiki-mode)
      (hywiki-mode 0)
      (should-not hywiki-mode))
    (should hywiki-mode)))

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
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--hywiki-create-page--adds-no-wiki-word-fails ()
  "Verify add page requires a WikiWord."
  ;; Should not leave erroneously created file after test but leaving
  ;; added error cleanup till later if it is even needed!? No file
  ;; should be created so only happens on error!? (If this is
  ;; considered an error case that is.)
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (should-not (cdr (hywiki-add-page "notawikiword")))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--action-key-on-hywikiword-displays-page ()
  "Verify `action-key' on a prefixed WikiWord, outside of hywiki-directory, creates a new page."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t))
          (wikifile "WikiWord.org"))
      (unwind-protect
          (with-temp-buffer
	    (hywiki-mode 1)
            (insert "[[hy:WikiWord]]")
            (goto-char 4)
            (action-key)
	    (should (equal (cons 'page wikifile) (hywiki-get-referent "WikiWord"))))
        (hy-delete-file-and-buffer (expand-file-name wikifile hywiki-directory))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--assist-key-on-hywikiword-displays-help ()
  "Verify `assist-key' on a prefixed WikiWord, outside of hywiki-directory, displays help for the WikiWord link."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (with-temp-buffer
	    (hywiki-mode 1)
            (insert "[[hy:WikiWord]]")
            (goto-char 6)
            (should (string= "WikiWord" (hywiki-word-at)))
            (delete-other-windows)
            (assist-key)
            (other-window 1)
            (should (string-prefix-p "*Help: Hyperbole " (buffer-name))))
        (hy-delete-dir-and-buffer hywiki-directory)))))

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
	      (hywiki-mode 1)
              (insert "WikiWord\n")
              (goto-char 4)
              (action-key)
              (should (string= hywiki-page-file (buffer-file-name)))
              (should (equal (cons 'page (file-name-nondirectory hywiki-page-file))
                             (hywiki-get-referent "WikiWord")))
              (if v
                  (should (looking-at-p "WikiWord page"))
                (insert "WikiWord page")
                (goto-char (point-min)))))
        (hy-delete-file-and-buffer hywiki-page-file)
        (hy-delete-dir-and-buffer hywiki-directory)))))

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
              (insert (format "%s\nbody\n" v)))
            (save-buffer)
	    (hywiki-mode 1)
            (dolist (v sections)
              (with-temp-buffer
                (insert "WikiWord#" (cadr (split-string v " ")))
                (goto-char 4)
                (action-key)
                (should (string= hywiki-page-file (buffer-file-name)))
	        (should (looking-at-p (regexp-quote v))))))
        (hy-delete-file-and-buffer hywiki-page-file)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--action-key-on-wikiword-and-line-column-displays-page ()
  "Verify `action-key' on a WikiWord with line and column specifications goes to expected point."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t))
	   (hywiki-page-file (expand-file-name "WikiWord.org" hywiki-directory)))
      (unwind-protect
          (progn
            (find-file hywiki-page-file)
            (insert "\
line 1
line 2
")
            (save-buffer)
	    (hywiki-mode 1)
            (dolist (l '(1 2))
              (dolist (c '("" ":C0" ":C5"))
                (with-temp-buffer
                  (insert (format "WikiWord:L%s%s" l c))
                  (goto-char 4)
                  (action-key)
                  (should (string= hywiki-page-file (buffer-file-name)))
                  (if (string= c ":C5")
	              (should (looking-at-p (format "%s$" l)))
	            (should (looking-at-p (format "line %s$" l))))))))
        (hy-delete-file-and-buffer hywiki-page-file)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--not-a-wikiword-unless-in-hywiki-mode ()
  "Verify WikiWord is not a WikiWord unless in `hywiki-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (with-temp-buffer
            (hywiki-mode 0)
            (insert "WikiWord")
            (goto-char 4)
            (should-not (hywiki-word-at))
            (hywiki-mode 1)
            (should (string= "WikiWord" (hywiki-word-at))))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--a-wikiword-in-hywiki-directory ()
  "Verify WikiWord is identified if in `hywiki-directory'."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t))
           (referent (hywiki-add-page "WikiWord"))
	   (wiki-page (cdr referent)))
      (unwind-protect
          (with-current-buffer (find-file-noselect wiki-page)
            (hywiki-mode 0)
            (insert "AnotherWikiWord")
	    (newline nil t)
            (goto-char 4)
            (should (hywiki-word-at)))
        (hy-delete-file-and-buffer wiki-page)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--wikiword-identified-with-delimiters ()
  "Verify WikiWord is identified when surrounded by delimiters."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (hywiki-mode 1)

          ;; Matches a WikiWord
          (dolist (v '("WikiWord" "[WikiWord]" "[[WikiWord]]" "{WikiWord}" "(WikiWord)"
                       "<WikiWord>" "<<WikiWord>>" "{[[WikiWord]]}" "([[WikiWord]])"
                       "[WikiWord AnotherWord WikiWord WikiWord]"
                       ))
            (with-temp-buffer
              (org-mode)
              (insert v)
	      (newline nil t)
              (goto-char 6)
              (if (string= "WikiWord" (hywiki-word-at))
		  (should t)
		(should-not v))))

          ;; Does not match as a WikiWord
          (dolist (v '("WikiWord#"))
            (with-temp-buffer
              (org-mode)
              (insert v)
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
              (insert v)
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
              (insert v)
	      (newline nil t)
              (goto-char 6)
              (font-lock-ensure)
              (should (hsys-org-face-at-p 'org-link))
              (if (string= "WikiWord" (hywiki-word-at))
		  (should-not v)
		(should t)))))
      (hywiki-mode 0)
      (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--at-wikiword-finds-word-and-section ()
  "Verify `hywiki-word-at' finds WikiWord and section if available."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hywiki-directory (make-temp-file "hywiki" t))
          (words '("WikiWord" "WikiWord:L1" "WikiWord:L1:C2"
                   "WikiWord#section" "WikiWord#section:L1" "WikiWord#section:L1:C2"
                   "WikiWord#section-subsection" "WikiWord#section-subsection:L1" "WikiWord#section-subsection:L1:C2"
                   ;; !! FIXME: Uncomment when implemented.
                   ;; ("(WikiWord#section with spaces)" . "WikiWord#section with spaces")
                   ;; ("(WikiWord#section)" . "WikiWord#section")
                   )))
      (unwind-protect
          (with-temp-buffer
            (hywiki-mode 1)
            (dolist (w words)
              (let ((in (if (stringp w) w (car w)))
                    (expect (if (stringp w) w (cdr w))))
                (erase-buffer)
                (insert in)
                (goto-char 4)
                (should (string= expect (hywiki-word-at))))))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--sections-with-dash-space ()
  "Verify `hywiki-word-at' finds sections with dash and space."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (hywiki-mode 1)
            (with-temp-buffer
              (insert "WikiWord#section rest is ignored")
              (goto-char 4)
              (should (string= "WikiWord#section" (hywiki-word-at))))
            (with-temp-buffer
              (insert "WikiWord#section-with-dash")
              (goto-char 4)
              (should (string= "WikiWord#section-with-dash" (hywiki-word-at))))
            (with-temp-buffer
              (insert "WikiWord#\"section-within-quotes\"")
              (goto-char 4)
              (should-not (string= "WikiWord#\"section-within-quotes\"" (hywiki-word-at)))))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--word-is-p ()
  "Verify `hywiki-word-is-p' identifies WikiWords."
  (should (hywiki-word-is-p "WikiWord"))
  (should (hywiki-word-is-p "WikiWord#section"))
  (should-not (hywiki-word-is-p "hy:WikiWord"))
  (should-not (hywiki-word-is-p "wikiWord")))

(ert-deftest hywiki-tests--maybe-at-wikiword-beginning ()
  "Verify `hywiki-maybe-at-wikiword-beginning' identifies if maybe at beginning of WikiWord."
  (with-temp-buffer
    (insert "WikiWord")
    (goto-char 1)
    (should (hywiki-maybe-at-wikiword-beginning))
    (goto-char 2)
    (should-not (hywiki-maybe-at-wikiword-beginning)))
  (dolist (acceptable-char '("(" "{" "<" "\"" "'" "`" "\t" "\n" "\r" "\f" " "))
    (with-temp-buffer
      (insert (format "%sWikiWord" acceptable-char))
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
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--active-in-current-buffer-p ()
  "Verify `hywiki-active-in-current-buffer-p'."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (cdr (hywiki-add-page "WikiWord")))
         (hywiki-word-highlight-flag t))
    (unwind-protect
        (with-current-buffer (find-file-noselect wiki-page)
          (should (hywiki-active-in-current-buffer-p))
          (let ((hywiki-word-highlight-flag nil))
            (should-not (hywiki-active-in-current-buffer-p)))
          (let ((hywiki-exclude-major-modes (list 'org-mode)))
            (should-not (hywiki-active-in-current-buffer-p)))
	  (let (hywiki-mode)
            (mocklet ((hywiki-in-page-p => nil))
              (should-not (hywiki-active-in-current-buffer-p))))
          (dired-mode)
          (should-not (hywiki-active-in-current-buffer-p)))
      (hy-delete-file-and-buffer wiki-page)
      (hy-delete-dir-and-buffer hywiki-directory))))

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
      (hy-delete-dir-and-buffer hywiki-directory))))

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
      (hy-delete-dir-and-buffer hywiki-directory))))

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
      (hy-delete-dir-and-buffer hywiki-directory))))

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
              (hy-delete-dir-and-buffer hywiki-directory))))
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

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
              (hy-delete-dir-and-buffer hywiki-directory))))
      (hy-delete-dir-and-buffer hywiki-directory))))

;; Following test cases for verifying proper face is some what
;; experimental. They need to be run in interactive mode.

(ert-deftest hywiki-tests--face-property-for-wikiword-with-wikipage ()
  "Verify WikiWord for a wiki page gets face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t))
           (wikipage (cdr (hywiki-add-page "WikiWord"))))
      (unwind-protect
          (progn
            (hywiki-mode 1)
            (with-temp-buffer
              (insert "WikiWor")
	      (hywiki-tests--command-execute #'self-insert-command 1 ?d)
              (goto-char 4)
              (should (hywiki-word-face-at-p)))
            (with-temp-buffer
              (insert "WikiWord")
	      (hywiki-tests--command-execute #'newline 1 'interactive)
              (goto-char 4)
              (should (hywiki-word-face-at-p))))
        (hy-delete-file-and-buffer wikipage)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--no-face-property-for-no-wikipage ()
  "Verify WikiWord for no wiki page does not get face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hsys-org-enable-smart-keys t)
           (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (hywiki-mode 0)
              (insert "WikiWor")
	      (hywiki-tests--command-execute #'self-insert-command 1 ?d)
              (goto-char 4)
              (should-not (hywiki-word-face-at-p))))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--verify-face-property-when-editing-wikiword ()
  "Verify face property changes when WikiWord is edited."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikipage (cdr (hywiki-add-page "WikiWord"))))
      (unwind-protect
          (with-temp-buffer
            (hywiki-mode 1)
            (insert "Wikiord ")
            (goto-char 5)
            (should (looking-at-p "ord "))
            (should-not (hywiki-word-face-at-p))

            (hywiki-tests--command-execute #'self-insert-command 1 ?W)
            (goto-char 5)
            (should (looking-at-p "Word "))
            (should (hywiki-word-face-at-p))

            (hywiki-tests--command-execute #'delete-char 1)
            (should (looking-at-p "ord "))
            (should-not (hywiki-word-face-at-p)))
        (hy-delete-files-and-buffers (list wikipage))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--verify-face-property-when-editing-wikiword-first-char ()
  "Verify face property changes when WikiWord is edited in the first char position."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikipage (cdr (hywiki-add-page "WikiWord"))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (hywiki-mode 1)
              (insert "WikiWord")
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
        (hy-delete-files-and-buffers (list wikipage))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--convert-words-to-org-link ()
  "Verify `hywiki-convert-words-to-org-links' converts WikiWords to org links."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikipage (cdr (hywiki-add-page "WikiWord"))))
      (unwind-protect
          (progn
            (hywiki-mode 1)
            (with-temp-buffer
              (insert "WikiWord")
              (hywiki-tests--command-execute #'self-insert-command 1 ? )
              (goto-char 4)
              (hywiki-convert-words-to-org-links)
              (should (string= "[[WikiWord]] "
                               (buffer-substring-no-properties (point-min) (point-max)))))
            (with-temp-buffer
              (insert "WikiWor")
	      (hywiki-tests--command-execute #'self-insert-command 1 ?d)
              (goto-char 4)
              (hywiki-convert-words-to-org-links)
              (should (string= "[[WikiWord]]"
                               (buffer-substring-no-properties (point-min) (point-max))))))
        (hy-delete-file-and-buffer wikipage)
        (hy-delete-dir-and-buffer hywiki-directory)))))

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

(ert-deftest hywiki-tests--org-link-resolve ()
  "Verify `hywiki-org-link-resolve' resolves a link to page."
  (should-not (hywiki-org-link-resolve 88)) ; Number
  (should-not (hywiki-org-link-resolve '("string"))) ; List
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (cdr (hywiki-add-page "WikiWord")))
	 (filename (when wikipage (file-name-nondirectory wikipage))))
    (unwind-protect
        (progn
          (should-not (hywiki-org-link-resolve "NoWikiWord"))
          (should (when filename (string= filename (hywiki-org-link-resolve "WikiWord"))))
          (should (when filename (string= filename (hywiki-org-link-resolve "hy:WikiWord"))))
          (should (when filename (string= (concat filename "::section")
					  (hywiki-org-link-resolve "WikiWord#section")))))
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--org-link-export ()
  "Verify `hywiki-org-link-export' output for different formats."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (cdr (hywiki-add-page "WikiWord")))
	 (filename (when wikipage (file-name-nondirectory wikipage)))
	 (filename-stem (when filename (file-name-sans-extension filename))))
    (unwind-protect
        (progn
          (should (string-match-p
                   (format "\\[hy\\] <doc:.*%s>" filename)
                   (hywiki-org-link-export "WikiWord" "doc" 'ascii)))
          (should (string-match-p
                   (format "<a href=\".*%s.html\">doc</a>" filename-stem)
                   (hywiki-org-link-export "WikiWord" "doc" 'html)))
          (should (string-match-p
                   (format "\\[doc\\](.*%s.md)" filename-stem)
                   (hywiki-org-link-export "WikiWord" "doc" 'md)))
          (should (string-match-p
                   (format "\\href{.*%s.latex}{doc}" filename-stem)
                   (hywiki-org-link-export "WikiWord" "doc" 'latex)))
          (should (string-match-p
                   (format "@uref{.*%s.texi,doc}" filename-stem)
                   (hywiki-org-link-export "WikiWord" "doc" 'texinfo)))
          (should (string-match-p
                   (format ".*%s" filename)
                   (hywiki-org-link-export "WikiWord" "doc" 'unknown)))
          (should (string= "NotAWikiPage" (hywiki-org-link-export "NotAWikiPage" "doc" 'ascii))))
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

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
                    ;; !! FIXME: Uncomment when implemented.
                    ;; ("(WikiWord#Bsection subsection)" . "** Bsection subsection")
                    ;; ("(WikiWord#Asection)" . "* Asection")
                    )))
      (unwind-protect
          (progn
            ;; Setup target WikiWord
            (with-current-buffer (find-file-noselect wikipage)
              (insert "\
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
              (hywiki-mode 1)
              (dolist (w words)
                (let ((wiki-link (car w))
                      (expected-str-at-pos (cdr w)))
                  (erase-buffer)
                  (insert wiki-link)
                  (goto-char 4)
                  (save-excursion
                    (action-key)
                    ;; (should (string-prefix-p "WikiWord.org" (buffer-name)))
                    (should (looking-at-p expected-str-at-pos)))))))
        (hy-delete-file-and-buffer wikipage)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--published-html-links-to-word-and-section ()
  "Verify published html links to WikiWord and section."
  :expected-result :failed
  (let* ((hywiki-directory (make-temp-file "hywiki_" t))
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
          (with-current-buffer (find-file-noselect wikiword)
            (insert "\
First line
* Asection
body A
** Bsection subsection
body B
")
            (save-buffer))
          (with-current-buffer (find-file-noselect wikipage)
            (insert "\
* WikiWord
* WikiWord#Asection
* WikiWord#Bsection-subsection
")
            (save-buffer))

          ;; Export the wiki
          (hywiki-publish-to-html t)

          ;; Verify files and folder are generated
          (should (file-exists-p hywiki-org-publishing-directory))
          (should (file-exists-p wikipage-html))
          (should (file-exists-p wikiword-html))

          ;; Verify links are generated
          (with-current-buffer (find-file-noselect wikipage-html)
            ;; (First check we even get the wikipage with sections)
            (should (<= 1 (count-matches (regexp-quote "WikiWord") (point-min) (point-max))))
            (should (= 1 (count-matches (regexp-quote "WikiWord#Asection") (point-min) (point-max))))
            (should (= 1 (count-matches (regexp-quote "WikiWord#Bsection-subsection") (point-min) (point-max))))

            ;; Then verify the href links are generated
            (should (= 1 (count-matches (regexp-quote "<a href=\"WikiWord.html\">WikiWord</a>") (point-min) (point-max))))
            (should (= 1 (count-matches (regexp-quote "<a href=\"WikiWord.html#Asection\">WikiWord#ASection</a>") (point-min) (point-max))))
            (should (= 1 (count-matches (regexp-quote "<a href=\"WikiWord.html#Bsection-subsection\">WikiWord#Bsection-subsection</a>") (point-min) (point-max))))))
      (hy-delete-files-and-buffers (list wikipage wikiword wikipage-html wikiword-html
                                         (expand-file-name "index.org" hywiki-directory)
                                         (expand-file-name "index.html" hywiki-org-publishing-directory)))
      (hy-delete-dir-and-buffer hywiki-org-publishing-directory)
      (hy-delete-dir-and-buffer hywiki-directory))))

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
      (hy-delete-dir-and-buffer hywiki-directory))))

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
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-bookmark ()
  "Verify `hywiki-add-bookmark'."
  (require 'bookmark)
  (let ((hywiki-directory (make-temp-file "hywiki" t))
        (bookmark-alist nil)
        (file (make-temp-file "hypb.txt")))
    (unwind-protect
        (progn
          (find-file file)
          (ert-simulate-keys "\r"
            (should-error (hywiki-add-bookmark "")))
          (ert-simulate-keys "WikiWord\r"
            (hywiki-add-bookmark "WikiWord")
            (should (equal '(bookmark . "WikiWord")
			   (hywiki-get-referent "WikiWord")))))
      (hy-delete-file-and-buffer file)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-command ()
  "Verify `hywiki-add-command'."
  (let ((hywiki-directory (make-temp-file "hywiki" t))
	(wikiword "WikiWord"))
    (unwind-protect
	(ert-simulate-keys "hpath:find\r"
	  (hywiki-add-command wikiword)
	  (should (equal '(command . hpath:find)
			 (hywiki-get-referent wikiword))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-find ()
  "Verify `hywiki-add-find'."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikiword "WikiWord")
	 (referent '(find . hywiki-word-grep)))
    (unwind-protect
        (progn
          (hywiki-add-find wikiword)
          (should (equal referent (hywiki-get-referent wikiword))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-global-button ()
  "Verify `hywiki-add-global-button'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(mocklet ((hargs:read-match => "gbtn"))
	  (should (equal '(global-button . "gbtn") (hywiki-add-global-button "WikiWord"))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-hyrolo ()
  "Verify `hywiki-add-hyrolo'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(progn
	  (hywiki-add-hyrolo "WikiWord")
	  (should (equal '(hyrolo . hyrolo-fgrep) (hywiki-get-referent "WikiWord"))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-info-index ()
  "Verify `hywiki-add-info-index'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (ert-simulate-keys "files\r"
          (info "emacs")
	  (hywiki-add-info-index "WikiWord")
	  (should (equal '(info-index . "(emacs)files") (hywiki-get-referent "WikiWord"))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-info-node ()
  "Verify `hywiki-add-info-node'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(ert-simulate-keys "(emacs)\r"
	  (hywiki-add-info-node "WikiWord")
	  (should (equal '(info-node . "(emacs)") (hywiki-get-referent "WikiWord"))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-key-series ()
  "Verify `hywiki-add-key-series'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
	(progn
	  (ert-simulate-keys "ABC\r"
	    (hywiki-add-key-series "WikiWord")
	    (should (equal '(key-series . "{ABC}") (hywiki-get-referent "WikiWord"))))
	  (ert-simulate-keys "{ABC}\r"
	    (hywiki-add-key-series "WikiWord")
	    (should (equal '(key-series . "{ABC}") (hywiki-get-referent "WikiWord")))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-path-link ()
  "Verify `hywiki-add-path-link'."
  (let ((hywiki-directory (make-temp-file "hywiki" t))
	(wikiword "WikiWord"))
    (unwind-protect
	(progn (hywiki-add-path-link wikiword "file" 20)
	       (should (equal '(path-link . "file:L1")
			      (hywiki-get-referent wikiword))))
      (hy-delete-dir-and-buffer hywiki-directory))))

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
                  (insert "* header\n")

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
      (hy-delete-dir-and-buffer hywiki-directory))))

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
      (hy-delete-dir-and-buffer hywiki-directory))))

(defmacro hywiki-tests--referent-test (expected &rest prepare)
  "Referent test boilerplate code.
EXPECTED is the result expected from hywiki-get-referent.  PREPARE sets
up the test."
  (declare (indent 0) (debug t))
  `(let* ((hywiki-directory (make-temp-file "hywiki" t))
	  (wiki-referent "WikiReferent")
          (wiki-page (cdr (hywiki-add-page "WikiPage" )))
          (mode-require-final-newline nil)
	  wiki-page-buffer)
     (unwind-protect
         (save-excursion
           (should (equal '("WikiPage") (hywiki-get-wikiword-list)))
	   (setq wiki-page-buffer (find-file wiki-page))
	   (erase-buffer)
           (insert wiki-referent)
           (save-buffer)
           (goto-char 4)

           ,@prepare

           (should (equal ,expected (hywiki-get-referent wiki-referent)))
           (should (file-exists-p (hywiki-cache-default-file)))
	   (set-buffer wiki-page-buffer)
           (should (string= wiki-referent (buffer-substring-no-properties
					   (point-min) (point-max))))

           ;; Simulate reload from cache
           (hywiki-cache-save)
           (setq hywiki--referent-hasht nil)
           (hywiki-make-referent-hasht)

           (should (equal ,expected (hywiki-get-referent wiki-referent))))

       (hy-delete-files-and-buffers (list wiki-page (hywiki-cache-default-file)))
       (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--save-referent-keyseries ()
  "Verify saving and loading a referent keyseries works ."
  (hywiki-tests--referent-test
   (cons 'key-series "{ABC}")
   (ert-simulate-keys "ABC\r"
     (hywiki-add-key-series wiki-referent))))

(ert-deftest hywiki-tests--save-referent-keyseries-use-menu ()
  "Verify saving and loading a referent keyseries works using Hyperbole's menu."
  ; The failure is intermittent. See expanded test case below.
  (skip-unless (not noninteractive))
  `(let* ((hywiki-directory (make-temp-file "hywiki" t))
          (wiki-page (cdr (hywiki-add-page "WikiPage" )))
          (mode-require-final-newline nil)
	  wiki-page-buffer)
     (unwind-protect
         (save-excursion
	   (setq wiki-page-buffer (find-file wiki-page))
	   (erase-buffer)
	   (insert "WikiWord")
           (save-buffer)
           (goto-char 4)
	   (should (hact 'kbd-key "C-u C-h hhck {C-e SPC ABC} RET"))
	   (should (equal (cons 'key-series "C-e SPC {ABC}")
			  (hywiki-get-referent "WikiWord")))
	   (should (string-equal "Wiki{C-e ABC}Referent"
				 (buffer-substring-no-properties
				  (point-min)
				  (point-max)))))
       (hy-delete-files-and-buffers (list wiki-page (hywiki-cache-default-file)))
       (hy-delete-dir-and-buffer hywiki-directory))))

;; Bookmark
(ert-deftest hywiki-tests--save-referent-bookmark ()
  "Verify saving and loading a referent bookmark works."
  (hywiki-tests--referent-test
   (cons 'bookmark wiki-referent)
   (ert-simulate-keys (concat wiki-referent "\r")
     (hywiki-add-bookmark wiki-referent))))

;; Command
(defun hywiki-tests--command (wikiword)
  "Test command."
  (interactive)
  (should (string= "WikiReferent" wikiword)))

(ert-deftest hywiki-tests--save-referent-command ()
  "Verify saving and loading a referent command works."
  (hywiki-tests--referent-test
    (cons 'command #'hywiki-tests--command)
    (ert-simulate-keys "hywiki-tests--command\r"
      (hywiki-add-command wiki-referent))))

(ert-deftest hywiki-tests--save-referent-command-use-menu ()
  "Verify saving and loading a referent command works using Hyperbole's menu.."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
    (cons 'command #'hywiki-tests--command)
    (should (hact 'kbd-key "C-u C-h hhcc hywiki-tests--command RET"))
    (hy-test-helpers:consume-input-events)))

;; Find
(ert-deftest hywiki-tests--save-referent-find ()
  "Verify saving and loading a referent find works."
  (hywiki-tests--referent-test
    (cons 'find #'hywiki-word-grep)
    (hywiki-add-find wiki-referent)))

(ert-deftest hywiki-tests--save-referent-find-use-menu ()
  "Verify saving and loading a referent find works using Hyperbole's menu.."
  (skip-unless (not noninteractive))
  (skip-unless (not (version< emacs-version "29"))) ;; Fails on 28!?
  (hywiki-tests--referent-test
    (cons 'find #'hywiki-word-grep)
    (with-mock
      (mock (hywiki-word-grep "WikiReferent") => t)
      (should (hact 'kbd-key "C-u C-h hhcf"))
      (hy-test-helpers:consume-input-events))))

;; Global-button
(ert-deftest hywiki-tests--save-referent-global-button ()
  "Verify saving and loading a referent global-button works."
  (hywiki-tests--referent-test
   (cons 'global-button "gbtn")
   (mocklet ((hargs:read-match => "gbtn"))
     (hywiki-add-global-button wiki-referent))))

(ert-deftest hywiki-tests--save-referent-global-button-use-menu ()
  "Verify saving and loading a referent global-button works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
   (cons 'global-button "global")

   (defvar test-buffer)
   (let* ((test-file (make-temp-file "gbut" nil ".txt"))
          (test-buffer (find-file-noselect test-file)))
     (unwind-protect
         (with-mock
           (mock (hpath:find-noselect (expand-file-name hbmap:filename hbmap:dir-user)) => test-buffer)
           (stub gbut:label-list => (list "global"))
           (mock (gbut:act "global") => t)
           (gbut:ebut-program "global" 'link-to-file test-file)
           (should (hact 'kbd-key "C-u C-h hhcg global RET"))
           (hy-test-helpers:consume-input-events))
       (hy-delete-file-and-buffer test-file)))))

;; HyRolo
(ert-deftest hywiki-tests--save-referent-hyrolo ()
  "Verify saving and loading a referent hyrolo works."
  (hywiki-tests--referent-test
   (cons 'hyrolo #'hyrolo-fgrep)
   (hywiki-add-hyrolo wiki-referent)))

;; Info index
(ert-deftest hywiki-tests--save-referent-info-index ()
  "Verify saving and loading a referent info index works."
  (hywiki-tests--referent-test
   (cons 'info-index "(emacs)files")
   (save-excursion
     (ert-simulate-keys "files\r"
       (info "emacs")
       (hywiki-add-info-index wiki-referent)))))

(ert-deftest hywiki-tests--save-referent-info-index-use-menu ()
  "Verify saving and loading a referent info index works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
    (cons 'info-index "(emacs)files")
    (save-excursion
      (unwind-protect
          (progn
            (should (hact 'kbd-key "C-u C-h hhci (emacs)files RET"))
            (hy-test-helpers:consume-input-events))
        (kill-buffer "*info*")))))

;; Info node
(ert-deftest hywiki-tests--save-referent-info-node ()
  "Verify saving and loading a referent info node works."
  (hywiki-tests--referent-test
   (cons 'info-node "(emacs)")
   (save-excursion
     (unwind-protect
         (ert-simulate-keys "(emacs)\r"
           (hywiki-add-info-node wiki-referent))
       (kill-buffer "*info*")))))

(ert-deftest hywiki-tests--save-referent-info-node-use-menu ()
  "Verify saving and loading a referent info node works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
   (cons 'info-node "(emacs)")
   (save-excursion
     (unwind-protect
         (progn
           ;; (should (hact 'kbd-key "C-u C-h hhc MyWiki RET n (emacs) RET"))
           (should (hact 'kbd-key "C-u C-h hhcn (emacs) RET"))
           (hy-test-helpers:consume-input-events))
       (kill-buffer "*info*")))))

;; Path link
(ert-deftest hywiki-tests--save-referent-path-link ()
  "Verify saving and loading a referent path link works."
  (hywiki-tests--referent-test
   (cons 'path-link "file:L1")
   (hywiki-add-path-link wiki-referent "file" 1)))

;; Org id
(ert-deftest hywiki-tests--save-referent-org-id ()
  "Verify saving and loading a referent org id works."
  (hywiki-tests--referent-test
   (cons 'org-id "ID: generated-org-id")
   (save-excursion
     (let ((filea (make-temp-file "hypb" nil ".org")))
      (unwind-protect
          (with-current-buffer (find-file filea)
            (insert "* header\n")
            (mocklet (((hmouse-choose-link-and-referent-windows) => (list nil (get-buffer-window)))
                      ((org-id-get-create) => "generated-org-id"))
              (goto-char (point-max))
	      (hywiki-add-org-id wiki-referent)))
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
     (hywiki-add-org-roam-node wiki-referent))))

(ert-deftest hywiki-tests--save-referent-org-roam-node-use-menu ()
  "Verify saving and loading a referent org roam node works using Hyperbole's menu."
  (skip-unless (not noninteractive))
  (hywiki-tests--referent-test
   (cons 'org-roam-node "node-title")
   (mocklet (((hypb:require-package 'org-roam) => t)
	     ((org-roam-node-read) => "node")
	     ((org-roam-node-title "node") => "node-title")
             (hywiki-display-org-roam-node => t))
     (should (hact 'kbd-key "C-u C-h hhcr"))
     (hy-test-helpers:consume-input-events))))

(ert-deftest hywiki-tests--delete-parenthesised-char ()
  "Verify removing a char between parentheses only removes the char.
See gh#rswgnu/hyperbole/669."
  (with-temp-buffer
    (insert "(a)")
    (goto-char 2)
    (hywiki-tests--command-execute #'delete-char 1)
    (should (string= "()" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest hywiki-tests--word-face-at-p ()
  "Verify `hywiki-word-face-at-p'."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wiki-page (cdr (hywiki-add-page "WikiWord"))))
      (with-temp-buffer
        (hywiki-mode 0)
        (insert "WikiWor")
        (hywiki-tests--command-execute #'self-insert-command 1 ?d)
        (goto-char 4)
        (should-not (hywiki-word-face-at-p)))
      (unwind-protect
          (progn
            (with-temp-buffer
              (hywiki-mode 1)
              (insert "WikiWor")
	      (hywiki-tests--command-execute #'self-insert-command 1 ?d)
              (goto-char 4)
              (should (hywiki-word-face-at-p))))
        (hy-delete-file-and-buffer wiki-page)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(defun hywiki-tests--word-n-face-at ()
  "Non-nil if at a WikiWord and it has `hywiki--word-face'."
  (cl-destructuring-bind (word beg end) (hywiki-word-at :range)
    (when word
      (when (hy-test-word-face-at-region beg end)
        word))))

(defvar hywiki-tests--with-face-test nil
  "Non-nil to perform face validation of WikiWord.")

(defun hywiki-tests--word-at ()
  "Choose what test to perform based on value of `hywiki-tests--with-face-test'."
  (if hywiki-tests--with-face-test
      (hywiki-tests--word-n-face-at)
    (hywiki-word-at)))

(defun hywiki-tests--verify-hywiki-word (expected)
  "Verify that `hywiki-word-at' returns t if a wikiword is EXPECTED.
If EXPECTED is a string also verify that the wikiword matches the
string."
  (if (not expected)
      (should-not (hywiki-tests--word-at))
    (let ((hywiki-word-found (hywiki-tests--word-at)))
      (if (stringp expected)
          (should (string= expected hywiki-word-found))
        (should hywiki-word-found))
      (should (hywiki-word-is-p hywiki-word-found)))))

(defun hywiki-tests--run-test-case (test-case)
  "Run the TEST-CASE from point.
Each test case consists of cons cells with an operation and the expected
state of the WikiWord being constructed.  Operations are either a string
to be inserted, a number of chars to be deleted or a symbol p<number>
for where to move point.  The expected state is either nil for not a
wikiword or non-nil for a wikiword.  The state is checked after all
chars of the string are inserted.  If equal to a string it is checked for
match with the wikiword.  Movement of point is relative to point when
the function is called."
  (let ((origin (point)))

    ;; For traceability when looking through the list of should
    ;; clauses in a failing test.
    (should (listp test-case))

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
                      (newpos (+ origin (1- pos))))
                 (when (or (> 0 newpos) (< (point-max) newpos))
                   (ert-fail (format "New point: '%s' is outside of buffer" newpos)))
                 (goto-char newpos))
               (hywiki-tests--verify-hywiki-word vfy))
              (t (ert-fail (format "Unknown step: '%s' in WikiWord verification" step))))))))

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
   (let* ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (progn
          (hywiki-mode 1)
          (dolist (testcase hywiki-tests--wikiword-step-check)
            (with-temp-buffer
              (hywiki-tests--run-test-case testcase))))
      (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--wikiword-step-check-verification-with-faces ()
  "Run the step check to verify WikiWord is identified under change.
Performs each operation from the step check and verifies if the
resulting state at point is a WikiWord or not."
  (skip-unless (not noninteractive))
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikiHiHo (cdr (hywiki-add-page "HiHo")))
           (wikiHiho (cdr (hywiki-add-page "Hiho")))
           (wikiHi (cdr (hywiki-add-page "Hi")))
           (wikiHo (cdr (hywiki-add-page "Ho")))
           (wiki-page-list (list wikiHiHo wikiHiho wikiHi wikiHo))
           (hywiki-tests--with-face-test t))
      (unwind-protect
          (progn
            (hywiki-mode 1)
            (dolist (testcase hywiki-tests--wikiword-step-check)
              (with-temp-buffer
                (hywiki-tests--run-test-case testcase))))
        (hy-delete-files-and-buffers wiki-page-list)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(defconst hywiki-tests--lorem-ipsum "\
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse
aliquet diam euismod turpis ultricies, et porta sem blandit. Sed vitae."
  "Bulk text for in the middle of text tests.")

(ert-deftest hywiki-tests--wikiword-step-check-verification-with-surrounding-text ()
  "Run the step check to verify WikiWord is identified under change.
Insert test in the middle of other text."
  (hywiki-tests--preserve-hywiki-mode
   (let* ((hywiki-directory (make-temp-file "hywiki" t)))
     (unwind-protect
         (progn
           (hywiki-mode 1)
           (with-temp-buffer
             (insert hywiki-tests--lorem-ipsum)
             (goto-char (/ (point-max) 2))
             (let ((pos (point)))
               (insert " HiHo ")
               (goto-char (1+ pos))
               (should (looking-at-p "HiHo ")))
             (hywiki-tests--run-test-case
              '((p3 . t)
                (" " . "Hi")
                (p1 . t) (p4 . t) (-1 . t))))
           (with-temp-buffer
             (insert hywiki-tests--lorem-ipsum)
             (goto-char (/ (point-max) 2))
             (let ((pos (point)))
               (insert " Hiho ")
               (goto-char (1+ pos))
               (should (looking-at-p "Hiho ")))
             (hywiki-tests--run-test-case
              '((p3 . t)
                (" " . "Hi")
                (p1 . t) (p4) (-1 . "Hiho")))))
       (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--wikiword-identified-in-emacs-lisp-mode ()
  "Verify WikiWord is identified when surrounded by delimiters in `emacs-lisp-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (let ((hsys-org-enable-smart-keys t)
          (hywiki-directory (make-temp-file "hywiki" t)))
      (unwind-protect
          (progn
            (hywiki-mode 1)

            ;; Matches a WikiWord
            (dolist (v '("WikiWord" "[WikiWord]" "[[WikiWord]]" "{WikiWord}" "(WikiWord)"
                         "<WikiWord>" "<<WikiWord>>" "{[[WikiWord]]}" "([[WikiWord]])"
                         "[WikiWord AnotherWord]"
                         ))
              (with-temp-buffer
                (emacs-lisp-mode)
                (insert (format ";; %s" v))
                (hywiki-tests--command-execute #'newline 1 'interactive)
                (goto-char 9)
                (should (string= "WikiWord" (hywiki-word-at))))

              (with-temp-buffer
                (emacs-lisp-mode)
                (insert (format  "(setq var \"%s\")" v))
                (hywiki-tests--command-execute #'newline 1 'interactive)
                (goto-char 16)
                (should (string= "WikiWord" (hywiki-word-at)))))

            ;; Does not match as a WikiWord
            (dolist (v '("WikiWord#"))
              (with-temp-buffer
                (emacs-lisp-mode)
                (insert (format ";; %s" v))
                (hywiki-tests--command-execute #'newline 1 'interactive)
                (goto-char 9)
                (should-not (hywiki-word-at)))

              (with-temp-buffer
                (emacs-lisp-mode)
                (insert (format  "(setq var \"%s\")" v))
                (hywiki-tests--command-execute #'newline 1 'interactive)
                (goto-char 16)
                (should-not (hywiki-word-at)))))
        (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki-tests--wikiword-identified-in-strings-in-emacs-lisp-mode ()
  "Verify WikiWord is identified when in strings in `emacs-lisp-mode'."
  (hywiki-tests--preserve-hywiki-mode
    (unwind-protect
        (let ((words '("Foo" "Bar" "Baz" "Qux")))
          (hywiki-mode 1)
          (with-temp-buffer
            (emacs-lisp-mode)
            (insert
             (format "\
(defun a ()
  \"%s.\"
  nil)
" (mapconcat 'identity words " ")))
            (goto-line 2)
            (dolist (v words)
              (should (search-forward v))
              (should (string= v (hywiki-word-at)))))))))

(ert-deftest hywiki-tests--filename-same-as-wiki-word ()
  "Regular files should not be WikiWords even when hywiki-mode is active."
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wiki-page (cdr (hywiki-add-page "DEMO")))
           (default-directory hyperb:dir))
      (unwind-protect
          (with-temp-buffer
            (insert "\"DEMO\" \"DEMO.org\"\n")
            (goto-char 2)
            (should (looking-at-p "DEMO\" "))
            (hywiki-mode 0)
            (should (string= "ibtypes::pathname" (hattr:get (ibut:at-p) 'categ)))
            (hywiki-mode 1)
            (should (string= "ibtypes::pathname" (hattr:get (ibut:at-p) 'categ)))
            (goto-char 9)
            ;; Verify that using the org extension selects the WikiWord.
            (should (looking-at-p "DEMO\\.org\""))
            (should (string= "ibtypes::hywiki-existing-word" (hattr:get (ibut:at-p) 'categ))))
        (hy-delete-file-and-buffer wiki-page)
        (hy-delete-dir-and-buffer hywiki-directory)))))

(provide 'hywiki-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hywiki-tests.el ends here
