;;; hywiki-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    18-May-24 at 23:59:48
;; Last-Mod:     24-Dec-24 at 00:48:15 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'with-simulated-input)
(require 'hy-test-helpers)
(require 'hywiki)
(require 'hsys-org)
(require 'ox-publish)

(ert-deftest hywiki-tests--hywiki-add-page--adds-file-in-wiki-folder ()
  "Verify add page creates file in wiki folder and sets hash table."
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
	 (hywiki-page-file (expand-file-name "WikiWord.org" hywiki-directory)))
    (unwind-protect
	(progn
          (should (string= hywiki-page-file
                           (hywiki-add-page "WikiWord")))
          ;; Verify hash table is updated
          (with-mock
            (not-called hywiki-add-page)
            (should (string= hywiki-page-file
                             (hywiki-get-referent "WikiWord")))))
      (hy-delete-file-and-buffer hywiki-page-file)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--hywiki-add-page--adds-no-wiki-word-fails ()
  "Verify add page requires a WikiWord."
  ;; Should not leave erroneously created file after test but leaving
  ;; added error cleanup till later if it is even needed!? No file
  ;; should be created so only happens on error!? (If this is
  ;; considered an error case that is.)
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (should-not (hywiki-add-page "notawikiword"))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--action-key-on-wikiword-displays-page ()
  "Verify `action-key' on a prefixed WikiWord, outside of hywiki-directory, creates a new page."
  (defvar wikifile)
  (let ((hsys-org-enable-smart-keys t)
        (hywiki-directory (make-temp-file "hywiki" t))
        (wikifile (make-temp-file "wikifile" nil ".org")))
    (unwind-protect
        (with-temp-buffer
	  (hywiki-mode 1)
          (insert "[[hy:WikiWord]]")
          (goto-char 4)
          (mocklet (((hywiki-display-referent "WikiWord" nil) => wikifile))
            (action-key)))
      (hy-delete-file-and-buffer wikifile))))

(ert-deftest hywiki-tests--assist-key-on-wikiword-displays-help ()
  "Verify `assist-key' on a prefixed WikiWord, outside of hywiki-directory, displays help for the WikiWord link."
  (defvar wikifile)
  (let ((hsys-org-enable-smart-keys t)
        (hywiki-directory (make-temp-file "hywiki" t))
        (wikifile (make-temp-file "wikifile" nil ".org")))
    (unwind-protect
        (with-temp-buffer
	  (hywiki-mode 1)
          (insert "[[hy:WikiWord]]")
          (goto-char 4)
          (should (string= "WikiWord" (hywiki-word-at)))
          (assist-key))
      (hy-delete-file-and-buffer wikifile))))

(ert-deftest hywiki-tests--not-a-wikiword-unless-in-hywiki-mode ()
  "Verify WikiWord is not a WikiWord unless in `hywiki-mode'."
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
      (hywiki-mode 0)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--a-wikiword-in-hywiki-directory ()
  "Verify WikiWord is identified if in `hywiki-directory'."
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (hywiki-add-page "WikiWord")))
    (unwind-protect
        (with-current-buffer (find-file-noselect wiki-page)
          (hywiki-mode 0)
          (insert "AnotherWikiWord")
	  (newline nil t)
          (goto-char 4)
          (should (hywiki-word-at)))
      (hy-delete-file-and-buffer wiki-page)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--wikiword-identified-with-delimiters ()
  "Verify WikiWord is identified when surrounded by delimiters."
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
              (org-mode)
              (insert v)
	      (newline nil t)
              (goto-char 6)
              (should (string= "WikiWord" (hywiki-word-at)))))

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
              (should (string= "WikiWord" (hywiki-word-at)))))

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
              (should-not (string= "WikiWord" (hywiki-word-at))))))
      (hywiki-mode 0)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--at-wikiword-finds-word-and-section ()
  "Verify `hywiki-word-at' finds WikiWord and section if available."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (with-temp-buffer
          (hywiki-mode)
          (insert "WikiWord")
          (goto-char 4)
          (should (string= "WikiWord" (hywiki-word-at)))

          ;; Section
          (goto-char (point-max))
          (insert "#section")
          (goto-char 4)
          (should (string= "WikiWord#section" (hywiki-word-at)))

          ;; Section with dash
          (goto-char (point-max))
          (insert "-subsection")
          (goto-char 4)
          (should (string= "WikiWord#section-subsection" (hywiki-word-at))))
      (hywiki-mode -1)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--sections-with-dash-space ()
  "Verify `hywiki-word-at' finds sections with dash and space."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (hywiki-mode)
            (insert "WikiWord#section rest is ignored")
            (goto-char 4)
            (should (string= "WikiWord#section" (hywiki-word-at))))

          (with-temp-buffer
            (hywiki-mode)
            (insert "WikiWord#section-with-dash")
            (goto-char 4)
            (should (string= "WikiWord#section-with-dash" (hywiki-word-at))))

          (with-temp-buffer
            (hywiki-mode)
            (insert "WikiWord#\"section-within-quotes\"")
            (goto-char 4)
            (should (string= "WikiWord#\"section-within-quotes\"" (hywiki-word-at)))))
      (hywiki-mode -1)
      (hy-delete-dir-and-buffer hywiki-directory))))

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
         (wiki-page (hywiki-add-page "WikiWord"))
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
         (wiki-page (hywiki-add-page "WikiWord"))
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
  "Verify `hywiki-get-page-list' returns one WikiWord."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (should (equal '("WikiWord") (hywiki-get-page-list)))
          (should (equal wiki-page (hywiki-add-page "WikiWord")))
          (should (equal '("WikiWord") (hywiki-get-page-list))))
      (hy-delete-file-and-buffer wiki-page)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-after-add-and-delete ()
  "Verify `hywiki-get-page-list' is updated when a page is added and removed."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wiki-page (hywiki-add-page "WordOne")))
    (unwind-protect
        (let ((wiki-page2 (hywiki-add-page "WordTwo")))
	  (unwind-protect
              (should (set:equal '("WordOne" "WordTwo")
				 (hywiki-get-page-list)))
	    ;; This delay is necessary or the test can fail sporadically
	    (sit-for 0.01)
            (hy-delete-file-and-buffer wiki-page2))
	  (should (set:equal '("WordOne") (hywiki-get-page-list))))
      (hy-delete-file-and-buffer wiki-page)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-multiple-words ()
  "Verify `hywiki-get-page-list' returns multiple WikiWords."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (basename "WikiWord")
         (wiki-page-list nil))
    (unwind-protect
        (progn
          (dolist (char '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J"))
            (push (hywiki-add-page (format "%s%s" basename char)) wiki-page-list))
          (should (= 10 (length wiki-page-list)))
          (should (= 10 (length (hywiki-get-page-list))))
          (should (= 10 (length (seq-uniq (hywiki-get-page-list))))))
      (hy-delete-files-and-buffers wiki-page-list)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-when-new-wiki-directory ()
  "Verify `hywiki-get-page-list' is empty for new `hywiki-directory'."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (should (= 1 (length (hywiki-get-page-list))))
          (let ((hywiki-directory (make-temp-file "hywiki" t)))
            (unwind-protect
                (progn
                  (should (= 0 (length (hywiki-get-page-list)))))
              (hy-delete-dir-and-buffer hywiki-directory))))
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--get-page-list-for-new-wiki-directory-after-added-referent ()
  "Verify `hywiki-get-page-list' is empty for new `hywiki-directory'."
  (defvar hywiki-add-referent-hook)
  (let ((hywiki-directory (make-temp-file "hywiki" t))
        (hywiki-add-referent-hook 'test-func))
    (unwind-protect
        (progn
          (mocklet (((test-func) => t))
            (should (eq 'referent (hywiki-add-referent "WikiWord" 'referent))))
          (should (= 1 (length (hywiki-get-page-list))))
          (let ((hywiki-directory (make-temp-file "hywiki" t)))
            (unwind-protect
                (should (= 0 (length (hywiki-get-page-list))))
              (hy-delete-dir-and-buffer hywiki-directory))))
      (hy-delete-dir-and-buffer hywiki-directory))))

;; Following three test cases for verifying proper face is some what
;; experimental. They need to be run in interactive mode.

(defun hywiki-tests--add-hywiki-hooks ()
  "Enable all hywiki hook functions."
  (add-hook 'pre-command-hook      'hywiki-debuttonize-non-character-commands 95)
  (add-hook 'post-command-hook     'hywiki-buttonize-non-character-commands 95)
  (add-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
  (add-hook 'window-buffer-change-functions
	    'hywiki-maybe-highlight-page-names-in-frame)
  (add-to-list 'yank-handled-properties
	       '(hywiki-word-face . hywiki-highlight-on-yank)))

(defun hywiki-tests--remove-hywiki-hooks ()
  "Disable all hywiki hook functions."
  (remove-hook 'pre-command-hook      'hywiki-debuttonize-non-character-commands)
  (remove-hook 'post-command-hook     'hywiki-buttonize-non-character-commands)
  (remove-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
  (remove-hook 'window-buffer-change-functions
	       'hywiki-maybe-highlight-page-names-in-frame)
  (setq yank-handled-properties
	(delete '(hywiki-word-face . hywiki-highlight-on-yank)
		yank-handled-properties)))

(defmacro with-hywiki-buttonize-hooks (&rest body)
  "Call BODY wrapped in hywiki hooks to simulate Emacs redisplay."
  (declare (indent 0) (debug t))
  `(progn
     (funcall 'hywiki-debuttonize-non-character-commands)
     (progn ,@body)
     (funcall 'hywiki-buttonize-non-character-commands)))

(defmacro with-hywiki-buttonize-and-insert-hooks (&rest body)
  "Call BODY wrapped in hywiki hooks to simulate Emacs redisplay."
  (declare (indent 0) (debug t))
  `(progn
     (progn ,@body)
     (funcall 'hywiki-debuttonize-non-character-commands)
     (funcall 'hywiki-buttonize-character-commands)
     (funcall 'hywiki-buttonize-non-character-commands)))

(ert-deftest hywiki-tests--face-property-for-wikiword-with-wikipage ()
  "Verify WikiWord for a wiki page gets face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (hywiki-tests--remove-hywiki-hooks)
          (with-temp-buffer
            (hywiki-mode 1)
            (with-hywiki-buttonize-and-insert-hooks (insert "WikiWord "))
            (goto-char 4)
            (should (hproperty:but-get (point) 'face hywiki-word-face)))
          (with-temp-buffer
            (hywiki-mode 1)
            (with-hywiki-buttonize-and-insert-hooks
              (insert "WikiWord")
	      (command-execute #'newline))
            (goto-char 4)
            (should (hproperty:but-get (point) 'face hywiki-word-face))))
      (hywiki-tests--add-hywiki-hooks)
      (hywiki-mode 0)
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--no-face-property-for-no-wikipage ()
  "Verify WikiWord for no wiki page does not get face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (progn
          (hywiki-tests--remove-hywiki-hooks)
          (with-temp-buffer
            (hywiki-mode 0)
            (with-hywiki-buttonize-and-insert-hooks
              (insert "WikiWord")
	      (newline nil t))
            (goto-char 4)
            (should-not (hproperty:but-get (point) 'face hywiki-word-face))))
      (hywiki-tests--add-hywiki-hooks)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--verify-face-property-when-editing-wikiword ()
  "Verify face property changes when WikiWord is edited."
  (skip-unless (not noninteractive))
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (hywiki-tests--remove-hywiki-hooks)
          (with-temp-buffer
            (hywiki-mode 1)
            (with-hywiki-buttonize-and-insert-hooks (insert "Wikiord "))
            (goto-char 5)
            (should (looking-at-p "ord"))
            (should-not (hproperty:but-get (point) 'face hywiki-word-face))

            (with-hywiki-buttonize-and-insert-hooks (insert "W"))
            (goto-char 5)
            (should (looking-at-p "Word"))
            (should (hproperty:but-get (point) 'face hywiki-word-face))

            (with-hywiki-buttonize-and-insert-hooks (delete-char 1))
            (should (looking-at-p "ord"))
            (should-not (hproperty:but-get (point) 'face hywiki-word-face))))
      (hywiki-tests--add-hywiki-hooks)
      (hywiki-mode 0)
      (hy-delete-files-and-buffers (list wikipage))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--verify-face-property-when-editing-wikiword-first-char ()
  "Verify face property changes when WikiWord is edited in the first char position."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (skip-unless (not noninteractive))
    (unwind-protect
        (progn
          (hywiki-tests--remove-hywiki-hooks)
          (with-temp-buffer
            (hywiki-mode 1)
            (with-hywiki-buttonize-and-insert-hooks (insert "WikiWord "))
            (goto-char 1)
            (should (looking-at-p "Wiki"))
            (should (hproperty:but-get (point) 'face hywiki-word-face))

	    (delete-char 1)
	    (hywiki-maybe-dehighlight-page-name t)
            (should (looking-at-p "iki"))
            (should-not (hproperty:but-get (point) 'face hywiki-word-face))

            (with-hywiki-buttonize-and-insert-hooks (insert "W"))
            (goto-char 1)
            (should (looking-at-p "Wiki"))
            (should (hproperty:but-get (point) 'face hywiki-word-face))))
      (hywiki-tests--add-hywiki-hooks)
      (hywiki-mode 0)
      (hy-delete-files-and-buffers (list wikipage))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--convert-words-to-org-link ()
  "Verify `hywiki-convert-words-to-org-links' converts WikiWords to org links."
  (skip-unless (not noninteractive))
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (hywiki-tests--remove-hywiki-hooks)
          (with-temp-buffer
            (hywiki-mode 1)
            (with-hywiki-buttonize-and-insert-hooks (insert "WikiWord "))
            (goto-char 4)
            (hywiki-convert-words-to-org-links)
            (should (string= "[[hy:WikiWord]] "
                             (buffer-substring-no-properties (point-min) (point-max)))))
          (with-temp-buffer
            (hywiki-mode 1)
            (with-hywiki-buttonize-and-insert-hooks
              (insert "WikiWord")
	      (newline nil t))
            (goto-char 4)
            (hywiki-convert-words-to-org-links)
            (should (string= "[[hy:WikiWord]]\n"
                             (buffer-substring-no-properties (point-min) (point-max))))))
      (hywiki-tests--add-hywiki-hooks)
      (hywiki-mode 0)
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

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
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (should-not (hywiki-org-link-resolve "NoWikiWord"))
          (should (string= wikipage (hywiki-org-link-resolve "WikiWord")))
          (should (string= wikipage (hywiki-org-link-resolve "hy:WikiWord")))
          (should (string= (concat wikipage "::section") (hywiki-org-link-resolve "WikiWord#section"))))
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--org-link-export ()
  "Verify `hywiki-org-link-export' output for different formats."
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (should (string-match-p
                   (format "\\[hy\\] <doc:.*%s>" wikipage)
                   (hywiki-org-link-export "WikiWord" "doc" 'ascii)))
          (should (string-match-p
                   (format "<a href=\".*%s\">doc</a>"
                           (replace-regexp-in-string "\\.org" ".html" wikipage))
                   (hywiki-org-link-export "WikiWord" "doc" 'html)))
          (should (string-match-p
                   (format "\\[doc\\](.*%s)" wikipage)
                   (hywiki-org-link-export "WikiWord" "doc" 'md)))
          (should (string-match-p
                   (format "\\href{.*%s}{doc}" wikipage)
                   (hywiki-org-link-export "WikiWord" "doc" 'latex)))
          (should (string-match-p
                   (format "@uref{.*%s,doc}" wikipage)
                   (hywiki-org-link-export "WikiWord" "doc" 'texinfo)))
          (should (string-match-p
                   (format ".*%s" wikipage)
                   (hywiki-org-link-export "WikiWord" "doc" 'unknown)))
          (should (string= "NotAWikiPage" (hywiki-org-link-export "NotAWikiPage" "doc" 'ascii))))
      (hy-delete-file-and-buffer wikipage)
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
  (let ((hywiki-directory (make-temp-file "hywiki" t))
        (hywiki-add-referent-hook 'test-func))
    (unwind-protect
        (progn
          (should-not (hywiki-add-referent "notawikiword" 'referent))
          (mocklet (((test-func) => t))
            (should (eq 'referent (hywiki-add-referent "WikiWord" 'referent))))
          (should (eq 'referent (hywiki-get-referent "WikiWord"))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-activity ()
  "Verify `hywiki-add-activity'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (progn
          (mocklet (((require 'activities) => t)
                    ((hywiki-add-referent "WikiWord" '(activities-resume "activity" :resetp nil)) => 'activity-referent)
                    (activities-completing-read => "activity"))
            (should (equal (hywiki-add-activity "WikiWord") 'activity-referent))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-bookmark ()
  "Verify `hywiki-add-bookmark'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (progn
          (mocklet ((bookmark-completing-read => ""))
            (should-error (hywiki-add-bookmark "WikiWord"))))
          (mocklet ((bookmark-completing-read => 'bkmark))
            (should (equal '(bookmark-jump bkmark) (caddr (hywiki-add-bookmark "WikiWord")))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--add-command ()
  "Verify `hywiki-add-command'."
  (mocklet ((hui:actype => 'command)
            ((hywiki-add-referent "WikiWord" 'command) => 'command))
    (should (eq 'command (hywiki-add-command "WikiWord")))))

(ert-deftest hywiki-tests--add-find ()
  "Verify `hywiki-add-find'."
  (mocklet (((hywiki-add-referent "WikiWord" #'hywiki-word-grep) => 'word-grep))
    (should (eq 'word-grep (hywiki-add-find "WikiWord")))))

(ert-deftest hywiki-tests--add-global-button ()
  "Verify `hywiki-add-global-button'."
  (mocklet ((hargs:read-match => "gbtn")
            ((hywiki-add-referent "WikiWord" '(gbut:act "gbtn")) => 'gbut-referent))
    (should (equal 'gbut-referent (hywiki-add-global-button "WikiWord")))))

(ert-deftest hywiki-tests--add-hyrolo ()
  "Verify `hywiki-add-hyrolo'."
  (mocklet (((hywiki-add-referent "WikiWord" '(hyrolo-fgrep "WikiWord")) => 'hyrolo-referent))
    (should (equal 'hyrolo-referent (hywiki-add-hyrolo "WikiWord")))))

(ert-deftest hywiki-tests--add-info-index ()
  "Verify `hywiki-add-info-index'."
  (mocklet (((info) => t)
	    ((Info-read-index-item-name "Info index item: ") => "index-name")
            ((Info-current-filename-sans-extension) => "info")
            ((hywiki-add-referent "WikiWord" '(Info-goto-node "(info)index-name")) => 'info-referent))
    (should (equal 'info-referent (hywiki-add-info-index "WikiWord")))))

(ert-deftest hywiki-tests--add-info-node ()
  "Verify `hywiki-add-info-node'."
  (mocklet (((info) => t)
	    ((Info-read-node-name "Info node: ") => "node-name")
            ((Info-current-filename-sans-extension) => "info")
            ((hywiki-add-referent "WikiWord" '(Info-goto-node "(info)node-name")) => 'info-referent))
    (should (equal 'info-referent (hywiki-add-info-node "WikiWord")))))

(ert-deftest hywiki-tests--add-key-series ()
  "Verify `hywiki-add-key-series'."
  (mocklet (((hywiki-add-referent "WikiWord" "{ABC}") => 'key-series-referent))
    (with-simulated-input "ABC RET"
      (should (equal 'key-series-referent (hywiki-add-key-series "WikiWord"))))
    (with-simulated-input "{ABC} RET"
      (should (equal 'key-series-referent (hywiki-add-key-series "WikiWord"))))))

(ert-deftest hywiki-tests--add-link ()
  "Verify `hywiki-add-link'."
  (mocklet (((hactypes:link-to-file-interactively) => '("file" 20))
            ((hpath:file-position-to-line-and-column "file" 20) => "file:L2:C3")
            ((hywiki-add-referent "WikiWord" "file:L2:C3") => 'path-referent))
    (should (equal 'path-referent (hywiki-add-link "WikiWord")))))

(ert-deftest hywiki-tests--add-org-id ()
  "Verify `hywiki-add-org-id'."
  ;; Error case - Non org-mode buffer
  (let ((filea (make-temp-file "hypb" nil ".txt")))
    (unwind-protect
        (with-current-buffer (find-file filea)
          (mocklet (((hmouse-choose-link-and-referent-windows) => (list nil (get-buffer-window))))
            (should-error (hywiki-add-org-id "WikiWord") :type '(error))))
      (hy-delete-file-and-buffer filea)))

  (let ((filea (make-temp-file "hypb" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file filea)
          (insert "* header\n")

          ;; Error-case - No Org ID and read only
          (setq buffer-read-only t)
          (mocklet (((hmouse-choose-link-and-referent-windows) => (list nil (get-buffer-window))))
	    (should-error (hywiki-add-org-id "WikiWord") :type '(error))

            ;; Normal case - Org-mode with Org ID
            (goto-char (point-max))
            (setq buffer-read-only nil)
            (defvar hywiki-test--org-id)
	    (let ((result (hywiki-add-org-id "WikiWord")))
	      (if (stringp result)
		  (should (string-prefix-p "ID: " result))
		(error "(hywiki-tests--add-org-id): result value is a non-string: %s" result)))))
      (hy-delete-file-and-buffer filea))))

(ert-deftest hywiki-tests--add-org-roam-node ()
  "Verify `hywiki-add-org-roam-node'."
  (let ((hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (progn
          (mocklet (((require 'org-roam) => t)
                    ((org-roam-node-read) => "org-roam-node")
                    ((hywiki-add-referent "WikiWord" '(org-roam-node-open "org-roam-node" (or (alist-get 'file org-link-frame-setup) (alist-get hpath:display-where hpath:display-where-alist)))) => 'org-roam-referent))
            (should (equal (hywiki-add-org-roam-node "WikiWord") 'org-roam-referent))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(provide 'hywiki-tests)
;;; hywiki-tests.el ends here
