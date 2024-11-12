;;; hywiki-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    18-May-24 at 23:59:48
;; Last-Mod:     11-Nov-24 at 22:53:15 by Mats Lidell
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
(require 'hy-test-helpers)
(require 'hywiki)
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
                             (hywiki-get-page "WikiWord")))))
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

(ert-deftest hywiki-tests--wikiword-with-prefix-creates-a-new-page ()
  "Verify `action-key' on a prefixed WikiWord, outside of hywiki-directory, creates a new page."
  (defvar wikifile)
  (let ((hsys-org-enable-smart-keys t)
        (hywiki-directory (make-temp-file "hywiki" t))
        (wikifile (make-temp-file "wikifile")))
    (hywiki-mode -1)
    (unwind-protect
        (with-temp-buffer
          (insert "[[hy:WikiWord]]")
          (goto-char 4)
          (mocklet (((hywiki-add-page "WikiWord" nil) => wikifile))
            (action-key)))
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
          (mocklet ((hywiki-in-page-p => nil))
            (should-not (hywiki-active-in-current-buffer-p)))
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
         (wiki-page (hywiki-add-page "WikiWord")))
    (unwind-protect
        (progn
          (should (= 1 (length (hywiki-get-page-list))))
          (let ((hywiki-directory (make-temp-file "hywiki" t)))
            (unwind-protect
                (progn
                  (should (= 0 (length (hywiki-get-page-list)))))
              (hy-delete-dir-and-buffer hywiki-directory))))
      (hy-delete-file-and-buffer wiki-page)
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
     (funcall 'hywiki-debuttonize-non-character-commands)
     (progn ,@body)
     (funcall 'hywiki-buttonize-character-commands)
     (funcall 'hywiki-buttonize-non-character-commands)))

(ert-deftest hywiki-tests--face-property-for-wikiword-with-wikipage ()
  "Verify WikiWord for a wiki page gets face property hywiki-word-face."
  :expected-result :failed
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
	      (newline nil t))
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
  :expected-result :failed
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

            (with-hywiki-buttonize-and-insert-hooks (delete-char 1))
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
  :expected-result :failed
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

(provide 'hywiki-tests)
;;; hywiki-tests.el ends here
