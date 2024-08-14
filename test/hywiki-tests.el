;;; hywiki-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    18-May-24 at 23:59:48
;; Last-Mod:     14-Aug-24 at 01:53:15 by Bob Weiner
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

(ert-deftest hywiki-tests--hywiki-add-page--adds-file-in-wiki-folder ()
  "Verify add page creates file in wiki folder and sets hash table."
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
	 (hywiki-page-file (expand-file-name "WikiWord.org" hywiki-directory)))
    (unwind-protect
        (mocklet (((make-empty-file (expand-file-name "WikiWord.org" hywiki-directory) t) => t))
          (should (string= hywiki-page-file
                           (hywiki-add-page "WikiWord")))
          ;; Verify hash table is updated
          (with-mock
            (not-called hywiki-add-page)
            (should (string= hywiki-page-file
                             (hywiki-get-page "WikiWord")))))
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
          (mocklet (((hywiki-add-page "WikiWord") => wikifile))
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
    (should (= 0 (hywiki-directory-get-mod-time))))
  (mocklet ((file-readable-p => t)
            (file-attributes => '(t 0 0 0 nil (100 100 100 100) nil 0 "drwxr-xr-x" t 0 0)))
    (should (= 6553700 (hywiki-directory-get-mod-time)))))

(ert-deftest hywiki-tests--directory-modified-p ()
  "Verify `hywiki-directory-modified-p'."
  (let ((hywiki--directory-mod-time 0))
    (should (hywiki-directory-modified-p)))
  (let ((hywiki--directory-mod-time 2))
    (mocklet ((hywiki-directory-get-mod-time => 2))
      (should-not (hywiki-directory-modified-p)))
    (mocklet ((hywiki-directory-get-mod-time => 1))
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
                  (should (hash-empty-p (hywiki-get-page-hasht)))
                    (should (= 0 (length (hywiki-get-page-list)))))
              (hy-delete-dir-and-buffer hywiki-directory))))
      (hy-delete-file-and-buffer wiki-page)
      (hy-delete-dir-and-buffer hywiki-directory))))

;; Following three test cases for verifying proper face is some what
;; experimental. They need to be run in interactive mode.

(ert-deftest hywiki-tests--face-property-for-wikiword-with-wikipage ()
  "Verify WikiWord for a wiki page gets face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (with-temp-buffer
          (hywiki-mode 1)
          (insert "WikiWord")
	  (newline nil t)
          (goto-char 4)
          (should (hproperty:but-get (point) 'face hywiki-word-face)))
      (hywiki-mode 0)
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--no-face-property-for-no-wikipage ()
  "Verify WikiWord for no wiki page does not get face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t)))
    (unwind-protect
        (with-temp-buffer
          (hywiki-mode 0)
          (insert "WikiWord")
	  (newline nil t)
          (goto-char 4)
          (should-not (hproperty:but-get (point) 'face hywiki-word-face)))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--convert-words-to-org-link ()
  "Verify `hywiki-convert-words-to-org-links' converts WikiWords to org links."
  (skip-unless (not noninteractive))
  (let* ((hywiki-directory (make-temp-file "hywiki" t))
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (with-temp-buffer
          (hywiki-mode 1)
          (insert "WikiWord")
	  (newline nil t)
          (goto-char 4)
          (hywiki-convert-words-to-org-links)
          (should (string= "[[hy:WikiWord]]\n"
                           (buffer-substring-no-properties (point-min) (point-max)))))
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

(provide 'hywiki-tests)
;;; hywiki-tests.el ends here
