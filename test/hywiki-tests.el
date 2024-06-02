;;; hywiki-tests.el --- one line summary                -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    18-May-24 at 23:59:48
;; Last-Mod:      1-Jun-24 at 16:16:00 by Mats Lidell
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
  (let ((hsys-org-enable-smart-keys t)
        (hywiki-directory (make-temp-file "hywiki" t))
        (hywiki--pages-hasht nil))
    (unwind-protect
        (progn
          (mocklet (((make-empty-file (expand-file-name "WikiWord.org" hywiki-directory) t) => t))
            (should (string= (expand-file-name "WikiWord.org" hywiki-directory)
                             (hywiki-add-page "WikiWord"))))
          ;; Verify hash table is updated
          (with-mock
            (not-called hywiki-add-page)
            (should (string= (expand-file-name "WikiWord.org" hywiki-directory)
                             (hywiki-get-page "WikiWord")))))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--hywiki-add-page--adds-no-wiki-word-fails ()
  "Verify add page requires a WikiWord."
  ;; Should not leave erroneously created file after test but leaving
  ;; added error cleanup till later if it is even needed!? No file
  ;; should be created so only happens on error!? (If this is
  ;; considered an error case that is.)
  (let ((hywiki-directory (make-temp-file "hywiki" t))
        (hywiki--pages-hasht nil))
    (unwind-protect
        (should-error (hywiki-add-page "notawikiword"))
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--wikiword-with-prefix-creates-a-new-page ()
  "Verify `action-key' on a prefixed WikiWord, outside of hywiki-directory, creates a new page."
  (defvar wikifile)
  (let ((hsys-org-enable-smart-keys t)
        (hywiki-directory (make-temp-file "hywiki" t))
        (hywiki--pages-hasht nil)
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
        (hywiki-directory (make-temp-file "hywiki" t))
        (hywiki--pages-hasht nil))
    (unwind-protect
        (with-temp-buffer
          (insert "WikiWord")
          (goto-char 4)
          (hywiki-mode -1)
          (should-not (hywiki-at-wikiword))
          (hywiki-mode)
          (should (string= "WikiWord" (hywiki-at-wikiword))))
      (hywiki-mode -1)
      (hy-delete-dir-and-buffer hywiki-directory))))

;; Following two test cases for verifying proper face is some what
;; experimental. They need to be run in interactive mode and with the
;; help of hy-test-helpers:consume-input-events it seems the property
;; can be verified. In the middle of it the "*ert*" buffer gets
;; swapped in and the temp buffer needs to be brought back!?

(ert-deftest hywiki-tests--face-property-for-wikiword-with-wikipage ()
  "Verify WikiWord for a wiki page gets face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
         (hywiki--pages-hasht nil)
         (wikipage (hywiki-add-page "WikiWord")))
    (unwind-protect
        (with-temp-buffer
          (let ((buffer (current-buffer)))
            (hywiki-mode)
            (insert "WikiWord")
            (should (hact 'kbd-key "RET"))
            (hy-test-helpers:consume-input-events)
            (should (string= "*ert*" (buffer-name)))
            (set-buffer buffer)
            (goto-char 4)
            (should (equal buffer (current-buffer)))
            (should (hproperty:but-get (point) 'face hywiki-word-face))))
      (hywiki-mode -1)
      (hy-delete-file-and-buffer wikipage)
      (hy-delete-dir-and-buffer hywiki-directory))))

(ert-deftest hywiki-tests--no-face-property-for-no-wikipage ()
  "Verify WikiWord for no wiki page does not get face property hywiki-word-face."
  (skip-unless (not noninteractive))
  (let* ((hsys-org-enable-smart-keys t)
         (hywiki-directory (make-temp-file "hywiki" t))
         (hywiki--pages-hasht nil))
    (unwind-protect
        (with-temp-buffer
          (let ((buffer (current-buffer)))
            (hywiki-mode)
            (insert "WikiWord")
            (should (hact 'kbd-key "RET"))
            (hy-test-helpers:consume-input-events)
            (should (string= "*ert*" (buffer-name)))
            (set-buffer buffer)
            (goto-char 4)
            (should (equal buffer (current-buffer)))
            (should-not (hproperty:but-get (point) 'face hywiki-word-face))))
      (hywiki-mode -1)
      (hy-delete-dir-and-buffer hywiki-directory))))

(provide 'hywiki-tests)
;;; hywiki-tests.el ends here
