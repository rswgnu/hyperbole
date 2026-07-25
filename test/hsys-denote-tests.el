;;; hsys-denote-tests.el --- Unit tests for hsys-denote.el  -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    23-Apr-21 at 20:55:00
;; Last-Mod:     10-Aug-26 at 09:57:28 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for "../hsys-denote.el"

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'hsys-denote)

(ert-deftest hsys-denote-tests--denote-link ()
  "Verify link-to-denote is called."
  (with-mock
    (mock (hsys-denote-link-at-p) => nil)
    (should-not (ibtypes::denote-link)))
  (with-mock
    (mock (hsys-denote-link-at-p) => '("20260710T003252" 10 20))
    (mock (ibut:label-set "20260710T003252" 10 20))
    (mock (actypes::link-to-denote "20260710T003252") => 'denote)
    (should (eq 'denote (ibtypes::denote-link)))))

(ert-deftest hsys-denote-tests--link-to-denote ()
  "."
  (defvar denote-date-identifier-regexp)
  (let ((denote-date-identifier-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"))

    (ert-info ("With optional but not existing file")
      (with-mock
        (stub require => t)
        (let ((err (should-error (actypes::link-to-denote "denote:20260710T003252" "/does/not/exists"))))
          (should (string-search "File is unreadable" (cadr err))))
        (mock (file-readable-p "/does/not/exists") => t)
        (mock (hpath:find "/does/not/exists") => 'file-found)
        (should (eq 'file-found (actypes::link-to-denote "denote:20260710T003252" "/does/not/exists")))))

    (ert-info ("With invalid ID")
      (with-mock
        (stub require => t)
        ;; Invalid id-and-section
        (let ((err (should-error (actypes::link-to-denote 'denote:20260710T003252))))
          (should (string-search "Invalid file ID and optional section" (cadr err))))))

    (ert-info ("Section prefix #")
      (with-mock
        (stub require => t)
        (mock (denote-extract-id-from-string "denote:20260710T003252#section") => "20260710T003252")
        (mock (denote-get-path-by-id "20260710T003252") => "file")
        (mock (file-readable-p "file") => t)
        (mock (hpath:find "file#section") => 'file-found)
        (should (eq 'file-found (actypes::link-to-denote "denote:20260710T003252#section")))))

    (ert-info ("Section prefix ::")
      (with-mock
        (stub require => t)
        (mock (denote-extract-id-from-string "denote:20260710T003252::section") => "20260710T003252")
        (mock (denote-get-path-by-id "20260710T003252") => "file")
        (mock (file-readable-p "file") => t)
        (mock (hpath:find "file#section") => 'file-found)
        (should (eq 'file-found (actypes::link-to-denote "denote:20260710T003252::section")))))

    (ert-info ("Section prefix ::#")
      (with-mock
        (stub require => t)
        (mock (denote-extract-id-from-string "denote:20260710T003252::#section") => "20260710T003252")
        (mock (hact 'link-to-org-id "section") => 'file-found)
        (should (eq 'file-found (actypes::link-to-denote "denote:20260710T003252::#section")))))

    (ert-info ("Section prefix ::*")
      (with-mock
        (stub require => t)
        (mock (denote-extract-id-from-string "denote:20260710T003252::*section") => "20260710T003252")
        (mock (denote-get-path-by-id "20260710T003252") => "file")
        (mock (file-readable-p "file") => t)
        (mock (hpath:find "file#section") => 'file-found)
        (should (eq 'file-found (actypes::link-to-denote "denote:20260710T003252::*section")))))))

(ert-deftest hsys-denote-tests--link-at-p ()
  "Verify `hsys-denote-link-at-p'."
  (defvar denote-date-identifier-regexp)
  (let ((denote-date-identifier-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)")
        (denote-links '(("denote:20260710T003252" . t)
                        ("denote:20260710T003252#section" . t)
                        ("denote:20260710T003252::section" . t)
                        ("denote:20260710T003252::#section" . t)
                        ("denote:20260710T003252::*section" . t)
                        ("dn:20260710T003252" . nil))))
    (with-temp-buffer
      (dolist (v denote-links)
        (let ((denote-link (car v))
              (valid (cdr v)))
          (erase-buffer)
          (insert denote-link)
          (goto-char 3)
          (with-mock
            (stub require => t)
            (if valid
                (should (hsys-denote-link-at-p))
              (should-not (hsys-denote-link-at-p))))))))

   (with-temp-buffer
    ;; Denote in an org link
    (insert "[[denote:20260710T003252][link]]")
    (with-mock
      (stub require => t)
      (should (equal (hsys-denote-link-at-p 1 33)
                     '("denote:20260710T003252" 3 25))))

    ;; String delimited
    (erase-buffer)
    (insert "\"denote:20260710T003252\"\n")
    (goto-char 4)
    (with-mock
      (stub require => t)
      (should (equal (hsys-denote-link-at-p)
                     '("denote:20260710T003252" 2 24))))

    ;; Org-link
    (erase-buffer)
    (insert "denote:20260710T003252")
    (goto-char 4)
    (with-mock
      (stub require => t)
      (mock (hsys-org-link-at-p) => '(1 23))
      (should (equal (hsys-denote-link-at-p)
                     '("denote:20260710T003252" 1 23))))

    (erase-buffer)
    (insert "[[denote:20260710T003252][link]]")
    (goto-char 4)
    (with-mock
      (stub require => t)
      (mock (hsys-org-link-at-p) => '(1 33))
      (should (equal (hsys-denote-link-at-p)
                     '("denote:20260710T003252" 3 25))))))

(ert-deftest hsys-denote-tests--file-at-p ()
  "Verify `hsys-denote-file-at-p'."
  (with-temp-buffer
    (org-mode)
    (insert "* h1\nbody")
    (let ((buffer-file-name "20260710T003252"))
      (dolist (v '((1 . "h1") (2 . "h1:C1") (6 . "h1:L2") (7 . "h1:L2:C1")))
        (goto-char (car v))
        (with-mock
          (stub require => t)
          (mock (denote-file-has-denoted-filename-p "20260710T003252") => t)
          (mock (denote-retrieve-filename-identifier "20260710T003252") => "20260710T003252")
          (should (string= (hsys-denote-file-at-p)
                           (concat "denote:20260710T003252#" (cdr v))))))
      ;; With ID
      (dolist (v '((1 . "ID") (2 . "ID:C1") (6 . "ID:L2") (7 . "ID:L2:C1")))
        (goto-char (car v))
        (with-mock
          (stub require => t)
          (mock (org-id-get) => "ID")
          (mock (denote-file-has-denoted-filename-p "20260710T003252") => t)
          (mock (denote-retrieve-filename-identifier "20260710T003252") => "20260710T003252")
          (should (string= (hsys-denote-file-at-p)
                           (concat "<[h1]> denote:20260710T003252#" (cdr v)))))))))

(provide 'hsys-denote-tests)
;;; hsys-denote-tests.el ends here
