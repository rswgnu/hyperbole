;;; kexport-tests.el --- test exporting Koutlines to file types   -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    10-Oct-21 at 17:30:00
;; Last-Mod:      5-Nov-23 at 16:55:35 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for kexport in "../kotl/kexport.el"

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'kexport "kotl/kexport")
(require 'hy-test-helpers "test/hy-test-helpers")

(ert-deftest kexport:html-creates-html-file ()
  "kexport:html creates an output html file."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (html-file (make-temp-file "hypb" nil ".html")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")
          (let ((auto-insert nil))
            (kexport:html kotl-file html-file))
          (should (file-exists-p html-file)))
      (progn
        (hy-delete-file-and-buffer kotl-file)
        (hy-delete-file-and-buffer html-file)))))

(ert-deftest kexport:html-sets-title-and-header ()
  "kexport:html set title and header from kotl filename without suffix."
  (let* ((kotl-file (make-temp-file "hypb" nil ".kotl"))
         (html-file  (make-temp-file "hypb" nil ".html"))
	 (title (file-name-sans-extension (file-name-nondirectory kotl-file))))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")
          (let ((auto-insert nil))
            (kexport:html kotl-file html-file))
          (find-file html-file)
          (goto-char (point-min))
          (re-search-forward (format "<title>%s</title>" title))
          (re-search-forward (format "<h1>%s</h1>" title)))
      (progn
        (hy-delete-file-and-buffer kotl-file)
        (hy-delete-file-and-buffer html-file)))))

(ert-deftest kexport:html-contains-each-cell ()
  "kexport:html contains each cell."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (html-file  (make-temp-file "hypb" nil ".html")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")
          (let ((auto-insert nil))
            (kexport:html kotl-file html-file))
          (find-file html-file)
          (goto-char (point-min))
          (re-search-forward "<pre>first</pre>")
          (re-search-forward "<pre>second</pre>"))
      (progn
        (hy-delete-file-and-buffer kotl-file)
        (hy-delete-file-and-buffer html-file)))))

(ert-deftest kexport:html-creates-hierarchy ()
  "kexport:html exports cells in a hierachy."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (html-file  (make-temp-file "hypb" nil ".html")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-child)
          (insert "second")
          (kotl-mode:add-child)
          (insert "third")
          (let ((auto-insert nil))
            (kexport:html kotl-file html-file))
          (find-file html-file)
          (goto-char (point-min))
          (re-search-forward "<pre.*1\\. .*</pre>")
          (re-search-forward "<pre>first</pre>")
          (re-search-forward "<pre.*1a\\. .*</pre>")
          (re-search-forward "<pre>second</pre>")
          (re-search-forward "<pre.*1a1\\. .*</pre>")
          (re-search-forward "<pre>third</pre>"))
      (progn
        (hy-delete-file-and-buffer kotl-file)
        (hy-delete-file-and-buffer html-file)))))

(ert-deftest kexport:html-creates-list-hierarchy ()
  "kexport:html exports cells in a hierachy using lists."
  (let ((kotl-file (make-temp-file "hypb" nil ".kotl"))
        (html-file  (make-temp-file "hypb" nil ".html")))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-child)
          (insert "second")
          (kotl-mode:add-child)
          (insert "third")
          (let ((auto-insert nil))
            (kexport:html kotl-file html-file))
          (find-file html-file)
          (goto-char (point-min))
          (should (= (count-matches "<ul\\b" (point-min) (point-max))
                     (count-matches "</ul\\b" (point-min) (point-max))))
          (should (= (count-matches "<li\\b" (point-min) (point-max))
                     (count-matches "</li\\b" (point-min) (point-max)))))
      (hy-delete-file-and-buffer kotl-file)
      (hy-delete-file-and-buffer html-file))))

(ert-deftest kexport:display-creates-html-file-and-displays-it ()
  "kexport:display creates html file and displays it in external browser."
  (let* ((kotl-file (make-temp-file "hypb" nil ".kotl"))
         (html-file (concat (file-name-sans-extension kotl-file) ".html"))
         (browse-url-browser-function
          `(lambda (url &rest _args)
             (should (string= url (concat "file://" ',html-file)))))
         (auto-insert nil))
    (unwind-protect
        (progn
          (find-file kotl-file)
          (insert "first")
          (kotl-mode:add-cell)
          (insert "second")
          (should (string= (kexport:display) html-file)))
      (progn
        (hy-delete-file-and-buffer kotl-file)
        (hy-delete-file-and-buffer html-file)))))

(ert-deftest kexport:koutline-calls-kexport:html ()
  "kexport:koutline calls kexport:html and returns html buffer name."
  (let* ((kotl-file (make-temp-file "hypb" nil ".kotl"))
         (html-file (concat (file-name-sans-extension kotl-file) ".html")))
    (unwind-protect
        (cl-letf (((symbol-function 'kexport:html)
                   (lambda (export-from output-to &optional soft-newlines-flag)
                     (should (string= export-from kotl-file))
                     (should (string= output-to html-file))
                     (should (equal soft-newlines-flag nil))
                     nil)))
          (find-file kotl-file)
          (should (string= (kexport:koutline) html-file)))
      (progn
        (hy-delete-file-and-buffer kotl-file)))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'kexport-tests)
;;; kexport-tests.el ends here
