;;; hypb-tests.el --- tests for hypb.el utility functions         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:     5-Apr-21 at 18:53:10
;; Last-Mod:     14-Sep-25 at 15:53:43 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for "../hypb.el"

;;; Code:

(require 'hypb)
(require 'hbut)
(require 'ert)
(require 'el-mock)
(require 'hy-test-helpers)

(ert-deftest hypb:installation-type-test ()
  "Verify installation type alternatives."
  ;; straight.el package install -- hyperbole gnu-elpa-mirror master 56cd3d8 2022-02-05
  (require 'package)
  (unless package--initialized
    (package-initialize))
  (when (featurep 'straight)
    (let* ((hyperb:dir (expand-file-name "straight/build/hyperbole" user-emacs-directory))
	   (default-directory (expand-file-name "straight/repos/hyperbole" user-emacs-directory))
	   (package-installed-p 'straight)
	   (plist (hypb:straight-package-plist "hyperbole"))
	   (install-type-list (when plist (hypb:installation-type)))
	   (commit (nth 1 install-type-list)))
      (when plist
	(should (and (equal (nth 0 install-type-list) "straight")
		     commit
		     (string-match-p "\\`[a-f0-9]+\\'" commit)
		     t)))))
  ;; elpa-devel package install -- hyperbole-8.0.0pre0.20220126.1138
  (let ((hyperb:dir (expand-file-name "home/user/.emacs.d/elpa/hyperbole-8.0.0pre0.20220126.1138" "/")))
    (should (equal (hypb:installation-type) '("elpa-devel" "8.0.0pre0.20220126.1138"))))
  ;; melpa/quelpa package instball -- hyperbole-20220205.1429
  (let ((hyperb:dir (expand-file-name "home/user/.emacs.d/elpa/hyperbole-20220126.1138" "/")))
    (should (equal (hypb:installation-type) '("melpa" "20220126.1138"))))
  ;; git install -- hyperbole d43d05a097
  (let ((hyperb:dir (expand-file-name "a_git_folder" "/")))
    (with-mock
      (mock (file-exists-p (expand-file-name ".git" hyperb:dir)) => t)
      (mock (shell-command-to-string "git rev-parse HEAD") => "d43d05a0973e8adcbfdd8c85681dac5de669aaa9")
      (should (equal (hypb:installation-type) '("git" "d43d05a097")))))
  ;; elpa package install -- /elpa/hyperbole-8.0.0"
  (let ((hyperb:dir (expand-file-name "home/user/.emacs.d/elpa/hyperbole-8.0.0" "/")))
    (should (equal (hypb:installation-type) '("elpa" "8.0.0"))))
  ;; tarball archive install -- hyperbole-8.0.0
  (let ((hyperb:dir (expand-file-name "home/user/hyperbole-8.0.0" "/")))
    (should (equal (hypb:installation-type) '("archive" "8.0.0"))))
  ;; unknown
  (let ((hyperb:dir (expand-file-name "home/user/hyperbole" "/")))
    (with-mock
      (mock (file-exists-p (expand-file-name ".git" hyperb:dir)) => nil)
      (should (equal (car (hypb:installation-type)) "unknown")))))

(ert-deftest hypb--oct-to-int ()
  "Verify oct to int conversion."
  (should (= (hypb:oct-to-int 0) 0))
  (should (= (hypb:oct-to-int 1) 1))
  (should (= (hypb:oct-to-int 7) 7))
  (should (= (hypb:oct-to-int 10) 8))
  (should (= (hypb:oct-to-int 2000) 1024))
  (should-error (hypb:oct-to-int 8) :type 'error))

(ert-deftest hypb--verify-info-index-is-correct ()
  "Verify that Hyperbole info page indexes are identified.
See Emacs bug#74042 related to usage of texi2any."
  (unwind-protect
      (progn
        (Info-goto-node "(Hyperbole)Top")
        (should (set:equal '("Key Index" "Function Index" "Concept Index") (Info-index-nodes))))
    (hy-test-helpers:kill-buffer "*info*")))

(ert-deftest hypb--in-string-p ()
  "Verify basic quote handing by `hypb:in-string-p'."
  (let ((s '(("\"str\"" . text-mode)            ;; double-quotes:
             ("'str'" . python-mode)            ;; Python single-quotes:
             ("'''str'''" . python-mode)        ;; Python triple single-quotes:
             ("\"\"\"str\"\"\"" . python-mode)  ;; Python triple double-quotes:
             ("``str''" . texinfo-mode))))      ;; Texinfo open and close quotes:
    (dolist (v s)
      (let ((str (car v))
            (mode (cdr v)))
        (with-temp-buffer
          (funcall mode)
          (insert str)
          (goto-char (/ (length str) 2))
          (should (hypb:in-string-p))
          (let ((seq (hypb:in-string-p nil t)))
            (should (sequencep seq))
            (cl-destructuring-bind (val beg end) seq
              (should val)
              (should (and beg end (= (- end beg) 3))))))))))

(ert-deftest hypb--in-string-p--max-lines ()
  "Verify max lines handling by `hypb:in-string-p'."
  (let* ((str "1\n\\\"2\n")
         (range (list str 2 8)))
    (with-temp-buffer
      (insert (format "\"%s\"" str))
      (goto-line 1) (move-to-column 1)
      ;; First line. Line starts with quote.
      (should-not (hypb:in-string-p 1))
      (should (hypb:in-string-p 2))
      (should (hypb:in-string-p 3))
      (should (hypb:in-string-p 99))

      ;; With range-flag
      (should (equal range (hypb:in-string-p 2 t)))
      (should (equal range (hypb:in-string-p 3 t)))
      (should (equal range (hypb:in-string-p 99 t)))

      ;; Zero max-lines
      (should-not (hypb:in-string-p 0))

      ;; Second line. No quote on the line.
      (goto-line 2)
      (should-not (hypb:in-string-p 1))
      (should (hypb:in-string-p 2))
      (should (hypb:in-string-p 3))

      ;; With range-flag
      (should (equal range (hypb:in-string-p 2 t)))
      (should (equal range (hypb:in-string-p 3 t))))))

(ert-deftest hypb--string-count-matches ()
  "Verify `hypb--string-count-matches'."
  (should (= 2 (hypb:string-count-matches "a" "abcabd")))
  (should (= 1 (hypb:string-count-matches "a" "abcabd" 0 2)))
  (should (= 0 (hypb:string-count-matches "a" "abcabd" 1 3)))
  (should (= 1 (hypb:string-count-matches "a" "abcabd" 1 4)))
  ;; Overlap
  (should (= 1 (hypb:string-count-matches "aba" "ababa")))
  (should (= 2 (hypb:string-count-matches "aba" "abababa")))
  ;; Errors
  (should-error (hypb:string-count-matches "a" "abc" -1 1))
  (should-error (hypb:string-count-matches "a" "a" 1 3))
  (should-error (hypb:string-count-matches "a" "a" 0 -1))
  (should-error (hypb:string-count-matches "a" "ab" 0 3)))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hypb-tests)
;;; hypb-tests.el ends here
