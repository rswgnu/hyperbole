;;; hypb-tests.el --- tests for hypb.el utility functions         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:     5-Apr-21 at 18:53:10
;; Last-Mod:     30-Dec-24 at 23:37:03 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
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

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hypb-tests)
;;; hypb-tests.el ends here
