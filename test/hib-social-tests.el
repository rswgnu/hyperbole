;;; hib-social-tests.el --- Test for hib-social      -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:24:00
;; Last-Mod:      3-Mar-24 at 11:28:17 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; 

;;; Code:

(require 'hib-social)
(require 'ert)                          ;To define `should' earlier.
(require 'el-mock)
(require 'hy-test-helpers)

;; Hib-social
(defun hib-social-should-browse-twitter-url (url &optional new-window)
  "Verify call with proper URL and optional NEW-WINDOW."
  (should (equal url "https://twitter.com/search?q=@fsf"))
  (should (equal new-window nil)))

(ert-deftest hib-social-twitter-test ()
  (with-temp-buffer
    (insert "tw@fsf")
    (goto-char 2)
    (let ((browse-url-browser-function 'hib-social-should-browse-twitter-url))
      (ibtypes::social-reference))))

;; Github
(defun hib-social-should-browse-github-url (url &optional new-window)
  "Verify call with proper URL and optional NEW-WINDOW."
  (should (equal url "https://github.com/rswgnu/hyperbole"))
  (should (equal new-window nil)))

(ert-deftest hib-github-user-default-test ()
  (with-temp-buffer
    (insert "gh#/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'hib-social-should-browse-github-url)
          (hibtypes-github-default-user "rswgnu"))
      (ibtypes::social-reference))))

(ert-deftest hib-github-ignore-default-test ()
  (with-temp-buffer
    (insert "gh#/rswgnu/hyperbole")
    (goto-char 4)
    (let ((browse-url-browser-function 'hib-social-should-browse-github-url)
          (hibtypes-github-default-user "whatever"))
      (ibtypes::social-reference))))

(ert-deftest hibtypes-repo-cache-does-not-exist-test ()
  "Verify cache is rebuild if it does not exist."
  (let ((hibtypes-git-repos-cache "not-existing-file"))
    (unwind-protect
        (mocklet (((hibtypes-git-build-repos-cache t) => t))
          (should (hibtypes-git-build-or-add-to-repos-cache "project")))
      (hy-delete-file-and-buffer hibtypes-git-repos-cache))))

(ert-deftest hibtypes-repo-cache-exist-test ()
  "Verify project is added if cache exist."
  (let ((hibtypes-git-repos-cache (make-temp-file "hypb" nil "cache" "cache contents")))
    (unwind-protect
        (mocklet (((hibtypes-git-add-project-to-repos-cache "project") => t))
          (should (hibtypes-git-build-or-add-to-repos-cache "project")))
      (hy-delete-file-and-buffer hibtypes-git-repos-cache))))

(provide 'hib-social-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hib-social-tests.el ends here

