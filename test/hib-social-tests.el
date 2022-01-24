;;; hib-social-tests.el --- Test for hib-social      -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:24:00
;; Last-Mod:     24-Jan-22 at 00:38:39 by Bob Weiner
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

(provide 'hib-social-tests)
;;; hib-social-tests.el ends here
