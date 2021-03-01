;;; hbut-tests.el --- hbut unit tests         -*- lexical-binding: t; -*-

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 28-Feb-21 at 22:52:00
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Runs unit tests on some of the examples given in the DEMO file.

;;; Code:

(require 'ert)
(require 'hbut)

(ert-deftest hbut-defal ()
  (defal hfile "${hyperb:dir}/\\1")
  (unwind-protect
      (with-temp-buffer
        (insert "<hfile DEMO>")
        (goto-char 4)
        (action-key)
        (should (string= (concat hyperb:dir "DEMO") buffer-file-name))))
    (progn
      (kill-buffer "DEMO")
      (fmakunbound 'ibtypes::hfile)))

(ert-deftest hbut-defal-fails-on-file-missing ()
  (defal hfile "${hyperb:dir}/\\1")
  (unwind-protect
      (with-temp-buffer
        (insert "<hfile UNKNOWN-FILE>")
        (goto-char 4)
        (condition-case err
            (action-key)
          (error
           (progn
             (should (equal (car err) 'error))
             (should (string-search "hpath:find" (cadr err)))))))
    (fmakunbound 'ibtypes::hfile)))

(ert-deftest hbut-defil ()
  (defil hfile "<<<" ">>>" ".*" "${hyperb:dir}/\\&")
  (unwind-protect
      (with-temp-buffer
        (insert "<<<DEMO>>>")
        (goto-char 4)
        (action-key)
        (should (string= (concat hyperb:dir "DEMO") buffer-file-name)))
    (progn
      (kill-buffer "DEMO")
      (fmakunbound 'ibtypes::hfile))))

(provide 'hbut-tests)
;;; hbut-tests.el ends here
