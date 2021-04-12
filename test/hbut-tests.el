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

(load (expand-file-name "hy-test-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))
(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")

(ert-deftest hbut-defal ()
  (defal defal-path "${hyperb:dir}/\\1")
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-path DEMO>")
        (goto-char 4)
        (action-key)
        (should (string= (expand-file-name "DEMO" hyperb:dir) buffer-file-name))))
    (progn
      (kill-buffer "DEMO")
      (ibtype:delete 'ibtypes::defal-path)))

(defun hbut-defal-url (url &optional new-window)
  "Verify call with proper URL and optional NEW-WINDOW."
  (should (equal url "https://github.com/rswgnu/hyperbole/pull/34"))
  (should (equal new-window nil)))

(ert-deftest hbut-defal-url ()
  (defal defal-url "https://github.com/rswgnu/hyperbole/pull/\\1")
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-url 34>")
        (goto-char 4)
        (let ((browse-url-browser-function 'hbut-defal-url))
          (action-key)))
    (ibtype:delete 'ibtypes::defal-url)))

(ert-deftest hbut-defal-url-%s ()
  "defal supports %s in LINK-EXPR."
  (defal defal-url "https://github.com/rswgnu/hyperbole/pull/%s")
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-url 34>")
        (goto-char 4)
        (let ((browse-url-browser-function 'hbut-defal-url))
          (action-key)))
    (ibtype:delete 'ibtypes::defal-url)))

(ert-deftest hbut-defal-key-sequence ()
  (skip-unless (not noninteractive))
  (defal defal-key "{C-h v \\1 RET}")
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-key emacs-version>")
        (goto-char 4)
        (action-key)
        (hy-test-helpers:consume-input-events)
        (set-buffer "*Help*")
        (should (looking-at "emacs-version")))
    (ibtype:delete 'ibtypes::defal-key)))

(defun hbut-verify-defal (x)
  "Verify function i called with X set to the string `test'."
  (should (string= x "test")))

(ert-deftest hbut-defal-function ()
  "defal call function should only supply the argument portion of
the button text"
  (defal defal-func 'hbut-verify-defal)
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-func test>")
        (goto-char 4)
        (action-key)))
    (progn
      (ibtype:delete 'ibtypes::defal-func)))
  
(ert-deftest hbut-defal-fails-on-file-missing ()
  (defal defal-path-missing "${hyperb:dir}/\\1")
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-path-missing UNKNOWN-FILE>")
        (goto-char 4)
        (condition-case err
            (action-key)
          (error
           (progn
             (should (equal (car err) 'error))
             (should (string-search "hpath:find" (cadr err)))))))
    (ibtype:delete 'ibtypes::defal-path-missing)))

(ert-deftest hbut-defil-it ()
  (defil defil-path-it "<<<" ">>>" ".*" "${hyperb:dir}/\\&")
  (unwind-protect
      (with-temp-buffer
        (insert "<<<DEMO>>>")
        (goto-char 4)
        (action-key)
        (should (string= (expand-file-name "DEMO" hyperb:dir) buffer-file-name)))
    (progn
      (kill-buffer "DEMO")
      (ibtype:delete 'ibtypes::defil-path-it))))

(ert-deftest hbut-defil ()
  (defil defil-path "<<<" ">>>" ".*" "${hyperb:dir}/\\&")
  (unwind-protect
      (with-temp-buffer
        (insert "<<<DEMO>>>")
        (goto-char 4)
        (hy-test-helpers:action-key-should-call-hpath:find "${hyperb:dir}/DEMO"))
    (progn
      (ibtype:delete 'ibtypes::defil-path))))

(ert-deftest hbut-defil-url ()
  (defil defil-url "<<<" ">>>" ".*" "https://github.com/rswgnu/hyperbole/pull/\\&")
  (unwind-protect
      (with-temp-buffer
        (insert "<<<34>>>")
        (goto-char 4)
        (let ((browse-url-browser-function 'hbut-defal-url))
          (action-key)))
    (ibtype:delete 'ibtypes::defil-url)))

(ert-deftest hbut-defil-key-sequence ()
  (skip-unless (not noninteractive))
  (defil defil-key "<<<" ">>>" ".*" "{C-h v \\& RET}")
  (unwind-protect
      (with-temp-buffer
        (insert "<<<emacs-version>>>")
        (goto-char 4)
        (action-key)
        (hy-test-helpers:consume-input-events)
        (set-buffer "*Help*")
        (should (looking-at "emacs-version")))
    (ibtype:delete 'ibtypes::defil-key)))

;; Labels
(ert-deftest hbut-ib-link-to-file-with-label ()
  (with-temp-buffer
    (insert "<[emacs]>: \"${hyperb:dir}/DEMO\"")
    (goto-char 4)
    (hy-test-helpers:action-key-should-call-hpath:find (expand-file-name "DEMO" hyperb:dir))))

(ert-deftest hbut-ib-link-to-file-with-label-variants ()
  (cl-loop for ch in '(?: ?- ?=) do
           (cl-loop for n from 0 to 3 do
                    (with-temp-buffer
                      (insert "<[demo]>" (make-string n ch) " \"${hyperb:dir}/DEMO\"")
                      (goto-char 4)
                      (hy-test-helpers:action-key-should-call-hpath:find (expand-file-name "DEMO" hyperb:dir))))))

(ert-deftest hbut-ib-url-with-label ()
  "Should find link but fails with (user-error \"No link found\")"
  :expected-result :failed
  (with-temp-buffer
    (insert "<[PR34]>: \"https://github.com/rswgnu/hyperbole/pull/34\"")
    (goto-char 4)
    (let ((browse-url-browser-function 'hbut-defal-url))
      (action-key))))

(ert-deftest hbut-ib-create-label ()
  "Create a label for an implicit button"
  (with-temp-buffer
    (insert "\"/tmp\"\n")
    (goto-char 3)
    (with-simulated-input "TMP RET"
      (hui:ibut-label-create)
      (should (string= "<[TMP]> \"/tmp\"\n" (buffer-string))))))

(ert-deftest hbut-ib-create-label-fails-if-label-exists ()
  "Creation of a label for an implicit button fails if a label exists."
  (with-temp-buffer
    (insert "<[LBL]>: \"/tmp\"\n")
    (goto-char 14)
    (with-simulated-input "TMP RET"
      (condition-case err
          (hui:ibut-label-create)
        (error
         (progn
           (should (equal (car err) 'error))
           (should (string-search "ibutton at point already has a label" (cadr err)))))))))

(provide 'hbut-tests)
;;; hbut-tests.el ends here
