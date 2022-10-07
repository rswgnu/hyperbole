;;; hibtypes-tests.el --- unit test for hib-kbd        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:45:00
;; Last-Mod:     12-Feb-22 at 13:33:53 by Bob Weiner
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Helper functions

;;; Code:

(require 'ert)
(require 'hib-kbd)
(require 'info)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")
(declare-function hy-test-helpers:should-last-message "hy-test-helpers")

;; (ert-deftest org-link-outside-org-mode-test ()

;; Mail address
(ert-deftest mail-address-at-p-test ()
  (with-temp-buffer
    (insert "someone@example.org")
    (goto-char 4)
    (should (mail-address-at-p))))

(ert-deftest mail-address-at-p-no-mail-should-fail-test ()
  (with-temp-buffer
    (insert "someone@example.test")
    (goto-char 4)
    (should (not (mail-address-at-p)))))

(ert-deftest ibtypes::mail-address-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "receiver@mail.org")
        (goto-char 2)
        (ibtypes::mail-address)
        (should (string= "*mail*" (buffer-name))))
    (kill-buffer "*mail*")))

;; Path name
(ert-deftest ibtypes::pathname-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s\"" (expand-file-name "DEMO" hyperb:dir)))
        (goto-char 2)
        (ibtypes::pathname)
        (should (string= "DEMO" (buffer-name))))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-lisp-variable-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO\"")
        (goto-char 2)
        (ibtypes::pathname)
        (should (string= "DEMO" (buffer-name))))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-env-variable-test ()
  (unwind-protect
      (when (getenv "HOME")
	(with-temp-buffer
          (insert "\"${HOME}\"")
          (goto-char 2)
          (ibtypes::pathname)
          (should (equal major-mode 'dired-mode))
          (should (= 0 (string-match (file-truename (getenv "HOME"))
				     (file-truename default-directory))))))
    nil))

(ert-deftest ibtypes::pathname-emacs-lisp-file-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\"hyperbole.el\"")
        (goto-char 2)
        (ibtypes::pathname)
        (should (equal major-mode 'emacs-lisp-mode))
        (should (string= "hyperbole.el" (buffer-name)))))
  (kill-buffer "hyperbole.el"))

(ert-deftest ibtypes::pathname-anchor-test ()
  "Pathname with anchor."
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO#Smart Keys\"")
        (goto-char 2)
        (ibtypes::pathname)
        (should (string= "DEMO" (buffer-name)))
        (should (looking-at "\* Smart Keys")))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-anchor-line-test ()
  "Pathname with anchor and line specification."
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO#Smart Keys:2\"")
        (goto-char 2)
        (ibtypes::pathname)
        (should (string= "DEMO" (buffer-name)))
        (forward-line -2)
        (should (looking-at "\* Smart Keys")))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-line-column-test ()
  "Pathname with line and position specification."
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO:3:45\"")
        (goto-char 2)
        (ibtypes::pathname-line-and-column)
        (should (string= "DEMO" (buffer-name)))
        (should (= (line-number-at-pos) 3))
        (should (= (current-column) 45)))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-load-path-line-column-test ()
  "Pathname with line and position specification."
  (unwind-protect
      (with-temp-buffer
        (insert "\"${load-path}/hypb.el:11:5\"")
        (goto-char 2)
        (ibtypes::pathname-line-and-column)
        (should (string= "hypb.el" (buffer-name)))
        (should (= (line-number-at-pos) 11))
        (should (= (current-column) 5)))
    (kill-buffer "hypb.el")))

(ert-deftest ibtypes::pathname-with-dash-loads-file-test ()
  "Pathname with dash loads file."
  (with-temp-buffer
    (insert "\"-${hyperb:dir}/test/hy-test-dependencies.el\"")
    (goto-char 2)
    (ibtypes::pathname)
    (hy-test-helpers:should-last-message "Loading")
    (hy-test-helpers:should-last-message "hy-test-dependencies.el")))

(ert-deftest ibtypes::pathname-dot-slash-in-other-folder-test ()
  "Invalid pathname that starts with ./ triggers an error when resolved."
  (with-temp-buffer
    (insert "\"./hypb.el\"")
    (goto-char 2)
    (condition-case err
        (action-key)
      (error
       (progn
         (should (equal (car err) 'error))
         (should (string-match "No action defined" (cadr err))))))))

(ert-deftest ibtypes::pathname-dot-slash-in-same-folder-test ()
  "Pathname that starts with ./ resolves properly when found in default-directory."
  (with-temp-buffer
    (insert "\"./hypb.el\"")
    (goto-char 2)
    (let ((help-buffer "*Help: Hyperbole Action Key*")
          (default-directory hyperb:dir))
      (hkey-help)
      (set-buffer help-buffer)
      (should (string-match "actype:.*link-to-file" (buffer-string))))))

(ert-deftest ibtypes::pathname-directory-test ()
  "Goto directory at point in path variable and open Dired."
  (let (visited-buf)
    (unwind-protect
	(with-temp-buffer
          (insert "\"/var/lib:/bar:/tmp:/foo\"")
          (goto-char 16)
          (ibtypes::pathname)
	  (setq visited-buf (current-buffer))
          (should (string-prefix-p "tmp" (buffer-name)))
          (should (eq major-mode 'dired-mode)))
      (when (and visited-buf
		 (buffer-live-p visited-buf))
 	(kill-buffer visited-buf)))))

;; !! Todo: XEmacs has a library to jump to a function definition in the current buffer.
;; What is the equivalent GNU Emacs one?

;; ibtypes::annot-bib
(ert-deftest ibtypes::annot-bib-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (re-search-forward "\\[FSF 19\\]")
        (backward-char 1)
        (ibtypes::annot-bib)
        (should (looking-at "\\[FSF 19\\] Free Software Foundation"))
        (forward-line -2)
        (should (looking-at "\\* References")))
    (kill-buffer "DEMO")))

;; markdown
; Can't find out how to use the markdown-internal-link ibtypes!?

;; rfc-toc
; Need rfc format of test buffer

;; id-cflow
; Need cross reference table built by externaö cxref program

;; ctags
;; Seems ctags -v does not give the proper answer
;; FIXME: Rewrite to not depend on hy-test-helpers.el
(ert-deftest ibtypes::ctags-vgrind-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "hy-test-helpers:consume-input-events hy-test-helpers.el 23\n")
        (goto-char (point-min))
        (forward-char 4)
        (let ((default-directory (expand-file-name "test" hyperb:dir)))
          (ibtypes::ctags)
          (set-buffer "hy-test-helpers.el")
          (should (looking-at "(defun hy-test-helpers:consume-input-events"))))
    (kill-buffer "hy-test-helpers.el")))

;; etags
;; FIXME: Rewrite to not depend on hy-test-helpers.el
(ert-deftest ibtypes::etags-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\n")
        (insert "hy-test-helpers.el,237\n")
        (insert "(defun hy-test-helpers:consume-input-events 23,518\n")
        (rename-buffer (concat "TAGS" (buffer-name)))
        (goto-char (point-min))
        (forward-line 2)
        (forward-char 10)
        (let ((default-directory (expand-file-name "test" hyperb:dir)))
          (ibtypes::etags)
          (set-buffer "hy-test-helpers.el")
          (should (looking-at "(defun hy-test-helpers:consume-input-events"))))
    (kill-buffer "hy-test-helpers.el")))

;; cscope

;; text-toc
(ert-deftest ibtypes::text-toc-test ()
  (unwind-protect
      (progn
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward " \* Koutl")
        (ibtypes::text-toc)
        (should (bolp))
        (should (looking-at "^* Koutliner")))
    (kill-buffer "DEMO")))

;; dir-summary
(ert-deftest ibtypes::dir-summary-test ()
  (unwind-protect
      (progn
        (find-file (expand-file-name "MANIFEST" hyperb:dir))
        (goto-char (point-min))
        (re-search-forward "HY-ABOUT")
        (forward-char -2)
        (let ((hpath:display-where 'this-window))
          (ibtypes::dir-summary)
          (should (string= "HY-ABOUT" (buffer-name)))))
    (progn
      (kill-buffer "MANIFEST")
      (kill-buffer "HY-ABOUT"))))

;; hib-debug

;; hib-kbd

;; rfc
(ert-deftest ibtypes::rfc-test ()
  (dolist (rfc '("RFC822" "RFC-822" "rfc 822"))
    (with-temp-buffer
      (insert rfc)
      (goto-char 2)
      (with-mock
        (mock (actypes::link-to-rfc "822") => t)
        (should (ibtypes::rfc))))))

;; man-apropos
(ert-deftest ibtypes::man-apropos-test ()
  (with-temp-buffer
    (insert "rm (1)   - remove")
    (goto-char 4)
    (with-mock
     (mock (man "rm(1)") => t)
     (ibtypes::man-apropos))))

;; klink

;; hlink

;; elink

;; glink

;; ilink

;; ipython-stack-frame

;; ripgrep-msg

;; grep-msg

;; debugger-source

;; pathname-line-and-column

;; elisp-compiler-msg

;; patch-msg

;; texinfo-ref

;; info-node
(ert-deftest ibtypes::info-node-test ()
  "Go to info node."
  (unwind-protect
      (with-temp-buffer
        (insert "\"(emacs)top\"")
        (goto-char 6)
        (ibtypes::Info-node)
        (should (string= "*info*" (buffer-name))))
    (kill-buffer "*info*")))

;; hyp-address

;; hyp-source

;; action

;; completion

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hibtypes-tests)
;;; hibtypes-tests.el ends here
