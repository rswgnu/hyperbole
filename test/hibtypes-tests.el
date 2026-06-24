;;; hibtypes-tests.el --- unit test for hib-kbd        -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    20-Feb-21 at 23:45:00
;; Last-Mod:     24-Jun-26 at 20:26:23 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2026  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'hibtypes)
(require 'info)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:consume-input-events "hy-test-helpers")

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
        (let ((mail-user-agent 'sendmail-user-agent))
          (ibtypes::mail-address))
        (should (string= "*mail*" (buffer-name))))
    (kill-buffer "*mail*")))

;; Path name
(ert-deftest ibtypes::pathname-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s\"" (expand-file-name "DEMO" hyperb:dir)))
        (goto-char 2)
	(let ((enable-local-variables nil))
          (ibtypes::pathname))
        (should (string= "DEMO" (buffer-name))))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-lisp-variable-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO\"")
        (goto-char 2)
	(let ((enable-local-variables nil))
	  (ibtypes::pathname))
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
        (let ((enable-local-variables nil))
	  (ibtypes::pathname))
        (should (string= "DEMO" (buffer-name)))
        (should (looking-at "\* Smart Keys")))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-anchor-line-test ()
  "Pathname with anchor and line specification."
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO#Smart Keys:3\"")
        (goto-char 2)
        (ibtypes::pathname)
        (should (string= "DEMO" (buffer-name)))
        (forward-line -2)
        (should (looking-at "\\* Smart Keys")))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes::pathname-line-column-test ()
  "Pathname with line and position specification."
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO:3:45\"")
        (goto-char 2)
	(let ((enable-local-variables nil))
          (ibtypes::pathname-line-and-column))
        (should (string= "DEMO" (buffer-name)))
        (should (= (line-number-at-pos) 3))
        (should (= (current-column) 45)))
    (kill-buffer "DEMO")))

(ert-deftest ibtypes-tests--hib-link-to-file-line ()
  "Verify `hib-link-to-file-line'."
  (let ((default-directory hyperb:dir))
    (ert-info ("Find file in default-directory.")
      (with-mock
        (mock (actypes::link-to-file-line (expand-file-name "DEMO" hyperb:dir) "20") => t)
        (hib-link-to-file-line "DEMO" "20")))
    (ert-info ("Find elisp file in load-path.")
      (with-mock
        (mock (actypes::link-to-file-line (locate-library "simple.el") "20") => t)
        (hib-link-to-file-line "simple.el" "20"))))
  (let* ((default-directory (make-temp-file "hypb" t))
         (simple "simple.el"))
    (unwind-protect
        (ert-info ("Prefer elisp file in default-directory before load-path.")
          (with-temp-file simple)
          (should (file-exists-p "simple.el"))
          (with-mock
            (mock (actypes::link-to-file-line (expand-file-name "simple.el" default-directory) "20") => t)
            (hib-link-to-file-line "simple.el" "20")))
      (hy-delete-file-and-buffer simple)
      (hy-delete-dir-and-buffer default-directory))))

(ert-deftest ibtypes::pathname-load-path-line-column-test ()
  "Pathname with line and position specification."
  (with-temp-buffer
    (unwind-protect
        (progn
	  (insert "\"${load-path}/hypb.el:11:5\"")
          (goto-char 2)
          (ibtypes::pathname-line-and-column)
          (should (string-prefix-p "hypb.el" (buffer-name)))
          (should (= (line-number-at-pos) 11))
          (should (= (current-column) 5)))
      (kill-buffer (buffer-name)))))

(ert-deftest ibtypes::pathname-with-dash-loads-file-test ()
  "Pathname with dash loads file."
  (with-temp-buffer
    (insert "\"-${hyperb:dir}/test/hy-test-dependencies.el\"")
    (goto-char 2)
    (ert-with-message-capture cap
      (ibtypes::pathname)
      (hy-test-helpers:should-last-message "Loading" cap)
      (hy-test-helpers:should-last-message "hy-test-dependencies.el" cap))))

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
  "Pathname that starts with ./ resolves properly when found in `default-directory'."
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

;; hyp-manual
(ert-deftest ibtypes::hyp-manual-test ()
  "Verify ibut for Hyperbole manual file path."
  (with-temp-buffer
    (insert "\"hyperbole.html#Smart Keys\"")
    (goto-char 2)
    (mocklet (((actypes::www-url (concat "file://" (expand-file-name "hyperbole.html" (hpath:expand "${hyperb:dir}/man/")) "#Smart-Keys")) => t))
      (ibtypes::hyp-manual)))
  (with-temp-buffer
    (insert "\"hyperbole.texi#Smart Keys\"")
    (goto-char 2)
    (mocklet (((actypes::link-to-file (concat (expand-file-name "hyperbole.texi" (hpath:expand "${hyperb:dir}/man/")) "#Smart Keys")) => t))
      (ibtypes::hyp-manual))))

;; markdown
; Can't find out how to use the markdown-internal-link ibtypes!?

;; rfc-toc
; Need rfc format of test buffer

;; id-cflow
; Need cross reference table built by externaö cxref program

;; ctags
;; Seems ctags -v does not give the proper answer
(ert-deftest ibtypes::ctags-vgrind-test ()
  (let ((default-directory (ert-resource-directory)))
    (unwind-protect
        (with-temp-buffer
          (insert "test-func test-data.el 19\n")
          (goto-char (point-min))
          (forward-char 4)
          (ibtypes::ctags)
          (set-buffer "test-data.el")
          (should (looking-at "(defun test-func")))
      (hy-test-helpers:kill-buffer "test-data.el"))))

;; etags
(ert-deftest ibtypes::etags-test ()
  (let ((tags (find-file (ert-resource-file "TAGS")))
        (default-directory (ert-resource-directory)))
    (unwind-protect
        (with-current-buffer tags
          (goto-char (point-min))
          (forward-line 2)
          (forward-char 10)
          (ibtypes::etags)
          (set-buffer "test-data.el")
          (should (looking-at "(defun test-func")))
      (hy-test-helpers:kill-buffer "test-data.el")
      (hy-test-helpers:kill-buffer tags))))

;; cscope

;; text-toc
(ert-deftest ibtypes::text-toc-test ()
  (unwind-protect
      (let ((enable-local-variables nil))
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward " \\* Koutl")
        (ibtypes::text-toc)
        (should (bolp))
        (should (looking-at "^[ \t]*\\* Koutliner")))
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

;; hib-debug !!FIXME: Add tests.

;; hib-kbd !!FIXME: Add tests.

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
  (require 'man)
  (with-temp-buffer
    (insert "rm (1)   - remove")
    (goto-char 2)
    (with-mock
     (mock (man "rm(1)") => t)
     (ibtypes::man-apropos))))

;; klink !!FIXME: Add tests.

;; hlink !!FIXME: Add tests.

;; elink
(ert-deftest ibtypes::elink-test ()
  "Verify link to ebut in the same buffer."
  (let ((file (make-temp-file "elink")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<elink: Button >\n")
          (ebut:program "Button" 'eval-elisp '(message "EBUT"))
          (goto-char 4)
          (should (string= "EBUT" (ibtypes::elink)))

          (goto-char (point-min))
          (insert "<elink: Other >\n")
          (goto-char 4)
          (let ((err (should-error (ibtypes::elink))))
            (should
             ;; Error message actually is: "No button ‘Other’ in ‘nil’"
             ;; Nil looks wrong for the file name.
             (string-match-p (rx "No button " (any punct) "Other" (any punct))
                             (cadr err)))))
      (hy-delete-files-and-buffers (list file)))))

(ert-deftest ibtypes::elink-ebut-in-other-file ()
  "Verify link to ebut in other file."
  (let ((file (make-temp-file "elink")))
    (unwind-protect
        (progn
          (find-file file)
          (ebut:program "Button" 'eval-elisp '(message "EBUT"))

          (save-excursion
            (with-temp-buffer
              (insert (format "<elink: Button:\"%s\">\n" file))
              (goto-char 4)
              (should (string= "EBUT" (ibtypes::elink)))

              (goto-char (point-min))
              (insert (format "<elink: Other:\"%s\">\n" file))
              (goto-char 4)
              (let ((err (should-error (ibtypes::elink))))
                (should
                 (string-match-p (rx "No button " (any punct) "Other" (any punct))
                                 (cadr err)))))))
      (hy-delete-files-and-buffers (list file)))))

;; glink
(ert-deftest ibtypes::glink-test ()
  "Verify link to global button."
  (let ((file (make-temp-file "glink")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "\
<glink: Button >
<[Button]> <identity \"ARG\">
")
          (goto-char 4)
          (with-mock
            (mock (gbut:act "Button") => "ARG")
            (should (string= "ARG" (ibtypes::glink))))

          (with-mock
            (mock (gbut:get "Button") => nil)
            (let ((err (should-error (ibtypes::glink) :type 'error)))
              (should
               (string-match-p "No global button found for label: Button"
                               (cadr err))))))
      (hy-delete-file-and-buffer file))))

;; ilink
(ert-deftest ibtypes::ilink-test ()
  "Verify link to ibut in same buffer."
  (let ((file (make-temp-file "ilink-test")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<ilink: Button1 >\n<[Button1]> <identity 1>")
          (goto-char 4)
          (should (ibtype:test-p 'ilink))
          (should (= (ibut:act) 1))

          ;; ilink with file name
	  (erase-buffer)
          (insert "<[Button2]> <identity 2>")
          (save-excursion
            (with-temp-buffer
              (insert (format "<ilink: Button2:\"%s\">\n" file))
              (goto-char 4)
              (should (ibtype:test-p 'ilink))
              (should (= (ibut:act) 2))))

          (erase-buffer)
          (insert "<ilink: Button3 >\n<[Other]> <identity 3>")
          (goto-char 4)
          (let ((err (should-error (ibtypes::ilink))))
            (should
             (string-match-p (rx "No button " (any punct) "Button3" (any punct) " in")
                             (cadr err)))))
      (hy-delete-file-and-buffer file))))

(ert-deftest ibtypes::ilink-ibut-in-other-file ()
  "Verify link to ibut in other file."
  (let ((file (make-temp-file "ilink.txt")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[Button]> <identity 1>")

          (save-excursion
            (with-temp-buffer
              (insert (format "<ilink: Button:\"%s\">\n" file))
              (goto-char 4)
              (should (= 1 (ibtypes::ilink)))))

          (insert "<[ABC]> <identity 2>")

          (save-excursion
            (with-temp-buffer
              (insert (format "<ilink: ABC:\"%s\">\n" file))
              (goto-char 4)
              (should (= 2 (ibtypes::ilink)))))

          (save-excursion
            (with-temp-buffer
              (insert (format "<ilink: XYZ:\"%s\">\n" file))
              (goto-char 4)
              (let ((err (should-error (ibtypes::ilink))))
              (should
               (string-match-p (rx "No button " (any punct) "XYZ" (any punct) " in")
                               (cadr err)))))))

      (hy-delete-file-and-buffer file))))

(ert-deftest ibtypes::ilink-error-case-missing-button ()
  "Check `ibtypes::ilink' errors when named button does not exist in `other-buffer'."
  :expected-result :failed
  ;; FIXME: This is kept as a separate test case for showing the
  ;; problem. When it is fixed the test can me merged with the working
  ;; inlink test.
  (let ((file (make-temp-file "ilink-test")))
    (unwind-protect
        (progn
          (find-file file)
          (insert "<[Button]> <identity 1234>\n")
          (with-temp-buffer
            (insert (format "<ilink: XYZ : \"%s\">\n" file))
            (goto-char 4)
            ;; FIXME: This is what happens
            ;; (should (= 1234 (ibtypes::ilink))))))
            (let ((err (should-error (ibtypes::ilink))))
              (should
               (string-match-p (rx "No button " (any punct) "XYZ" (any punct) " in")
                               (cadr err))))))
      (hy-delete-file-and-buffer file))))

;; ipython-stack-frame !!FIXME: Add tests.

;; ripgrep-msg
(ert-deftest ibtypes::ripgrep-msg-test ()
  "Verify `ripgrep-msg'."
  ;; Date is picked up as a line number but file existence test before
  ;; concluding it is a button save it from being identified as
  ;; a ripgrep-msg.
  (with-temp-buffer
    (insert "one two three\n2024-07-30 line\n")
    (goto-line 2)
    (should-not (eq (hattr:get (hbut:at-p) 'actype) 'hib-link-to-file-line))
    (should-not (ibtypes::ripgrep-msg)))

  ;; Regular ripgrep-msg case.
  (with-temp-buffer
    (insert "hibtypes.el\n20: line\n")
    (goto-line 2)
    (mocklet (((hib-link-to-file-line "hibtypes.el" "20") => t)
              ((file-exists-p "hibtypes.el") => t))
      (should (eq (hattr:get (hbut:at-p) 'actype) 'hib-link-to-file-line))
      (should (ibtypes::ripgrep-msg))))

  ;; Regular match but file does not exist case.
  (with-temp-buffer
    (insert "unknown-file\n20: line\n")
    (goto-line 2)
    (should-not (eq (hattr:get (hbut:at-p) 'actype) 'hib-link-to-file-line))
    (should-not (ibtypes::ripgrep-msg))))

;; grep-msg
(ert-deftest ibtypes-tests--grep-msg ()
  "Verify `grep-msg' calls `hib-link-to-file-line' with grep patterns file name."
  ;; Regular grep-msg case.
  (with-temp-buffer
    (insert "filename:20: line\n")
    (goto-line 1)
    (mocklet (((hib-link-to-file-line "filename" "20") => t))
      (should (eq (hattr:get (hbut:at-p) 'actype) 'hib-link-to-file-line))
      (should (ibtypes::grep-msg)))))

;; debugger-source
(ert-deftest hibtypes-tests--hib-python-traceback ()
  "Verify `hib-python-traceback' finds expected matches.
Used in debugger-source for Python pdb or traceback, pytype error."
  (with-temp-buffer
    (ert-info ("Python traceback and pytype error")
      (insert "\
Traceback (most recent call last):
  File \"test.py\", line 1, in <module>
    5/0
    ~^~
ZeroDivisionError: division by zero
")
      (goto-char (point-min))
      (forward-line 1)
      (with-mock
       (mock (hact 'link-to-file-line "test.py" 1) => 'hact)
       (should (equal 'hact (hib-python-traceback)))
       (should (string= "test.py:1" (hattr:get 'hbut:current 'lbl-key)))))

    (ert-info ("pdb")
      (erase-buffer)
      (insert "\
ZeroDivisionError: division by zero
> test.py(1)<module>()
-> 5/0
")
      (goto-char (point-min))
      (forward-line 1)
      (with-mock
       (mock (hact 'link-to-file-line "test.py" 1) => 'hact)
       (should (equal 'hact (hib-python-traceback)))
       (should (string= "test.py:1" (hattr:get 'hbut:current 'lbl-key)))))

    (ert-info ("Negative test: Second line on ibut name")
      (erase-buffer)
      (insert "\
<[My
  Name]> - ls(1)
")
      (goto-char (point-min))
      (forward-line 1)
      (should-not (hib-python-traceback)))))

;; pathname-line-and-column !!FIXME: Add tests.

;; elisp-compiler-msg
(ert-deftest elisp-compiler-msg-test ()
  "Verify elisp-compiler-msg."
  (let ((orig-buffer-name (symbol-function 'buffer-name)))
    (cl-letf (((symbol-function 'buffer-name)
               (lambda (&optional buffer)
                 (if (string-prefix-p " *temp*" (funcall orig-buffer-name))
                     "*Compile-Log*"
                   (funcall orig-buffer-name)))))
      (with-temp-buffer
        (insert "    passed  1/1  abcde (0.000100 sec)\n")
        (goto-line 1)
        (mocklet (((smart-tags-display "abcde" nil) => t))
          (should (ibtypes::elisp-compiler-msg))))

      (with-temp-buffer
        (insert "Test abcde backtrace:\n")
        (goto-line 1)
        (mocklet (((smart-tags-display "abcde" nil) => t))
          (should (ibtypes::elisp-compiler-msg))))

      (with-temp-buffer
        (insert "Compiling /home/user/file.el...

In hyperbole-test:
file.el:10:20: Warning: Message
")
        (goto-line 3)
        (mocklet (((actypes::link-to-regexp-match "^(def[a-z \11]+hyperbole-test[ \11\n\15(]" 1 "/home/user/file.el" nil) => t))
          (should (ibtypes::elisp-compiler-msg)))))))

;; patch-msg !!FIXME: Add tests.

;; texinfo-ref !!FIXME: Add tests.

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

;; hyp-address !!FIXME: Add tests.

;; hyp-source !!FIXME: Add tests.

;; action !!FIXME: Add tests.

;; completion !!FIXME: Add tests.

(ert-deftest ibtypes:org-id-test ()
  "Verify `org-id' ibut."
  (with-temp-buffer
    (org-mode)
    (insert (format ":ID: %s" (hypb:uuid)))
    (goto-char 10)
    (should (string=
             "On Org ID definition; use {C-u M-RET} to copy a link to an ID."
             (ibtypes::org-id))))

  (let ((file (make-temp-file "hypb" nil ".org")))
    (unwind-protect
        (let ((id (hypb:uuid)))
          (find-file file)
          (org-mode)
          (insert (format "* header
:PROPERTIES:
:ID: %s
:END:

<id:%s>
" id id))
          (goto-char (point-min))
          (should (and (search-forward "<id:") (looking-at-p id)))
          (mocklet (((actypes::link-to-org-id-marker *) => t))
            (should (ibtypes::org-id))))
      (hy-delete-file-and-buffer file))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hibtypes-tests)
;;; hibtypes-tests.el ends here
