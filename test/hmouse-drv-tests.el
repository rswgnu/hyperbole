;;; hmouse-drv-tests.el --- hmouse-drv unit tests         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    28-Feb-21 at 22:52:00
;; Last-Mod:     25-Apr-25 at 10:01:41 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Runs tests that are based on using the action-key.
;; See "../hmouse-drv.el"

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'hbut)
(require 'el-mock)
(require 'hy-test-helpers "test/hy-test-helpers")

(ert-deftest hbut-defal ()
  (defal defal-path "${hyperb:dir}/\\1")
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-path DEMO>")
        (goto-char 4)
        (hy-test-helpers:action-key-should-call-hpath:find "${hyperb:dir}/DEMO"))
    (when (string-match-p "DEMO" (buffer-name))
      (kill-buffer (current-buffer)))
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
  "Use defal with %s in LINK-EXPR."
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
  "Verify function is called with X set to the string `test'."
  (should (string= x "test")))

(ert-deftest hbut-defal-function ()
  "Use only the argument portion of the button text."
  (defal defal-func 'hbut-verify-defal)
  (unwind-protect
      (with-temp-buffer
        (insert "<defal-func test>")
        (goto-char 4)
        (action-key))
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
             (should (string-match-p "hpath:find" (cadr err)))))))
    (ibtype:delete 'ibtypes::defal-path-missing)))

(ert-deftest hbut-defil-it ()
  (defil defil-path-it "<<<" ">>>" ".*" "${hyperb:dir}/\\&")
  (let ((vc-follow-symlinks nil)
        (enable-local-variables nil))
    (unwind-protect
	(with-temp-buffer
          (insert "<<<DEMO>>>")
          (goto-char 4)
          (action-key)
          (should (string= (expand-file-name "DEMO" hyperb:dir) (hypb:buffer-file-name))))
      (hy-test-helpers:kill-buffer "DEMO")
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

(ert-deftest hbut-defil-verbatim ()
  "Verify defil to recognize filenames highlighted using org =verbatim= text."
  ;; gh#rswgnu/hyperbole/503
  (defil defil-verbatim "=" "=" ".*" "\\&")
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "=~/test.txt=\n")
        (goto-char 4)
        (hy-test-helpers:action-key-should-call-hpath:find "~/test.txt"))
    (ibtype:delete 'ibtypes::defil-verbatim)))

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
  "Find link using label."
  (with-temp-buffer
    (insert "<[PR34]>: \"https://github.com/rswgnu/hyperbole/pull/34\"")
    (goto-char 4)
    (let ((browse-url-browser-function 'hbut-defal-url))
      (action-key))
    (should (hattr:ibtype-is-p 'www-url))))

(ert-deftest hbut-ib-create-label ()
  "Create a label for an implicit button."
  (with-temp-buffer
    (insert "\"/tmp\"\n")
    (goto-char 3)
    (ert-simulate-keys "TMP\r"
      (let ((enable-recursive-minibuffers t))
	(hui:ibut-label-create)
	(should (string= "<[TMP]> - \"/tmp\"\n" (buffer-string)))))))

(ert-deftest hbut-ib-create-label-fails-if-label-exists ()
  "Creation of a label for an implicit button fails if a label exists."
  (with-temp-buffer
    (insert "<[LBL]>: \"/tmp\"\n")
    (goto-char 14)
    (ert-simulate-keys "TMP\r"
      (condition-case err
          (hui:ibut-label-create)
        (error
         (progn
           (should (equal (car err) 'error))
           (should (string-match "ibutton at point already has a label" (cadr err)))))))))

(ert-deftest hbut-pathname-path-variable-test ()
  "Find file in path variable value."
  (with-temp-buffer
    (insert "\"/var/lib:/bar:/tmp:/foo\"")
    (goto-char 16)
    (hy-test-helpers:action-key-should-call-hpath:find "/tmp")))

(ert-deftest hbut-pathname-path-variable-with-two-colons-a-path-test ()
  "Path variable value with two colons is recognized as a path variable value."
  (with-temp-buffer
    (insert "\"/var/lib:/bar:/tmp\"")
    (goto-char 16)
    (should (hpath:at-p))))

(ert-deftest hbut-pathname-path-variable-with-three-colons-is-a-path-test ()
  "Path variable value with three colons is sufficient to be recognized as a path variable value."
  (with-temp-buffer
    (insert "\"/var/lib:/bar:/tmp:/foo\"")
    (goto-char 16)
    (hy-test-helpers:action-key-should-call-hpath:find (expand-file-name "tmp" "/"))))

(ert-deftest hbut-pathname-path-variable-with-short-first-element-is-not-tramp-url-test ()
  "Path variable with three colons is not seen as a tramp url."
  (with-temp-buffer
    (insert "\"/a:/bar:/tmp:~foo\"")
    (goto-char 14)
    (hy-test-helpers:action-key-should-call-hpath:find "/tmp")))

;; Mail address
(ert-deftest hbut-mail-address-test ()
  "Open mail from mail address."
  (unwind-protect
      (with-temp-buffer
        (insert "receiver@mail.org")
        (goto-char 2)
        (let ((mail-user-agent 'sendmail-user-agent))
          (action-key))
	(should (hattr:ibtype-is-p 'mail-address))
        (should (string-match "mail\\*" (buffer-name))))
    (hy-test-helpers:kill-buffer "*mail*")))

;; Path name
(ert-deftest hbut-pathname-test ()
  (unwind-protect
      (with-temp-buffer
        (insert (format "\"%s\"" (expand-file-name "DEMO" hyperb:dir)))
        (goto-char 2)
	(let ((enable-local-variables nil))
          (action-key))
	(should (hattr:ibtype-is-p 'pathname))
        (should (string= "DEMO" (buffer-name))))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest hbut-pathname-lisp-variable-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\"${hyperb:dir}/DEMO\"")
        (goto-char 2)
	(let ((enable-local-variables nil))
          (action-key))
	(should (hattr:ibtype-is-p 'pathname))
        (should (string= "DEMO" (buffer-name))))
    (hy-test-helpers:kill-buffer "DEMO")))

(ert-deftest hbut-pathname-env-variable-test ()
  (with-temp-buffer
    (insert "\"${HOME}\"")
    (goto-char 2)
    (action-key)
    (should (hattr:ibtype-is-p 'pathname))
    (should (equal major-mode 'dired-mode))
    (should (equal (expand-file-name default-directory)
		   (file-name-as-directory (getenv "HOME"))))))

(ert-deftest hbut-pathname-emacs-lisp-file-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "\"hyperbole.el\"")
        (goto-char 2)
        (action-key)
	(should (hattr:ibtype-is-p 'pathname))
        (should (equal major-mode 'emacs-lisp-mode))
        (should (hypb:buffer-file-name))
        (should (string= "hyperbole.el" (buffer-name))))
    (hy-test-helpers:kill-buffer "hyperbole.el")))

(ert-deftest hbut-pathname-line-test ()
  "Pathname with line number specification."
  (let ((file (make-temp-file "hypb" nil nil
                              "Line1\nLine2\n")))
    (unwind-protect
        (with-temp-buffer
          (insert (concat "\"" file ":1\""))
          (goto-char 2)
          (action-key)
	  (should (hattr:ibtype-is-p 'pathname-line-and-column))
          (should (string= file (hypb:buffer-file-name)))
          (should (looking-at "Line1")))
      (hy-delete-file-and-buffer file))))

(ert-deftest hbut-pathname-line-test-duplicate ()
  "Pathname with line number specification, duplicate ibut to same file."
  (let* ((file
          (make-temp-file "hypb" nil nil
                          "Line1\nLine2\n"))
         (ibutfile
           (make-temp-file "hypb" nil nil
                           (concat "\"" file ":1\"" "\n" "\"" file ":2\"\n"))))
    (unwind-protect
        (progn
          (find-file ibutfile)
          (forward-line)
          (should (looking-at-p (concat "\"" file ":2\"")))
          (forward-char 2)
          (action-key)
	  (should (hattr:ibtype-is-p 'pathname-line-and-column))
          (should (string= file (hypb:buffer-file-name)))
          (should (looking-at "Line2")))
      (hy-delete-file-and-buffer file)
      (hy-delete-file-and-buffer ibutfile))))

(ert-deftest hbut-pathname-anchor-test ()
  "Pathname with anchor."
  (let ((file (make-temp-file "hypb" nil nil
                              "Text\n* Anchor\n")))
    (unwind-protect
        (with-temp-buffer
          (insert (concat "\"" file "#Anchor\""))
          (goto-char 2)
          (action-key)
	  (should (hattr:ibtype-is-p 'pathname))
          (should (string= file (hypb:buffer-file-name)))
          (should (looking-at "\* Anchor")))
      (hy-delete-file-and-buffer file))))

(ert-deftest hbut-pathname-anchor-trailing-text ()
  "Pathname with anchor and trailing parenthesised text."
  (let ((file (make-temp-file "hypb" nil nil
                              "Text\n* Anchor Plus (Parenthesised text follows)\n")))
  (unwind-protect
      (with-temp-buffer
        (insert (concat "\"" file "#Anchor Plus\""))
        (goto-char 2)
        (action-key)
	(should (hattr:ibtype-is-p 'pathname))
        (should (string= file (hypb:buffer-file-name)))
        (should (looking-at "\* Anchor Plus \(Parenthesised text follows\)")))
    (hy-delete-file-and-buffer file))))

(ert-deftest hbut-pathname-anchor-must-match-all ()
  "Verify that the whole anchor must match."
  (let ((file (make-temp-file "hypb" nil nil
                              "Text\n* Anchor Plus non parenthesised text\n")))
  (unwind-protect
      (with-temp-buffer
        (insert (concat "\"" file "#Anchor Plus\""))
        (goto-char 2)
        (let ((err (should-error (action-key) :type 'error)))
          (should (string-match-p
                   (concat "\(hpath:to-markup-anchor\): "
                           (buffer-name)
                           " - Section .Anchor Plus. not found in the visible buffer portion")
                   (cadr err)))))
    (hy-delete-file-and-buffer file))))

(ert-deftest hbut-pathname-anchor-line-test ()
  "Pathname with anchor and line specification."
  (let ((file (make-temp-file "hypb" nil nil
                              "Text\n* Anchor\nNext Line\n")))
  (unwind-protect
      (with-temp-buffer
        (insert (concat "\"" file "#Anchor:2\""))
        (goto-char 2)
        (action-key)
	(should (hattr:ibtype-is-p 'pathname-line-and-column))
        (should (string= file (hypb:buffer-file-name)))
        (should (looking-at "Next Line")))
    (hy-delete-file-and-buffer file))))

(ert-deftest hbut-pathname-line-column-test ()
  "Pathname with line and position specification."
  (let ((file (make-temp-file "hypb" nil nil
                              "Text\n* Anchor\nNext Line\n")))
    (unwind-protect
        (with-temp-buffer
          (insert (concat "\"" file "#Anchor:2:5\""))
          (goto-char 2)
          (action-key)
	  (should (hattr:ibtype-is-p 'pathname-line-and-column))
          (should (string= file (hypb:buffer-file-name)))
          (should (= (line-number-at-pos) 3))
          (should (= (current-column) 5))
          (should (looking-at "Line")))
      (hy-delete-file-and-buffer file))))

(ert-deftest hbut-pathname-load-path-line-column-test ()
  "Pathname with `load-path', line and position specification."
  (unwind-protect
      (with-temp-buffer
        (insert "\"${load-path}/hypb.el:11:5\"")
        (goto-char 2)
        (action-key)
	(should (hattr:ibtype-is-p 'pathname-line-and-column))
        (should (string= "hypb.el" (buffer-name)))
        (should (= (line-number-at-pos) 11))
        (should (= (current-column) 5)))
    (hy-test-helpers:kill-buffer "hypb.el")))

(ert-deftest hbut-pathname-with-dash-loads-file-test ()
  "Pathname with dash loads file."
  (with-temp-buffer
    (insert "\"-${hyperb:dir}/test/hy-test-dependencies.el\"")
    (goto-char 2)
    (action-key)
    (should (hattr:ibtype-is-p 'pathname))
    (hy-test-helpers:should-last-message "Loading")
    (hy-test-helpers:should-last-message "hy-test-dependencies.el")))

(ert-deftest hbut-pathname-directory-test ()
  "Pathname with directory opens Dired."
  (unwind-protect
      (with-temp-buffer
        (insert "\"/tmp\"")
        (goto-char 2)
        (action-key)
	(should (hattr:ibtype-is-p 'pathname))
        (should (string-equal default-directory "/tmp/"))
        (should (eq major-mode 'dired-mode)))
    (hy-test-helpers:kill-buffer "tmp")))

(ert-deftest hbut-pathname-dot-slash-in-other-folder-should-fail-test ()
  "Pathname that starts with ./ only works if in same folder."
  (with-temp-buffer
    (insert "\"./hypb.el\"")
    (goto-char 2)
    (let ((help-buffer "*Help: Hyperbole Action Key*")
          (default-directory (expand-file-name "test" hyperb:dir)))
      (condition-case err
          (action-key)
        (error
         (progn
	   (should-not (hattr:ibtype-is-p 'pathname))
           (should (equal (car err) 'error))
           (should (string-match
                    "(Hyperbole Action Key): No action defined for this context; try another location"
                    (cadr err)))))))))

;; ctags
; Seems ctags -v does not give the proper answer
(ert-deftest hbut-ctags-vgrind-test ()
  (unwind-protect
      (with-temp-buffer
        (insert "test-func test-data.el 19\n")
        (goto-char (point-min))
        (forward-char 4)
        (let ((default-directory (ert-resource-directory)))
          (action-key)
	  (should (hattr:ibtype-is-p 'ctags))
          (should (looking-at "(defun test-func"))))
    (hy-test-helpers:kill-buffer "test-data.el")))

;; etags
(ert-deftest hbut-etags-test ()
  (let ((tags (find-file (ert-resource-file "TAGS"))))
    (unwind-protect
        (with-current-buffer tags
          (goto-char (point-min))
          (forward-line 2)
          (forward-char 10)
          (let ((default-directory (ert-resource-directory)))
            (action-key)
	    (should (hattr:ibtype-is-p 'etags))
            (set-buffer "test-data.el")
            (should (looking-at "(defun test-func"))))
      (hy-test-helpers:kill-buffer "test-data.el")
      (hy-test-helpers:kill-buffer tags))))

;; text-toc
(ert-deftest hbut-text-toc-test ()
  (unwind-protect
      (let ((enable-local-variables nil))
        (hypb:display-file-with-logo "DEMO")
        (goto-char (point-min))
        (re-search-forward "^[ \t]*\\* Koutl")
        (action-key)
	(should (hattr:ibtype-is-p 'text-toc))
        (should (bolp))
        (should (looking-at "^[ \t]*\\* Koutliner")))
    (hy-test-helpers:kill-buffer "DEMO")))

;; dir-summary
(ert-deftest hbut-dir-summary-test ()
  (unwind-protect
      (progn
        (find-file (expand-file-name "MANIFEST" hyperb:dir))
        (goto-char (point-min))
        (re-search-forward "HY-ABOUT")
        (forward-char -2)
        (let ((hpath:display-where 'this-window))
          (action-key)
	  (should (hattr:ibtype-is-p 'dir-summary))
          (should (string= "HY-ABOUT" (buffer-name)))))
    (hy-test-helpers:kill-buffer "MANIFEST")
    (hy-test-helpers:kill-buffer "HY-ABOUT")))

;; rfc
(ert-deftest hbut-rfc-test ()
  (dolist (rfc '("RFC822" "RFC-822" "rfc 822"))
    (with-temp-buffer
      (insert rfc)
      (goto-char 2)
      (with-mock
        (mock (actypes::link-to-rfc "822") => t)
	(should (action-key))
	(should (hattr:ibtype-is-p 'rfc))))))

;; man-apropos
(ert-deftest hbut-man-apropos-test ()
  (with-temp-buffer
    (insert "rm (1)   - remove")
    (goto-char 4)
    (with-mock
     (mock (man "rm(1)") => t)
     (should (action-key))
     (should (hattr:ibtype-is-p 'man-apropos)))))

;; info-node
(ert-deftest hbut-info-node-test ()
  "Test Info node link."
  (unwind-protect
      (with-temp-buffer
        (insert "\"(emacs)Top\"")
        (goto-char 6)
        (action-key)
	(should (hattr:ibtype-is-p 'Info-node))
        (should (string= "*info*" (buffer-name))))
    (hy-test-helpers:kill-buffer "*info*")))

;; exec-shell-cmd
(ert-deftest hbut-find-exec-shell-cmd-test ()
  "Path prefix ! will run pathname as a non windowed program."
  (with-temp-buffer
    (insert "\"!/bin/ls\"")
    (goto-char 2)
    (let ((was-called nil))
      (cl-letf (((symbol-function 'actypes::exec-shell-cmd)
                 (lambda (filename)
                   (setq was-called (should (string= "/bin/ls" filename))))))
        (action-key)
        (should was-called)))))

;; exec-window-cmd
(ert-deftest hbut-find-exec-window-cmd-test ()
  "Path prefix & will run pathname as a windowed program."
  (with-temp-buffer
    (insert "\"&/bin/ls\"")
    (goto-char 2)
    (let ((was-called nil))
      (cl-letf (((symbol-function 'actypes::exec-window-cmd)
                 (lambda (filename)
                   (setq was-called (should (string= "/bin/ls" filename))))))
        (action-key)
        (should was-called)))))

(ert-deftest hbut-load-modifier-loads-file ()
  "Path prefix - will load elisp file."
  (with-temp-buffer
    (insert "\"-/folder/hyperbole.el\"")
    (goto-char 2)
    (let ((was-called nil))
      (cl-letf (((symbol-function 'load)
                 (lambda (filename)
                   (setq was-called (should (string= "/folder/hyperbole.el" filename))))))
        (action-key)
        (should was-called)))))

(ert-deftest hbut-load-modifier-with-plain-file-loads-file-from-load-path ()
  "Path prefix - filename without directory will load from `load-path'."
  (setq features (delq 'tutorial features))
  (with-temp-buffer
    (insert "\"-tutorial.el\"")
    (goto-char 2)
    (action-key)
    (should (featurep 'tutorial))))

(ert-deftest hbut-key-press-on-hyphen-in-elisp-symbol ()
  "Key press on hyphen in elisp symbol uses smart-lisp-find-tag.
Regression: Looked up path name '-narrow'."
  (let* ((symbol-name "hmail:msg-narrow")
         (el-file (make-temp-file "hypb" nil ".el" (concat "(" symbol-name ")"))))
    (unwind-protect
        (with-current-buffer (find-file el-file)
          (goto-char (point-min))
          (goto-char (1- (re-search-forward "-")))
          (should (string= (smart-lisp-at-tag-p) symbol-name))
          (with-mock
            (mock (smart-lisp-find-tag nil nil) => t)
            (action-key)))
      (hy-delete-file-and-buffer el-file))))

(ert-deftest hmouse-drv--hmouse-choose-link-and-referent-windows--two-windows-same-frame ()
  "Verify two windows in the same frame are chosen."
  (let ((assist-key-depress-window)
        (assist-key-release-window)
        (filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".txt" "1234567890")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (split-window)
          (find-file filea)
          (let ((result (hmouse-choose-link-and-referent-windows)))
            (should (= (length result) 2))
            (should (equal (list (get-buffer-window (get-file-buffer filea))
                                 (get-buffer-window (get-file-buffer fileb)))
                           result))))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))


(ert-deftest hmouse-drv--hmouse-choose-link-and-referent-windows--two-windows-different-frame ()
  "Verify two windows in the same frame are chosen.
The frame setup is mocked."
  (defvar fileb-window)
  (let ((assist-key-depress-window)
        (assist-key-release-window)
        (filea (make-temp-file "hypb" nil ".txt"))
        (fileb (make-temp-file "hypb" nil ".txt" "1234567890")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (find-file fileb)
          (split-window)
          (find-file filea)
          (setq fileb-window (get-buffer-window (get-file-buffer fileb)))
          (mocklet ((hypb:count-visible-windows => 2)
                    ((next-window nil nil 'visible) => fileb-window)
                    (count-windows => 1))
            (let ((result (hmouse-choose-link-and-referent-windows)))
              (should (= (length result) 2))
              (should (equal (list (get-buffer-window (get-file-buffer filea))
                                   (get-buffer-window (get-file-buffer fileb)))
                             result)))))
      (hy-delete-file-and-buffer filea)
      (hy-delete-file-and-buffer fileb))))

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hmouse-drv-tests)
;;; hmouse-drv-tests.el ends here
