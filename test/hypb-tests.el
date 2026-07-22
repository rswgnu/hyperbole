;;; hypb-tests.el --- tests for hypb.el utility functions         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:     5-Apr-21 at 18:53:10
;; Last-Mod:     20-Jul-26 at 00:24:21 by Mats Lidell
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
  "Verify basic quote handling by `hypb:in-string-p'.
Verify with and without caching."
  (dolist (v '(nil t))
    (let ((s '(("   \"str\"   " . text-mode)         ;; double-quotes:
               ("   'str'   " . python-mode)         ;; Python single-quotes:
               (" '''str''' " . python-mode)         ;; Python triple single-quotes:
               (" \"\"\"str\"\"\" " . python-mode)   ;; Python triple double-quotes:
               ("  ``str''  " . texinfo-mode)))      ;; Texinfo open and close quotes:
          (test-num 0)
          str
          mode
          (hypb:in-string-cache-disable v))
      (with-temp-buffer
        (dolist (v s)
          (setq str (car v)
                mode (cdr v))
          (erase-buffer)
          (funcall mode)
          (insert str)
          (let ((pos 0)
                (response-list '(nil nil nil nil t t t nil nil nil nil)))
            (dolist (response response-list)
              (setq pos (1+ pos))
              (goto-char pos)
              (if (not response)
                  (progn
                    (ert-info ((format "Test #%d: At pos %d, char '%c', expected outside string text \"%s\" in mode: %s"
                                       test-num (point) (char-after (point)) str mode))
                      (should-not (hypb:in-string-p))))
                (ert-info ((format "Test #%d: At pos %d, expected inside string text \"%s\" in mode: %s"
                                   test-num (point) str mode))
                  (should (hypb:in-string-p))
                  (let ((seq (hypb:in-string-p nil t)))
                    (should (sequencep seq))
                    (cl-destructuring-bind (val beg end) seq
                      (should (stringp val))
                      (should (and beg end (= (- end beg) 3))))))))))))))

;; hypb:in-string-p tests with multiple strings
;; 'one string with "double \"quoted\" nested" that \' \' ends here'.
(defun hypb--gen-response-list (prefix q1 swq q2 suffix)
  "Generate a response list from the prefix, suffix and string with quotes."
  (append (make-list (length prefix) nil)
          (make-list (length q1) nil) ;; Starting quote regarded as outside quote
          (make-list (length swq) t)
          (make-list (length q2) t) ;; Ending quote regarded as inside quote
          (make-list (length suffix) nil)))

(ert-deftest hypb--in-string-p--strings-with-quotes ()
  "Verify that strings containing quotes are identified.
The test string is built by concatenating prefix, mode-start-quote,
string-with-quotes, mode-end-quote, and suffix.  Points within prefix
and suffix are checked to be outside of the string.  Points within
string-with-quotes is checked to be inside of the string.  For each test
string a list of mode settings that are applicable for that test string
are tried.  If `hypb:in-string-p' is expected to see point as within
string is generated by `hypb--gen-response-list'."
  (let ((prefix " pre ")
        (suffix " suff ")
        (mode-list '((txt . (text-mode "\"" "\""))
                     (py1 . (python-mode "'" "'"))
                     (py2 . (python-mode "'''" "'''"))
                     (py3 . (python-mode "\"\"\"" "\"\"\""))
                     (tex . (texinfo-mode "``" "''"))))
        (swq-list
         '(("word" . (txt py1 py2 py3 tex))
           ("wo'rd" . (txt py2 py3 tex))
           ("wo\"rd" . (py1 py2 py3 tex))
           (" \\\"quoted string\\\" " . (txt py1 py2 py3 tex))
           ("\\\"quoted string\\\"" . (txt py1 py2 py3 tex))
           (" \\\"quoted ' string\\\" " . (txt py2 py3 tex))
           (" 'quoted \\\" string' " . (txt py2 py3 tex))
           (" 'quoted string' " . (txt py2 py3 tex))
           (" 'quoted \\\"in quotes\\\" string' " . (txt py2 py3 tex))
           ("'quoted string'" . (txt py2 py3 tex))
           ("'quoted \\\"in quotes\\\" string'" . (txt py2 py3 tex))))
        (test-num 0))
    (with-temp-buffer
      (dolist (swq-word swq-list)
        (let ((swq (car swq-word))
              (modes (cdr swq-word)))
          (dolist (m modes)
            (let* ((mode (nth 0 (alist-get m mode-list)))
                   (quote1 (nth 1 (alist-get m mode-list)))
                   (quote2 (nth 2 (alist-get m mode-list)))
                   (s (concat prefix quote1 swq quote2 suffix)))
              (setq test-num (1+ test-num))
              (erase-buffer)
              (funcall mode)
              (insert s)
              (let ((pos 0)
                    (response-list (hypb--gen-response-list prefix quote1 swq quote2 suffix)))
                (dolist (response response-list)
                  (setq pos (1+ pos))
                  (goto-char pos)
                  (if (not response)
                      (progn
                        (ert-info ((format "Test #%d: At pos %d, char '%c', expected outside string text >|%s|< in mode: %s"
                                           test-num (point) (char-after (point)) s mode))
                          (should-not (hypb:in-string-p))))
                    (ert-info ((format "Test #%d: At pos %d, char '%c', expected inside string text >|%s|< in mode: %s"
                                       test-num (point) (char-after (point)) s mode))
                      (should (hypb:in-string-p))
                      (let ((seq (hypb:in-string-p nil t)))
                        (should (sequencep seq))
                        (cl-destructuring-bind (val beg end) seq
                          (should (stringp val))
                          (should (and beg end (= (- end beg) (length swq)))))))))))))))))

(ert-deftest hypb--in-string-p--max-lines ()
  "Verify max lines handling by `hypb:in-string-p'.
Verify with and without caching."
  (dolist (v '(nil t))
    (let* ((str "1\n\\\"2\n")
           (range (list str 2 8))
           (hypb:in-string-cache-disable v))
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
        (should (equal range (hypb:in-string-p 3 t)))))))

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

(ert-deftest hypb--users-package-manager ()
  "Verify `hypb:users-package-manager'."
  (hy-test-mocked-feature 'straight
    (should (eq 'straight (hypb:users-package-manager))))
  (hy-test-mocked-feature 'elpaca
    (should (eq 'elpaca (hypb:users-package-manager))))
  (should (eq 'package (hypb:users-package-manager))))

(ert-deftest hypb--package-el-install ()
  "Verify `hypb:package-el-install'."
  (let ((hypb:ask-to-install-package-flag t))
    (with-mock
      (mock (y-or-n-p *) => nil)
      (hypb:package-el-install 'hypb-dummy-package-name)))
  (let* ((hypb:ask-to-install-package-flag nil)
         (err (should-error (hypb:package-el-install 'hypb-dummy-package-name) :type 'error)))
    ;;FIXME: Emacs-28 includes a '-' when there is no version
    ;;number. Can be removed when we drop support for Emacs 28.
    (should (string-match-p (rx "Package " punct "hypb-dummy-package-name" (optional "-") punct " is unavailable") (cadr err)))))

(ert-deftest hypb--notify-manual-install-needed ()
  "Verify `hypb:notify-manual-install-needed'.
Verifies it raises a 'need to install' package manager error."
  (let ((err (should-error (hypb:notify-manual-install-needed 'la-package 'la-manager) :type 'user-error)))
    (should (string-search "la-package" (cadr err)))
    (should (string-search "la-manager" (cadr err)))))

(ert-deftest hypb--ensure-dependency ()
  "Verify `hypb:ensure-dependency'."
  (with-mock
    (mock (hypb:users-package-manager) => 'package)
    (mock (hypb:package-el-install 'the-package) => t)
    (should (hypb:ensure-dependency 'the-package)))
  (with-mock
    (mock (hypb:users-package-manager) => 'package)
    (mock (hypb:package-el-install 'the-package) => nil)
    (should-not (hypb:ensure-dependency 'the-package)))
  (with-mock
    (mock (hypb:users-package-manager) => 'elpaca)
    (mock (hypb:notify-manual-install-needed 'the-package 'elpaca) => (user-error "Error"))
    (should-error (hypb:ensure-dependency 'the-package) :type 'user-error)))

(ert-deftest hypb--require-package ()
  "Verify `hypb:require-package' signals if automatic install fails."
  (with-mock
    (mock (hypb:ensure-dependency 'package) => nil)
    (let ((err (should-error (hypb:require-package 'package) :type 'error)))
      (should (string-search "could not be found" (cadr err))))))

;; This file can't be byte-compiled without the `el-mock' package (because of
;; the use of the `with-mock' macro), which is not a dependency of Hyperbole.
;;  Local Variables:
;;  no-byte-compile: t
;;  End:

(provide 'hypb-tests)
;;; hypb-tests.el ends here
