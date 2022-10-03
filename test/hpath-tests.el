;;; hpath-tests.el --- unit tests for hpath         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    28-Feb-21 at 23:26:00
;; Last-Mod:     12-Sep-22 at 22:11:14 by Mats Lidell
;;
;; Copyright (C) 2021-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Unit tests for "../hpath.el"

;;; Code:

(require 'ert)
(require 'hpath)
(require 'hy-test-helpers "test/hy-test-helpers")

(declare-function hy-test-helpers:action-key-should-call-hpath:find "hy-test-helpers")

(defconst hpath--should-exist-paths
  (let ((hyperb-dir-basename (file-name-nondirectory (directory-file-name hyperb:dir))))
    (list
     "hypb.el"
     "kotl/kview.el"
     "${hyperb:dir}"
     "${hyperb:dir}/hypb.el"
     "${hyperb:dir}/kotl/kview.el"
     "kview.el"
     "${load-path}/kview.el"
     "${load-path}/kotl/kview.el"
     "${hyperb:dir}/./hypb.el"
     (format "${hyperb:dir}/../%s/hypb.el" hyperb-dir-basename)
     "./hypb.el"
     (format "../%s/hypb.el" hyperb-dir-basename)
     (format "../%s/./hypb.el" hyperb-dir-basename)
     "~"
     "~/."
     (format "${load-path}/../%s/${DOT}/hypb.el" hyperb-dir-basename)
     (format "${load-path}/../%s/$DOT/hypb.el" hyperb-dir-basename)
     "$DOT"
     "${DOT}"
     ))
  "List of paths to test that should exist when expanded in ${hyperb:dir}.")

(defconst hpath--should-not-exist-paths
  '("hyb.el"
    "kol/kview.el"
    "${perb:dir}"
    "${hyperb:dir}/hyb.el"
    "${hyperb:dir}/kot/kview.el"
    "kvie.el"
    "${load-pat}/kview.el"
    "${load-path}/kotl/kvie.el"
    "{hyperb:dir}/./hypb.el"
    "${hyperb:dir}/../hyper/hypb.el"
    "./hypb.e"
    "../hyperole/hypb.el"
    "../hyperole/./hypb.el"
    "~/zzzzz"
    "~/./zzzzz"
    "${load-path}/../hyperbole/${DOT}/hyp.el"
    "${load-path}/../hyperbole/$DOT/hyp.el"
    "$DT"
    "${DT}"
    )
  "List of paths to test that should not exist when expanded in ${hyperb:dir}.")

(defun hpath--should-exist-p (path)
  (let ((default-directory hyperb:dir)
	(expanded (condition-case err
		      (hpath:expand path)
		    (error (list path err)))))
    (if (file-exists-p expanded)
	t
      (list path expanded))))

(defun hpath--should-not-exist-p (path)
  (let ((default-directory hyperb:dir)
	(expanded (condition-case err
		      (hpath:expand path)
		    (error path))))
    (if (not (file-exists-p expanded))
	t
      (list path expanded))))

(ert-deftest hpath:should-exist-paths ()
  "Expand paths in `hpath--should-exist-paths' and trigger an error on the first one that does not exist."
  (setenv "DOT" ".")
  (let ((failures (delq t (mapcar #'hpath--should-exist-p hpath--should-exist-paths))))
    (if failures
        (ert-fail (cons "These (original-path expanded-path) entries failed to exist when expanded with hpath:expand:" failures))
      t)))

(ert-deftest hpath:should-not-exist-paths ()
  "Expand paths in `hpath--should-not-exist-paths' and trigger an error on the first one that exists."
  (let ((failures (delq t (mapcar #'hpath--should-not-exist-p hpath--should-not-exist-paths))))
    (if failures
        (ert-fail (cons "These (original-path expanded-path) entries improperly existed when expanded with hpath:expand:" failures))
      t)))

(ert-deftest hpath:find-report-lisp-variable-path-name-when-not-exists ()
  "Test that hpath:find expands and returns filename when it is non-existent."
  (condition-case err
      (hpath:find "${hyperb:dir}/UNKNOWNFILE")
    (error (should (string-match (concat hyperb:dir "UNKNOWNFILE") (cadr err))))))

(ert-deftest hpath:path-at-point-in-path-variable-test ()
  "Find path at point in path variable."
  (with-temp-buffer
    (insert "\":foo:bar:emacs\"")
    (goto-char 8)
    (should (string= (hpath:at-p nil t) (expand-file-name "bar")))))

(ert-deftest hpath:path-at-point-in-path-variable-shorter-than-three-colons-returns-nil-test ()
  "Do not identify path variables with less than three colons."
  (with-temp-buffer
    (insert "\"foo:bar:lisp\"")
    (goto-char 7)
    (should (not (hpath:at-p)))))

(ert-deftest hpath:find-exec-shell-cmd-test ()
  "Path prefix ! will run pathname as a non windowed program."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'actypes::exec-shell-cmd)
               (lambda (filename)
                 (setq was-called (should (string= "/bin/ls" filename))))))
      (hpath:find "!/bin/ls")
      (should was-called))))

(ert-deftest hpath:find-exec-window-cmd-test ()
  "Path prefix & will run pathname as a windowed program."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'actypes::exec-window-cmd)
               (lambda (filename)
                 (setq was-called (should (string= "/bin/ls" filename))))))
      (hpath:find "&/bin/ls")
      (should was-called))))

(ert-deftest hpath:load-modifier-loads-file ()
  "Path prefix - will load elisp file."
  (let ((was-called nil))
    (cl-letf (((symbol-function 'load)
               (lambda (filename)
                 (setq was-called (should (string= "/folder/hyperbole.el" filename))))))
      (hpath:find "-/folder/hyperbole.el")
      (should was-called))))

(ert-deftest hpath:load-modifier-with-plain-file-loads-file-from-load-path ()
  "Path prefix - with filename without diretory will load from`load-path'."
  (setq features (delq 'tutorial features))
  (hpath:find "-tutorial.el")
  (should (featurep 'tutorial)))

(ert-deftest hpath:substitute-value-test ()
  "Environment and Lisp variables shall be substituted in a path."
  (progn
    (setq hypb:lc-var "lower")
    (setq hypb:uc-var "UPPER")
    (setenv "HYPB_TEST_ENV" "env")

    (should (string= (hpath:substitute-value "/nothing/to/substitute") "/nothing/to/substitute"))

    (should (string= (hpath:substitute-value "${hypb:lc-var}") hypb:lc-var))
    (should (string= (hpath:substitute-value "${hypb:uc-var}") hypb:uc-var))
    (should (string= (hpath:substitute-value "${HYPB_TEST_ENV}") (getenv "HYPB_TEST_ENV")))

    (should (string= (hpath:substitute-value "prefix${hypb:lc-var}suffix") (concat "prefix" hypb:lc-var "suffix")))
    (should (string= (hpath:substitute-value "prefix/${HYPB_TEST_ENV}/suffix") (concat "prefix/" (getenv "HYPB_TEST_ENV") "/suffix")))
    (should (string= (hpath:substitute-value "prefix${HYPB_TEST_ENV}suffix") (concat "prefix" (getenv "HYPB_TEST_ENV") "suffix")))

    (should (string= (hpath:substitute-value "${hypb:lc-var}${hypb:uc-var}") (concat hypb:lc-var hypb:uc-var)))
    (should (string= (hpath:substitute-value "${HYPB_TEST_ENV}/${HYPB_TEST_ENV}") (concat (getenv "HYPB_TEST_ENV") "/" (getenv "HYPB_TEST_ENV"))))
    (should (string= (hpath:substitute-value "${HYPB_TEST_ENV}${HYPB_TEST_ENV}") (concat (getenv "HYPB_TEST_ENV") (getenv "HYPB_TEST_ENV"))))

    (should (string= (hpath:substitute-value "prefix${hypb:lc-var}/${HYPB_TEST_ENV}/suffix") (concat "prefix" hypb:lc-var "/" (getenv "HYPB_TEST_ENV") "/suffix")))

    (should (string= (hpath:substitute-value "$UNDEFINED_IS_NOT_SUBSTITUTED") "$UNDEFINED_IS_NOT_SUBSTITUTED"))
    (should (string= (hpath:substitute-value "${UNDEFINED_IS_NOT_SUBSTITUTED}") "${UNDEFINED_IS_NOT_SUBSTITUTED}"))))

(defun hypb-run-shell-test-command (command buffer)
  "Run a shell COMMAND with output to BUFFER and select it."
  (switch-to-buffer buffer)
  (shell-mode)
  (goto-char (point-max))
  (shell-command command buffer nil))

(ert-deftest hpath:prepend-shell-directory-test ()
  "Find file in ls -R listing."
  (let ((shell-buffer "*hypb-test-shell-buffer*"))
    (unwind-protect
        (let* ((explicit-shell-file-name (or (executable-find "sh")
					     (executable-find "bash")
					     (executable-find "cmd")))
	       (shell-file-name explicit-shell-file-name)
	       (shell-cmd
		(if (memq system-type '(windows-nt cygwin ms-dos))
		    "dir -R"
		  "ls -R"))
               (default-directory hyperb:dir))
	  (should explicit-shell-file-name)
          (hypb-run-shell-test-command shell-cmd shell-buffer)
          (dolist (file '("COPYING" "man/hkey-help.txt" "man/version.texi" "man/im/demo.png"))
            (goto-char (point-min))
            (should (search-forward (car (last (split-string file "/"))) nil t))
            (backward-char 5)
            (hy-test-helpers:action-key-should-call-hpath:find (expand-file-name file hyperb:dir))))
      (kill-buffer shell-buffer))))

(ert-deftest hpath:auto-variable-alist-load-path-test ()
 "An elisp file, even without double quotes, should be looked up in the load path."
  (let ((load-path (list (expand-file-name "kotl" hyperb:dir)))
        (el-file "kview.el"))
    (with-temp-buffer
      (insert el-file)
      (goto-char 4)
      (hy-test-helpers:action-key-should-call-hpath:find (expand-file-name el-file (car load-path))))))

(defun hpath-tests--insert (str &optional with-quotes)
  "Insert STR with quotes if WITH-QUOTES is not nil."
  (concat (when with-quotes "\"") str (when with-quotes "\"")))

(ert-deftest hpath:auto-variable-alist-org-folder-test ()
  "An org file should be looked up in the org directory."
  (let ((org-directory (expand-file-name "HY-TALK" hyperb:dir))
        (org-file "HY-TALK.org"))
    (dolist (with-quotes '(nil t))
      (with-temp-buffer
        (insert (hpath-tests--insert org-file with-quotes))
        (goto-char 4)
        (hy-test-helpers:action-key-should-call-hpath:find (expand-file-name org-file org-directory))))))

(ert-deftest hpath:auto-variable-alist-pythonpath-test ()
  "A python file should be looked up in the PYTHONPATH."
  (let ((py-file "topwin.py")
        (old-python-path (getenv "PYTHONPATH")))
    (unwind-protect
        (progn
          (setenv "PYTHONPATH" hyperb:dir)
          (dolist (with-quotes '(nil t))
            (with-temp-buffer
              (insert (hpath-tests--insert py-file with-quotes))
              (goto-char 4)
              (hy-test-helpers:action-key-should-call-hpath:find (expand-file-name py-file hyperb:dir)))))
      (setenv "PYTHONPATH" old-python-path))))

(ert-deftest hpath:remote-at-p ()
  "Verify hpath:remote-at-p match a tramp remote file name."
  (let ((tramp-file "/ssh:user@host.org:/home/username/file"))
    (with-temp-buffer
      (insert (concat "\"" tramp-file "\""))
      (goto-char 5)
      (should (string= (hpath:remote-at-p) tramp-file)))))

(provide 'hpath-tests)
;;; hpath-tests.el ends here
