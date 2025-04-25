;;; hpath-tests.el --- unit tests for hpath         -*- lexical-binding: t; -*-

;; Author:       Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date:    28-Feb-21 at 23:26:00
;; Last-Mod:     25-Apr-25 at 21:51:39 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
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
(require 'el-mock)
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
	(expanded (condition-case _err
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
  "Test that `hpath:find' errors when it is non-existent."
  (condition-case _err
      (hpath:find "${hyperb:dir}/UNKNOWNFILE")
    (error (should t))))

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
  (defvar hypb:lc-var)
  (defvar hypb:uc-var)
  (setq hypb:lc-var "lower"
	hypb:uc-var "UPPER")
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
  (should (string= (hpath:substitute-value "${UNDEFINED_IS_NOT_SUBSTITUTED}") "${UNDEFINED_IS_NOT_SUBSTITUTED}")))

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
          (dolist (file '("DEMO" "man/hkey-help.txt" "kotl/kotl-mode.el" "man/im/demo.png"))
            (goto-char (point-min))
            (should (search-forward (car (last (split-string file "/"))) nil t))
            (backward-char (/ (length file) 2))
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
        (let ((default-directory hyperb:dir))
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

(ert-deftest hpath--at-p-checks-files-with-hash-in-name-exists ()
  "Verify that file existence is checked for filenames containing a hash character."
  (let ((dir (make-temp-file "hypb" t)))
    (unwind-protect
        (dolist (fn '("#file#" "#.file#" "#file path#" "path#section" "#path#section"))
          (let ((filename (expand-file-name fn dir)))
            (unwind-protect
                (progn
                  (find-file filename)
                  (insert "\"" fn "\"")
                  (save-buffer)
                  (goto-char 3)
                  (should (string= (hpath:at-p) fn))
                  (should (string= (hpath:is-p fn) fn)))
              (hy-delete-file-and-buffer filename))))
      (delete-directory dir))))

(ert-deftest hpath--at-p-checks-file-that-with-hash-that-does-not-exist-returns-nil ()
  "Verify that file existence is checked for filenames containing a hash character."
  (dolist (fn '("#file#" "#.file#" "#file path#" "path#section" "#path#section"))
    (with-temp-buffer
      (insert "\"" fn "\"")
      (goto-char 3)
      (should-not (hpath:at-p))
      (should-not (hpath:is-p fn)))))

(ert-deftest hpath--expand-no-wildcards-existing-path ()
  "Verify expand with no wildcards gives path back."
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (should (string= (hpath:expand file) file))
      (hy-delete-file-and-buffer file))))

(ert-deftest hpath--expand-variations-non-existing-path ()
  "Verify expand non existing paths."
  (should (string= (hpath:expand "/not/existing/file") "/not/existing/file"))
  (should-not (hpath:expand "/not/existing/file" t))
  (should (string= (hpath:expand "/not/existing/file*" t) "/not/existing/file*"))
  (should (string= (hpath:expand "/not/existing/file[]" t) "/not/existing/file[]"))
  (should (string= (hpath:expand "/not/existing/file?" t) "/not/existing/file?")))

(ert-deftest hpath--expand-elisp-variable ()
  "Verify an elisp ${variable} is expanded."
  (let ((hyperb-no-trailing-slash (substring hyperb:dir 0 -1)))
    (should (string= (hpath:expand "${hyperb:dir}") hyperb-no-trailing-slash))
    (should (string= (hpath:expand "${hyperb:dir}" t) hyperb-no-trailing-slash))
    (should (string= (hpath:expand "${hyperb:dir}/notexisting") "${hyperb:dir}/notexisting"))
    (should-not (hpath:expand "${hyperb:dir}/notexisting" t))))

(ert-deftest hpath--expand-environment-variable ()
  "Verify that a $VAR environment is expanded."
  (let ((envvar "hpath_test"))
    (unwind-protect
        (progn
          (setenv "HPATH" envvar)
          (should (string= (hpath:expand "$HPATH") envvar))
          ; Should next not work? See below where is works
          ;(should (string= (hpath:expand "${HPATH}") envvar))
          (should-not (hpath:expand "$HPATH" t))
          (should-not (hpath:expand "${HPATH}" t)))
      (setenv "HPATH")))
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (setenv "HPATH" file)
          (should (string= (hpath:expand "$HPATH") file))
          (should (string= (hpath:expand "${HPATH}") file))
          (should (string= (hpath:expand "$HPATH" t) file))
          (should (string= (hpath:expand "${HPATH}" t) file)))
      (setenv "HPATH")
      (hy-delete-file-and-buffer file))))

(ert-deftest hpath--expand-auto-variable-alist ()
  "Verify relative paths matching auto-variable-alist are expanded."
  (let ((hpath:auto-variable-alist '(("\\.el" . load-path))))
    (should (string= (hpath:expand "dired.el")
		     (locate-library "dired.el")))
    (should (string= (hpath:expand "dired.elc")
		     (hpath:resolve "dired.elc")))))

(ert-deftest hpath--resolve-auto-variable-alist ()
  "Verify relative paths matching auto-variable-alist are resolved."
  (let ((hpath:auto-variable-alist '(("\\.el" . load-path))))
    (should (string= (hpath:resolve "dired.el")
		     (locate-library "dired.el")))
    (should (string= (hpath:resolve "dired.elc")
		     (hpath:expand "dired.elc")))))

(ert-deftest hpath--expand-list-return-a-list ()
  "Verify expand-list returns a list of paths."
  (let ((file (make-temp-file "hypb")))
    (unwind-protect
        (progn
          (should (equal (hpath:expand-list '("/file1")) '("/file1")))
          (should (equal (hpath:expand-list '("/file1") ".*" t) '()))
          (should (equal (hpath:expand-list (list file)) (list file)))
          (should (equal (hpath:expand-list (list file) ".*" t) (list file)))
          (should (equal (hpath:expand-list '("/file1" "/file2")) '("/file1" "/file2")))
          (should (equal (hpath:expand-list (list "/file1" file)) (list "/file1" file)))
          (should (equal (hpath:expand-list (list "/file1" file) ".*" t) (list file))))
      (hy-delete-file-and-buffer file))))

(ert-deftest hpath--expand-list-match-regexp ()
  "Verify expand-list selects files using match regexp."
  (let* ((temp-dir (make-temp-file "hypb-dir" t))
	 (file-prefix (expand-file-name "hypb" temp-dir))
         (org1-file (make-temp-file file-prefix nil ".org"))
         (org2-file (make-temp-file file-prefix nil ".org"))
         (kotl-file (make-temp-file file-prefix nil ".kotl")))
    (unwind-protect
        (progn
          (should (= (length (hpath:expand-list (list temp-dir))) 3))
          (should (= (length (hpath:expand-list (list temp-dir) ".*")) 3))
          (should (= (length (hpath:expand-list (list temp-dir) "\\.org$")) 2))
          (should (= (length (hpath:expand-list (list temp-dir) "\\.kotl$")) 1))
          (should (= (length (hpath:expand-list (list temp-dir) "\\.md$")) 0)))
      (dolist (f (list org1-file org2-file kotl-file))
        (hy-delete-file-and-buffer f))
      (delete-directory temp-dir))))

(ert-deftest hpath--hpath:line-and-column-regexp ()
  "Verify that the regexp identifies paths with line and optional column.
See `hpath:line-and-column-regexp'."
  (should (string-match hpath:line-and-column-regexp "/foo/bar.org:1:2"))
  (should (string-match hpath:line-and-column-regexp "/foo/bar.org:L1:2"))
  (should (string-match hpath:line-and-column-regexp "/foo/bar.org:1:C2"))
  (should (string-match hpath:line-and-column-regexp "/foo/bar.org:L1:C2"))
  (should (string-match hpath:line-and-column-regexp "/foo/bar.org:1"))
  (should (string-match hpath:line-and-column-regexp "/foo/bar.org:L1"))
  (should-not (string-match hpath:line-and-column-regexp "/foo/bar.org:LL1"))
  (should-not (string-match hpath:line-and-column-regexp "/foo/bar.org:C1")))

(ert-deftest hpath--hpath:delimited-possible-path ()
  "Verify delimited path is found in an `ls -R' listings in `shell-mode'."
  (let ((files
         '(("file1.ext file2.ext file3.ext"                   ; Space delimited
            ("file1" "file2" "file3"))
           ("file1.ext\tfile2.ext\tfile3.ext"                 ; Tab delimited
            ("file1" "file2" "file3"))
           ("'file1.ext' 'file2.ext' 'file3.ext'"             ; Single quoted
            ("file1" "file2" "file3"))
           ("'file1.ext' file2.ext 'file3.ext'"               ; Single quoted mixed
            ("file1" "file2" "file3"))
           ("'file 1.ext' 'file 2.ext' 'file 3.ext'"          ; Single quoted with space
            ("file 1" "file 2" "file 3"))
           ("\"file1.ext\" \"file2.ext\" \"file3.ext\""       ; Double quoted
            ("file1" "file2" "file3"))
           ("\"file1.ext\" file2.ext \"file3.ext\""           ; Double quoted mixed
            ("file1" "file2" "file3"))
           ("\"file 1.ext\" \"file 2.ext\" \"file 3.ext\""    ; Double quoted with space
            ("file 1" "file 2" "file 3"))
           ("\"file1.ext\" 'file2.ext' \"file3.ext\""         ; Mixed quotes 1
            ("file1" "file2" "file3"))
           ("'file1.ext' \"file2.ext\" 'file3.ext'"           ; Mixed quotes 2
            ("file1" "file2" "file3"))
           (" file1.ext file2.ext file3.ext"                  ; Leading space
            ("file1" "file2" "file3"))
           ("\tfile1.ext file2.ext file3.ext"                 ; Leading tab
            ("file1" "file2" "file3"))

           ;; Failing cases
           ;; ("'file1\".ext' 'file2\".ext' 'file3\".ext'"    ; Single quoted with double quote
           ;;  ("file1\"" "file2\"" "file3\""))
           ;; ("\"file1'.ext\" \"file2'.ext\" \"file3'.ext\"" ; Double quoted with single quote
           ;;  ("file1'" "file2'" "file3'"))
           )))
    (dolist (fls files)
      (with-temp-buffer
        (insert "\
$ ls -R dir
dir/subdir:
" (car fls))
        (goto-char (point-min))
        (shell-mode)
        (dolist (v (cadr fls))
          (let ((filename v))
            (search-forward filename)
            (should (looking-at-p "\\.ext"))
            (mocklet (((file-exists-p "dir/subdir") => t))
              (should (string= (expand-file-name (concat filename ".ext") "dir/subdir")
                               (hpath:delimited-possible-path))))))))))

(ert-deftest hpath--to-markup-anchor ()
  "Verify `hpath:to-markup-anchor'."
  (dolist (v '((text-mode 2) (outline-mode 0)))
    (with-temp-buffer
      (funcall (car v)) ;; Mode
      (insert "Line 1\n* anchor\n* anchor")
      (goto-char 4)
      (hpath:to-markup-anchor nil "anchor")
      (should (= (line-number-at-pos) 2))
      (should (= (current-column) (cadr v)))

      (goto-char 4)
      (hpath:to-markup-anchor nil "anchor" 2)
      (should (= (line-number-at-pos) 3))
      (should (= (current-column) (cadr v)))

      (goto-char 4)
      (hpath:to-markup-anchor t nil)
      (should (= (line-number-at-pos) 1))
      (should (= (current-column) 0)))))

(ert-deftest hpath--spaces-to-dashes-markup-anchor ()
  "Verify `hpath:spaces-to-dashes-markup-anchor'."
  (with-temp-buffer
    ;; Normal case: Replace space wih dash.
    (should (string= "a-b-c" (hpath:spaces-to-dashes-markup-anchor "a b c")))
    ;; Mixed case: No conversion.
    (should (string= "a b-c" (hpath:spaces-to-dashes-markup-anchor "a b-c")))
    ;; Prog-mode: No conversion.
    (prog-mode)
    (should (string= "a b c" (hpath:spaces-to-dashes-markup-anchor "a b c")))
    (should (string= "a-b-c" (hpath:spaces-to-dashes-markup-anchor "a-b-c")))
    (should (string= "a b-c" (hpath:spaces-to-dashes-markup-anchor "a b-c")))))

(ert-deftest hpath--dashes-to-spaces-markup-anchor ()
  "Verify `hpath:dashes-to-spaces-markup-anchor'."
  (with-temp-buffer
    ;; Normal case: Replace dash with space.
    (should (string= "a b c" (hpath:dashes-to-spaces-markup-anchor "a-b-c")))
    ;; Mixed case: No conversion.
    (should (string= "a b-c" (hpath:dashes-to-spaces-markup-anchor "a b-c")))
    ;; Prog-mode: No conversion.
    (prog-mode)
    (should (string= "a b c" (hpath:spaces-to-dashes-markup-anchor "a b c")))
    (should (string= "a-b-c" (hpath:dashes-to-spaces-markup-anchor "a-b-c")))
    (should (string= "a b-c" (hpath:dashes-to-spaces-markup-anchor "a b-c")))))

(provide 'hpath-tests)

;; This file can't be byte-compiled without the `el-mock' package
;; which is not a dependency of Hyperbole.
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hpath-tests.el ends here
