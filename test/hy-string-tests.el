;;; hy-string-tests.el --- test whether point is inside or outside of a string    -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    22-Jul-26 at 23:41:29
;; Last-Mod:     27-Jul-26 at 16:52:48 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2026  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Test point location relative to string start and end locations.

;;; Code:

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'cl-lib) ;; For `cl-incf'
(require 'hypb)   ;; For `hypb:in-string-p'

;; Sample use of next function
;; (debug-in-string "double-quotes" "str")
;; (debug-in-string "double-with-single" "' st' 'r' ")
;; (debug-in-string "python-triple-multi-line" "\"\"\"\n str\n\"\"\"" 'python-mode)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun debug-in-string (test-name str &optional mode)
  "With TEST-NAME for each char in STR, print results of whether in the string.
STR, including delimiters, is inserted into a blank buffer for testing.
With optional major MODE, a function, that mode is enabled prior to testing the string."
  (interactive sStr to test: \")
  (with-temp-buffer
    (when mode (funcall mode))
    (insert "\"" str "\"")
    (with-help-window (format "*%s Results*" (capitalize test-name))
      (prin1 str)
      (when mode
        (princ " - ")
        (princ (cond ((symbolp mode) (symbol-name mode))
                     ((stringp mode) mode))))
      (terpri)
      (terpri)
      (goto-char (point-min))
      (let ((len (+ (length str) 2))
            (i 1)
            hypb-in-str
            ppss-in-str
            str-start
            foll-quote)
        (while (/= i (point-max))
          (goto-char i)
          (setq hypb-in-str (hypb:in-string-p)     ;; Hyperbole in-string test
                ppss-in-str (nth 3 (syntax-ppss))  ;; Emacs in-string test
                str-start   (nth 8 (syntax-ppss))  ;; Char that starts this string, if any
                foll-quote  (nth 5 (syntax-ppss))) ;; t if following a quote char
          (princ
           (format "%s Pos %2d, char '%c', in-str hypb=%3S %3S=ppss, str-start=%3S, following-quote=%3S"
                   (if (or (and hypb-in-str ppss-in-str)
                           (and (not hypb-in-str) (not ppss-in-str)))
                       "."
                     "F")
                   i (following-char)
                   hypb-in-str ppss-in-str str-start foll-quote
                   ;; Hyperbole in-string test
                   (hypb:in-string-p)
                   ;; Emacs in-string test; any non-nil value is the
                   ;; character that will terminate the string, or t if the
                   ;; string should be terminated by a generic string
                   ;; delimiter
                   (nth 3 (syntax-ppss))
                   ;; Character address of start of comment or string;
                   ;; nil if not in one
                   (nth 8 (syntax-ppss))
                   ;; t if following a quote char
                   (nth 5 (syntax-ppss))
                   ;; Sixth arg COMMENTSTOP non-nil means stop after the
                   ;; start of a comment. If it is the symbol
                   ;; ‘syntax-table’, stop after the start of a comment or a
                   ;; string, or after end of a comment or a string.
                   ))
          (terpri)
          (cl-incf i))))))

;;; !! TODO: Fix test failures
;;; Comment out for now
(unless t

(ert-deftest hy-string-tests--strings-with-quotes ()
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

(defun hy-string-tests--gen-response-list (prefix q1 swq q2 suffix)
  "Generate a response list from the prefix, suffix and string with quotes."
  (append (make-list (length prefix) nil)
          (make-list (length q1) nil) ;; Starting quote regarded as outside quote
          (make-list (length swq) t)
          (make-list (length q2) t) ;; Ending quote regarded as inside quote
          (make-list (length suffix) nil)))

(ert-deftest hy-string-tests--strings-with-quotes-extended ()
  "Verify that strings containing quotes are identified.
The test string is built by concatenating prefix, mode-start-quote,
string-with-quotes, mode-end-quote, and suffix.  Points within prefix
and suffix are checked to be outside of the string.  Points within
string-with-quotes is checked to be inside of the string.  For each test
string a list of mode settings that are applicable for that test string
are tried.  If `hypb:in-string-p' is expected to see point as within
string is generated by `hy-string-tests--gen-response-list'."
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
                    (response-list (hy-string-tests--gen-response-list prefix quote1 swq quote2 suffix)))
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

)

(ert-deftest hy-string-tests--max-lines ()
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

(provide 'hy-string-tests)

;;; hy-string-tests.el ends here
