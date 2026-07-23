;;; hy-string-tests.el --- test whether point is inside or outside of a string    -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    22-Jul-26 at 23:41:29
;; Last-Mod:     22-Jul-26 at 23:49:34 by Bob Weiner
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


(provide 'hy-string-tests)

;;; hy-string-tests.el ends here
