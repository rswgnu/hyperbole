;;; hypb-tests.el --- test for hypb.el         -*- lexical-binding: t; -*-

;; Author: Mats Lidell <matsl@gnu.org>
;;
;; Orig-Date: 5-Apr-21 at 18:53:10
;;
;; Copyright (C) 2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;; Tests for "../hypb.el"

;;; Code:

(require 'hypb)
(require 'hbut)
(require 'ert)

;; Test for replace-regexp-in-string copied from emacs src
(ert-deftest hypb:replace-match-string-test ()
  (should (equal (hypb:replace-match-string "a+" "abaabbabaaba" "xy")
                 "xybxybbxybxybxy"))
  ;; FIXEDCASE
  (let ((case-fold-search t))
    (should (equal (hypb:replace-match-string "a+" "ABAABBABAABA" "xy")
                   "XYBXYBBXYBXYBXY"))
    (should (equal (hypb:replace-match-string "a+" "ABAABBABAABA" "xy" nil t)
                   "xyBxyBBxyBxyBxy"))
    (should (equal (hypb:replace-match-string
                    "a[bc]*" "a A ab AB Ab aB abc ABC Abc AbC aBc" "xyz")
                   "xyz XYZ xyz XYZ Xyz xyz xyz XYZ Xyz Xyz xyz"))
    (should (equal (hypb:replace-match-string
                    "a[bc]*" "a A ab AB Ab aB abc ABC Abc AbC aBc" "xyz" nil t)
                   "xyz xyz xyz xyz xyz xyz xyz xyz xyz xyz xyz")))
  (let ((case-fold-search nil))
    (should (equal (hypb:replace-match-string "a+" "ABAABBABAABA" "xy")
                   "ABAABBABAABA")))
  ;; group substitution
  (should (equal (hypb:replace-match-string
                  "a\\(b*\\)" "babbcaabacbab" "<\\1,\\&>")
                 "b<bb,abb>c<,a><b,ab><,a>cb<b,ab>"))
  (should (equal (hypb:replace-match-string
                  "x\\(?2:..\\)\\(?1:..\\)\\(..\\)\\(..\\)\\(..\\)"
                  "yxabcdefghijkl" "<\\3,\\5,\\4,\\1,\\2>")
                 "y<ef,ij,gh,cd,ab>kl"))
  ;; LITERAL
  (should (equal (hypb:replace-match-string
                  "a\\(b*\\)" "babbcaabacbab" "<\\1,\\&>" t nil)
                 "b<\\1,\\&>c<\\1,\\&><\\1,\\&><\\1,\\&>cb<\\1,\\&>"))
  (should (equal (hypb:replace-match-string
                  "a" "aba" "\\\\,\\?")
                 "\\,\\?b\\,\\?"))
  (should (equal (hypb:replace-match-string
                  "a" "aba" "\\\\,\\?" t nil)
                 "\\\\,\\?b\\\\,\\?"))
  ;; SUBEXP
  ; Available in subr-replace-regexp-in-string. Not supported here
  ; (should (equal (hypb:replace-match-string
  ;                 "\\(a\\)\\(b*\\)c" "xy" "babbcdacd" nil nil 2)
  ;                "baxycdaxycd"))
  ;; START
  ; Available in subr-replace-regexp-in-string. Not supported here
  ; (should (equal (hypb:replace-match-string
  ;                 "ab" "x" "abcabdabeabf" nil nil nil 4)
  ;                "bdxexf"))
  ;; An empty pattern matches once before every character.
  (should (equal (hypb:replace-match-string "" "abc" "x")
                 "xaxbxc"))
  (should (equal (hypb:replace-match-string "y*" "abc" "x")
                 "xaxbxc"))
  ;; replacement function
  (should (equal (hypb:replace-match-string
                  "a\\(b*\\)c"
                  "babbcaacabc"
                  (lambda (s)
                    (format "<%s,%s,%s,%s,%s>"
                            s
                            (match-beginning 0) (match-end 0)
                            (match-beginning 1) (match-end 1))))
                 "b<abbc,0,4,1,3>a<ac,0,2,1,1><abc,0,3,1,2>")))

(ert-deftest hypb:replace-match-string-after-27.1-test ()
  ;; anchors (bug#15107, bug#44861)
  (when (version< "27.1" emacs-version)
    (should (equal (hypb:replace-match-string "a\\B" "a aaaa" "b")
                   "a bbba"))
    (should (equal (hypb:replace-match-string "\\`\\|x" "--xx--" "z")
                   "z--zz--"))))

(provide 'hypb-tests)
;;; hypb-tests.el ends here
