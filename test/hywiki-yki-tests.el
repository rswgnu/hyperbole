;;; hywiki-yki.tests --- Yank, kill and insert tests for hywiki  -*- lexical-binding: t; -*-
;;
;; Author:       Mats Lidell
;;
;; Orig-Date:    13-Jul-25 at 19:50:37
;; Last-Mod:     15-Jul-25 at 22:55:07 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2025  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; Development area for new hywiki-tests and their support functions.
;; Will eventually, when finished, be moved into "hywiki-test.el" or
;; maybe stay as here in a separate file!?

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'ert-x)
(require 'hy-test-helpers)
(require 'hywiki)

(require 'hywiki-tests) ;; For functions and variables in hywiki-tests

;; The testing idea here is based on functions in simple-test.el add
;; support for also marking highlighted words in buffers using the
;; same idea with point-tag and mark-tag also for start and end of
;; highlighted regions. Note that for setting the highlighted areas
;; hywiki-mode is used. The tag notation is not used for that. It is
;; only used for the verification.

(defconst hywiki-test--point-char "^")
(defconst hywiki-test--highlight-start-char "<")
(defconst hywiki-test--highlight-end-char ">")

(defun hywiki-test--insert-chars (positions)
  "Insert in string representation of current buffer start, end and point chars.
POSITIONS is a list of cons cells (START . END) with beginning of string
at index 1.  For every highlighted word in the buffer a
`hywiki-test--highlight-start-char' and
`hywiki-test--highlight-end-char' surrounding the highlighted word is
inserted.  Finally a `hywiki-test--point-char' is inserted where point is."
  (let* ((buffer-string (buffer-substring-no-properties (point-min) (point-max)))
         (current-point (point))
         ;; Convert 1-based positions to 0-based for string operations
         ;; Note: Emacs ranges are (start . end) where end is exclusive
         (zero-based-positions (mapcar (lambda (pos)
                                        (cons (1- (car pos)) (1- (cdr pos))))
                                      (or positions '())))
         ;; Sort ranges in reverse order for right-to-left processing
         (sorted-positions (sort zero-based-positions
                                (lambda (a b) (> (car a) (car b)))))
         (result buffer-string))
    
    ;; First, process all ranges from right to left
    (dolist (pos sorted-positions)
      (let* ((start (car pos))
             (end (cdr pos))
             (before (substring result 0 start))
             (middle (substring result start end))
             (after (substring result end)))
        
        (setq result (concat before hywiki-test--highlight-start-char middle hywiki-test--highlight-end-char after))))
    
    ;; Then insert point char adjusting for inserted chars.
    (let* ((point-pos (1- current-point))
           (tags-before-point 0))

      ;; Count characters and check if point is within ranges
      (dolist (pos zero-based-positions)
        (let ((range-start (car pos))
              (range-end (cdr pos)))
          (cond
           ((and (> point-pos range-start) (< point-pos range-end))
            (setq tags-before-point (+ tags-before-point (length hywiki-test--highlight-start-char))))
           ((<= range-end point-pos)
            (setq tags-before-point (+ tags-before-point
                                     (length hywiki-test--highlight-start-char)
                                     (length hywiki-test--highlight-end-char)))))))
      
      ;; Insert point char at adjusted position
      (let* ((adjusted-point (+ point-pos tags-before-point))
             (before (substring result 0 adjusted-point))
             (after (substring result adjusted-point)))
        (setq result (concat before hywiki-test--point-char after))))
    
    result))

(ert-deftest hywiki-test--insert--test ()
  "Verify `hywiki-test--insert-chars'."
  (with-temp-buffer
    (insert "Hello World\n")
    (goto-char 6)
    (should (string= (hywiki-test--insert-chars nil)
                     "Hello^ World\n"))
    (should (string= (hywiki-test--insert-chars '((1 . 6) (7 . 12)))
                     "<Hello>^ <World>\n"))
    (goto-char 5)
    (should (string= (hywiki-test--insert-chars '((1 . 6) (7 . 12)))
                     "<Hell^o> <World>\n"))))

(defun hywiki-test--set-buffer-text-with-point-and-highlight (description)
  "Set the current buffer's text, point and mark according to DESCRIPTION.

Erase current buffer and insert DESCRIPTION.  Set point to the first
occurrence of `hywiki-test--point-char' in the buffer, removing it.  If
there is no `hywiki-test--point-char', set point to the beginning of the
buffer.

End the insertion of text by turning on hywiki-mode and perform a dummy
command to get the pre- and post-hooks executed.  This creates the
highlighting overlays we want to test."
  (erase-buffer)
  (hywiki-mode 0) ; Deactivate hywiki-mode, disable highlighting
  (insert description)
  (goto-char (point-min))
  (when (search-forward hywiki-test--point-char nil t)
    (delete-char (- (length hywiki-test--point-char))))
  (hywiki-mode 1) ; Activate hywiki-mode activates highlighting
  (save-excursion ; Force pre- and post-hooks.
    (goto-char (point-max))
    (hywiki-tests--command-execute #'self-insert-command 1 ? )
    (hywiki-tests--command-execute #'delete-char -1)))

(defun hywiki-test--get-buffer-text-with-point-and-highlight ()
  "An inverse of `hywiki-test--set-buffer-text-with-point-and-highlight'.
Inserts tags for highlighted areas as well as point."
  (hywiki-test--insert-chars (hywiki-get-reference-positions)))

(defun hywiki-test--insert (string)
  "Command to insert a STRING at point."
  (interactive "s: ")
  (dolist (c (string-to-list string))
    (hywiki-tests--command-execute #'self-insert-command 1 c)))

(ert-deftest hywiki--verify-get-buffer-text-with-point-and-highlight ()
  "Verify proper highlighting after different editing actions.
Actions can be insertion, killing and deletion.

Each test is constructed as three phases:

* First phase empties the buffer from any previous test and then
  prepares the text and sets the point.  Hywiki-mode is activated in the
  prepare phase in order to set any initial
  highlighting.  (`hywiki-test--set-buffer-text-with-point-and-highlight')

* The second phase performs some action.  It can be insertion, killing
  or deletion.  The action should call the pre- and post-command-hooks
  in order for the highlighting overlays to be constructed.

* The third phase does a verification.  A representation of the
  `buffer-string' as a string is constructed where chars are used for
  point, and start and stop of the highlighted WikiWord.
  (`hywiki-test--get-buffer-text-with-point-and-highlight').  That is
  then compared to the expected string."
  (skip-unless (not noninteractive))    ; Only works in interactive mode for now
  (hywiki-tests--preserve-hywiki-mode
   (let* ((hywiki-directory (make-temp-file "hywiki" t))
          (wikiHi (cdr (hywiki-add-page "Hi")))
          (wikiHo (cdr (hywiki-add-page "Ho")))
          (wikiWord (cdr (hywiki-add-page "WikiWord")))
          (hywiki-tests--with-face-test t))
     (unwind-protect
         (with-temp-buffer
           (ert-info ("1" :prefix "Verify point, no highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "hej^hopp")
             (should (string= "hej^hopp"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))
           (ert-info ("2" :prefix "Verify point, no highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "hej^hopp")
             (forward-char 1)
             (should (string= "hejh^opp"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))
           (ert-info ("3" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "^Hi")
             (should (string= "^<Hi>"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))
           (ert-info ("4" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "Hi^Ho")
             (hywiki-test--insert "\"text\"")
             (should (string= "Hi\"text\"^<Ho>"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))
           (ert-info ("5" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "Hi^Ho")
             (hywiki-test--insert " \"text\"")
             (should (string= "<Hi> \"text\"^<Ho>"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; PASS: Wiki<delete-region>Word -> highlight {WikiWord} after delete
           (ert-info ("6" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "Wiki^delete-regionWord")
             (hywiki-tests--command-execute #'delete-region (point)
                                            (+ (point) (length "delete-region")))
             (should (string= "<Wiki^Word>"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; PASS: Wiki#sec<tion>Word -> no highlight after adding "tion"
           (ert-info ("7" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "Wiki#sec^tionWord")
             (hywiki-tests--command-execute #'delete-region (point)
                                            (+ (point) (length "tion")))
             (should (string= "Wiki#sec^Word"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; PASS: Wiki<#section>Word -> highlight {WikiWord} after delete of "#section"
           (ert-info ("8" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "Wiki^#sectionWord")
             (hywiki-tests--command-execute #'delete-region (point)
                                            (+ (point) (length "#section")))
             (should (string= "<Wiki^Word>"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))
           
           ;; PASS: WikiWord -> dehighlight "WikiWo<kill-word>rd"
           (ert-info ("8" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "WikiWo^kill-wordrd")
             (hywiki-tests--command-execute #'delete-region (point)
                                            (+ (point) (length "kill-word")))
             (should (string= "<WikiWo^rd>"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; PASS: "WikiWord#section with spaces" -> shrink highlight
           ;;        to {WikiWord#section} with this operation:
           ;;        <delete-char>"WikiWord#section with spaces"
           (ert-info ("9" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "^\"WikiWord#section with spaces\"")
             (hywiki-tests--command-execute #'delete-char 1)
             (should (string= "^<WikiWord#section> with spaces\""
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; PASS: "WikiWord#section"<delete-char-backwards> -> no
           ;; highlight change "{WikiWord#section}
           (ert-info ("10" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "\"WikiWord#section\"^")
             (hywiki-tests--command-execute #'backward-delete-char-untabify 1)
             (should (string= "\"<WikiWord#section>^"
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; FAIL: "WikiWord#section with
           ;; spaces"<delete-char-backwards> -> shrink highlight to
           ;; "{WikiWord#section} with spaces
           (ert-info ("11" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "\"WikiWord#section with spaces\"^")
             (hywiki-tests--command-execute #'backward-delete-char-untabify 1)
             (should (string= "\"<WikiWord#section with spaces>^" ;;; <= FAIL: Not correct highlight
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; FAIL: WikiWord <kill-word> abc WikiWord -> improperly
           ;; dehighlights *SECOND* WikiWord
           (ert-info ("12" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "WikiWord ^<kill-word> abc WikiWord")
             (hywiki-tests--command-execute #'delete-region (point)
                                            (+ (point) (length "<kill-word> abc "))) ; Delete trailing space.
             (should (string= "<WikiWord> ^WikiWord" ;;; <= FAIL: Not correct highlight
                              (hywiki-test--get-buffer-text-with-point-and-highlight))))

           ;; PASS: WikiWord <kill-word> abc WikiWord -> *BOTH
           ;; WikiWord's are highlighted
           (ert-info ("13" :prefix "Verify highlighting: ")
             (hywiki-test--set-buffer-text-with-point-and-highlight "WikiWord ^<kill-word> abc WikiWord")
             (hywiki-tests--command-execute #'delete-region (point)
                                            (+ (point) (length "<kill-word> abc")))
             (should (string= "<WikiWord> ^ <WikiWord>"
                              (hywiki-test--get-buffer-text-with-point-and-highlight)))))
       (hy-delete-files-and-buffers (list wikiHi wikiHo wikiWord))
       (hy-delete-dir-and-buffer hywiki-directory)))))

(ert-deftest hywiki--verify-get-buffer-text-with-point-and-highlight-compact ()
  "Example test with more compact notation using `cl-flet'.
Can be expanded with alternatives for insert, delete and yank instead if
exec which does not cover all cases."
  (skip-unless (not noninteractive))    ; Only works in interactive mode for now
  (hywiki-tests--preserve-hywiki-mode
    (let* ((hywiki-directory (make-temp-file "hywiki" t))
           (wikiHi (cdr (hywiki-add-page "Hi")))
           (wikiHo (cdr (hywiki-add-page "Ho")))
           (wikiWord (cdr (hywiki-add-page "WikiWord")))
           (hywiki-tests--with-face-test t))
      (cl-flet ((pre: (start)
                  (hywiki-test--set-buffer-text-with-point-and-highlight start))
                (exec: (cmd &rest args)
                  (apply #'hywiki-tests--command-execute cmd args))
                (post: (stop)
                  (should (string= stop (hywiki-test--get-buffer-text-with-point-and-highlight)))))
        (unwind-protect
            (ert-info ("1" :prefix "Verify highlighting: ")
              (pre: "WikiWord ^<kill-word> abc WikiWord")
              (exec: #'delete-region (point) (+ (point) (length "<kill-word> abc")))
              (post: "<WikiWord> ^ <WikiWord>"))
          (hy-delete-files-and-buffers (list wikiHi wikiHo wikiWord))
          (hy-delete-dir-and-buffer hywiki-directory))))))

(provide 'hywiki-yki-tests)

;;; hywiki-yki-tests.el ends here
