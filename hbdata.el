;;; hbdata.el --- GNU Hyperbole button attribute accessor functions  -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     2-Apr-91
;; Last-Mod:      7-Oct-22 at 23:17:52 by Mats Lidell
;;
;; Copyright (C) 1991-2021  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This module handles Hyperbole button data/attribute storage.  In
;;  general, it should not be extended by anyone other than Hyperbole
;;  maintainers.  If you alter the formats or accessors herein, you are
;;  likely to make your buttons incompatible with future releases.
;;  System developers should instead work with and extend the "hbut.el"
;;  module which provides much of the Hyperbole application programming
;;  interface and which hides the low level details handled by this
;;  module.
;;
;;
;;  Button data is typically stored within a file that holds the button
;;  data for all files within that directory.  The name of this file is
;;  given by the variable 'hattr:filename,' usually it is ".hypb".
;;
;;  Here is a sample from a Hyperbole V2 button data file.  Each button
;;  data entry is a list of fields:
;;
;;    
;;    "TO-DO"
;;    (Key            Placeholders  LinkType      <arg-list>             creator and modifier with times)
;;    ("alt.mouse.el" nil nil       link-to-file  ("./ell/alt-mouse.el") "zzz@gnu.org" "19991027:09:19:26" "zzz@gnu.org" "19991027:09:31:36")
;;
;;  which means:  button \<(alt.mouse.el)> found in file "TO-DO" in the current
;;  directory provides a link to the local file "./ell/alt-mouse.el".  It was
;;  created and last modified by zzz@gnu.org.
;;
;;  All link entries that originate from the same source file are stored
;;  contiguously, one per line, in reverse order of creation.
;;  Preceding all such entries is the source name (in the case of a file
;;  used as a source, no directory information is included, since only
;;  sources within the same directory as the button data file are used as
;;  source files within it.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hversion) ;; For hyperb:microsoft-os-p
(require 'hbmap)
(require 'hgnus)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; ------------------------------------------------------------------------
;;; Button data accessor functions
;;; ------------------------------------------------------------------------
(defun hbdata:action (hbdata)
  "[Hyp V2] Return action overriding button's action type or nil."
  (nth 1 hbdata))

(defun hbdata:actype (hbdata)
  "Return the action type in HBDATA as a string."
  (let ((nm (symbol-name (nth 3 hbdata)))
	actype-sym)
    (and nm (if (or (= (length nm) 2) (string-match "::" nm))
		nm
	      ;; RSW 2020-11-01 - Updated to handle programmatically created
	      ;; explicit buttions whose action types may be non-actypes,
	      ;; regular Elisp functions.
	      (setq actype-sym (intern-soft (concat "actypes::" nm)))
              (if (and actype-sym (fboundp actype-sym))
		  (symbol-name actype-sym)
		nm)))))

(defun hbdata:args (hbdata)
  "Return the list of any arguments given in HBDATA."
  (nth 4 hbdata))

(defun hbdata:categ (_hbdata)
  "Return the category of HBDATA's button."
  'explicit)

(defun hbdata:creator (hbdata)
  "Return the user-id of the original creator of HBDATA's button."
  (nth 5 hbdata))

(defun hbdata:create-time (hbdata)
  "Return the original creation time given for HBDATA's button."
  (nth 6 hbdata))

(defun hbdata:key (hbdata)
  "Return the indexing key in HBDATA as a string."
  (car hbdata))

(defun hbdata:loc-p (hbdata)
  "[Hyp V1] Return 'L iff HBDATA referent is within a local file system.
Return 'R if remote and nil if irrelevant for button action type."
  (nth 1 hbdata))

(defun hbdata:modifier (hbdata)
  "Return the user-id of the most recent modifier of HBDATA's button.
Nil is returned when button has not been modified."
  (nth 7 hbdata))

(defun hbdata:mod-time (hbdata)
  "Return the time of the most recent change to HBDATA's button.
Nil is returned when button has not beened modified."
  (nth 8 hbdata))

(defun hbdata:referent (hbdata)
  "Return the referent name in HBDATA."
  (nth 2 hbdata))

(defun hbdata:search (buf label partial)
  "Go to Hyperbole hbdata BUF and find LABEL whole or PARTIAL match.
Search is case-insensitive.  Return list with elements:
\(<button-src> <label-key1> ... <label-keyN>)."
  (set-buffer buf)
  (let ((case-fold-search t) (src-matches) (src) (matches) (end))
    (goto-char (point-min))
    (while (re-search-forward "^\^L\n\"\\([^\"]+\\)\"" nil t)
      (setq src (match-string 1)
	    matches nil)
      (save-excursion
	(setq end (if (re-search-forward "^\^L" nil t)
		      (1- (point)) (point-max))))
      (while (re-search-forward
	      (concat "^(\"\\(" (if partial "[^\"]*")
		      (regexp-quote (ebut:label-to-key label))
		      (if partial "[^\"]*") "\\)\"") nil t)
	(setq matches (cons (match-string 1) matches)))
      (if matches
	  (setq src-matches (cons (cons src matches) src-matches)))
      (goto-char end))
    src-matches))

;;; ------------------------------------------------------------------------
;;; Button data operators
;;; ------------------------------------------------------------------------

(defun hbdata:build (&optional mod-lbl-key but-sym)
  "Construct button data from optional MOD-LBL-KEY and BUT-SYM.
Modify BUT-SYM attributes.  MOD-LBL-KEY nil means create a new
entry, otherwise modify existing one.  Nil BUT-SYM means use
`hbut:current'  If successful, return a cons of
 (button-data . button-instance-str), else nil."
  (let* ((b (hattr:copy (or but-sym 'hbut:current) 'but))
	 (l (hattr:get b 'loc))
	 (key (or mod-lbl-key (hattr:get b 'lbl-key)))
	 (new-key (if mod-lbl-key (hattr:get b 'lbl-key) key))
	 (lbl-instance) (creator) (create-time) (modifier) (mod-time)
	 (entry) loc dir)
    (when l
      (setq loc (if (bufferp l) l (file-name-nondirectory l))
	    dir (if (bufferp l) nil (file-name-directory l)))
      (when (setq entry (hbdata:to-entry key loc dir (not mod-lbl-key)))
	(if mod-lbl-key
	    (progn
	      (setq creator     (hbdata:creator entry)
		    create-time (hbdata:create-time entry)
		    modifier    (let* ((user (hypb:user-name))
				       (addr hyperb:user-email))
				  (if (equal creator addr)
				      user addr))
		    mod-time    (htz:date-sortable-gmt)
		    entry       (cons new-key (cdr entry)))
	      (hbdata:delete-entry-at-point)
	      (when (setq lbl-instance (hbdata:instance-last new-key loc dir))
		(setq lbl-instance (concat ebut:instance-sep
					   (int-to-string (1+ lbl-instance))))
		;; This expression is needed to ensure that the highest
		;; numbered instance of a label appears before
		;; other instances, so 'hbdata:instance-last' will work.
		(when (hbdata:to-entry-buf loc dir)
		  (forward-line 1))))
	  (let ((inst-num (hbdata:instance-last new-key loc dir)))
	    (setq lbl-instance (if inst-num
				   (hbdata:instance-next
				    (concat new-key ebut:instance-sep
					    (int-to-string inst-num))))))))
      (when (or entry (not mod-lbl-key))
	(hattr:set b 'lbl-key (concat new-key lbl-instance))
	(hattr:set b 'loc loc)
	(hattr:set b 'dir dir)
	(let* ((actype)
	       (hbdata (list (hattr:get b 'lbl-key)
			     (hattr:get b 'action)
			     ;; Hyperbole V1 referent compatibility, always nil in V2
			     (hattr:get b 'referent)
			     ;; Save actype without class prefix.
			     (and (setq actype (hattr:get b 'actype))
				  (symbolp actype)
				  (setq actype (symbol-name actype))
				  (intern
				   (substring actype (if (string-match "::" actype)
							 (match-end 0) 0))))
			     (let ((mail-dir (and (fboundp 'hmail:composing-dir)
						  (hmail:composing-dir l)))
				   (args (hattr:get b 'args)))
			       ;; Replace matches for variable values with their variable names in any pathname args.
			       (hattr:set b 'args
					  (mapcar #'hpath:substitute-var
						  (if mail-dir
						      ;; Make pathname args absolute for outgoing mail and news messages.
						      (hpath:absolute-arguments actype args mail-dir)
						    args))))
			     (hattr:set b 'creator (or creator hyperb:user-email))
			     (hattr:set b 'create-time (or create-time (htz:date-sortable-gmt)))
			     (hattr:set b 'modifier modifier)
			     (hattr:set b 'mod-time mod-time))))
	  ;; Ensure modified attributes are saved to `but-sym' or hbut:current.
	  (hattr:copy b (or but-sym 'hbut:current))
	  (cons hbdata lbl-instance))))))

(defun hbdata:get-entry (lbl-key key-src &optional directory)
  "Return button data entry given by LBL-KEY, KEY-SRC and optional DIRECTORY.
Return nil if no matching entry is found.
A button data entry is a list of attribute values.  Use methods from
class `hbdata' to operate on the entry."
  (hbdata:apply-entry
   (lambda () (read (current-buffer)))
   lbl-key key-src directory))

(defun hbdata:instance-next (lbl-key)
  "Return string for button instance number following LBL-KEY's.
Nil if LBL-KEY is nil."
  (and lbl-key
       (if (string-match
	    (concat (regexp-quote ebut:instance-sep) "[0-9]+$") lbl-key)
	   (concat ebut:instance-sep
		   (int-to-string
		    (1+ (string-to-number
			 (substring lbl-key (1+ (match-beginning 0)))))))
	 ":2")))

(defun hbdata:instance-last (lbl-key key-src &optional directory)
  "Return highest instance number for repeated button label.
1 if not repeated, nil if no instance.
Utilize arguments LBL-KEY, KEY-SRC and optional DIRECTORY."
  (hbdata:apply-entry
   (lambda ()
     (if (looking-at "[0-9]+")
	 (string-to-number (match-string 0))
       1))
   lbl-key key-src directory nil 'instance))

(defun hbdata:delete-entry (lbl-key key-src &optional directory)
  "Delete button data entry given by LBL-KEY, KEY-SRC and optional DIRECTORY.
Return entry deleted (a list of attribute values) or nil.
Use methods from class `hbdata' to operate on the entry.
If the hbdata buffer is blank/empty, kill it and remove the associated file."
  (hbdata:apply-entry
   (lambda ()
     (prog1 (read (current-buffer))
       (let ((empty-file-entry "[ \t\n\r]*\\(\^L\\|\\'\\)")
	     (kill))
	 (beginning-of-line)
	 (hbdata:delete-entry-at-point)
	 (when (looking-at empty-file-entry)
	   (let ((end (point))
		 (empty-hbdata-file "[ \t\n\r]*\\'"))
	     (forward-line -1)
	     (when (eq (following-char) ?\")
		 ;; Last button entry for filename, so del filename.
		 (forward-line -1)
		 (delete-region (point) end))
	     (save-excursion
	       (goto-char (point-min))
	       (when (looking-at empty-hbdata-file)
		 (setq kill t)))
	     (when kill
	       (let ((fname buffer-file-name))
		 (erase-buffer) (save-buffer) (kill-buffer nil)
		 (hbmap:dir-remove (file-name-directory fname))
		 (delete-file fname))))))))
   lbl-key key-src directory))

(defun hbdata:delete-entry-at-point ()
  (delete-region (point) (progn (forward-line 1) (point))))

(defun hbdata:to-entry (but-key key-src &optional directory instance)
  "Return button data entry indexed by BUT-KEY, KEY-SRC, optional DIRECTORY.
Return nil if entry is not found.  Leave point at start of entry when
successful or where entry should be inserted if unsuccessful.
A button entry is a list.  Use methods from class `hbdata' to operate on the
entry.  Optional INSTANCE non-nil means search for any button instance matching
but-key."
  (let ((pos-entry-cons
	 (hbdata:apply-entry
	  (lambda ()
	    (beginning-of-line)
	    (cons (point) (read (current-buffer))))
	  but-key key-src directory 'create instance)))
    (hbdata:to-entry-buf key-src directory)
    (forward-line 1)
    (when pos-entry-cons
      (goto-char (car pos-entry-cons))
      (cdr pos-entry-cons))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hbdata:apply-entry (func lbl-key key-src &optional directory
			   create-flag instance-flag)
  "Invoke FUNC with point at hbdata entry.
Hbdata is given by LBL-KEY, KEY-SRC and optional DIRECTORY.
With optional CREATE-FLAG, if no such line exists, insert a new file entry at
the beginning of the hbdata file (which is created if necessary).
INSTANCE-FLAG non-nil means search for any button instance matching LBL-KEY and
call FUNC with point right after any `ebut:instance-sep' in match.
Return value of evaluation when a matching entry is found or nil."
  (let (found
	rtn
	opoint
	end-func)
    (save-excursion
      (save-restriction
	(unwind-protect
	    (progn
	      (when (get-buffer key-src)
		(set-buffer key-src)
		(unless buffer-file-name
		  (cond ((hmail:editor-p)
			 (setq end-func (lambda ()
					  (hmail:msg-narrow))))
			((and (hmail:lister-p)
			      (progn (rmail:summ-msg-to) (rmail:to)))
			 (setq opoint (point)
			       key-src (current-buffer)
			       end-func (lambda ()
					  (hmail:msg-narrow)
					  (goto-char opoint)
					  (lmail:to))))
			((and (hnews:lister-p)
			      (progn (rnews:summ-msg-to) (rnews:to)))
			 (setq opoint (point)
			       key-src (current-buffer)
			       end-func (lambda ()
					  (hmail:msg-narrow)
					  (goto-char opoint)
					  (lnews:to))))
			;; Any non-file buffer
			(t
			 (setq opoint (point)
			       key-src (current-buffer)
			       end-func (lambda ()
					  (widen)
					  (goto-char opoint)
					  (narrow-to-region (point-min)
							    (hmail:hbdata-start))))))))
	      (setq found (hbdata:to-entry-buf key-src directory create-flag)))
	  (when found
	    (let ((case-fold-search t)
		  (qkey (regexp-quote lbl-key))
		  (end (save-excursion (if (search-forward "\n\^L" nil t)
					   (point) (point-max)))))
	      (if (if instance-flag
		      (re-search-forward
		       (concat "\n(\"" qkey "["
			       ebut:instance-sep "\"]") end t)
		    (search-forward (concat "\n(\"" lbl-key "\"") end t))
		  (progn
		    (unless instance-flag
		      (beginning-of-line))
		    (let (buffer-read-only)
		      (setq rtn (funcall func)))))))
	  (when end-func (funcall end-func)))))
    rtn))

(defun hbdata:to-hbdata-buffer (dir &optional create)
  "Read in the file containing DIR's button data, if any, and return buffer.
If it does not exist and optional CREATE is non-nil, create a new
one and return buffer, otherwise return nil."
  (let* ((file (expand-file-name hattr:filename (or dir default-directory)))
	 (existing-file (or (file-exists-p file) (get-file-buffer file)))
	 (buf (or (get-file-buffer file)
		  (and (or create existing-file)
		       (find-file-noselect file)))))
    (when buf
      (set-buffer buf)
      (unless (verify-visited-file-modtime (get-file-buffer file))
	(cond ((yes-or-no-p
		"Hyperbole button data file has changed, read new contents? ")
	       (revert-buffer t t))))
      (or (= (point-max) 1) (eq (char-after 1) ?\^L)
	  (error "File %s is not a valid Hyperbole button data table" file))
      (unless (equal (buffer-name) file)
	(rename-buffer file))
      (setq buffer-read-only nil)
      (unless existing-file
	(hbmap:dir-add (file-name-directory file)))
      buf)))


(defun hbdata:to-entry-buf (key-src &optional directory create)
  "Move point to end of line in but data buffer matching KEY-SRC.
Use hbdata file in KEY-SRC's directory, or optional DIRECTORY or if nil, use
`default-directory'.
With optional CREATE, if no such line exists, insert a new file entry at the
beginning of the hbdata file (which is created if necessary).
Return non-nil if KEY-SRC is found or created, else nil."
  (let ((rtn) (ln-dir))
    (if (and (get-buffer key-src)
	     (setq rtn (set-buffer key-src))
	     (not buffer-file-name))
	;; Button buffer has no file attached
	(progn (setq buffer-read-only nil)
	       (unless (hmail:hbdata-to-p)
		 (insert "\n" hmail:hbdata-sep "\n"))
	       (backward-char 1))
      (setq directory (or (file-name-directory key-src) directory))
      (let ((ln-file) (link-p key-src))
	(while (setq link-p (file-symlink-p link-p))
	  (setq ln-file link-p))
	(if ln-file
	    (setq ln-dir (file-name-directory ln-file)
		  key-src (file-name-nondirectory ln-file))
	  (setq key-src (file-name-nondirectory key-src))))
      (if (or (hbdata:to-hbdata-buffer directory create)
	      (and ln-dir (hbdata:to-hbdata-buffer ln-dir nil)
		   (setq create nil
			 directory ln-dir)))
	  (progn
	    (goto-char 1)
	    (cond ((search-forward (concat "\^L\n\"" key-src "\"")
				   nil t)
		   (setq rtn t))
		  (create
		   (setq rtn t)
		   (insert "\^L\n\"" key-src "\"\n")
		   (backward-char 1))))))
    rtn))

(defun hbdata:write (&optional orig-lbl-key but-sym)
  "Try to write Hyperbole button data from optional ORIG-LBL-KEY and BUT-SYM.
ORIG-LBL-KEY nil means create a new entry, otherwise modify existing one.
BUT-SYM nil means use `hbut:current'.  If successful, return
a button instance string to append to button label or t when first instance.
On failure, return nil."
  (let ((cons (hbdata:build orig-lbl-key but-sym))
	entry lbl-instance)
    (unless (or (and buffer-file-name (not (file-writable-p buffer-file-name)))
		(null cons))
      (setq entry (car cons) lbl-instance (cdr cons))
      (prin1 entry (current-buffer))
      (terpri (current-buffer))
      (or lbl-instance t))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hbdata)

;;; hbdata.el ends here
