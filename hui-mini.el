;;; hui-mini.el --- Single line command menus for GNU Hyperbole   -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Oct-91 at 20:13:17
;; Last-Mod:     11-Sep-24 at 08:51:59 by Mats Lidell
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1991-2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hypb)
(require 'hsettings)                    ; For hyperbole-web-search-alist
(require 'browse-url)

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar hargs:reading-type)             ; "hargs.el"
(defvar hbmap:dir-user)                 ; "hbmap.el"
(defvar hbmap:filename)                 ; "hbmap.el"
(defvar hui:menu-highlight-flag)        ; "hui-mini.el"
(defvar hui:menu-hywiki nil)            ; "hui-mini.el"
(defvar hui:menu-mode-map)              ; "hui-mini.el"
(defvar hui:menu-rolo nil)              ; "hui-mini.el"
(defvar hyperbole-mode-map)             ; "hyperbole.el"
(defvar hyrolo-add-hook)                ; "hyrolo.el"
(defvar hyrolo-edit-hook)               ; "hyrolo.el"
(defvar hyrolo-file-list)               ; "hyrolo.el"
(defvar org-mode-map)                   ; "org.el"

(declare-function hpath:find "hpath")
(declare-function hmouse-update-smart-keys "hmouse-key")
(declare-function hargs:at-p "hargs")
(declare-function kbd-key:hyperbole-mini-menu-key-p "hib-kbd")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hui:menu-exit-hyperbole  "X"
  "*Upper case character string which exits from/disables Hyperbole mode.
Also exits any active minibuffer menu.")
(defvar hui:menu-select          "\C-m"
  "*Character string which selects the Hyperbole menu item at point.")
(defvar hui:menu-quit            "Q"
  "*Upper case character string which quits selecting from a Hyperbole menu item.")
(defvar hui:menu-abort           "\C-g"
  "*Beeps and aborts from any Hyperbole menu.")
(defvar hui:menu-top             "\C-t"
  "*Character string which returns to top Hyperbole menu.")

(defvar hui:menu-keys            ""
  "String of keys pressed for current or last Hyperbole command.
This excludes the prefix used to invoke the Hyperbole menu, except
when on the top-level menu.")

(defvar hui:menu-p nil
  "Non-nil iff the Hyperbole minibuffer menu is active.")

(defvar hui:menus nil
  "Hyperbole minibuffer command menus.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; Used as the autoloaded main entry point to Hyperbole.  The "hyperbole"
;;; file is loaded when this is invoked.
;;; This command brings up a series of minibuffer menus of Hyperbole commands.
;;;###autoload
(defun hyperbole (&optional menu menu-list doc-flag help-string-flag)
  "Invoke the Hyperbole minibuffer menu when not already active.
\\[hyperbole] runs this.  Non-interactively, return t if a menu is
displayed by this call, else nil (e.g. when already in a Hyperbole
mini-menu).

Two optional arguments may be given to invoke alternative menus.
MENU (a symbol) specifies the menu to invoke from MENU-LIST, (a
Hyperbole menu list structure).  MENU defaults to \\='hyperbole and MENU-LIST
to `hui:menus'.  See `hui:menus' definition for the format of the menu list
structure.

Two additional optional arguments may be given when documentation for
a menu item should be shown rather than display of a menu.  DOC-FLAG
non-nil means show documentation for any item that is selected by the
user.  HELP-STRING-FLAG non-nil means show only the first line of the
documentation, not the full text."

  (interactive (list nil nil nil nil))
  (if (and hui:menu-p (> (minibuffer-depth) 0))
      (progn (beep) nil)
    (unwind-protect
	(progn
	  (hyperbole-mode 1)
	  (hyperb:init-menubar)
	  (setq hui:menu-p t)
	  (hui:menu-act (or menu 'hyperbole) menu-list doc-flag help-string-flag)
	  t)
      (setq hui:menu-p nil))))

;;;###autoload
(defun hyperbole-demo (&optional arg)
  "Display the Hyperbole FAST-DEMO.
With a prefix ARG, display the older, more extensive \"DEMO\" file."
  (interactive "P")
  (hypb:display-file-with-logo (if arg "DEMO" "FAST-DEMO")))

;;;###autoload
(defun hyperbole-set-key (keymap key binding)
  "In KEYMAP, bind KEY to Hyperbole minibuffer BINDING.
If KEYMAP is nil, use the value of (global-key-map).

KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

BINDING is one of:
  nil     - immediately remove key binding from keymap
  string  - upon key press, execute the BINDING string as a key series
  command - upon key press, run the command interactively.

Note that other local or minor mode bindings may shadow/override any
binding made with this function."
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set Hyperbole key globally: ")))
     (setq hui:menu-keys "")
     (list nil
	   key
	   ;; Read Hyperbole minibuffer menu keys to bind to 'key' in 'keymap'
	   (concat
	    ;; Normalize the key prefix that invokes the Hyperbole minibuffer menu
	    (kbd (key-description (car (where-is-internal 'hyperbole))))
	    (hui:get-keys)))))
  (when (and keymap (not (keymapp keymap)))
    (error "(hyperbole-set-key): First arg must be either nil or a keymap, not '%s'" keymap))
  (unless keymap
    (setq keymap (current-global-map)))
  (or (vectorp key) (stringp key)
      (error "(hyperbole-set-key): Second arg must be a vector or string key sequence, not '%s'" key))
  (prog1 (cond ((stringp binding)
		(if (string-empty-p binding)
		    (error "(hyperbole-set-key): Third arg must be a non-empty string")
		  (define-key keymap key `(lambda () (interactive) (kbd-key:act ,binding)))))
	       ((or (null binding) (commandp binding))
		(define-key keymap key binding))
	       (t
		(error "(hyperbole-set-key): Invalid binding for {%s}: '%s'" key binding)))
    (when (called-interactively-p 'interactive)
      (message "{%s} set to invoke {%s}" (key-description key) binding))))

(defun hui:menu-hyperbole-prefix ()
  "Return prefix keys that invoke the Hyperbole minibuffer menu."
  (kbd (key-description (where-is-internal #'hyperbole (current-global-map) t))))

(defun hui:menu-act (menu &optional menu-list doc-flag help-string-flag)
  "Prompt user with Hyperbole MENU (a symbol) and perform selected item.
Optional second argument MENU-LIST is a Hyperbole menu list structure from
which to extract MENU.  It defaults to `hui:menus'.  See its definition for
the menu list structure.

Two additional optional arguments may be given when documentation for
a menu item should be shown rather than display of a menu.  DOC-FLAG
non-nil means show documentation for any item that is selected by the
user.  HELP-STRING-FLAG non-nil means show only the first line of the
documentation, not the full text."
  (setq hui:menu-keys (hui:menu-hyperbole-prefix))
  (let ((show-menu t)
	(rtn)
	menu-alist act-form)
    (while (and show-menu (or (and menu (symbolp menu)
			           (setq menu-alist
				         (cdr (assq menu (or menu-list hui:menus)))))
		              (hypb:error "(hui:menu-act): Invalid menu symbol arg: `%s'"
				          menu)))
      (cond ((and (consp (setq act-form (hui:menu-choose menu-alist doc-flag help-string-flag)))
		  (cdr act-form)
		  (symbolp (cdr act-form)))
	     ;; Display another menu
	     (setq menu (cdr act-form)))
	    (act-form
	     (let ((prefix-arg current-prefix-arg))
	       (cond ((symbolp act-form)
		      (unless (eq act-form t)
			(set--this-command-keys hui:menu-keys)
			(setq show-menu nil
			      this-command act-form)
			      rtn (call-interactively act-form)))
		     ((stringp act-form)
		      (if (or doc-flag help-string-flag)
			  (setq show-menu nil
				rtn act-form)
			(hui:menu-help act-form)
			;; Loop and show menu again.
			))
		     (t (set--this-command-keys hui:menu-keys)
			(setq show-menu nil
			      this-command act-form)
			      rtn (eval act-form)))))
	    (t (setq show-menu nil))))
    rtn))

(defun hui:get-keys ()
  "Invoke the Hyperbole minibuffer menu and return menu keys pressed.
Return nil when already in a Hyperbole minibuffer menu."
  (if (and hui:menu-p (> (minibuffer-depth) 0))
      (progn (beep) nil)
    (unwind-protect
	(progn
	  (hyperb:init-menubar)
	  (setq hui:menu-p t)
	  (hui:menu-get-keys 'hyperbole))
      (setq hui:menu-p nil))))

(defun hui:menu-get-keys (menu &optional menu-list)
  "Prompt with Hyperbole MENU symbol, select an item and return the keys pressed.
Optional second argument MENU-LIST is a Hyperbole menu list structure from
which to extract MENU.  It defaults to `hui:menus'.  See its definition for
the menu list structure."
  (setq hui:menu-keys "")
  (let ((show-menu t)
	menu-alist act-form)
    (while (and show-menu (or (and menu (symbolp menu)
			           (setq menu-alist
				         (cdr (assq menu (or menu-list hui:menus)))))
		              (hypb:error "(hui:menu-get-keys): Invalid menu symbol arg: `%s'"
			                  menu)))
      (cond ((and (consp (setq act-form (hui:menu-choose menu-alist)))
		  (cdr act-form)
		  (symbolp (cdr act-form)))
	     ;; Display another menu
	     (setq menu (cdr act-form)))
	    (act-form
	     (let ((prefix-arg current-prefix-arg))
	       (cond ((or (symbolp act-form)
			  (stringp act-form))
		      (unless (eq act-form t)
			(setq show-menu nil)))
		     (t (setq show-menu nil)))))
	    (t (setq show-menu nil))))
    hui:menu-keys))

(defun hui:menu-backward-item (&optional arg)
  "Move point back to optional ARGth start of a selectable minibuffer menu item.
If on the menu name prefix or the first item, move to the last item."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (hui:menu-forward-item (- arg))
    (let (opoint)
      (while (> arg 0)
	;; First skip back past menu name/description prompt, if within it.
	(when (hui:menu-name-at-p)
	  (setq opoint (point))
	  (skip-chars-backward "^ \t\n\r")
	  (skip-chars-forward " \t")
	  (skip-chars-forward "^<> \t\n\r")
	  (unless (looking-at ">\\s-")
	    (goto-char opoint)
	    (skip-chars-backward "^ \t\n\r")))
	(if (re-search-backward "\\s-[^> \t\n\r]" nil t)
	    (forward-char 1)
	  (goto-char (point-max))
	  (skip-chars-backward "^ \t\n\r"))
	(setq arg (1- arg))))))

(defun hui:menu-doc (key-sequence &optional help-string-flag)
  "Return documentation for a normalized Hyperbole minibuffer menu KEY-SEQUENCE.
The documentation is formatted.  With optional HELP-STRING-FLAG,
instead returns the one line help string for the key sequence."
  (when (and (stringp key-sequence)
	     (not (string-equal key-sequence ""))
	     (kbd-key:hyperbole-mini-menu-key-p key-sequence))
    (let ((hargs:reading-type 'hmenu-help)
	  (hmenu-key-seq (car (where-is-internal #'hyperbole))))
      (unless hmenu-key-seq
	(hypb:error "(hui:menu-doc): The 'hyperbole' command must be bound to a key"))
      (setq unread-command-events
	    (nconc unread-command-events
		   (mapcar #'identity (substring key-sequence (length hmenu-key-seq)))))
      (prog1 (hui:menu-act 'hyperbole nil t help-string-flag)
	;; Ignore any keys past the first menu item activation.
	(discard-input)))))

(defun hui:menu-exit-hyperbole ()
  "Exit any Hyperbole minibuffer menu and disable `hyperbole-mode'."
  (interactive)
  (hyperbole-mode 0)
  (hui:menu-enter hui:menu-quit))

(defun hui:menu-enter (&optional char-str)
  "Use CHAR-STR or last input character as minibuffer argument."
  (interactive)
  (let ((input (or char-str (aref (recent-keys) (1- (length (recent-keys)))))))
    (and (not (integerp input))
	 (eventp input)
	 (setq input (event-basic-type input)))
    (if (or (symbolp input)
	    (and (integerp input) (= input ?\r)))
	;; If no item selected, when on the menu name (ending with
	;; '>', input will be set to 1.  If at the end of the menu,
	;; input will be set to 0.  See `hui:menu-choose' for how
	;; these are handled.
	(setq input (hargs:at-p)))
    (erase-buffer)
    (when (or (characterp input) (stringp input))
      (setq this-command #'self-insert-command)
      (insert input)))
  (exit-minibuffer))

(defalias 'hui:menu-quit   #'hui:menu-enter)
(defalias 'hui:menu-abort  #'hui:menu-enter)
(defalias 'hui:menu-top    #'hui:menu-enter)
(defalias 'hui:menu-select #'hui:menu-enter)

(defun hui:menu-forward-item (&optional arg)
  "Move point to the optional prefix ARGth next selectable minibuffer menu item.
If on the menu name prefix or the last item, move to the first item."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (hui:menu-backward-item (- arg))
    (let (opoint)
      (while (> arg 0)
	;; First skip past menu name/description prompt, if within it.
	(when (hui:menu-name-at-p)
	  (setq opoint (point))
	  (skip-chars-backward "^ \t\n\r")
	  (skip-chars-forward " \t")
	  (skip-chars-forward "^<> \t\n\r")
	  (unless (looking-at ">\\s-")
	    (goto-char opoint)))
	(if (re-search-forward "\\s-+[^> \t\n\r]" nil t)
	    (backward-char 1)
	  (goto-char (point-min))
	  (when (looking-at "[^< \t\n\r]+>\\s-")
	    (hui:menu-forward-item)))
	(setq arg (1- arg))))))

(defun hui:menu-help (help-str)
  "Display HELP-STR in a small window at the bottom of the selected frame."
  (let* ((window-min-height 2)
	 (owind (selected-window))
	 (buf-name (hypb:help-buf-name "Menu")))
    (unwind-protect
	(progn
	  (save-window-excursion
	    (hkey-help-show buf-name)) ;; Needed to save wconfig.
	  (if (eq (selected-window) (minibuffer-window))
	      (other-window 1))
	  (and (= (window-top-line) 0)
	       (< (- (frame-height) (window-height)) 2)
	       (split-window-vertically nil))
	  (select-window (hui:bottom-window))
	  (switch-to-buffer (get-buffer-create buf-name))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "\n" help-str)
	  (set-buffer-modified-p nil)
	  (let ((neg-shrink-amount (- (+ 3 (hypb:char-count ?\n help-str)))))
	    (if (window-resizable-p (selected-window) neg-shrink-amount)
		(shrink-window (+ (window-height) neg-shrink-amount)))))
      (if (eq owind (minibuffer-window))
	  (select-window owind)))))

(defun hui:menu-item-key (item)
  "Return ordered list of keys that activate Hypb minibuffer MENU-ALIST items.
For each item, the key is either the first capital letter in ITEM
or if there are none, then its first character."
  ;; Return either the first capital letter in item or if
  ;; none, then its first character.
  (or (catch 'capital
	(progn (mapc (lambda (c) (and (<= ?A c) (>= ?Z c)
				      (throw 'capital c)))
		     item)
	       ;; Ensure nil is returned from catch if no
	       ;; matching char is found
	       nil))
      (aref item 0)))

(defun hui:menu-item-keys (menu-alist)
  "Return ordered list of keys that activate Hypb minibuffer MENU-A[LIST items.
For each item, the key is either the first capital letter in item
or if there are none, then its first character."
  (mapcar (lambda (item) (hui:menu-item-key item))
	  (mapcar 'car (cdr menu-alist))))

(defun hui:menu-item-toggle-highlight (&optional arg)
  "Toggle highlighting Hyperbole minibuffer menu item keys.
With optional ARG, enable iff ARG is positive."
  (interactive "P")
  (if (or (and arg (<= (prefix-numeric-value arg) 0))
	  (and (not (and arg (> (prefix-numeric-value arg) 0)))
	       hui:menu-highlight-flag))
      (progn (customize-set-variable 'hui:menu-highlight-flag nil)
	     (message "Menu key highlighting is off."))
    (customize-set-variable 'hui:menu-highlight-flag t)
    (message "Menu key highlighting is on")))

(defun hui:menu-read-from-minibuffer (prompt &optional initial-contents keymap read
				      hist default-value inherit-input-method)
  "Hyperbole minibuffer menu replacement for `read-from-minibuffer'.
Allows custom handling of menu lines before selecting an item."
  (when (and (stringp initial-contents)
	     (string-prefix-p "Org M-RET" initial-contents))
    (let* ((org-m-ret-options (cddr (assq 'cust-org hui:menus)))
	   (option-lookups (mapcar (lambda (option)
				     (cons (car (last (nth 1 option)))
					   (car option)))
				   org-m-ret-options))
	   (current-name (cdr (assq hsys-org-enable-smart-keys option-lookups))))
      (when (and (stringp current-name) (stringp initial-contents))
	(setq initial-contents (string-replace current-name
					       (concat "==" current-name "==")
					       initial-contents)))))
  (setq initial-contents (hui:menu-maybe-highlight-item-keys initial-contents))
  (read-from-minibuffer prompt initial-contents keymap read
			hist default-value inherit-input-method))

(defun hui:menu-choose (menu-alist &optional doc-flag help-string-flag)
  "Prompt user to choose the first capitalized char of any item from MENU-ALIST.
Return the Lisp form associated with the selected item, which may be
another menu.

The character may be entered in lowercase.  If chosen by direct
selection with the Assist Key, return any help string for item,
else return the action form for the item.

Two additional optional arguments may be given when documentation for
a menu item should be shown rather than menu display.  DOC-FLAG
non-nil means show documentation for any item that is selected by the
user.  HELP-STRING-FLAG non-nil means show only the first line of the
documentation, not the full text."
  (let* ((menu-line (hui:menu-line menu-alist))
	 (set:equal-op 'eq)
	 (select-char (string-to-char hui:menu-select))
	 (exit-char (string-to-char hui:menu-exit-hyperbole))
	 (quit-char (string-to-char hui:menu-quit))
	 (abort-char (string-to-char hui:menu-abort))
	 (top-char  (string-to-char hui:menu-top))
	 (item-keys (hui:menu-item-keys menu-alist))
	 ;; 0 matches an empty string return, no selection
	 (keys (apply #'list 0 1 select-char exit-char quit-char abort-char
		      top-char item-keys))
	 (key 0)
	 (hargs:reading-type 'hmenu))
    (while (not (memq (setq key (upcase
				 (string-to-char
				  (hui:menu-read-from-minibuffer
				   "" menu-line hui:menu-mode-map nil t))))
		      keys))
      (beep)
      (setq hargs:reading-type 'hmenu)
      (discard-input))
    ;; Here, the minibuffer has been exited, and `key' has been set to one of:
    ;;   a menu item first capitalized character code;
    ;;   a menu command character code;
    ;;   1 for in the menu prefix area (moves to top menu);
    ;;   0 for at the end of the menu (does nothing).
    (cond ((eq key exit-char)
	   (set--this-command-keys (concat hui:menu-keys hui:menu-exit-hyperbole))
	   (setq this-command #'hui:menu-exit-hyperbole)
	   nil)
	  ((eq key quit-char)
	   (set--this-command-keys (concat hui:menu-keys hui:menu-quit))
	   (setq this-command #'hui:menu-quit)
	   nil)
	  ((eq key 0)
	   nil)
	  ((eq key abort-char)
	   (beep)
	   (set--this-command-keys (concat hui:menu-keys hui:menu-abort))
	   (setq this-command #'hui:menu-abort)
	   nil)
	  ((and (eq key 1) (listp (car menu-alist))
		(stringp (caar menu-alist))
		(string-match "^Hy[.0-9]*[a-zA-Z0-9]*>$" (caar menu-alist)))
	   ;; RET pressed on Hyperbole top-level menu prefix, reload
	   ;; Smart Key handlers and minibuffer menus to reflect any updates.
	   (hmouse-update-smart-keys)
	   (set--this-command-keys (concat hui:menu-keys hui:menu-quit))
	   (setq this-command #'hmouse-update-smart-keys)
	   nil)
	  ((memq key (list 1 top-char))
	   (setq hui:menu-keys (concat hui:menu-keys (char-to-string top-char)))
	   '(menu . hyperbole))
	  ((and (eq key select-char)
		(save-excursion
		  (if (search-backward " " nil t)
		      (progn (skip-chars-forward " ")
			     ;; Get the next following capital letter
			     (let (case-fold-search)
			       (setq key
				     (if (looking-at "[^ \t\nA-Z]*[A-Z]")
					 (char-before (match-end 0))
				       (following-char))))
			     nil)  ;; Drop through.
		    t))))
	  (t (hui:menu-item key doc-flag help-string-flag nil menu-alist)))))

(defun hui:menu-to-personal-section (section)
  "Go to top-level SECTION in personal button file; add the section if necessary."
  (let* ((hypb-personal-file (expand-file-name hbmap:filename hbmap:dir-user))
	 (hyrolo-file-list (list hypb-personal-file))
	 (hyrolo-add-hook)
	 (hyrolo-edit-hook)) ;; Prevent addition of dates when add navigation sections
    (if (= 1 (hyrolo-fgrep section 1 nil t t))
	(hpath:find (concat hypb-personal-file "#" section))
      (hyrolo-add section))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hui:bottom-window ()
  "Return a window that is at the bottom of the selected frame."
  (let* ((winds (hypb:window-list 'no-mini))
	 (bot-list (mapcar
		    (lambda (wind)
		      (nth 3 (window-edges wind)))
		    winds))
	 (bot (apply #'max bot-list)))
    (nth (- (length winds) (length (memq bot bot-list))) winds)))

(defun hui:menu-item (key doc-flag help-string-flag &optional menu menu-alist)
  "Return either action or documentation for a Hyperbole minibuffer menu item KEY.
If DOC-FLAG is non-nil, returns the fully formatted documentation unless
HELP-STRING-FLAG is non-nil, in which case only the first line of
documentation is returned.  If both are nil, the action form for the
item is returned.

Two additional optional arguments determine the items from which key
selects, MENU and MENU-ALIST are Hyperbole minibuffer menu internal
constructs.  If not given, the top level Hyperbole menu is used."
  (unless menu-alist
    (setq menu-alist (or (cdr (assq (or (and (symbolp menu) menu) 'hyperbole)
				    hui:menus))
			 (hypb:error "(hui:menu-item): Invalid menu symbol arg: `%s'"
				     menu))))
  (let ((item-keys (hui:menu-item-keys menu-alist))
	 sublist)
    (when (setq sublist (memq key item-keys))
      (setq hui:menu-keys (concat hui:menu-keys (downcase (char-to-string key))))
      (let* ((label-act-help-list
	      (nth (- (1+ (length item-keys)) (length sublist))
		   menu-alist))
	     (label (car label-act-help-list))
	     (act-form (cadr label-act-help-list)))
	(if (or (eq hargs:reading-type 'hmenu-help)
		(and doc-flag
		     ;; Not another menu to display
		     (not (and (listp act-form) (atom (car act-form)) (atom (cdr act-form))))))
	    (let* ((help-str (caddr label-act-help-list))
		   (cmd (if help-str nil (cadr label-act-help-list)))
		   (doc-str (if help-str nil (and (functionp cmd) (documentation cmd)))))
	      (and doc-str (string-match "\n" doc-str)
		   (setq doc-str (substring doc-str 0 (match-beginning 0))))
	      (setq help-str (or help-str doc-str "No help documentation for this item."))
	      (if help-string-flag
		  help-str
		(concat (car label-act-help-list) "\n  "
			help-str "\n    Action: "
			(prin1-to-string act-form))))
	  (if (eq act-form #'hui:menu-to-personal-section)
	      (list #'hui:menu-to-personal-section label)
	    act-form))))))

(defun hui:menu-maybe-highlight-item-keys (menu-str)
  "Maybe highlight the first capital letter of each MENU-STR item.
Highlight if customization variable `hui:menu-highlight-flag' is
non-nil and the display supports underlined faces.  Return the
potentially modified MENU-STR."
  (if (and hui:menu-highlight-flag
           (display-supports-face-attributes-p
            '(:underline t) (window-frame)))
      (let ((after-menu-name-flag)
	    (after-word-capital-letter-flag)
	    (pos 0))
	(mapc (lambda (c)
		(cond ((= c ?>)
		       (setq after-menu-name-flag t))
		      ((= c ?\ )
		       (setq after-word-capital-letter-flag nil))
		      ((and after-menu-name-flag
			    (not after-word-capital-letter-flag)
			    (<= ?A c) (>= ?Z c))
		       (put-text-property pos (1+ pos)
					  'face 'read-multiple-choice-face
					  menu-str)
		       (setq after-word-capital-letter-flag t)))
		(setq pos (1+ pos)))
	      menu-str)
	menu-str)
    menu-str))

(defun hui:menu-line (menu-alist)
  "Return a menu line string built from MENU-ALIST."
  (let ((menu-prompt (concat (caar menu-alist) "  "))
	(menu-items (mapconcat 'car (cdr menu-alist) "  "))
	(width (1- (frame-width)))
	menu-line)
    (setq menu-line (concat menu-prompt menu-items))
    (when (>= (length menu-line) width)
      ;; Narrow menu by changing 2 spaces to 1 if too wide for current frame.
      (setq menu-line (concat menu-prompt (mapconcat #'car (cdr menu-alist) " "))))
    (if (>= (length menu-line) width)
	;; If still too wide, switch to a multi-line layout.
	(hui:menu-multi-line menu-alist)
      menu-line)))

(defun hui:menu-multi-line (menu-alist)
  "Return the formatted text for a multi-line minibuffer window popup menu.
The menu is a menu of commands from MENU-ALIST."
  (let* ((items-in-line 0)
	 (item-start 0)
	 (menu-strings (mapcar #'car menu-alist))
	 (max-item-len
	  (when menu-strings (+ 1 (apply 'max (mapcar #'length menu-strings))))))
    (unless menu-strings
      (error "(hui:menu-multi-line): Invalid menu specified, '%s'" menu-alist))
    (with-temp-buffer
      (let (indent-tabs-mode)
	(mapc
	 (lambda (s)
	   (setq item-start (* max-item-len items-in-line))
	   (if (or (>= item-start (frame-width))
		   (>= (+ item-start max-item-len) (frame-width)))
	       (progn
		 (setq items-in-line 0)
		 (insert "\n" s))
	     (move-to-column item-start t)
	     (insert s))
	   (setq items-in-line (1+ items-in-line)))
	 menu-strings)
	(buffer-string)))))

(defun hui:menu-name-at-p ()
  "Return non-nil if point is within a Hyperbole minibuffer menu name."
  (when (and hui:menu-p (eq (minibuffer-window) (selected-window)))
    (save-excursion (not (re-search-backward "\\(^\\|[ \t]\\)[^< \t\n\r]+>" nil t)))))

(defun hui:menu-web-search ()
  "Hyperbole minibuffer menu of web search engines."
  (let* (service
	 action
	 (web-mini-menu
	  (cons 'web
		(cons '("Web>")
		      (mapcar (lambda (service-and-action)
				(setq service (car service-and-action)
				      action  (cdr service-and-action))
				(if (stringp action)
				    (list service
					  (list #'hyperbole-web-search service)
					  (format "Search %s" service))
				  (list service
					;; a command symbol
					action
					"Run a command that prompts for a search term and performs the search")))
			      hyperbole-web-search-alist)))))
    web-mini-menu))

(defun hui-search-web ()
  "Prompt for a web search engine and search term and then perform the search.

If the key that invokes this command in `hyperbole-minor-mode' is also
bound in the current major mode map, then interactively invoke that
command instead.  Typically prevents clashes over {\\`C-c' /}."
  (interactive)
  (let* ((key (hypb:cmd-key-vector #'hui-search-web hyperbole-mode-map))
	 (major-mode-binding (lookup-key (current-local-map) key))
	 (this-key-flag (and (called-interactively-p 'any)
			     (equal (this-single-command-keys) key))))
    (if (and major-mode-binding (not (integerp major-mode-binding))
	     this-key-flag)
	;; If the key that invokes this command in `hyperbole-minor-mode'
	;; is also bound in the current major mode map, then
	;; interactively invoke that command instead.  Typically
	;; prevents clashes over {C-c /}.
	(call-interactively major-mode-binding)
      ;;
      ;; No key conflicts, perform normal Hyperbole operation
      (hyperbole 'web))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;; Hyperbole menu mode is suitable only for specially formatted data.
(put 'hui:menu-mode 'mode-class 'special)

(defvar hui:menu-mode-map nil
  "Keymap containing Hyperbole minibuffer menu commands.")
(unless hui:menu-mode-map
  (setq hui:menu-mode-map (make-keymap))
  ;; Make self-inserting chars all execute hui:menu-enter
  (suppress-keymap hui:menu-mode-map)
  ;;
  (let ((i 32))
    (while (<= i 126)
      (define-key hui:menu-mode-map (char-to-string i) #'hui:menu-enter)
      (setq i (1+ i))))
  ;;
  ;; Bind any active keys for menu mode
  (define-key hui:menu-mode-map hui:menu-exit-hyperbole #'hui:menu-exit-hyperbole)
  (define-key hui:menu-mode-map hui:menu-quit           #'hui:menu-quit)
  (define-key hui:menu-mode-map hui:menu-abort          #'hui:menu-abort)
  (define-key hui:menu-mode-map hui:menu-top            #'hui:menu-top)
  (define-key hui:menu-mode-map hui:menu-select         #'hui:menu-select)
  (define-key hui:menu-mode-map "\M-b"                  #'hui:menu-backward-item)
  (define-key hui:menu-mode-map "\M-f"                  #'hui:menu-forward-item)
  (define-key hui:menu-mode-map "\C-i"                  #'hui:menu-forward-item) ;; TAB
  (define-key hui:menu-mode-map [backtab]               #'hui:menu-backward-item) ;; Shift-TAB
  (define-key hui:menu-mode-map "\M-\C-i"               #'hui:menu-backward-item)) ;; M-TAB

;;; ************************************************************************
;;; Hyperbole Minibuffer Menus
;;; ************************************************************************

(defun hyperbole-minibuffer-menu ()
  "Rebuild and return the entire Hyperbole minibuffer menu structure."
(setq
 hui:menus
 (delq nil
 (list (cons
	'hyperbole
	(append
	 (let ((version (if (= (aref hyperb:version 0) ?0)
			    (substring hyperb:version 1)
			  hyperb:version)))
	   (list (list (concat "Hy" version ">"))))
	 '(
	   ("Act"         hui:hbut-act      "Activate button at point or prompt for a labeled button in buffer.")
	   ("Butfile/"    (menu . butfile)  "Quick access button files menus.")
	   ("Cust/"       (menu . cust)     "Customize Hyperbole by setting major options.")
	   ("Doc/"        (menu . doc)      "Quick access to Hyperbole documentation.")
	   ("Ebut/"       (menu . ebut)     "Explicit button commands.")
	   ("Find/"       (menu . find)     "Find matching line commands.")
	   ("Gbut/"       (menu . gbut)     "Global button commands.")
	   ("HyWiki/"     (menu . hywiki)   "HyWiki commands.")
	   ("Ibut/"       (menu . ibut)     "Implicit button and button type commands.")
	   ("Kotl/"       (menu . kotl)     "Autonumbered outlining and hyperlink capabilities.")
	   ("Msg/"        (menu . msg)      "Mail and News messaging capabilities.")
	   ("Rolo/"       (menu . hyrolo)   "Hierarchical, multi-file rolo lookup and edit commands.")
	   ("Screen/"     (menu . screen)   "Screen display management commands.")
	   ;; ("To/"         (menu . to)       "A-Z menu to search and add Emacs artifacts")
	   ("Win/"        (menu . win)      "Window configuration management commands.")
	   ("historY"     hhist:pop
	    "Jump back to location prior to last Hyperbole button follow.")
	   )))
       '(butfile .
	 (("Butfile>")
	  ("DirFile"      (find-file hbmap:filename)
	   "Edit directory-specific button file.")
	  ("Info"
	   (id-info "(hyperbole)Button Files")
	   "Display manual section on button files.")
	  ("PersonalFile" (find-file
			    (expand-file-name hbmap:filename hbmap:dir-user))
	   "Edit user-specific button file.")))
       '(cust .
         (("Cust>")
	  ("All-Options"       (customize-browse 'hyperbole)
	   "Display tree of Hyperbole customizable options by group.")
	  ("Debug-Toggle"      hkey-toggle-debug
	   "Toggle display of Smart Key context after each press, for debugging.")
	  ("Find-File-URLs"    hpath:find-file-urls-mode
	   "Toggle find-file support for ftp and www URLs.")
	  ("Highlight-Menu-Keys-Toggle" hui:menu-item-toggle-highlight
	   "Toggle highlighting of minibuffer menu keys.")
	  ("Isearch-Invisible" hypb:toggle-isearch-invisible
	   "Toggle whether isearch searches invisible text or not.")
	  ("KeyBindings/"      (menu . cust-keys) "Rebinds global Hyperbole keys.")
	  ("Msg-Toggle-Ebuts"  hyperbole-toggle-messaging
	   "Toggle Hyperbole support for explicit buttons in mail and news buffers.")
	  ("Org-M-RET/"        (menu . cust-org)
	   "Set how much of Hyperbole Smart Key behavior is enabled in Org mode.")
	  ("Referents/"        (menu . cust-referents)
	   "Set where Hyperbole button referents are displayed.")
	  ("Smart-Key-at-Eol/" (menu . cust-eol)
	   "Set how scrolling via end of line presses works.")
	  ("Toggle-Rolo-Dates" hyrolo-toggle-datestamps
	   "Toggle whether date stamps are updated when rolo entries are edited.")
	  ("URL-Display/"      (menu . cust-urls) "Set where URLs are displayed.")
	  ("Web-Search/"       (menu . cust-web) "Set where Web Searches are displayed.")))
       '(cust-eol .
         (("Smart Key press at eol scrolls>")
	  ("Proportionally" (setq smart-scroll-proportional t))
	  ("Windowful"      (setq smart-scroll-proportional nil))))
       '(cust-keys .
         (("Change Keys>")
	  ("ActionKey"     (hui:bind-key #'hkey-either))                        ;; {M-RET}
	  ("ButRename"     (hui:bind-key #'hui:ebut-rename))                    ;; None
	  ("DragKey"       (hui:bind-key #'hkey-operate))                       ;; {M-o}
	  ("FindWeb"       (hui:bind-key #'hui-search-web)) 	                ;; {C-c /}
	  ("GridOfWindows" (hui:bind-key #'hycontrol-windows-grid))             ;; {C-c @}
	  ("HypbMenu"      (hui:global-bind-key #'hyperbole))                   ;; {C-h h}
	  ("JumpThing"     (hui:bind-key #'hui-select-goto-matching-delimiter)) ;; {C-c .}
	  ("MarkThing"     (hui:bind-key #'hui-select-thing))                   ;; {C-c RET}
	  ("SmartHelp"     (hui:bind-key #'hkey-help))                          ;; {C-h A}
	  ("WinControl"    (hui:bind-key #'hycontrol-enable-windows-mode))      ;; {C-c \}
	  ))
       '(cust-org .
         (("Org M-RET Overrides>")
	  ("All-Hyperbole-Contexts" (customize-save-variable 'hsys-org-enable-smart-keys t)
	   "Smart Keys override Org M-RET in all Org mode Smart Key contexts (see `smart-org').")
	  ("Hyperbole-Buttons-Only" (customize-save-variable 'hsys-org-enable-smart-keys :buttons)
	   "Smart Keys override Org M-RET only when on a Hyperbole recognized button.")
	  ("None"                   (customize-save-variable 'hsys-org-enable-smart-keys nil)
	   "Smart Keys never override Org M-RET operation.")))
       '(cust-referents .
         (("Ref Display>")
	  ("Any-Frame"   (setq hpath:display-where 'other-frame))
	  ("Current-Win" (setq hpath:display-where 'this-window))
	  ("Diff-Frame-One-Win"
	   (setq hpath:display-where 'other-frame-one-window))
	  ("New-Frame"   (setq hpath:display-where 'new-frame))
	  ("Other-Win"   (setq hpath:display-where 'other-window))
	  ("Single-Win"  (setq hpath:display-where 'one-window))))
       '(cust-urls .
         (("URL Display>")
	  ("Chrome"      (setq browse-url-browser-function #'browse-url-chrome))
	  ("Default"     (setq browse-url-browser-function
			       (if (and (boundp 'browse-url-generic-program) (stringp browse-url-generic-program))
				   #'browse-url-generic
				 #'browse-url-default-browser)))
	  ("EWW"         (setq browse-url-browser-function #'eww-browse-url))
	  ("Firefox"     (setq browse-url-browser-function #'browse-url-firefox))
	  ("KDE"         (setq browse-url-browser-function #'browse-url-kde))
	  ("XTerm"       (setq browse-url-browser-function #'browse-url-text-xterm))))
       '(cust-web .
         (("Web Search>")
	  ("Chrome"      (setq hyperbole-web-search-browser-function #'browse-url-chrome))
	  ("Default"     (setq hyperbole-web-search-browser-function
			       (if (and (boundp 'browse-url-generic-program) (stringp browse-url-generic-program))
				   #'browse-url-generic
				 #'browse-url-default-browser)))
	  ("EWW"         (setq hyperbole-web-search-browser-function #'eww-browse-url))
	  ("Firefox"     (setq hyperbole-web-search-browser-function #'browse-url-firefox))
	  ("KDE"         (setq hyperbole-web-search-browser-function #'browse-url-kde))
	  ("XTerm"       (setq hyperbole-web-search-browser-function #'browse-url-text-xterm))))
       '(doc .
	 (("Doc>")
	  ("About"       (hypb:display-file-with-logo "HY-ABOUT") "Overview of Hyperbole.")
	  ("Concepts"    (find-file (expand-file-name "HY-CONCEPTS.kotl" hyperb:dir))
	   "Explains connections among Hyperbole concepts.")
	  ("Demo"        hyperbole-demo                           "Demonstrate Hyperbole features.")
	  ("Files"       (hypb:display-file-with-logo "MANIFEST")
	   "Summarizes Hyperbole system files.  Click on an entry to view it.")
	  ("Glossary"    (id-info "(hyperbole)Glossary")          "Glossary of Hyperbole terms.")
	  ("Info"        (id-info "(hyperbole)Top")               "Online Info version of Hyperbole manual.")
	  ("New"         (hypb:display-file-with-logo "HY-NEWS")  "Recent changes to Hyperbole.")
	  ("SmartKeys"   (hkey-summarize 'current-window)         "Summarize Smart Key mouse or keyboard handling.")
	  ("Types/"      (menu . types)                           "Provide documentation on Hyperbole types.")
	  ("WhyUse"      (find-file (expand-file-name "HY-WHY.kotl" hyperb:dir))
	   "Lists use cases for Hyperbole.")))
       '(ebut .
	 (("EButton>")
	  ("Act"         hui:ebut-act
	    "Activates explicit button at point or prompts for explicit button to activate.")
	  ("Create"      hui:ebut-create                         "Add an explicit button to the current buffer.")
	  ("Delete"      hui:ebut-delete                         "Remove an explicit button from the current buffer.")
	  ("Edit"        hui:ebut-edit                           "Modify any desired button attributes.")
	  ("Help/"       (menu . ebut-help)                      "Summarize button attributes.")
	  ("Info"        (id-info "(hyperbole)Explicit Buttons") "Display manual section on explicit buttons.")
	  ("Link"        hui:ebut-link-directly                  "Insert an ebut link at point to other/another window.")
	  ("Rename"      hui:ebut-rename                         "Relabel an explicit button.")
	  ("Search"      hui:ebut-search                         "Locate and displays personally created buttons in context.")
	  ("Types"       (hui:htype-help-current-window 'actypes)
	   "Displays documentation for one or all action types used by explicit buttons.")))
       '(ebut-help .
	 (("Help on>")
	  ("BufferButs"  (hui:hbut-report -1) "Summarize all explicit buttons in buffer.")
	  ("CurrentBut"  (hui:hbut-report)    "Summarize only current button in buffer.")
	  ("OrderedButs" (hui:hbut-report 1)  "Summarize explicit buttons in lexicographically order.")))
       '(find .
         (("Find>")
	  ("GrepFiles"           hypb:rgrep        "Show numbered line matches in all specified files.")
	  ("LocateFiles"         hypb:locate       "Locate matching file names anywhere across a system.")
	  ("MatchFileBuffers"    moccur            "Show numbered line matches for regexp in all file-based buffers.")
	  ("OccurHere"           occur             "Show numbered line matches for regexp from this buffer.")
	  ("RemoveLines"         hypb:remove-lines "Following point, remove all lines that match regexp.")
	  ("SaveLines"           hypb:save-lines   "Following point, keep only lines that match regexp.")
	  ("Web/"                (menu . web)      "Search major web sites.")))
       '(gbut .
	 (("GButton>")
	  ("Act"    gbut:act        "Activate global button by name.")
	  ("Create" hui:gbut-create "Add a global button to (gbut:file).")
	  ("Delete" hui:gbut-delete "Remove a global button from (gbut:file).")
	  ("Edit"   hui:gbut-edit   "Modify global button attributes.")
	  ("Help"   gbut:help       "Report on a global button by name.")
	  ("Info"   (id-info        "(hyperbole)Global Buttons")
	   "Display manual section on global buttons.")
	  ("Link"   hui:gbut-link-directly "Add a named global button link to point in other/another window.")
	  ("Rename" hui:gbut-rename "Rename a global button.")))
       (cons 'hywiki hui:menu-hywiki)
       '(ibut .
	 (("IButton>")
	  ("Act"            hui:ibut-act
	    "Activate implicit button at point or prompts for labeled implicit button to activate.")
	  ("Create"         hui:ibut-create
	    "Label and create an implicit button of any type.")
	  ("DeleteType"     (hui:htype-delete 'ibtypes)
	   "Delete specified implicit button type.")
	  ("Edit"           hui:ibut-edit "Edit/modify named implicit button attributes.")
	  ("Help"           hkey-help "Report on button's attributes.")
	  ("Info"           (id-info "(hyperbole)Implicit Buttons")
	   "Display manual section on implicit buttons.")
	  ("Link"           hui:ibut-link-directly "Insert an ibut link at point to other/another window.")
	  ("Name"           hui:ibut-label-create
	   "Create an implicit button label preceding an existing implicit button at point, if any.")
	  ("Rename"         hui:ibut-rename
	   "Modify a label preceding an implicit button in the current buffer.")
	  ("Types"          (hui:htype-help 'ibtypes 'no-sort)
	   "Display documentation for one or all implicit button types.")))
       '(kotl
	 . (("Kotl>")
	    ("All"       kotl-mode:show-all        "Expand all collapsed cells.")
	    ("Blanks"    kvspec:toggle-blank-lines "Toggle blank lines between cells on or off.")
	    ("Create"    kfile:find                "Create or edit an outline file.")
	    ("Downto"    kotl-mode:hide-sublevels  "Hide all cells in outline deeper than a particular level.")
	    ("Examp"     kotl-mode:example         "Display a self-descriptive example outline file.")
	    ("Format/"   (menu . kotl-format)      "Import/Export Koutlines.")
	    ("Hide"      (progn (kotl-mode:is-p)
				(kotl-mode:hide-tree (kcell-view:label)))
	     "Collapse tree rooted at point.")
	    ("Info"
	     (id-info "(hyperbole)Koutliner")
	     "Display manual section on Hyperbole Koutliner.")
	    ("Kill"      kotl-mode:kill-tree
	     "Kill ARG following trees starting from point.")
	    ("Link"      klink:create
	     "Create and insert an implicit link at point.")
	    ("Overvw"    kotl-mode:overview
	     "Show first line of each cell.")
	    ("Show"      (progn (kotl-mode:is-p)
				(kotl-mode:show-tree (kcell-view:label)))
	     "Expand tree rooted at point.")
	    ("Top"       kotl-mode:top-cells
	     "Hide all but top-level cells.")
	    ("Vspec"     kvspec:activate
	     "Prompt for and activate a view specifiction.")))
       '(kotl-format .
	 (("Format>")
	    ("Display-in-Browser"      kexport:display  "Export and display current Koutline in default web browser")
            ("File-Import-to-Koutline" kimport:file     "Import a buffer/file into a new or existing Koutline.")
	    ("Html-Export-Other"       kexport:html     "Prompt for a Koutline buffer/file and output HTML file; export it.")
	    ("Koutline-Export-to-Html" kexport:koutline "Export current Koutline and save as an HTML file for web usage.")))
       '(msg .
	 (("Msg>")
	  ("Compose-Hypb-Mail"
	   (hmail:compose "hyperbole-users@gnu.org" '(hact 'hyp-config))
	   "Send a message to the Hyperbole discussion list.")
	  ("Join-Hypb-List"
	   (hmail:compose "hyperbole-users-join@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Subscribe to the Hyperbole discussion list.")
	  ("Leave-Hypb-List"
	   (hmail:compose "hyperbole-users-leave@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Unsubscribe from the Hyperbole discussion list.")
	  ("Report-Hypb-Bug"
	   (hmail:compose "bug-hyperbole@gnu.org" '(hact 'hyp-config))
	   "Send a message to the Hyperbole bug reporting list.")
	  ("Subscribe-Hypb-Bug"
	   (hmail:compose "bug-hyperbole-join@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Subscribe to the Hyperbole bug reporting list.")
	  ("Unsub-Hypb-Bug"
	   (hmail:compose "bug-hyperbole-leave@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Unsubscribe from the Hyperbole bug reporting list.")))
       (cons 'hyrolo hui:menu-rolo)
       '(screen .
	 (("Screen>")
	  ("FramesControl"    hycontrol-enable-frames-mode
	   "Interactively delete, jump to, move, replicate, and resize frames.")
	  ("WindowsControl"   hycontrol-enable-windows-mode
	   "Interactively delete, jump to, rebalance, resize, and split windows.")))
       ;; (cons 'to hui:menu-to)
       '(types .
	 (("Types>")
	  ("ActionTypes"      (hui:htype-help-current-window 'actypes)
	   "Displays documentation for one or all action types.")
	  ("IButType"        (hui:htype-help-current-window 'ibtypes 'no-sort)
	   "Display documentation for one or all implicit button types.")))
       '(win .
	 (("WinConfig>")
	  ("AddName"          hywconfig-add-by-name     "Name current window configuration.")
	  ("DeleteName"       hywconfig-delete-by-name  "Delete named window configuration.")
	  ("RestoreName"      hywconfig-restore-by-name "Restore frame to window configuration given by name.")
	  ("PopRing"          hywconfig-delete-pop-continue
	   "Restore window configuration from ring and removes it from ring.")
	  ("SaveRing"         (hywconfig-ring-save)
	   "Save current window configuration to ring.")
	  ("YankRing"         hywconfig-yank-pop-continue
	   "Restore next window configuration from ring.")))
       (hui:menu-web-search)))))

;;; ************************************************************************
;;; Public Customizations - must come after menus are defined
;;; ************************************************************************

(defcustom hui:menu-highlight-flag t
  "*If non-nil, highlight the first capitalized character of each menu item.
Highlight with `read-multiple-choice-face'.  The display must
support underlined faces as well."
  :type 'boolean
  :group 'hyperbole-buttons)

(unless hui:menu-hywiki
  (makunbound 'hui:menu-hywiki))
(defcustom hui:menu-hywiki
  (delq nil
	(list
	 '("HyWiki>")
	 '("Act"            hywiki-word-activate
	   "Activate HyWikiWord link at point or emulate a press of a Smart Key.")
	 '("Create"         hywiki-add-page-and-display
	    "Create and display a new HyWiki page.  Shows existing page names to aid in new naming.")
	 '("Edit"           hywiki-find-page
	   "Prompt with completion for and display a HyWiki page ready for editing.")
	 (when (fboundp 'consult-grep) ;; allow for autoloading
	   '("GrepConsult"    hywiki-consult-grep
	     "Grep over HyWiki pages with interactive consult-grep."))
	 '("Help"           hkey-help
	   "Report on a HyWikiWord's attributes.")
	 '("Info"           (id-info "(hyperbole)HyWiki")
	   "Display Hyperbole manual section on HyWiki.")
	 '("Link"           hywiki-add-link
	   "Prompt for and add a link at point to a HyWiki page.")
         '("ModeToggle"     hywiki-mode
	   "Toggle whether HyWikiWords are highlighted and active in buffers outside of the HyWiki page directory.")
	 '("Org-M-RET/"     (menu . cust-org)
	   "Set how much of Hyperbole Smart Key behavior is enabled in Org mode.")
         '("Publish"        hywiki-publish-to-html
	   "Publish modified pages in the HyWiki to HTML; prefix arg to publish all pages.")
	 '("TagFind"        hywiki-tags-view
	   "Find HyWiki Org tags.")
	 (when (fboundp 'consult-grep) ;; allow for autoloading
	   '("WikiWordConsult" hywiki-word-consult-grep
	     "Use `hywiki-consult-grep' to show occurrences of a prompted for HyWikiWord."))))
  "*Hyperbole minibuffer HyWiki menu items of the form:
\(LABEL-STRING ACTION-SEXP DOC-STR)."
  :set  (lambda (var value)
	  (if (fboundp #'hyperbole-minibuffer-menu)
	      (progn (set-default var value)
		     (hyperbole-minibuffer-menu))
	    (set-default var value)))
  :type '(cons (list string) (repeat (list string sexp string)))
  :group 'hyperbole-buttons)

(unless hui:menu-rolo
  (makunbound 'hui:menu-rolo))
(defcustom hui:menu-rolo
  (delq nil
	(list
	 '("Rolo>")
	 '("Add"              hyrolo-add	            "Add a new rolo entry.")
	 (when (fboundp 'consult-grep) ;; allow for autoloading
           '("ConsultFind"      hyrolo-consult-grep           "Interactively narrow HyRolo matches using Consult/Vertico."))
	 '("Display"          hyrolo-display-matches        "Display last found rolo matches again.")
	 '("Edit"             hyrolo-edit                   "Edit an existing rolo entry.")
	 '("File"             hyrolo-find-file              "Edit an existing rolo file.")
	 (when (fboundp 'helm-org-rifle-files) ;; allow for autoloading
           '("HelmFind"         hyrolo-helm-org-rifle         "Interactively narrow HyRolo matches using Helm."))
	 '("Info"             (id-info "(hyperbole)HyRolo") "Display manual section on HyRolo.")
	 '("Kill"             hyrolo-kill                   "Kill an existing rolo entry.")
	 '("Mail"             hyrolo-mail-to                "Mail to address following point.")
	 '("Order"            hyrolo-sort                   "Order rolo entries in a file.")
	 '("RegexFind"        hyrolo-grep                   "Find entries containing a regexp.")
	 '("StringFind"       hyrolo-fgrep                  "Find entries containing a string.")
	 '("TagFind"          hyrolo-tags-view              "Find HyRolo Org tags.")
	 '("WordFind"         hyrolo-word                   "Find entries containing words.")
	 '("Yank"             hyrolo-yank                   "Find an entry containing a string and insert it at point.")))
  "*Hyperbole minibuffer Rolo menu items of the form:
\(LABEL-STRING ACTION-SEXP DOC-STR)."
  :set  (lambda (var value)
	  (if (fboundp #'hyperbole-minibuffer-menu)
	      (progn (set-default var value)
		     (hyperbole-minibuffer-menu))
	    (set-default var value)))
  :type '(cons (list string) (repeat (list string sexp string)))
  :group 'hyperbole-buttons)

(defcustom hui:menu-to
      '(("To>")
	("Agenda-or-Search"           org-agenda)
	("Bookmarks"                  bookmark-jump)
	("Calendar"                   calendar)
	("Directories"                hui:menu-to-personal-section)
	;; ("E")
	("recent-Files"               recentf-open-files)
	("Global-Buttons"             (find-file (expand-file-name hbmap:filename hbmap:dir-user)))
	;; ("Helm"                       (menu . helm) "Display Hyperbole helm control menu")
	;; ("I")
	("Jump-to-Websites"           webjump)
	("Koutlines"                  hui:menu-to-personal-section)
	;; ("L")
	("buffer-Menu-Filter")
	("Notes"                      hyrolo-org)
	("Org-Search"                 helm-org-rifle-org-directory)
	("Projects"                   hui:menu-to-personal-section)
	("Rolo"                       hyrolo-fgrep)
	;; ("<Quit-Menu>")
	("Shell-Commands"             hui:menu-to-personal-section)
	("Todos"                      org-todo-list)
	("URL-Links"                  hui:menu-to-personal-section)
	;; ("V")
	("Web-Search/"                (menu . web) "Display Hyperbole web search menu")
	;; ("X")
	;; ("Y")
	;; ("Zettelkasten-Search")
	)
      "*Hyperbole minibuffer To menu items of the form:
\(LABEL-STRING ACTION-SEXP DOC-STR)."
  :set  (lambda (var value)
	  (if (fboundp #'hyperbole-minibuffer-menu)
	      (progn (set-default var value)
		     (hyperbole-minibuffer-menu))
	    (set-default var value)))
  :type '(cons (list string) (repeat (list string sexp string)))
  :group 'hyperbole-buttons)

(defcustom hui:doc-a-z
      '(("a-Z>")
	("Apropos-Symbol"             hypb:helm-apropos)
	;; ("B")
	;; ("C")
	("Devdocs-Lookup"             hypb:devdocs-lookup)
	("Emacs-Index-Search"         emacs-index-search)
	;; ("F")
	;; ("G")
	;; ("H")
	("Info-Search"                hypb:helm-info)
	;; ("J")
	;; ("K")
	;; ("L")
	;; ("M")
	;; ("N")
	;; ("O")
	;; ("P")
	("<Quit-Menu>")
	;; ("R")
	;; ("S")
	;; ("T")
	;; ("U")
	;; ("V")
	;; ("W")
	;; ("X")
	;; ("Y")
	;; ("Z")
	)
      "*Hyperbole minibuffer To menu items of the form:
\(LABEL-STRING ACTION-SEXP DOC-STR)."
  :set  (lambda (var value)
	  (if (fboundp #'hyperbole-minibuffer-menu)
	      (progn (set-default var value)
		     (hyperbole-minibuffer-menu))
	    (set-default var value)))
  :type '(cons (list string) (repeat (list string sexp)))
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Initializations
;;; ************************************************************************

;; Always rebuild the Hyperbole minibuffer menu when this file is loaded.
(hyperbole-minibuffer-menu)

(provide 'hui-mini)

;;; hui-mini.el ends here
