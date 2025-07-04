;;; hui-menu.el --- Menubar menu of GNU Hyperbole commands -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    28-Oct-94 at 10:59:44
;; Last-Mod:     18-Jan-25 at 16:41:44 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 1994-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(hpath hui-jmenu hyrolo-menu browse-url easymenu)))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(declare-function gbut:label-list "hbut")
(declare-function ebut:list "hbut")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hui-menu-max-list-length 24
  "Positive integer that caps the length of a Hyperbole dynamic menu lists.")

(defvar hui-menu-order-explicit-buttons t
  "When non-nil (default), explicit button menu list is lexicographically ordered.
Otherwise, explicit buttons are listed in their order of appearance within
the current buffer.")

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defmacro hui-menu-browser (title browser-option)
  "Browser menu with a TITLE.
BROWSER-OPTION marks current active menu option as selected."
  `(list
    (list ,title
	  ["Chrome (Google)"
	   (setq ,browser-option #'browse-url-chrome)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-chrome)]
	  ["Chromium"
	   (setq ,browser-option #'browse-url-chromium)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-chromium)]
	  ["Default (System wide)"
	   (setq ,browser-option #'browse-url-default-browser)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-default-browser)]
	  ["EWW (Emacs)"
	   (setq ,browser-option #'eww-browse-url)
	   :style radio
	   :selected (eq ,browser-option #'eww-browse-url)]
	  ;; Whatever browse-url-text-browser is set to, default is Lynx
	  ["Emacs Text Browser"
	   (setq ,browser-option #'browse-url-text-emacs)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-text-emacs)]
	  ["Firefox"
	   (setq ,browser-option #'browse-url-firefox)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-firefox)]
	  ["KDE"
	   (setq ,browser-option #'browse-url-kde)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-kde)]
	  ["XTerm Text Browser"
	   (setq ,browser-option #'browse-url-text-xterm)
	   :style radio
	   :selected (eq ,browser-option #'browse-text-xterm)]
	  "----"
	  ["Toggle-URLs-in-New-Window"
	   (setq browse-url-new-window-flag (not browse-url-new-window-flag))
	   :style toggle
	   :selected browse-url-new-window-flag])))

;; List explicit buttons in the current buffer for menu activation.
(defun hui-menu-explicit-buttons (rest-of-menu)
  "Explicit button menu to go before REST-OF-MENU."
  (delq nil
	(append
	 '(["Manual"   (id-info "(hyperbole)Explicit Buttons") t]
	   "----")
	 (let ((labels (ebut:list))
	       (cutoff))
	   (if labels
	       (progn
		 ;; Cutoff list if too long.
		 (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) labels))
		     (setcdr cutoff nil))
		 (delq nil
		       (append
			'("----"
			  ["Alphabetize-List"
			   (setq hui-menu-order-explicit-buttons
				 (not hui-menu-order-explicit-buttons))
			   :style toggle :selected hui-menu-order-explicit-buttons]
			  "Activate:")
			(mapcar (lambda (label) (vector label `(ebut:act-label ,label) t))
				(if hui-menu-order-explicit-buttons
				    (sort labels #'string-lessp)
				  labels))
			(if cutoff '(". . ."))
			'("----" "----"))))))
	 rest-of-menu)))

(defun hui-menu-cutoff-list (lst)
  "If list LST is longer than, `hui-menu-max-list-length', then cut it off there.
Return t if cutoff, else nil."
  (let ((cutoff))
    (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) lst))
	(setcdr cutoff nil))
    (if cutoff t)))

;; List existing global buttons for menu activation.
(defun hui-menu-global-buttons (rest-of-menu)
  "Global button menu to go before REST-OF-MENU."
  (delq nil
	(append
	 '(["Manual" (id-info "(hyperbole)Global Buttons") t]
	   "----")
	 (let ((labels (gbut:label-list))
	       (cutoff))
	   (when labels
	     ;; Cutoff list if too long.
	     (setq cutoff (hui-menu-cutoff-list labels))
	     (delq nil (append
			'("----" "Activate:")
			(mapcar (lambda (label) (vector label `(gbut:act ,label) t))
				(sort labels 'string-lessp))
			(if cutoff '(". . ."))
			'("----" "----")))))
	 rest-of-menu)))

(defun hui-menu-key-binding-item (item-name command)
  "Return a key binding menu item string built from ITEM-NAME and COMMAND."
  (concat item-name (when (where-is-internal command nil t)
		      (format "(%s)" (key-description (where-is-internal command nil t))))))

(defun hui-menu-key-bindings (rest-of-menu)
  "Key binding menu to go before REST-OF-MENU."
  (nconc
   (list
    (vector (hui-menu-key-binding-item "Action-Key          \t\t\t" 'hkey-either)                        '(hui:bind-key #'hkey-either) t)                    ;; {M-RET}
    (vector (hui-menu-key-binding-item "Button-Rename-Key   \t"     'hui:ebut-rename)                    '(hui:bind-key #'hui:ebut-rename) t)                ;; None
    (vector (hui-menu-key-binding-item "Drag-Emulation-Key  \t\t"   'hkey-operate)                       '(hui:bind-key #'hkey-operate) t)                   ;; {M-o}
    (vector (hui-menu-key-binding-item "Find-Web-Key        \t\t"   'hui-search-web)                     '(hui:bind-key #'hui-search-web) t)                 ;; {C-c /}
    (vector (hui-menu-key-binding-item "Grid-of-Windows-Key \t"     'hycontrol-windows-grid)             '(hui:bind-key #'hycontrol-windows-grid) t)         ;; {C-c @}
    (vector (hui-menu-key-binding-item "Hyperbole-Menu-Key  \t"     'hyperbole)                          '(hui:global-bind-key #'hyperbole) t)                      ;; {C-h h}
    (vector (hui-menu-key-binding-item "Jump-Thing-Key      \t\t"   'hui-select-goto-matching-delimiter) '(hui:bind-key #'hui-select-thing) t)               ;; {C-c .}
    (vector (hui-menu-key-binding-item "Mark-Thing-Key      \t\t"   'hui-select-thing)                   '(hui:bind-key #'hui-select-thing) t)               ;; {C-c RET}
    (vector (hui-menu-key-binding-item "Smart-Help-Key      \t\t"   'hkey-help)                          '(hui:bind-key #'hkey-help) t)                      ;; {C-h A}
    (vector (hui-menu-key-binding-item "Windows-Control-Key\t"      'hycontrol-enable-windows-mode)      '(hui:bind-key #'hycontrol-enable-windows-mode) t)) ;; {C-C \}
   rest-of-menu))

;; Dynamically compute submenus for Screen menu
(defun hui-menu-screen (_ignored)
  "Screen menu."
  (list
   ["Manual" (id-info "(hyperbole)HyControl") t]
   "----"
   ["Control-Frames"  hycontrol-enable-frames-mode t]
   ["Control-Windows" hycontrol-enable-windows-mode t]
   "----"
   (hui-menu-of-buffers)
   (hui-menu-of-frames)
   (hui-menu-of-windows)))

(defun hui-menu-web-search ()
  "Web search pulldown menu."
  (let* (service
	 action)
    (mapcar (lambda (service-and-action)
	      (setq service (car service-and-action)
		    action  (cdr service-and-action))
	      (if (stringp action)
		  (vector service
			  (list #'hyperbole-web-search service nil)
			  t)
		(vector service
			;; a command symbol
			action
			t)))
	    hyperbole-web-search-alist)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hui-menu-about
  (vector (concat "About-Hyperbole-"
		  (if (= (aref hyperb:version 0) ?0)
		      (substring hyperb:version 1)
		    hyperb:version))
	  '(hypb:display-file-with-logo "HY-ABOUT")
	  t))

(defconst hui-menu-org-meta-return-options
  '("Org M-RET Overrides"
     "----"
     "----"
     ["All-Hyperbole-Contexts"
      (customize-save-variable 'hsys-org-enable-smart-keys t)
      :style radio :selected (when (boundp 'hsys-org-enable-smart-keys)
			       (eq hsys-org-enable-smart-keys t))]
     ["Hyperbole-Buttons-Only"
      (customize-save-variable 'hsys-org-enable-smart-keys :buttons)
      :style radio :selected (when (boundp 'hsys-org-enable-smart-keys)
			       (memq hsys-org-enable-smart-keys '(:buttons buttons)))]
     ["None"
      (customize-save-variable 'hsys-org-enable-smart-keys nil)
      :style radio :selected (when (boundp 'hsys-org-enable-smart-keys)
			       (eq hsys-org-enable-smart-keys nil))])
  "Settings for Hyperbole Smart Key overrides to {M-RET} within Org mode.")

(defconst hui-menu-options
  (append '(["All-Hyperbole-Options" (customize-browse 'hyperbole) t]
	     "----"
	    ["Hyperbole-on-Menubar"
	     (cond ((and (boundp 'menubar-configuration)
			 (not (memq 'Hyperbole menubar-configuration)))
		    ;; Hyperbole may be included as part of the menubar but
		    ;; may be invisible due to a menubar configuration
		    ;; setting.  Invoking this item should then make it
		    ;; visible.
		    (hyperb:init-menubar))
		   ((global-key-binding [menu-bar Hyperbole])
		    ;; Already on the menubar, remove it.
		    (hui-menu-remove Hyperbole))
		   (t;; Add it.
		    (hyperb:init-menubar)))
	     :style toggle
	     :selected
	      (cond ((boundp 'menubar-configuration)
		     (memq 'Hyperbole menubar-configuration))
		    (t
		     (and (global-key-binding [menu-bar Hyperbole]) t)))]
	    "----"
	    ["Find-File-Accepts-URLs"
	     hpath:find-file-urls-mode
	     :style toggle
	     :selected hpath:find-file-urls-mode]
	    "----")
	  '(("Change-Key-Bindings" :filter hui-menu-key-bindings))
	  '("----")
	  (list (cons "Display-Referents-in"
		      (mapcar (lambda (sym)
				(vector
				 (capitalize (symbol-name sym))
				 `(setq hpath:display-where ',sym)
				 :style 'radio
				 :selected `(eq hpath:display-where ',sym)))
			      (mapcar #'car hpath:display-where-alist))))
	  '("----")
	  (hui-menu-browser "Display-URLs-in" browse-url-browser-function)
	  '("----")
	  (hui-menu-browser "Display-Web-Searches-in" hyperbole-web-search-browser-function)
	  '("----")
	  hui-menu-org-meta-return-options
	  '("----")
	  '(("Smart-Key-Press-at-Eol"
	     "----"
	     "----"
	     ;; This menu may be loaded by InfoDock before hsettings.el has
	     ;; defined `smart-scroll-proportional'.  Handle that case
	     ;; with a conditional.
	     ["Scrolls-a-Windowful"
	      (setq smart-scroll-proportional nil)
	      :style radio :selected (when (boundp 'smart-scroll-proportional)
				       (null smart-scroll-proportional))]
	     ["Scrolls-Proportionally"
	      (setq smart-scroll-proportional t)
	      :style radio :selected (when (boundp 'smart-scroll-proportional)
				       smart-scroll-proportional)]))
	  '("----"
	    ["Toggle-Isearch-Invisible-Text" hypb:toggle-isearch-invisible
	     :visible (boundp 'isearch-invisible)
	     :style toggle :selected (when (boundp 'isearch-invisible)
				       isearch-invisible)]
	    ["Toggle-Messaging-Explicit-Buttons" hyperbole-toggle-messaging
	     :style toggle :selected (not inhibit-hyperbole-messaging)]
	    ["Toggle-Rolo-Dates" hyrolo-toggle-datestamps
	     :style toggle :selected (and (boundp 'hyrolo-add-hook)
					  (listp hyrolo-add-hook)
					  (memq 'hyrolo-set-date hyrolo-add-hook))]
	    ["Toggle-Smart-Key-Debug (HyDebug)" hkey-toggle-debug
	     :style toggle :selected hkey-debug]))
  "Untitled menu of Hyperbole options.")

(defconst hui-menu-hywiki
  (delq nil
	(list
	 ["Manual"               (id-info "(hyperbole)HyWiki") t]
	 "----"
	 ["Activate-HyWiki-Word" hywiki-word-activate t]
	 ["Create-HyWiki-Word"   hywiki-word-create-and-display t]
	 ["Edit-HyWiki-Pages"    hywiki-directory-edit t]
	 ["Find-HyWiki-Referent" hywiki-find-referent t]
	 (when (fboundp 'consult-grep) ;; allow for autoloading
	   ["Grep-Consult-Pages" hywiki-consult-grep t])
	 ["Help"                 hkey-help t]
	 ["HyWiki-Mode-Toggle"   hywiki-mode t]
	 ["HyWiki-Tag-Find"      hywiki-tags-view t]
	 ["Insert-HyWiki-Link"   hywiki-insert-link t]
	 hui-menu-org-meta-return-options
	 ["Publish-HyWiki"       hywiki-publish-to-html t]
	 (when (fboundp 'consult-grep) ;; allow for autoloading
	   ["HyWiki-Word-Consult"   hywiki-word-consult-grep t])))
  "Menu items for HyWiki editing and publishing.")

(defvar infodock-hyperbole-menu nil)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Add Hyperbole menu to menubar.
(defun hyperbole-menubar-menu ()
  "Add to or update the Hyperbole menu on the global menubar."
  (cond ((boundp 'menubar-configuration)
	 (unless (memq 'Hyperbole menubar-configuration)
	   ;; Hyperbole may be included as part of the menubar but
	   ;; may be invisible due to a menubar configuration
	   ;; setting.  Make it visible here.
	   (if (fboundp 'customize-set-variable)
	       (customize-set-variable 'menubar-configuration
				       (cons 'Hyperbole menubar-configuration))
	     (setq menubar-configuration
		   (cons 'Hyperbole menubar-configuration)))))
	(t (let ((add-before (cond ((and (boundp 'infodock-menubar-type)
					 (eq infodock-menubar-type 'menubar-infodock))
				    "Key")
				   ((global-key-binding [menu-bar Koutline])
				    "Koutline")
				   ((global-key-binding [menu-bar OO-Browser])
				    "OO-Browser"))))
             (easy-menu-add-item (current-global-map) '("menu-bar") (infodock-hyperbole-menu t) add-before))))
  ;; Force a menu-bar update.
  (force-mode-line-update))

(defun hyperbole-popup-menu (&optional rebuild-flag)
  "Popup the Hyperbole menubar menu.
With optional REBUILD-FLAG rebuild the menu."
  (interactive "P")
  (popup-menu (infodock-hyperbole-menu rebuild-flag)))

;;; Don't change this name; doing so will break the way InfoDock
;;; initializes the Hyperbole menu.
(defun infodock-hyperbole-menu (&optional rebuild-flag)
  "Return the Hyperbole menu for the global InfoDock menubar.
Uses any non-nil existing value of the menu unless optional
REBUILD-FLAG is non-nil, in which case the menu is rebuilt."
  (when (or rebuild-flag (null infodock-hyperbole-menu))
    (setq infodock-hyperbole-menu
	  (delq nil
		(list
		 "Hyperbole"
		 :config 'Hyperbole
		 hui-menu-about
		 ["Demonstration"  hyperbole-demo t]
		 ;; Comment InfoDock manual reference out until
		 ;; InfoDock is modernized for Emacs 25.
		 ;; (if (and (boundp 'infodock-version) infodock-version)
		 ;;     ["Manual"      (id-info "(infodock)Hyperbole Menu") t]
		 ;;   ["Manual"      (id-info "(hyperbole)Top") t])
		 ["Manual"         (id-info "(hyperbole)Top") t]
		 ["What-is-New?"   (hypb:display-file-with-logo "HY-NEWS") t]
		 ["Why-Use?"       (find-file
				    (expand-file-name "HY-WHY.kotl" hyperb:dir)) t]
		 "----"
		 ["Remove-This-Menu"
		  (progn
		    ;; Delete Hyperbole menu from all menubars.
		    (hui-menu-remove Hyperbole)
		    ;;
		    ;; Remove Hyperbole button comment from future outgoing mail.
		    (when (boundp 'smail:comment) (setq smail:comment nil)))
		  t]
		 "----"
		 ["Activate-Button-in-Buffer" hui:hbut-act t]
		 ["Back-to-Prior-Location" hhist:pop
		  (and (boundp '*hhist*) *hhist*)]
		 '("Button-File"
		   ["Manual"  (id-info "(hyperbole)Button Files") t]
		   "----"
		   ["Edit-Per-Directory-File" (find-file hbmap:filename) t]
		   ["Edit-Personal-File" (find-file
					  (expand-file-name
					   hbmap:filename hbmap:dir-user)) t])
		 (cons "Customize" hui-menu-options)
		 '("Documentation"
		   ["Manual"      (id-info "(hyperbole)Top") t]
		   "----"
		   ["About"       (hypb:display-file-with-logo "HY-ABOUT") t]
		   ["Concepts"    (find-file (expand-file-name
					      "HY-CONCEPTS.kotl" hyperb:dir)) t]
		   ["Demonstration"  hyperbole-demo t]
		   ["Files"       (hypb:display-file-with-logo "MANIFEST") t]
		   ["Glossary"    (id-info "(hyperbole)Glossary") t]
		   ["New-Features" (hypb:display-file-with-logo "HY-NEWS") t]
		   ["Smart-Key-Summary" (id-browse-file (hypb:hkey-help-file)) t]
		   ("Types"
		    ["Action-Types-Manual"
		     (id-info "(hyperbole)Action Types") t]
		    ["Implicit-Button-Types-Manual"
		     (id-info "(hyperbole)Implicit Buttons") t]
		    "----"
		    ["Show-Action-Types"
		     (hui:htype-help-current-window 'actypes) t]
		    ["Show-Implicit-Button-Types"
		     (hui:htype-help-current-window 'ibtypes 'no-sort) t])
		   ["WhyUse"     (find-file (expand-file-name "HY-WHY.kotl" hyperb:dir)) t])
		 '("Explicit-Button"
		   :filter hui-menu-explicit-buttons
		   ["Activate" hui:ebut-act t]
		   ["Create" hui:ebut-create t]
		   ["Delete" hui:ebut-delete t]
		   ["Edit"   hui:ebut-edit t]
		   ("Help"
		    ["Manual"   (id-info "(hyperbole)Location") t]
		    "----"
		    ["Buffer-Buttons"   (hui:hbut-report -1) t]
		    ["Current-Button"   (hui:hbut-report)    t]
		    ["Ordered-Buttons"  (hui:hbut-report 1)  t])
		   ["Link"   hui:ebut-link-directly t]
		   ["Rename" hui:ebut-rename t]
		   ["Search" hui:ebut-search t]
		   ["Types"
		    (hui:htype-help-current-window 'actypes) t])
		 (append
		  '("Find"
		    ["Manual"   (id-info-item "menu, Find") t]
		    "----"
		    ;; Show numbered line matches in all specified files.
		    ["Grep-Files"           hypb:rgrep t]
		    ;; Show numbered line matches for regexp in all file-based buffers.
		    ["Locate-Files"         locate t]
		    ;; Show numbered line matches for regexp in all file-based buffers.
		    ["Match-File-Buffers"   moccur t]
		    ;; Show numbered line matches for regexp from this buffer.
		    ["Occur-Here"           occur  t]
		    ;; Following point, remove all lines that match regexp.
		    ["Remove-Lines-Here"    hypb:remove-lines t]
		    ;; Following point, keep only lines that match regexp.
		    ["Save-Lines-Here"      hypb:save-lines t]
		    "----"
		    "Web-Search:")
		  (hui-menu-web-search))
		 '("Global-Button"
		   :filter hui-menu-global-buttons
		   ["Create" hui:gbut-create t]
		   ["Delete" hui:gbut-delete t]
		   ["Edit"   hui:gbut-edit t]
		   ["Help"   gbut:help t]
		   ["Link"   hui:gbut-link-directly t]
                   ["Rename" hui:gbut-rename t])
		 (cons "HyWiki" hui-menu-hywiki)
		 '("Implicit-Button"
		   ["Manual"   (id-info "(hyperbole)Implicit Buttons") t]
		   "----"
		   ["Activate" hui:ibut-act t]
		   ["Create"   hui:ibut-create t]
		   ["Delete-Type" (hui:htype-delete 'ibtypes) t]
		   ["Edit"   hui:ibut-edit t]
		   ["Help"   hkey-help t]
		   ["Link"   hui:ibut-link-directly t]
		   ["Name"   hui:ibut-label-create t]
		   ["Rename" hui:ibut-rename t]
		   ["Types"  (hui:htype-help 'ibtypes 'no-sort) t])
		 '("Koutliner"
		   ["Manual" (id-info "(hyperbole)Koutliner") t]
		   ["Example"   kotl-mode:example t]
		   "----"
		   ["Create-File"    kfile:find t]
		   ["View-File"      kfile:view t]
		   "----"
		   ["Collapse-Tree" (progn (kotl-mode:is-p)
					   (kotl-mode:hide-tree
					    (kcell-view:label)))
		    (eq major-mode 'kotl-mode)]
		   ["Create-Link" klink:create
		    (eq major-mode 'kotl-mode)]
		   ["Expand-All-Trees" kotl-mode:show-all
		    (eq major-mode 'kotl-mode)]
		   ["Expand-Tree" (progn (kotl-mode:is-p)
					 (kotl-mode:show-tree
					  (kcell-view:label)))
		    (eq major-mode 'kotl-mode)]
		   ["Show-Top-Level-Only" kotl-mode:hide-body
		    (eq major-mode 'kotl-mode)])
		 '("Mail-Lists"
		   ["Manual" (id-info "(hyperbole)Suggestion or Bug Reporting")
		    t]
		   "----"
		   ["Mail-to-Hyperbole-Users-List"
		    (hmail:compose "hyperbole-users@gnu.org" '(hact 'hyp-config)) t]
		   ["Mail-to-Hyperbole-Bug-Report-List"
		    (hmail:compose "bug-hyperbole@gnu.org" '(hact 'hyp-config)) t]
		   "----"
		   ["Join-Hyperbole-Users-List"
		    (hmail:compose "hyperbole-users-join@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t]
		   ["Join-Hyperbole-Bug-Report-List"
		    (hmail:compose "bug-hyperbole-join@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t]
		   "----"
		   ["Leave-Hyperbole-Users-List"
		    (hmail:compose "hyperbole-users-leave@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t]
		   ["Leave-Hyperbole-Bug-Report-List"
		    (hmail:compose "bug-hyperbole-leave@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t])
		 infodock-hyrolo-menu
		 '("Screen (HyControl)" :filter hui-menu-screen)
		 hui-menu-hywconfig)))))

(provide 'hui-menu)

;;; hui-menu.el ends here
