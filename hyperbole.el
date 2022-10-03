;;; hyperbole.el --- GNU Hyperbole: The Everyday Hypertextual Information Manager  -*- lexical-binding: t; -*-

;; Copyright (C) 1992-2022  Free Software Foundation, Inc.

;; Author:           Bob Weiner
;; Maintainer:       Bob Weiner <rsw@gnu.org>, Mats Lidell <matsl@gnu.org>
;; Created:          06-Oct-92 at 11:52:51
;; Last-Mod:      6-Aug-22 at 21:23:08 by Mats Lidell
;; Released:         01-May-22
;; Version:          8.0.1pre
;; Keywords:         comm, convenience, files, frames, hypermedia, languages, mail, matching, mouse, multimedia, outlines, tools, wp
;; Package:          hyperbole
;; Package-Requires: ((emacs "27.0"))
;; URL:              http://www.gnu.org/software/hyperbole

;; See the "HY-COPY" file for license information.

;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; GNU Hyperbole (pronounced Gnew Hi-per-bo-lee), or just Hyperbole, is like
;; Markdown for hypertext, letting you use little or no markup in files to
;; produce easy-to-navigate hypermedia files.  It is an easy-to-use, yet
;; powerful and programmable hypertextual information management system
;; implemented as a GNU Emacs package.  It offers rapid views and interlinking
;; of all kinds of textual information, utilizing Emacs for editing.  It can
;; dramatically increase your productivity and greatly reduce the number of
;; keyboard/mouse keys you'll need to work efficiently.
;; 
;; Hyperbole lets you:
;; 
;; 1. Quickly create hyperlink buttons either from the keyboard or by dragging
;; between a source and destination window with a mouse button depressed.
;; Later activate buttons by pressing/clicking on them or by giving the name of
;; the button.
;; 
;; 2. Activate many kinds of `implicit buttons' recognized by context within
;; text buffers, e.g. URLs, grep output lines, and git commits.  A single key
;; or mouse button automatically does the right thing in dozens of contexts;
;; just press and go.
;; 
;; 3. Build outlines with multi-level numbered outline nodes, e.g. 1.4.8.6,
;; that all renumber automatically as any node or tree is moved in the
;; outline.  Each node also has a permanent hyperlink anchor that you can
;; reference from any other node;
;; 
;; 4. Manage all your contacts quickly with hierarchical categories and embed
;; hyperlinks within each entry.  Or create an archive of documents with
;; hierarchical entries and use the same search mechanism to quickly find any
;; matching entry;
;; 
;; 5. Use single keys to easily manage your Emacs windows or frames and quickly
;; retrieve saved window and frame configurations;
;; 
;; 6. Search for things in your current buffers, in a directory tree or across
;; major web search engines with the touch of a few keys.
;; 
;; The common thread in all these features is making retrieval, management and
;; display of information fast and easy.  That is Hyperbole's purpose.
;; 
;; ----
;;
;; See the "INSTALL" file for installation instructions and the "README" file
;; for general information.
;;
;; There is no need to manually edit this file unless there are specific
;; customizations you would like to make, such as whether a Hyperbole Action
;; Mouse Key is bound to the middle mouse button.  (See the call of the
;; function, `hmouse-install', below).
;;
;; Other site-specific customizations belong in "hsettings.el".

;;; Code:
;;; ************************************************************************
;;; Start Initializations
;;; ************************************************************************

(defconst hyperbole-loading t
  "Temporary constant available for testing while Hyperbole is loading.")

;; Ensure defgroup and defcustom are defined for use throughout Hyperbole.
(require 'custom)

(defgroup hyperbole nil
  "Hyperbole customizations category."
  :group 'applications)

(defgroup hyperbole-buttons nil
  "Hyperbole explicit, global and implicit button customizations."
  :group 'hyperbole)

(defgroup hyperbole-commands nil
  "Hyperbole command customizations."
  :group 'hyperbole)

(defgroup hyperbole-keys nil
  "Hyperbole keyboard and mouse key customizations."
  :group 'hyperbole)

;; defgroup hyperbole-rolo is in "hyrolo.el".

(defgroup hyperbole-screen nil
  "Hyperbole screen/display customizations, typically frame or window-related."
  :group 'hyperbole)

;; Reinitialize hyperb:dir on reload if initialization failed for any reason.
(eval-and-compile
  (when (and (boundp 'hyperb:dir) (null hyperb:dir))
    (makunbound 'hyperb:dir)
    (setq features (delq 'hload-path features)
	  features (delq 'hversion features)))

  ;; Defines hyperb:path-being-loaded, hyperb:stack-frame,
  ;; (hyperb:window-system) and hyperb:dir, which are used later in
  ;; this file.  Also adds Hyperbole to the load-path if need be.
  ;;
  ;; This handles the case when the Hyperbole package directory is not yet in load-path.
  (unless (or (require 'hversion nil t)
	      (and (stringp load-file-name)
		   (require 'hversion (expand-file-name
				       "hversion"
				       (file-name-directory load-file-name))
			    t)))
    (error "(Hyperbole): Startup failure: `hyperb:dir' must be manually added to `load-path' to fix")))

(defgroup hyperbole-koutliner nil
  "Hyperbole multi-level autonumbered outliner customizations."
  :group 'hyperbole)

(defvar hyperbole-mode-map (make-sparse-keymap)
  "Keymap for the GNU Hyperbole global minor mode.
See `hkey-initialize'.")

(defcustom hyperbole-mode-lighter " Hypb"
  "String to display in mode line when the Hyperbole global minor mode is enabled.
  Use nil for no Hyperbole mode indicator."
  :type 'string
  :group 'hyperbole)

;;;###autoload
(define-minor-mode hyperbole-mode
  "Toggle Hyperbole global minor mode.

Hyperbole is the Everyday Hypertextual Information Manager.

When Hyperbole mode is enabled, the `hyperbole-mode' variable
is non-nil, Hyperbole menus are enabled, as are Hyperbole keys.

Invoke the Hyperbole minibuffer menu with \\[hyperbole].  See the
Info documentation at \"(hyperbole)Top\".

\\{hyperbole-mode-map}"
  :global t
  :keymap 'hyperbole-mode-map
  :lighter hyperbole-mode-lighter
  :require 'hyperbole
  (if hyperbole-mode
      (hyperbole--enable-mode)
    (hyperbole--disable-mode)))

(defvar hyperbole--mark-even-if-inactive
  "Stores value of `mark-even-if-inactive' prior to enabling `hyperbole-mode'.")

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hload-path)
;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))
(require 'hypb)
(require 'hui-select)  ;; This requires 'hvar which defines the var:append function.

;;; ************************************************************************
;;; Public Variables
;;; ************************************************************************

(defcustom hkey-init t
  "*A non-nil value (default) at system load time binds Hyperbole keys.
Keys bound are the Action and Assist Keyboard Keys, as well as
other keys.  {\\[hkey-either]} invokes the Action Key and
{C-u \\[hkey-either]} invokes the Assist Key.  Additionally,
{\\[hkey-help]} shows what the Action Key will do in the current
context (wherever point is).  {C-u \\[hkey-help]} shows what the
Assist Key will do."
  :type 'boolean
  :group 'hyperbole-keys)

;;; ************************************************************************
;;; Public key bindings
;;; ************************************************************************

(defun hkey-get-bindings ()
  "Return a keymap of Hyperbole key bindings.
`hkey-initialize' must have already been called or the list will be empty."
  hyperbole-mode-map)

(make-obsolete 'hkey-global-set-key 'hkey-set-key "8.0.0")
(defun hkey-global-set-key (key command &optional _no-add)
  "Define a Hyperbole KEY bound to COMMAND.  Optional third arg, NO-ADD is ignored."
  (define-key hyperbole-mode-map key command))

(make-obsolete 'hkey-maybe-global-set-key 'hkey-maybe-set-key "8.0.0")
(defun hkey-maybe-global-set-key (key command &optional _no-add)
  "Define a Hyperbole KEY bound to COMMAND if KEY is not in `hyperbole-mode-map'.
Third argument NO-ADD is ignored."
  (hkey-maybe-set-key key command))

(defun hkey-maybe-set-key (key command &optional _no-add)
  "Define a Hyperbole KEY bound to COMMAND if KEY is not in `hyperbole-mode-map'.
Third argument NO-ADD is ignored."
  (let ((lookup-result (lookup-key hyperbole-mode-map key)))
    (cond ((integerp lookup-result)
	   (hypb:error "(hkey-maybe-set-key): Prefix key used is not defined: {%s}" (key-description key)))
	  ((null lookup-result)
	   (hkey-set-key key command)))))

(defun hkey-set-key (key command)
  "Define a Hyperbole global minor mode KEY bound to COMMAND."
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set Hyperbole key: " nil t)))
     (list key
           (read-command (format "Set key %s to command: "
                                 (key-description key))))))
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (prog1 (define-key hyperbole-mode-map key command)
    (when (called-interactively-p 'interactive)
      (message "{%s} set to invoke `%s' when Hyperbole is active"
	       (key-description key) command))))

(defvar hmouse-middle-flag)
(defvar hmouse-bindings-flag)
(defvar hyperb:user-email)

(defun hkey-initialize ()
  "If `hkey-init' is non-nil, initialize Hyperbole key bindings.
Some keys are conditionally bound only if there are no existing prior bindings
of the commands."
  (when hkey-init
    ;;
    ;; Setup so Hyperbole menus can be accessed from a key.  If not
    ;; already bound to a key, this typically binds the command `hyperbole'
    ;; globally to {C-h h} and activates Hyperbole minor mode.
    (unless (where-is-internal #'hyperbole (current-global-map))
      ;; In GNU Emacs, this binding replaces a command that shows
      ;; the word hello in foreign languages; this binding makes this
      ;; key much more useful.
      (global-set-key (vector help-char ?h) #'hyperbole))

    ;; Set C-c prefix key in hyperbole-mode-map for later key bindings
    (hkey-set-key "\C-c" (make-sparse-keymap))
    ;;
    ;; Bind the Action Key to {M-RET} and the Assist Key to {C-u M-RET}
    ;; and load the Hyperbole mouse key bindings.
    (unless (where-is-internal #'hkey-either)
      ;; Need to map all these variants to ensure can override
      ;; `org-meta-return' in Org mode when desired.
      (mapc (lambda (key) (hkey-set-key (kbd key) #'hkey-either))
	    '("\M-\C-m" "M-<return>" "M-RET" "ESC <return>" "ESC RET")))
    ;;
    ;; Typically bind the key, {C-h A}, for Action Key help and {C-u C-h A} for Assist key
    ;; help.
    (unless (where-is-internal #'hkey-help)
      (hkey-set-key (vector help-char ?A) #'hkey-help))
    ;;
    ;; Define virtual key used to activate hyperbole minor modeline menu
    ;; (hkey-set-key [hyperbole] (infodock-hyperbole-menu t))
    ;;
    ;; Provide a site standard way of emulating most Hyperbole mouse drag
    ;; commands from the keyboard.  This is most useful for rapidly creating
    ;; Hyperbole link buttons from the keyboard without invoking the Hyperbole
    ;; menu.  Works only if Hyperbole is run under a window system.
    (when (hyperb:window-system)
      (if (eq (global-key-binding "\M-o") #'facemenu-keymap)
	  ;; Override facemenu package that adds a keymap on M-o,
	  ;; since this binding is more important to Hyperbole
	  ;; users.
	  (hkey-set-key "\M-o" #'hkey-operate)
	(hkey-maybe-set-key "\M-o" #'hkey-operate)))
    ;;
    ;; Explicit button renames without invoking the Hyperbole menu.
    ;; No binding by default.
    ;; Don't override prior bindings of this key.
    ;; (hkey-maybe-set-key "\C-cr" #'hui:ebut-rename)
    ;;
    ;; Bind {C-c RET} to select larger and larger syntactical units in a
    ;; buffer when invoked repeatedly, showing in the minibuffer the type
    ;; of unit selected each time.
    (hkey-maybe-set-key "\C-c\C-m" #'hui-select-thing)
    ;;
    ;; Override the {M-w} command from "simple.el" when hyperbole-mode is active
    ;; to allow copying delimited things, kcell references or regions to the kill ring.
    (hkey-set-key [remap kill-ring-save] #'hui-kill-ring-save)
    ;;
    ;; Override the {C-x r s} command from "register.el" when hyperbole-mode is active
    ;; to allow copying delimited things, kcell references or regions to a register.
    (hkey-set-key "\C-xrs" #'hui-copy-to-register)
    ;;
    ;; Bind {C-c @} to create a user-specified sized grid of windows
    ;; displaying different buffers.
    ;;
    ;; Don't override prior bindings of this key.
    (hkey-maybe-set-key "\C-c@" #'hycontrol-windows-grid)
    ;;
    ;; Bind {C-c \} to interactively manage windows and frames.
    (hkey-maybe-set-key "\C-c\\" #'hycontrol-enable-windows-mode)
    ;;
    ;; Bind {C-c /} to display the Hyperbole Find/Web search menu.
    (hkey-maybe-set-key "\C-c/" #'hui-search-web)
    ;;
    ;; Bind {C-c .} to jump between the start and end of a delimited thing.
    ;; Don't override prior bindings of this key.
    (hkey-maybe-set-key "\C-c." #'hui-select-goto-matching-delimiter)
    ;;
    ;; Initialize the Smart Mouse Key bindings.  Shifted mouse buttons
    ;; are always set up.  Under InfoDock or with `hmouse-middle-flag'
    ;; non-nil, also bind the middle mouse button to the Action Key.
    ;; These bindings are ignored if a particular frame does not have mouse
    ;; support.
    (hmouse-install hmouse-middle-flag)
    ;;
    ;; Make a double or triple click of the left mouse button do the
    ;; same thing as {C-c RET}.  It also sets up Java, C++ and HTML modes
    ;; for proper entity selection.
    (hui-select-initialize)))

;;; ************************************************************************
;;; Load Hyperbole mouse bindings
;;; ************************************************************************

;; From mouse-position.c in Emacs:
;;    f = SELECTED_FRAME ();
;;    XSETFRAME (lispy_dummy, f);
;;
;;  It seems like the XSETFRAME macro is not properly copying the value of f on initial frame selection under the macOS window system.
;;  The problem occurs on other systems as well, e.g. Emacs 25.2 under Windows 7.
;;
;;  Hyperbole resolves this problem by setting the
;;  `mouse-position-function' variable below to properly set the
;;  newly selected frame.
(if (boundp 'mouse-position-function)
    (setq mouse-position-function
	  (lambda (frame-x-dot-y)
	    "Make `mouse-position' and `mouse-pixel-position' return the selected frame.
Under macOS and Windows 7 at least, upon initial selection of a new
frame, those functions by default still return the prior frame."
	    (if (consp frame-x-dot-y) (setcar frame-x-dot-y (selected-frame)))
	    frame-x-dot-y)))

;; hmouse-drv will load hui-mouse and hmouse-key
(mapc #'require '(hsettings hmouse-drv hmouse-sh))

;;; ************************************************************************
;;; You shouldn't need to modify anything below here.
;;; ************************************************************************

;; Add Hyperbole Info directory to Info-directory-list after the Info
;; manual reader package is loaded.
(eval-after-load "info"
  '(when (boundp 'hyperb:dir)
     (info-initialize)
     (let ((info-dir (expand-file-name "man/" hyperb:dir)))
       (if (file-exists-p info-dir)
	   (add-to-list 'Info-directory-list info-dir)))))

;;; ************************************************************************
;;; Display Hooks
;;; ************************************************************************

;; Permits restore of the prior window configuration after any help buffer
;; is shown by pressing either the Action or Assist Key at the end of the
;; help buffer.  (Help buffer names end with "Help*".)  Only one of
;; these two settings is used, dependent on emacs version.
;;
(add-hook 'temp-buffer-show-hook #'hkey-help-show)
(setq temp-buffer-show-function #'hkey-help-show)


;;; ************************************************************************
;;; Outline Mode Aliases
;;; ************************************************************************

(require 'outline)
(unless (fboundp 'outline-invisible-in-p)
  (defun outline-invisible-in-p (beg end)
    "Return t if there is an invisible character between BEG and END, else nil."
    (catch 'result
      (let ((p beg))
	(while (< p end) 
	  (when (eq (get-char-property p 'invisible) 'outline)
	    (throw 'result t))
	  (setq p (1+ p))))
      nil)))

;;; ************************************************************************
;;; Message System Support Configuration
;;; ************************************************************************

;; Even if you don't need some of the following hook settings, you might
;; as well leave them in so that if they ever become useful to you, you
;; need not reconfigure Hyperbole.  These settings do nothing if the
;; corresponding subsystems are never invoked.
;;
;; GNUS USENET news reader/poster support.
;;
(var:append 'gnus-Startup-hook '(Gnus-init))
;;
;; Hyperbole mail reader support configuration.
;;
;; Rmail
(var:append 'rmail-mode-hook    '(Rmail-init))
;; Mh-e
(var:append 'mh-inc-folder-hook '(Mh-init))
;;
;; VM support is based on V5.72 beta of VM.  If you have a version of VM
;; earlier than 5.70 beta, you should either upgrade or comment out the
;; following line so that Hyperbole support for VM is not enabled.
(var:append 'vm-mode-hook       '(Vm-init))
;;
;; Hyperbole mail composer support configuration.
;;
(var:append 'message-mode-hook   (list (lambda () (require 'hsmail))))
(var:append 'mh-letter-mode-hook (list (lambda () (require 'hsmail))))
(var:append 'vm-mail-mode-hook   (list (lambda () (require 'hsmail))))


;;; ************************************************************************
;;; URL Browsing
;;; ************************************************************************

(require 'browse-url)

;; Use any obsolete URL setting from earlier Hyperbole releases to set the
;; new URL browsing variable.
(when (and (boundp 'action-key-url-function) action-key-url-function
	   (eq action-key-url-function #'eww))
  (setq browse-url-browser-function #'browse-url-emacs))

;;; ************************************************************************
;;; Load Site-specific Configurations and Initialize Hyperbole Package
;;; ************************************************************************

(defconst hyperb:cygdrive '("\\`/cygdrive/" . "/"))

(defun hyperb:init ()
  "Initialize standard Hyperbole configuration."
  (interactive)
  (message "Initializing Hyperbole...")
  ;;
  (run-hooks 'hyperbole-init-hook)
  (hyperb:check-dir-user)
  (unless (stringp hyperb:user-email)
    (setq hyperb:user-email
	  (or (and (boundp 'user-mail-address)
		   (stringp user-mail-address)
		   (string-match "@" user-mail-address)
		   user-mail-address)
	      (concat (user-login-name) (hypb:domain-name)))))
  ;;
  ;; Modify syntactic character pairs for use with implicit button activations.
  (hbut:modify-syntax)
  ;;
  ;; Conditionally initialize Hyperbole key bindings (when hkey-init is t).
  (hkey-initialize)
  ;;
  ;; The keymaps in `emulation-mode-map-alists' take precedence over
  ;; `minor-mode-map-alist'; add this only if other minor modes are
  ;; overriding Hyperbole keys.
  ;; (add-to-list 'emulation-mode-map-alists `((hyperbole-mode . ,hyperbole-mode-map)))
  ;;
  ;; Initialize this option after hyperbole-mode-map has been
  ;; initialized, if not yet set by the user.
  (when (eq hsys-org-enable-smart-keys 'unset)
    (customize-set-variable 'hsys-org-enable-smart-keys
			    (if (hsys-org-meta-return-shared-p)
				'buttons
			      t)))
  ;;
  ;; Hyperbole initialization is complete.
  (message "Initializing Hyperbole...done"))

;; This call loads the rest of the Hyperbole system.
(require 'hinit)

(defun hyperbole--enable-mode ()
  "Enable Hyperbole global minor mode."
  ;; Store the current value and set `mark-even-if-inactive' to nil so
  ;; can select delimited things if the region is not active when
  ;; hyperbole-mode is enabled.
  (setq hyperbole--mark-even-if-inactive mark-even-if-inactive
	mark-even-if-inactive nil)
  ;;
  ;; Abbreviate MSWindows /cygdrive mount point paths.
  (when (file-exists-p "/cygdrive")
    (add-to-list 'directory-abbrev-alist hyperb:cygdrive))
  ;; When running under a POSIX system with possible access to MSWindows servers,
  ;; cache valid MSWindows mount points.
  (hpath:cache-mswindows-mount-points)
  ;;
  ;; Save button attribute file whenever same dir file is saved and
  ;; `ebut:hattr-save' is non-nil.
  (add-hook (if (boundp 'write-file-functions)
		'write-file-functions
	      'write-file-hooks)
	    #'hattr:save t)
  ;;
  (hyperb:init-menubar))

(defun hyperbole--disable-mode ()
  "Disable Hyperbole keys, menus and hooks."
  ;; Deactivate hyperbole-mode
  ;; Delete Hyperbole menu from all menubars.
  (hui-menu-remove Hyperbole)
  ;;
  ;; Remove Hyperbole button comment from future outgoing mail.
  (when (boundp 'smail:comment) (setq smail:comment nil))
  (remove-hook 'after-init-hook #'hyperb:init)
  ;;
  (setq directory-abbrev-alist (remq hyperb:cygdrive
				     directory-abbrev-alist)
	hpath:posix-mount-point-to-mswindows-alist nil)
  ;;
  ;; Reset the value of `mark-even-if-inactive' if the user has not
  ;; changed it while Hyperbole was active.
  (unless mark-even-if-inactive
    (setq mark-even-if-inactive hyperbole--mark-even-if-inactive))
  ;;
  (remove-hook (if (boundp 'write-file-functions)
		   'write-file-functions
		 'write-file-hooks)
	       #'hattr:save))

;; This next expression initializes the Hyperbole keymap but does not
;; activate Hyperbole.  The only user-visible change it should make is
;; to globally bind {C-h h} to 'hyperbole' which when invoked will both
;; activate Hyperbole and show its minibuffer menu.
(if after-init-time
    ;; Initialize Hyperbole key bindings and hooks.
    (hyperb:init)
  ;; Initialize after other key bindings are loaded at startup.
  (add-hook 'after-init-hook #'hyperb:init t))

;; !! FIXME: This next expression activates Hyperbole for compatibility
;; with prior releases where (require 'hyperbole) was enough to
;; activate its key bindings.  However, loading a file should not
;; change Emacs's behavior, so after educating users to add this
;; next line to their Emacs initializations, it should be removed.
(hyperbole-mode 1)

(makunbound 'hyperbole-loading)

(provide 'hyperbole)

;;; hyperbole.el ends here
