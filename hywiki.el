;;; hywiki.el --- Hyperbole's auto-wikiword note-taking system     -*- lexical-binding: t -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Acpr-24 at 22:41:13
;; Last-Mod:      4-May-25 at 10:46:18 by Bob Weiner
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Copyright (C) 2024-2025  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This is Hyperbole's markup-free personal Wiki system for note-taking
;;  and automatic wiki word highlighting and hyperlinking.  It uses Org
;;  mode for note taking and adds automatic hyperlinking of HyWikiWords
;;  within Org files in `hywiki-directory' (default = "~/hywiki"), where
;;  a HyWikiWord is a capitalized word that contains upper and lowercase
;;  letters only and has a corresponding HyWikiWord.org wiki page file
;;  below `hywiki-directory'.  HyWikiWords require no delimiters.
;;
;;  HyWikiWords are also recognized in text buffers after the global
;;  minor mode, `hywiki-mode' is enabled via {M-x hywiki-mode RET}.  To
;;  create or jump to a HyWiki page, simply type out a potential
;;  HyWikiWord or move point onto one and press the Action Key {M-RET}.
;;  This will create the associated page if it does not exist.  This
;;  also highlights any other instances of HyWikiWords across all
;;  visible Emacs windows.  HyWiki is built for scalability and has been
;;  tested to be performant with 10,000 HyWikiWords.
;;
;;  Once Hyperbole has been loaded and activated, HyWikiWords (with or
;;  without delimiters) are automatically highlighted and active in
;;  the following contexts:
;;    - HyWiki page buffers;
;;    - non-special text buffers, when `hywiki-mode' is enabled;
;;    - comments and strings in programming buffers, when
;;      `hywiki-mode' is enabled.
;;  
;;  As HyWikiWords are typed, highlighting occurs after a trailing
;;  whitespace or punctuation character is added, or when it is
;;  surrounded by a matching pair of characters such as curly braces
;;  or single square brackets.  Since Org links use double square
;;  brackets and Org targets use double or triple angle brackets,
;;  HyWikiWords within these delimiters are ignored.
;;
;;  You can also create Org links to HyWikiWords in any non-special text
;;  buffer by surrounding them with double square brackets and the
;;  'hy:' prefix, as in: [[hy:MyWikiWord]].  If you set
;;  `hywiki-org-link-type-required' to `nil', then you don't need the
;;  prefix, e.g. [[MyWikiWord]]; existing HyWiki page names then will
;;  override Org's standard handling of such links.  To prevent Org
;;  mode's binding of {M-RET} from splitting lines and creating new
;;  headlines when on a HyWiki word whose page has not yet been
;;  created, set `hsys-org-enable-smart-keys' to `t' so that
;;  Hyperbole's Action Key does the right thing in this context.
;;
;;  HyWikiWord links can also link to a section headline within a page
;;  by simply following the page name with a '#' character and then the
;;  section headline name.  For example, if your Emacs page has a "Major
;;  Modes" section, then either Emacs#Major-Modes or [[hy:Emacs#Major
;;  Modes]] will work as a link to that section.  Note that without the
;;  square bracket delimiters, you must convert spaces in section names
;;  to '-' characters.  As long as the page exists, section links are
;;  highlighted regardless of whether associated sections exist or not.
;;  When activating a link with a section reference, you will get an
;;  error if the section does not exist.
;;
;;  The custom setting, `hywiki-word-highlight-flag' (default = t),
;;  means HyWikiWords will be auto-highlighted within HyWiki pages.
;;  Outside of such pages, `hywiki-mode' must also be enabled for such
;;  auto-highlighting.  Auto-highlighting depends on pre- and
;;  `post-command-hook' settings.  If an error occurs running one of
;;  these, the associated hook is removed.  To restore the auto-highlight
;;  hooks either use {M-x hywiki-word-set-auto-highlighting RET} or
;;  {C-u C-h h h h m} to toggle `hywiki-mode'; this also enables
;;  auto-highlighting if `hywiki-word-highlight-flag' is non-nil.

;;  The custom setting, `hywiki-exclude-major-modes' (default = nil), is
;;  a list of major modes to exclude from HyWikiWord auto-highlighting
;;  and recognition.
;;
;;  Within programming modes, HyWikiWords are highlighted/hyperlinked
;;  within comments and double-quoted strings only.  For programming
;;  modes in which you want HyWikiWords recognized everywhere, add
;;  them to the custom setting, `hywiki-highlight-all-in-prog-modes'
;;  (default = '(lisp-interaction-mode)).
;;
;;  HyWiki adds two implicit button types to Hyperbole:
;;    `hywiki-word' - creates and displays HyWikiWord referents;
;;    `hywiki-existing-word' - display an existing HyWikiWord referent.
;;
;;  `hywiki-word' is one of the lowest priority implicit button types
;;  so that it triggers only when other types are not recognized first.
;;
;;  A HyWiki can be exported to HTML for publishing to the web via Org
;;  mode's publish a project feature.  {M-x hywiki-publish-to-html RET}
;;  will and that's it!  Add a prefix argument to force regeneration of all
;;  HyWiki pages, rather than only those that have been updated.
;;
;;  The full set of HyWiki-specific Org publish properties are set in
;;  the variable `hywiki-org-publish-project-alist'.  When the HyWiki
;;  code is loaded into Emacs, it automatically integrates these
;;  properties with Org's publishing framework, so when in a HyWiki
;;  page, you can use the standard {C-c C-e P p} current project publish
;;  command.
;;  
;;  There are a few publishing settings you can customize prior to
;;  loading Hyperbole's HyWiki code.
;;
;;  HyWiki html files are saved in:
;;    (hywiki-org-get-publish-property :publishing-directory)
;;  Customize this directory with:
;;    {M-x customize-variable RET hywiki-org-publishing-directory RET}.
;;
;;  HyWiki html files are generated by the function given by:
;;    (hywiki-org-get-publish-property :publishing-function)
;;  Customize the value of this function if necessary with:
;;    {M-x customize-variable RET hywiki-org-publishing-function RET}.
;;
;; This section summarizes HyWikiWord Actions based on the
;;
;; hywiki-referent-prompt-flag      When nil                   When t
;;  -------------------------------------------------------------------------------------
;;  Action Key              hywiki-word-create-and-display 
;;    or HyWiki/Create      Create Page and Display          Create Referent and Display
;;  Assist Key              hywiki-word-create-and-display
;;    or C-u HyWiki/Create  Create Referent and Display      Create Page and Display
;;  hywiki-word-create      hywiki-create-page with Msg      hywiki-create-referent with Msg
;;  C-u hywiki-word-create  hywiki-create-referent with Msg  hywiki-create-page with Msg

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hactypes)   ;; For `link-to-file-interactively'
(require 'hargs)
(require 'hasht)
(require 'hbut)       ;; For `hbut:syntax-table'
;; (unless (featurep 'hibtypes)
;;   (require 'hibtypes))   ;; For `pathname' and `pathname-line-and-column'
(require 'hpath)
(require 'hproperty)
(require 'hsys-consult)
(require 'hui)        ;; For `hui:actype'
(require 'hui-mini)   ;; For `hui:menu-act'
(require 'hypb)       ;; Requires `seq'
(require 'outline)    ;; For `outline-mode-syntax-table'
(require 'seq)        ;; For 'seq-contains-p' and 'seq-difference'
(require 'subr-x)     ;; For `string-remove-prefix'
(require 'thingatpt)

(eval-and-compile
  '(when (require 'company nil t)
     (add-to-list 'company-backends 'hywiki-company-hasht-backend)))

;;; ************************************************************************
;;; Public declarations
;;; ************************************************************************

(defvar action-key-modeline-buffer-id-function)  ;; "hui-mouse.el"
(defvar bookmark-current-bookmark)               ;; "bookmark.el"
(defvar hkey-value)                              ;; "hui-mouse.el"
(defvar hywiki-referent-menu nil)                ;; "hywiki.el"
(defvar org-agenda-buffer-tmp-name)              ;; "org-agenda.el"
(defvar org-export-with-broken-links)            ;; "ox.el"
(defvar org-publish-project-alist)               ;; "ox-publish.el"

(declare-function activities-completing-read "activities" (:prompt prompt :default default))
(declare-function activities-new "activities" (name))
(declare-function activities-resume "activities" (activity :resetp resetp))
(declare-function bookmark-completing-read "bookmark" (prompt &optional default))
(declare-function bookmark-location "bookmark" (bookmark-name-or-record))
(declare-function hsys-org-at-tags-p "hsys-org")
(declare-function ibtypes::pathname "hpath")
(declare-function ibtypes::pathname-line-and-column "hpath")
(declare-function org-link-store-props "ol" (&rest plist))
(declare-function org-publish-property "ox-publish" (property project &optional default))
(declare-function org-roam-node-from-title-or-alias "org-roam-node" (s &optional nocase))
(declare-function org-roam-node-open "org-roam" (note &optional cmd force))
(declare-function org-roam-node-read "org-roam" (&optional initial-input filter-fn sort-fn require-match prompt))
(declare-function org-roam-node-title "org-roam-node" (node))
(declare-function smart-treemacs-edit "hui-treemacs" (&optional dir))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;; Must be set after `hywiki-get-buttonize-characters' is defined
(defconst hywiki--buttonize-characters nil
  "String of single character keys bound to `hywiki-buttonize-character-commands'.
Each such key self-inserts before highlighting any prior HyWiki word
in `hywiki-mode'.")

(defconst hywiki--buttonize-character-regexp nil
  "Regexp matching a single separating character following a HyWiki word.")

(defconst hywiki--word-and-buttonize-character-regexp nil
  "Regexp matching HyWikiWord#section plus a valid word separating character.
Group 1 is the entire HyWikiWord#section:Lnum:Cnum expression.")

(defvar hywiki--directory-checksum ""
  "String checksum for `hywiki-directory' page names.")

(defvar hywiki--directory-mod-time nil
  "Last mod time for `hywiki-directory' or nil if the value has not been read.
See `current-time' function for the mod time format.")

;; Redefine the `org-mode-syntax-table' for use in `hywiki-get-buttonize-characters'
;; so do not have to load all of Org mode there.
(defvar hywiki--org-mode-syntax-table
  (let ((st (make-syntax-table outline-mode-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?~ "_" st)
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    st)
  "Standard syntax table for Org mode buffers with HyWiki support.")

(defvar hywiki--pages-directory nil)
(defvar hywiki--referent-alist nil
  "HyWiki alist generated from `hywiki--referent-hasht' for storage in cache.
Each element is of the form: (wikiword . (referent-type . referent-value)).")
(defvar hywiki--referent-hasht nil
  "HyWiki hash table for fast WikiWord referent lookup.")

;; Globally set these values to avoid using 'let' with stack allocations
;; within `hywiki-maybe-highlight-page-name' frequently.
(defvar hywiki--any-wikiword-regexp-list nil)
(defvar hywiki--buts nil)
(defvar hywiki--but-end nil)
(defvar hywiki--but-start nil)
(defvar hywiki--buttonize-end (make-marker))   ;; This must always stay a marker
(defvar hywiki--buttonize-start (make-marker)) ;; This must always stay a marker
(defvar hywiki--current-page nil)
(defvar hywiki--end nil)
(defvar hywiki--flag nil)
(defvar hywiki--highlighting-done-flag t)
(defvar hywiki--word-pre-command nil)
(defvar hywiki--word-only nil)
(defvar hywiki--range nil)
(defvar hywiki--save-case-fold-search nil)
(defvar hywiki--save-org-link-type-required nil)
(defvar hywiki--start nil)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hywiki-word-highlight-flag t
  "The default, non-nil value treats HyWikiWords in HyWiki pages as hyperlinks.
A nil value disables HyWikiWord hyperlink buttons in both HyWiki
pages and all other buffers (since it also disables `hywiki-mode').

Outside of HyWiki pages, the global minor mode `hywiki-mode' must be
manually enabled for auto-HyWikiWord highlighting; programmatically,
use `(hywiki-mode 1) to enable it.

Use `hywiki-active-in-current-buffer-p' to determine if HyWikiWord
hyperlinks are currently active in a buffer or not.

Regardless of this flag, HyWikiWords in Org links and targets are not
highlighted nor treated as hyperlinks; they are handled normally by Org."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'hyperbole-hywiki)

(defcustom hywiki-exclude-major-modes nil
  "List of major modes to exclude from HyWiki word highlighting and recognition."
  :type '(list symbol)
  :group 'hyperbole-hywiki)

(defcustom hywiki-highlight-all-in-prog-modes '(lisp-interaction-mode)
  "List of programming major modes to highlight HyWikiWords outside of comments."
  :type '(list symbol)
  :group 'hyperbole-hywiki)

(defcustom hywiki-mode-lighter  " HyWiki"
  "String to display in mode line when the HyWiki global minor mode is enabled.
Use nil for no HyWiki mode indicator."
  :type 'string
  :group 'hyperbole-hywiki)

(defvar hywiki-allow-suffix-referent-types '(page path-link)
  "List of referent type symbols that support # and :L line number suffixes.")

(defvar hywiki-file-suffix ".org"
  "File suffix (including period) to use when creating HyWiki pages.")

;;;###autoload
(defun hywiki-let-directory (option value)
  (set option value)
  (hywiki-clear-referent-hasht)
  (hywiki-make-referent-hasht))

;;;###autoload
(defun hywiki-set-directory (option value)
  (unless (and (boundp 'hywiki-directory)
	       (equal hywiki-directory (file-name-as-directory value))
	       (hash-table-p hywiki--referent-hasht))
    (set-default option (file-name-as-directory value))
    (hywiki-clear-referent-hasht)
    (hywiki-make-referent-hasht))
  (hywiki-org-set-publish-project))

(defcustom hywiki-directory "~/hywiki/"
  "Directory that holds all HyWiki pages in Org format.
See `hywiki-org-publishing-directory' for exported pages in html format."
  :initialize #'custom-initialize-default
  :set #'hywiki-set-directory
  :type 'string
  :group 'hyperbole-hywiki)

(defun hywiki-directory-changed (option set-to-value operation _where)
  "Watch function for variable `hywiki-directory'.
Function is called with 4 arguments: (OPTION SET-TO-VALUE OPERATION WHERE)."
  (if (memq operation '(let unlet)) ;; not setting global value
      (hywiki-let-directory option set-to-value)
    (hywiki-set-directory option set-to-value)))

;; This next line is needed to invoke `hywiki-set-directory' when
;; `hywiki-directory' is changed via `setq' or `let' rather than
;; `customize-set-variable'.
(add-variable-watcher 'hywiki-directory #'hywiki-directory-changed)

(defvar-local hywiki-buffer-highlighted-state nil
  "State of HyWikiWords highlighting in the associated buffer.
\\='h means the buffer was already highlighted;
\\='d means the buffer was dehighlighted;
nil means no full buffer highlighting has occurred.")

(defvar hywiki-non-character-commands
  '(;; Org mode
    org-cycle                         ;; TAB
    org-open-line                     ;; C-o
    org-return                        ;; RET, \r
    org-return-and-maybe-indent       ;; C-j, \n
    ;; Markdown mode
    markdown-cycle                    ;; TAB
    markdown-enter-key                ;; RET, \r
    electric-newline-and-maybe-indent ;; C-j, \n
    ;; Global
    newline                           ;; RET, \r
    newline-and-indent                ;; RET, \r
    open-line                         ;; C-o
    quoted-insert                     ;; C-q
    )
  "Commands that insert characters but whose input events do not
  arrive as characters or that quote another character for input.")

;; Define the keymap for hywiki-mode.
(defvar hywiki-mode-map nil
  "Keymap for `hywiki-mode'.
Presently, there are no key bindings; this is for future use.")

(defconst hywiki-org-link-type "hy"
  "HyWiki string prefix type for Org links.  Excludes trailing colon.")

(defvar hywiki-org-link-type-required t
  "When t, [[hy:HyWiki Org links]] must start with `hywiki-org-link-type':.
Otherwise, this prefix is not needed and HyWiki word Org links
override standard Org link lookups.  See \"(org)Internal Links\".")

(defcustom hywiki-org-publishing-broken-links 'mark
  "HyWiki Org publish option that determines how invalid links are handled.
The default is \\='mark.

When this option is non-nil, broken HyWiki links are ignored,
without stopping the export process.  If it is set to \\='mark,
broken links are marked with a string like:

  [BROKEN LINK: path]

where PATH is the un-resolvable reference."
  :initialize #'custom-initialize-default
  :set (lambda (option value)
	 (set option value)
	 (hywiki-org-set-publish-project))
  :type 'symbol
  :group 'hyperbole-hywiki)

(defcustom hywiki-org-publishing-directory "~/public_hywiki"
  "Directory where HyWiki pages are converted into html and published."
  :initialize #'custom-initialize-default
  :set (lambda (option value)
	 (set option value)
	 (hywiki-org-set-publish-project))
  :type 'string
  :group 'hyperbole-hywiki)

(defcustom hywiki-org-publishing-function 'org-html-publish-to-html
  "HyWiki Org publish function used to export a HyWiki page to html."
  :initialize #'custom-initialize-default
  :set (lambda (option value)
	 (set option value)
	 (hywiki-org-set-publish-project))
  :type 'symbol
  :group 'hyperbole-hywiki)

(defcustom hywiki-org-publishing-sitemap-title
  (let ((dir-name (file-name-base
		   (directory-file-name
		    (file-name-as-directory hywiki-directory)))))
    (if (equal dir-name "hywiki")
	"HyWiki"
      dir-name))
  "HyWiki Org publish sitemap title."
  :initialize #'custom-initialize-default
  :set (lambda (option value)
	 (set option value)
	 (hywiki-org-set-publish-project))
  :type 'string
  :group 'hyperbole-hywiki)

(defvar hywiki-org-publish-project-alist nil
  "HyWiki-specific export properties added to `org-publish-project-alist'.")

(defun hywiki-org-make-publish-project-alist ()
  (setq org-export-with-broken-links hywiki-org-publishing-broken-links
	hywiki-org-publish-project-alist
	(list
	 "hywiki"
	 :auto-sitemap t
	 :base-directory (expand-file-name hywiki-directory)
	 :html-head (format
		     "<link rel=\"stylesheet\" type=\"text/css\" href=\"%sman/hyperbole.css\"/>"
		     hyperb:dir)
	 :html-link-home "index.html"
	 ;; :html-link-up "theindex.html"
	 ;; !! TODO: The :makeindex property is disabled for now, until a process is
	 ;; developed to force the Org publish process to regenerate the
	 ;; index after index entries are inserted into the temporary Org
	 ;; buffer prior to export to HTML.
	 :html-postamble t
	 :html-postable-format '(("en" "<p class=\"author\">Author: %a (%e)</p>
                                  <p class=\"last-mod\">Last Modified: %C</p>
                                  <p class=\"creator\">%c</p>"))
	 :makeindex nil
	 :publishing-directory hywiki-org-publishing-directory
	 :publishing-function hywiki-org-publishing-function
	 :section-numbers t
	 :shell "shell-command"
	 :sitemap-filename "index.org"
	 ;; sitemap (TOC) is stored in "sitemap.html"
	 :sitemap-title hywiki-org-publishing-sitemap-title
	 :with-date t
	 :with-toc nil)))

(defvar-local hywiki-page-flag nil
  "Set to t after finding a HyWiki page file, else nil.
The file must be below `hywiki-directory'.

For reference, this is set when `window-buffer-change-functions' calls
`hywiki-maybe-highlight-page-names' which calls `hywiki-in-page-p'.")

(defcustom hywiki-referent-prompt-flag nil
  "When t, the Action Key and HyWiki/Create always prompt for referent type.
Nil by default."
  :type 'boolean
  :initialize #'custom-initialize-default
  :group 'hyperbole-hywiki)

(defconst hywiki-word-regexp
  "\\<\\([[:upper:]][[:alpha:]]+\\)\\>"
  "Regexp that matches a HyWiki word only.
Do not use a start or end line/string anchor in this regexp.")

(defconst hywiki-word-section-regexp
  "\\(#[^][# \t\n\r\f]+\\)"
  "Regexp that matches a non-delimited HyWiki word #section extension.
After the first # character, this may contain any non-square-bracket,
non-# and non-whitespace characters.")

(defconst hywiki-word-line-and-column-numbers-regexp
  (concat "\\(" hpath:line-and-column-numbers "\\)")
  "Group 2 is the 1-based line number.
Group 4 is the optional 0-based column number.")

(defconst hywiki-word-with-optional-suffix-regexp
  (concat hywiki-word-regexp hywiki-word-section-regexp "??"
	  hywiki-word-line-and-column-numbers-regexp "?")
  "Regexp for a HyWiki word with an optional #section, :Lline-num, :Ccol-num.
Section may not contain whitespace or square brackets.  Use '-' to
substitute for spaces in the section/headline name.

Group 1 is the HyWiki word.
Group 2 is any optional #section with the # included.
Group 4 is any optional 1-based line number to jump to for any
file-based referents (relative to any section given).
Group 6 is any optional 0-based column number to jump to for any
file-based referents.")

(defconst hywiki-word-with-optional-suffix-exact-regexp
  (concat "\\`" hywiki-word-regexp "\\(#[^][\n\r\f]+\\)??"
	  hywiki-word-line-and-column-numbers-regexp "?\\'")
  "Exact match regexp for a HyWiki word with an optional #section.
The section may contain spaces or tabs but not square brackets;
it is preferable, however, to substitute '-' for whitespace in
the section/headline name to simplify recognition.

Group 1 is the HyWiki word.
Group 2 is any optional #section with the # included.
Group 4 is any optional 1-based line number to jump to for any
file-based referents (relative to any section given).
Group 6 is any optional 0-based column number to jump to for any
file-based referents.")

(defconst hywiki-word-suffix-regexp "\\(#\\|::\\|:L\\)\\(.+\\)\\'"
  "Regexp matching any trailing part of a HyWikiWord reference.
It may be a section or a line number reference.  Group one is the type
of reference and group two is the rest of the suffix reference.")

(defface hywiki--word-face
  '((((min-colors 88) (background dark)) (:foreground "orange"))
    (((background dark)) (:background "orange" :foreground "black"))
    (((min-colors 88)) (:foreground "orange"))
    (t (:background "orange")))
  "Face for HyWiki word highlighting."
  :group 'hyperbole-hywiki)

(defcustom hywiki-word-face 'hywiki--word-face
  "Hyperbole face for HyWiki word highlighting."
  :initialize #'custom-initialize-default
  :type 'face
  :group 'hyperbole-hywiki)

(defcustom hywiki-display-page-function #'hpath:find
  "Hyperbole function to display HyWiki page pathnames.
Only argument is the page's pathname."
  :initialize #'custom-initialize-default
  :type 'string
  :group 'hyperbole-hywiki)

(defcustom hywiki-allow-plurals-flag t
  "Non-nil means plural HyWikiWords have the same referent as the singular form.
Non-nil is the default."
  :initialize #'custom-initialize-default
  :set (lambda (option value)
	 (set option value)
	 (setq hywiki--any-wikiword-regexp-list nil))
  :type 'boolean
  :group 'hyperbole-hywiki)

;;; ************************************************************************
;;; hywiki minor mode and text edit command hooks
;;; ************************************************************************

(defun hywiki-debuttonize-non-character-commands ()
  "Store any HyWikiWord before or after point for later comparison.
Triggered by `pre-command-hook' for non-character commands, including
deletion commands and those in `hywiki-non-character-commands'."
  (setq hywiki--word-pre-command nil)
  (set-marker hywiki--buttonize-start nil)
  (set-marker hywiki--buttonize-end nil)

  (unless (hywiki-non-hook-context-p)
    ;; Record the WikiWord from any WikiWord ref that point is on
    (setq hywiki--word-pre-command (hywiki-get-singular-wikiword (hywiki-word-at)))
    (when (or (memq this-command hywiki-non-character-commands)
	      (and (symbolp this-command)
		   (string-match-p "^\\(org-\\)?\\(delete-\\|kill-\\)\\|\\(-delete\\|-kill\\)\\(-\\|$\\)" (symbol-name this-command))))
      ;; Test if at delimiters surrounding a WikiWord and if so,
      ;; record those for use by post hooks.
      (cl-destructuring-bind (start end)
	  ;; Get delimited region only if before or after delimiters,
	  ;; else return (nil nil).
	  (hywiki-at-range-delimiter) ;; includes delimiters
	;; Use these to store any range of a delimited HyWikiWord#section
	(set-marker hywiki--buttonize-start start)
	(set-marker hywiki--buttonize-end end)
	start)))
  (setq hywiki--flag nil))

(defun hywiki-buttonize-non-character-commands ()
  "Highlight any HyWikiWord before or after point as a Hyperbole button.
Triggered by `post-command-hook' for non-character-commands, including
deletion commands and those in `hywiki-non-character-commands'."
  (unless (or hywiki--flag (hywiki-non-hook-context-p))
    (when (or (memq this-command hywiki-non-character-commands)
	      (and (symbolp this-command)
		   (string-match-p "^\\(org-\\)?\\(delete-\\|kill-\\)\\|\\(-delete\\|-kill\\)\\(-\\|$\\)" (symbol-name this-command))))
      (setq hywiki--range nil)

      ;; Dehighlight any previously highlighted WikiWord at point
      ;; before we move to the start of any current WikiWord and
      ;; rehighlight that.
      (hywiki--maybe-dehighlight-at-point)

      (save-excursion
	(cond ((marker-position hywiki--buttonize-start)
	       ;; Point was before or after a WikiWord delimiter
	       (goto-char (1+ hywiki--buttonize-start)))
	      ((setq hywiki--range (hywiki-word-at :range))
	       (cl-destructuring-bind (_ start end)
		   hywiki--range
		 (if (and start end)
		     (progn
		       ;; On a non-delimited HyWikiWord
		       (set-marker hywiki--buttonize-start start)
		       (set-marker hywiki--buttonize-end end)
		       (goto-char start)
		       (skip-chars-backward "-" (line-beginning-position))
		       t)
		   (setq hywiki--range nil)))))

	(hywiki--maybe-rehighlight-at-point)))))

(defun hywiki-buttonize-character-commands ()
  "Turn any HyWikiWords between point into highlighted Hyperbole buttons.
Triggered by `post-self-insert-hook' after self-inserting one or more
characters after `post-command-hook' has run."
  ;; If `hywiki--flag' is set non-nil below, then
  ;; `hywiki-buttonize-non-character-commands' on `post-command-hook'
  ;; does nothing.
  (unless (setq hywiki--flag (hywiki-non-hook-context-p))
    (setq hywiki--range nil)

    ;; Dehighlight any previously highlighted WikiWord at point
    ;; before we move to the start of any current WikiWord and
    ;; rehighlight that.
    (hywiki--maybe-dehighlight-at-point)

    (save-excursion
      (cond ((marker-position hywiki--buttonize-start)
	     ;; Point was before or after a WikiWord delimiter
	     (goto-char hywiki--buttonize-start)
	     (skip-chars-backward "-" (line-beginning-position))
	     (goto-char (1- (point))))
	    ((not (equal (setq hywiki--range (hywiki-word-at :range))
			 '(nil nil nil)))
	     (cl-destructuring-bind (_ start end)
		 hywiki--range
	       (if (and start end)
		   (progn
		     ;; On a non-delimited HyWikiWord
		     (set-marker hywiki--buttonize-start start)
		     (set-marker hywiki--buttonize-end end)
		     (goto-char start)
		     (skip-chars-backward "-" (line-beginning-position))
		     t)
		 (setq hywiki--range nil))))
	    ((not (equal (setq hywiki--range (hywiki-at-range-delimiter)) ;; includes delimiters
			 '(nil nil)))
	     ;; At delimiters surrounding a WikiWord
	     (let ((start (nth 0 hywiki--range))
		   (end   (nth 1 hywiki--range)))
	       (when (and start end)
		 ;; Use these to store any range of a delimited HyWikiWord#section
		 (set-marker hywiki--buttonize-start (1+ start))
		 (set-marker hywiki--buttonize-end (1- end))))))

      ;; This first rehighlighting is needed to ensure
      ;; any wikiword before an inserted whitespace character is
      ;; properly highlighted when separating two words or after a
      ;; closing delimiter.
      (save-excursion
	(goto-char (max (1- (point)) (point-min)))
	(hywiki--maybe-rehighlight-at-point))

      (hywiki--maybe-rehighlight-at-point))))

(defun hywiki-buttonize-word (func start end face)
  "Create a HyWikiWord button by calling FUNC with START and END positions.
Function may apply FACE to highlight the button or may transform it
into an Org link, etc.  Function operates on the current buffer and
takes 3 arguments: `range-start', `range-end' and `face' to apply to
the button."
 (funcall func start end face))

(defun hywiki-get-buttonize-characters ()
  "Return a string of Org self-insert keys that have punctuation/symbol syntax."
  (let (key
	cmd
	key-cmds
	result)
    ;; Org and other text mode self-insert-command bindings are just
    ;; remaps inherited from global-map.  Create key-cmds list of
    ;; parsable (key . cmd) combinations where key may be a
    ;; (start-key . end-key) range of keys.
    (map-keymap (lambda (key cmd) (setq key-cmds (cons (cons key cmd) key-cmds))) (current-global-map))
    (dolist (key-cmd key-cmds (concat (seq-difference (nreverse result)
						      "-_*#:" #'=)))
      (setq key (car key-cmd)
	    cmd (cdr key-cmd))
      (when (eq cmd 'self-insert-command)
	(cond ((and (characterp key)
		    (eq (char-syntax key) ?.))
	       ;; char with punctuation/symbol syntax
	       (setq result (cons key result)))
	      ((and (consp key)
		    (characterp (car key))
		    (characterp (cdr key))
		    (<= (cdr key) 256))
	       ;; ASCII char range, some of which has punctuation/symbol syntax
	       (with-syntax-table hywiki--org-mode-syntax-table
		 (dolist (k (number-sequence (car key) (cdr key)))
		   (when (memq (char-syntax k) '(?. ?_))
		     (setq result (cons k result)))))))))))

(defun hywiki-non-hook-context-p ()
  (or (minibuffer-window-active-p (selected-window))
      (and (boundp 'edebug-active) edebug-active
	   (active-minibuffer-window))
      (and (derived-mode-p 'prog-mode)
	   (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes))
	   ;; Not inside a comment or a string
	   (not (or (nth 4 (syntax-ppss)) (hypb:in-string-p))))))

;;;###autoload
(define-minor-mode hywiki-mode
  "Toggle HyWiki global minor mode with \\[hywiki-mode].

HyWiki automatically highlights and turns instances of known
HyWikiWords into implicit buttons if they are within buffers with
files attached from `hywiki-directory'.  Such buttons either link
to HyWiki pages or activate typed referents such as bookmarks.

HyWiki Minor Mode enables the same behavior in most other text and
programming buffers except those with a major mode in
`hywiki-exclude-major-modes'.

HyWikiWord references may also include optional suffixes:

   - a #section reference that links to a HyWiki page Org headline or
     other outline file.  Spaces in the headline must be converted
     to dash characters for proper recognition;

   - optionally followed by :L<line-number>:C<column-number>
     where the column part is also optional.  If a section is
     given, the line number is relative to the section and the
     section headline is line 1.

When hywiki-mode is enabled, the `hywiki-mode' variable is
non-nil.

See the Info documentation at \"(hyperbole)HyWiki\".

\\{hywiki-mode-map}"
  :global t
  :lighter hywiki-mode-lighter
  :keymap hywiki-mode-map
  :group 'hyperbole-hywiki
  (if hywiki-mode
      ;; enable mode
      (progn
	;; Need hyperbole-mode
	(require 'hyperbole)
	(unless hyperbole-mode
	  (hyperbole-mode 1))
	(unless hywiki-mode-map
          (setq hywiki-mode-map (make-sparse-keymap)))
	;; Next line triggers a call to `hywiki-maybe-highlight-wikiwords-in-frame'
	(set-variable 'hywiki-word-highlight-flag t)
	(hywiki-word-set-auto-highlighting 1))
    ;; disable mode
    ;; Dehighlight HyWikiWords in this buffer when 'hywiki-mode' is
    ;; disabled and this is not a HyWiki page buffer. If this is a
    ;; HyWiki page buffer, then dehighlight when
    ;; `hywiki-word-highlight-flag' is nil.
    (hywiki-maybe-highlight-wikiwords-in-frame t)))

;;; ************************************************************************
;;; Public Implicit Button and Action Types
;;; ************************************************************************

(defib hywiki-word ()
  "When on a non-existing HyWikiWord, create it and display its referent.
If the associated HyWiki referent is a page, create it automatically
unless it is the first HyWiki page to be created, in which case,
prompt the user whether to create it, to prevent any unexpected HyWiki
use.

Existing HyWikiWords are handled by the implicit button type
`hywiki-existing-word'."
  (let* ((wikiword-start-end (hywiki-word-at t))
	 (wikiword (nth 0 wikiword-start-end))
	 (start    (nth 1 wikiword-start-end))
	 (end      (nth 2 wikiword-start-end)))
    (when wikiword
      (unless (or (ibtypes::pathname-line-and-column)
		  (ibtypes::pathname))
	(ibut:label-set wikiword start end)
	(hact 'hywiki-word-create-and-display wikiword)))))

(defun hywiki-display-referent-type (wikiword referent)
  "Display WIKIWORD REFERENT, a cons of (<referent-type> . <referent-value>).
Function used to display is \"hywiki-display-<referent-type>\"."
  (let* ((referent-type   (car referent)) ;; a symbol
	 (referent-value  (cdr referent))
	 (display-function (intern-soft (concat "hywiki-display-"
						(symbol-name referent-type)))))
    (when (equal (hywiki-get-singular-wikiword wikiword) (hywiki-word-at-point))
      ;; Set referent attributes of current implicit button
      (hattr:set 'hbut:current 'referent-type referent-type)
      (hattr:set 'hbut:current 'referent-value referent-value))
    (cond ((fboundp display-function)
	   (funcall display-function wikiword referent-value))
	  ((symbolp referent-type)
	   (error "(hywiki-display-referent-type): No hywiki-display function for referent type '%s'" referent-type))
	  (t
	   (error "(hywiki-display-referent-type): Referent type must be a symbol, not %s" referent-type)))))

(defun hywiki-display-referent (&optional wikiword prompt-flag)
  "Display HyWiki WIKIWORD or a regular file with WIKIWORD nil.
Return the WIKIWORD's referent if successfully found or nil otherwise.
The referent is a cons of (<referent-type> . <referent-value>).
For further details, see documentation for `hywiki-find-referent'.
After successfully finding a page and reading it into a buffer, run
`hywiki-display-referent-hook'."
  (let ((in-page-flag (null wikiword))
	(in-hywiki-directory-flag (hywiki-in-page-p)))
    (if (or (stringp wikiword) in-hywiki-directory-flag)
	(progn
	  (when in-page-flag
	    ;; Current buffer must be the desired page
	    (unless in-hywiki-directory-flag
	      (error "(hywiki-display-referent): No `wikiword' given; buffer file must be in `hywiki-directory', not %s"
		     default-directory))
	    (unless (hypb:buffer-file-name)
	      (error "(hywiki-display-referent): No `wikiword' given; buffer must have an attached file"))
	    (setq wikiword (file-name-sans-extension (file-name-nondirectory (hypb:buffer-file-name)))))
	  (let* ((_suffix (when (string-match hywiki-word-suffix-regexp wikiword)
			    (substring wikiword (match-beginning 0))))
		 (referent (cond (prompt-flag
				  (hywiki-create-referent wikiword))
				 ((hywiki-get-referent wikiword))
				 (t (hywiki-add-page wikiword)))))
	    (if (not referent)
		(error "(hywiki-display-referent): Invalid `%s' referent: %s"
		       wikiword referent)
 	      ;; Ensure highlight any page name at point in case called as a
	      ;; Hyperbole action type
	      (hywiki-maybe-highlight-page-name t)
	      (hywiki-display-referent-type wikiword referent)
	      (hywiki-maybe-highlight-page-names)
	      (run-hooks 'hywiki-display-referent-hook)
	      referent)))
      ;; When called without a wikiword and outside hywiki-directory,
      ;; just find as a regular file and use next line to highlight
      ;; HyWikiWords only if buffer was not previously highlighted.
      (hywiki-maybe-highlight-page-names)
      nil)))

;;; ************************************************************************
;;; Public referent menus and utility functions
;;; ************************************************************************

(unless hywiki-referent-menu
  (makunbound 'hywiki-referent-menu))
(defcustom hywiki-referent-menu
  (delq nil
	(list
	 '("HyWiki Add>")
	 (when (fboundp #'activities-new)
	   '("Activity"   (hywiki-add-activity hkey-value)
	     "Add a HyWikiWord that activates a saved activity from the Activities package."))
	 '("Bookmark"     (hywiki-add-bookmark hkey-value)
	   "Add a HyWikiWord that jumps to an Emacs bookmark.")
	 '("Command"      (hywiki-add-command hkey-value)
	   "Add a HyWikiWord that runs an Emacs command or Hyperbole action type.")
	 '("Find"         (hywiki-add-find hkey-value)
	   "Add a HyWikiWord that greps through `hywiki-directory' for its matches.")
	 ;; "<(global explicit button name)>"
	 ;; "<[global implicit button name]>"
	 '("Gbut"         (hywiki-add-global-button hkey-value)
	   "Add a HyWikiWord that activates a named Hyperbole global button.")
	 '("HyRolo"       (hywiki-add-hyrolo hkey-value)
	   "Add a HyWikiWord that searches `hyrolo-file-list' for matches.")
	 ;; "(hyperbole)action implicit button"
	 '("InfoIndex"    (hywiki-add-info-index hkey-value)
	   "Add a HyWikiWord that displays an Info index item.")
	 ;; "{key series}" wikiword
	 '("Keys"         (hywiki-add-key-series hkey-value)
	   "Add a HyWikiWord that executes a key series.")
	 '("pathLink"     (hywiki-add-path-link hkey-value)
	   "Add a HyWikiWord that links to a path and possible position.")
	 ;; "(hyperbole)Smart Keys"
	 '("infoNode"     (hywiki-add-info-node hkey-value)
	   "Add a HyWikiWord that displays an Info node.")
	 ;; "ID: org-id"
	 '("OrgID"        (hywiki-add-org-id hkey-value)
	   "Add a HyWikiWord that displays an Org section given its Org ID.")
	 ;; "pathname:line:col"
	 ;; "#in-buffer-section"
	 '("Page"         (hywiki-add-page hkey-value)
	   "Add/Reset a HyWikiWord to link to its standard HyWiki page.")
	 ;; e.g. (kbd "key sequence")
	 '("orgRoamNode"  (hywiki-add-org-roam-node hkey-value)
	   "Add a HyWikiWord that displays an Org Roam node given its title.")
	 '("Sexp"         (hywiki-add-sexpression hkey-value)
	   "Add a HyWikiWord that evaluates an Elisp sexpression.")))
  "*Menu of HyWikiWord custom referent types of the form:
\(LABEL-STRING ACTION-SEXP DOC-STR)."
  :set  (lambda (var value) (set-default var value))
  :type '(cons (list string) (repeat (list string sexp string)))
  :group 'hyperbole-buttons)

(defun hywiki-add-referent (wikiword referent)
  "Add WIKIWORD (sans any suffix) that displays REFERENT to HyWiki.
Return REFERENT if WIKIWORD is of valid format, otherwise return nil.
REFERENT must be a cons of (<referent-type> . <referent-value>) or
an error is triggered."
  (hywiki-validate-referent referent)
  (when (hywiki-word-is-p wikiword)
    (when (match-string-no-properties 2 wikiword)
      ;; Remove any #section suffix in PAGE-NAME.
      (setq wikiword (match-string-no-properties 1 wikiword)))
    (unless (hash-add referent (hywiki-get-singular-wikiword wikiword)
		      (hywiki-get-referent-hasht))
      (error "(hywiki-add-referent): Failed: (hash-add %s %s %s)"
	     referent (hywiki-get-singular-wikiword wikiword)
		      (hywiki-get-referent-hasht)))
    (setq hywiki--any-wikiword-regexp-list nil)
    (unless (hyperb:stack-frame '(hywiki-maybe-highlight-wikiwords-in-frame))
      (hywiki-cache-save)
      (hywiki-maybe-highlight-wikiwords-in-frame t))
    (run-hooks 'hywiki-add-referent-hook)
    referent))

(defun hywiki-create-referent (wikiword &optional message-flag)
  "Prompt for, add to HyWiki lookups and return a WIKIWORD custom referent.
With optional prefix arg MESSAGE-FLAG non-nil, display a minibuffer message
with the referent."
  (interactive (list nil current-prefix-arg))
  (unless (stringp wikiword)
    (setq wikiword (hywiki-word-read-new "Create/Edit HyWikiWord: ")))
  (setq hkey-value wikiword)
  (let ((referent
	 (hui:menu-act 'hywiki-referent-menu
		       (list (cons 'hywiki-referent-menu
				   (cons (list (format "%s RefType>"
						       (if (string-match hywiki-word-suffix-regexp wikiword)
							   (substring wikiword 0 (match-beginning 0))
							 wikiword)))
					 (cdr hywiki-referent-menu)))))))
    (if referent
	(when (or message-flag (called-interactively-p 'interactive))
	  (message "HyWikiWord '%s' referent: %S" wikiword referent))
      (user-error "(hywiki-create-referent): Invalid HyWikiWord: '%s'; must be capitalized, all alpha" wikiword))
    referent))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hywiki-active-in-current-buffer-p ()
  "Return non-nil if HyWiki word links are active in the current buffer.
Exclude the minibuffer if selected and return nil."
  (and hywiki-word-highlight-flag
       (not (minibuffer-window-active-p (selected-window)))
       (not (and (boundp 'edebug-active) edebug-active (active-minibuffer-window)))
       (or (derived-mode-p 'kotl-mode)
	   (not (eq (get major-mode 'mode-class) 'special)))
       (not (apply #'derived-mode-p hywiki-exclude-major-modes))
       (or hywiki-mode (hywiki-in-page-p))))

(defun hywiki-add-activity (wikiword)
  "Make WIKIWORD resume a prompted for activity.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the activity, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (hypb:require-package 'activities)
  (let ((activity (activities-completing-read :prompt "Resume activity" :default nil)))
    (hywiki-add-referent wikiword (cons 'activity activity))))

(defun hywiki-display-activity (_wikiword activity)
  (activities-resume activity :resetp nil))

(defun hywiki-add-bookmark (wikiword)
  "Make WIKIWORD display a bookmark at point and return the action.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the bookmark, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit Bookmark HyWikiWord: "))))
  (require 'bookmark)
  (if (string-empty-p wikiword)
      (error "(hywiki-add-bookmark): No bookmark specified")
    (bookmark-set wikiword)
    (hywiki-add-referent wikiword (cons 'bookmark wikiword))))

(defun hywiki-display-bookmark (_wikiword bookmark)
  (let ((loc (bookmark-location bookmark)))
    ;; Use Hyperbole-specified display location
    (cond ((bufferp loc)
	   (hpath:display-buffer loc))
	  ((get-buffer loc)
	   (hpath:display-buffer (get-buffer loc)))
	  ((stringp loc)
	   (hywiki-display-page loc)))
    (bookmark-jump bookmark)))

(defun hywiki-add-command (wikiword)
  "Set a custom command symbol for WIKIWORD and return it.
Command is the symbol used in the definition expression, which
may be an Emacs command or a Hyperbole action type.  When invoked,
it receives the single argument of WIKIWORD.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the actype, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (let ((command (hui:actype nil (format "Command for %s: " wikiword))))
    (hywiki-add-referent wikiword (cons 'command command))))

(defun hywiki-display-command (wikiword command)
  (if (fboundp command)
      (actype:act command wikiword)
    (error "(hywiki-display-command): Unbound referent command, '%s'" command)))

(defun hywiki-add-find (wikiword)
  "Make WIKIWORD grep across `hywiki-directory' for matches to itself.
Return the command to invoke.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the grep, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (hywiki-add-referent wikiword (cons 'find #'hywiki-word-grep)))

(defun hywiki-display-find (wikiword func)
  (if (fboundp func)
      (actype:act func wikiword)
    (error "(hywiki-display-find): Unbound referent function, '%s'" func)))

(defun hywiki-add-global-button (wikiword)
  "Make WIKIWORD evaluate a prompted for global button.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the button link, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (let ((gbut-name (hargs:read-match "Global button: "
				     (mapcar #'list (gbut:label-list))
				     nil t nil 'gbut)))
    (hywiki-add-referent wikiword (cons 'global-button gbut-name))))

(defun hywiki-display-global-button (_wikiword gbut-name)
  (gbut:act gbut-name))

(defun hywiki-add-hyrolo (wikiword)
  "Make WIKIWORD search and display `hyrolo-file-list' matches.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the hyrolo search, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (require 'hyrolo)
  ;; !! TODO: Change PaulAllenWinter to search for "Winter, Paul Allen".
  (hywiki-add-referent wikiword (cons 'hyrolo #'hyrolo-fgrep)))

(defun hywiki-display-hyrolo (wikiword search-func)
  (funcall search-func wikiword))

(defun hywiki-add-info-index (wikiword)
  "Make WIKIWORD display an Info manual index item and return it.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the Info index item, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (let ((item (save-window-excursion
		(info)
		(Info-read-index-item-name "Info index item: "))))
    (when (stringp item)
      (unless (= (aref item 0) ?\()
	(setq item (format "(%s)%s" (Info-current-filename-sans-extension) item)))
      (hywiki-add-referent wikiword (cons 'info-index item)))))

(defun hywiki-display-info-index (_wikiword item-name)
  (hact 'link-to-Info-index-item item-name))

(defun hywiki-add-info-node (wikiword)
  "Make WIKIWORD display an Info manual node and return it.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the Info node, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (let ((node (save-window-excursion
		(info)
		(Info-read-node-name "Info node: "))))
    (when (stringp node)
      (unless (= (aref node 0) ?\()
	(setq node (format "(%s)%s" (Info-current-filename-sans-extension) node)))
      (hywiki-add-referent wikiword (cons 'info-node node)))))

(defun hywiki-display-info-node (_wikiword node)
  (hact 'link-to-Info-node node))

(defun hywiki-add-key-series (wikiword)
  "Make WIKIWORD invoke a prompted for key series and return it.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the key series, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (let ((key-series (read-string "Key series (with or without {}): ")))
    (unless (string-match-p "\\`{.+}\\'" key-series)
      (setq key-series (concat "{" (string-trim key-series) "}")))
    (hywiki-add-referent wikiword (cons 'key-series key-series))))

(defun hywiki-display-key-series (_wikiword key-series)
  (hact 'kbd-key key-series))

(defun hywiki-add-org-id (wikiword)
  "Make WIKIWORD display an Org file or headline with an Org id.
If no id exists, it is created.  Return the string \"ID: org-id-string\".

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the sexpression, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (cl-destructuring-bind (_src-window referent-window)
      (hmouse-choose-link-and-referent-windows)
    (with-selected-window referent-window
      (unless (hsys-org-mode-p)
	(user-error "(hywiki-add-org-id): Referent buffer <%s> must be in org-mode, not %s"
		    (buffer-name)
		    major-mode))
      (let ((org-id (hyperb:with-suppressed-warnings ((callargs org-id-get))
                      (if (>= (action:param-count #'org-id-get) 4)
			(org-id-get nil nil nil t)
		      (org-id-get)))))
	(when (and (null org-id) buffer-read-only)
	  (user-error "(hywiki-add-org-id): Referent buffer <%s> point has no Org ID and buffer is read-only"
		      (buffer-name)))
	(unless org-id
	  (setq org-id (org-id-get-create)))
	(hywiki-add-referent wikiword (cons 'org-id (concat "ID: " org-id)))))))

(defun hywiki-display-org-id (_wikiword org-id)
  (hact 'link-to-org-id org-id))

(defun hywiki-add-org-roam-node (wikiword)
  "Make WIKIWORD display an Org Roam Node and return the action.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the action, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (hypb:require-package 'org-roam)
  (let ((node-title (org-roam-node-title (org-roam-node-read))))
    (hywiki-add-referent wikiword (cons 'org-roam-node node-title))))

(defun hywiki-display-org-roam-node (_wikiword referent)
  (hypb:require-package 'org-roam)
  (org-roam-node-open (if (stringp (cdr referent))
			  (org-roam-node-from-title-or-alias (cdr referent))
			;; Older links were Org Roam nodes rather than titles
			(cdr referent))
		      (or (alist-get 'file org-link-frame-setup)
			  (alist-get hpath:display-where hpath:display-where-alist))))

(defun hywiki-create-page (wikiword &optional message-flag)
  "Prompt for, add to HyWiki lookups and return a WIKIWORD page.
With optional prefix arg MESSAGE-FLAG non-nil, display a minibuffer message
with the page."
  (interactive (list nil current-prefix-arg))
  (unless (stringp wikiword)
    (setq wikiword (hywiki-word-read-new "Create/Edit HyWikiWord: ")))
  (setq hkey-value wikiword)
  (let ((page-file (hywiki-add-page wikiword t)))
    (if (or message-flag (called-interactively-p 'interactive))
	(when page-file
	  (message "HyWikiWord '%s' page: \"%s\"" wikiword page-file))
      (user-error "(hywiki-create-page): Invalid HyWikiWord: '%s'; must be capitalized, all alpha" wikiword))
    page-file))

(defun hywiki-add-page (page-name &optional force-flag)
  "Add a new or return any existing HyWiki page path for PAGE-NAME.
Returned format is: \\='(page . \"<page-file-path>\") or nil when none.

With optional FORCE-FLAG prefix arg non-nil, force an update to
the page's modification time.  If PAGE-NAME is invalid, trigger a
`user-error' if called interactively or return nil if not.

By default, create any non-existent page.  When not in batch or
ert test results mode, if this is the first HyWiki page in
`hywiki-directory', prompt to create it.

After successfully adding a page, run `hywiki-add-page-hook'.

Use `hywiki-get-referent' to determine whether a HyWiki page exists."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWiki page: "))
		     current-prefix-arg))
  (if (hywiki-word-is-p page-name)
      (when (or noninteractive
		(not (hash-empty-p (hywiki-get-referent-hasht)))
		(hyperb:stack-frame '(ert-run-test))
		(y-or-n-p (concat "Create new HyWiki page `" page-name "'? ")))
	(when (match-string-no-properties 2 page-name)
	  ;; Remove any #section suffix in PAGE-NAME.
	  (setq page-name (match-string-no-properties 1 page-name)))

	(let* ((page-file (hywiki-get-page-file page-name))
	       (page-file-readable (file-readable-p page-file))
	       (referent-hasht (hywiki-get-referent-hasht))
	       (page-in-hasht (hywiki-get-referent page-name)))
	  (unless page-file-readable
	    (if (file-writable-p page-file)
		(write-region "" nil page-file nil 0)
	      (user-error "(hywiki-add-page): No permission to write HyWikiWord page file:\n  \"%s\"" page-name)))
	  (if (or force-flag (not page-in-hasht))
	      (progn
		(hash-add (cons 'page (file-name-nondirectory page-file))
			  page-name referent-hasht)
		(setq hywiki--any-wikiword-regexp-list nil)
		(when (called-interactively-p 'interactive)
		  (message "Added HyWikiWord page: \"%s\"" page-file)))
	    (when (called-interactively-p 'interactive)
	      (message "HyWikiWord page exists: \"%s\"" page-file)))
	  (unless (or (hyperb:stack-frame '(hywiki-maybe-highlight-wikiwords-in-frame))
		      (and (not force-flag) page-file-readable page-in-hasht))
	    (hywiki-cache-save))
	  (run-hooks 'hywiki-add-page-hook)
	  (when page-file (cons 'page page-file))))
    (when (called-interactively-p 'interactive)
      (user-error "(hywiki-add-page): Invalid HyWikiWord: '%s'; must be capitalized, all alpha" page-name))))

;;;###autoload
(defun hywiki-word-create (wikiword &optional arg)
  "Create a HyWiki referent for WIKIWORD and return it; don't display it.
This replaces any existing referent the WIKIWORD may have.

With either `hywiki-referent-prompt-flag' set or optional prefix ARG,
prompt for and choose a typed referent, otherwise, create and/or display
a HyWiki page.  See `hywiki-referent-menu' for valid referent types.

Use `hywiki-get-referent' to test for and retrieve an existing HyWikiWord
referent."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new
			  (format "Create HyWikiWord %s: "
				  (if (or (and hywiki-referent-prompt-flag
					       (null current-prefix-arg))
					  current-prefix-arg)
				      "referent"
				    "page"))))
		     current-prefix-arg))
  (if (or (and hywiki-referent-prompt-flag (null arg))
	  arg)
      (hywiki-create-referent wikiword t)
    (hywiki-create-page wikiword t)))

(defun hywiki-word-create-and-display (wikiword &optional arg)
  "Display the HyWiki referent for WIKIWORD and return it.
If there is no existing WIKIWORD referent, add one.
With either `hywiki-referent-prompt-flag' set or optional prefix ARG,
prompt for and choose a typed referent, otherwise, create and/or display
a HyWiki page.  See `hywiki-referent-menu' for valid referent types.

Use `hywiki-get-referent' to determine whether a HyWikiWord referent
exists."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new
			  (format "Add/Edit and display HyWiki %s: "
				  (if (or (and hywiki-referent-prompt-flag
					       (null current-prefix-arg))
					  current-prefix-arg)
				      "referent"
				    "page"))))
		     current-prefix-arg))
  (hywiki-create-page-and-display wikiword (or (and hywiki-referent-prompt-flag
						    (null arg))
					       arg)))

(defun hywiki-create-referent-and-display (wikiword)
  "Display the HyWiki referent for WIKIWORD and return it.
If there is no existing WIKIWORD referent, prompt for and choose
a referent type; see `hywiki-referent-menu' for valid referent
types.

Use `hywiki-get-referent' to determine whether a HyWikiWord referent
exists."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit and display HyWiki referent: "))))
  (hywiki-create-page-and-display wikiword t))

(defun hywiki-create-page-and-display (wikiword &optional prompt-flag)
  "Display the HyWiki referent for WIKIWORD and return it.
If there is no existing WIKIWORD referent, add a HyWiki page for
it unless optional prefix arg, PROMPT-FLAG, is given, then prompt
for and create another referent type.  See `hywiki-referent-menu'
for valid referent types.

Use `hywiki-get-referent' to determine whether a HyWiki page exists."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit and display HyWiki page: "))
		     current-prefix-arg))
  (when (and (not prompt-flag) hywiki-referent-prompt-flag
	     (called-interactively-p 'interactive))
    (setq prompt-flag t))
  (let* ((normalized-word (hywiki-get-singular-wikiword wikiword))
	 (referent (hywiki-find-referent wikiword prompt-flag)))
    (cond (referent)
	  ((and (null referent) (hywiki-word-is-p normalized-word))
	   (when (hywiki-add-page normalized-word)
	     (hywiki-display-page normalized-word)))
	  (t (user-error "(hywiki-create-page-and-display): Invalid HyWikiWord: '%s'; must be capitalized, all alpha" wikiword)))))

(defun hywiki-display-page (&optional wikiword file-name)
  "Display an optional WIKIWORD page and return the page file.
Use `hywiki-display-page-function' to display the page.

If FILE is provided, it includes any #section from the WIKIWORD.

If WIKIWORD is omitted or nil and `hywiki-display-page-function'
is an interactive function, it is called interactively and prompts for
an existing or new HyWikiWord."
  (if (and (null wikiword) (commandp hywiki-display-page-function))
      (call-interactively hywiki-display-page-function)
    (when (null wikiword)
      (setq wikiword (hywiki-word-read-new "Find HyWiki page: ")))
    (let ((file (hywiki-get-page-file (or file-name wikiword))))
      (funcall hywiki-display-page-function file)
      ;; Set referent attributes of current implicit button
      (hattr:set 'hbut:current 'referent-type 'page)
      (hattr:set 'hbut:current 'referent-value file)
      file)))

(defun hywiki-add-path-link (wikiword &optional file pos)
  "Set a path link anchored possible position for WIKIWORD and return it.
If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

Interactively prompt for the file and whether to use the current
position if a buffer is visiting the file; non-interactively, you may
optionally provide the FILE and POS arguments.

After successfully adding the path link, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (let* ((path-args (if (and file pos)
			(list file pos)
		      (hactypes:link-to-file-interactively)))
	 (path-link (and (= (length path-args) 2)
			 (hpath:file-position-to-line-and-column
			  (car path-args) (cadr path-args)))))
    (when path-link
      (hywiki-add-referent wikiword (cons 'path-link path-link)))))

(defun hywiki-display-path-link (_wikiword path)
  (funcall hywiki-display-page-function path))

(defun hywiki-add-sexpression (wikiword)
  "Make WIKIWORD evaluate a prompted for sexpression and return it.

If WIKIWORD is invalid, trigger a `user-error' if called interactively
or return nil if not.

After successfully adding the sexpression, run `hywiki-add-referent-hook'.

Use `hywiki-get-referent' to determine whether WIKIWORD exists prior to
calling this function."
  (interactive (list (or (hywiki-word-at)
			 (hywiki-word-read-new "Add/Edit HyWikiWord: "))))
  (hywiki-add-referent wikiword (cons 'sexpression
				      (read--expression "Sexpression: "))))

(defun hywiki-display-sexpression (_wikiword sexpression)
  (eval sexpression))

(defun hywiki-add-to-referent (wikiword text position)
  "Display WIKIWORD referent and insert TEXT at POSITION.
Create page if it does not exist.  If WIKIWORD is invalid, return
nil, else return \\='(page . \"<page-file-path>\")."
  (when-let* ((referent (hywiki-add-page wikiword)))
    (hywiki-find-referent wikiword)
    (barf-if-buffer-read-only)
    (save-excursion
      (save-restriction
	(widen)
	(when position
	  (goto-char position))
	(unless (bolp)
	  (insert (newline)))
	(insert text)
	(unless (bolp)
	  (insert (newline)))
	(when position
	  (goto-char position))))
    referent))

(defun hywiki-at-tags-p (&optional at-tag-flag)
  "Return non-nil if point is in a HyWiki buffer and at Org tags."
  (and (or at-tag-flag (hsys-org-at-tags-p))
       (or (hywiki-in-page-p) (string-prefix-p "*HyWiki Tags*" (buffer-name)))))

;;;###autoload
(defun hywiki-consult-grep (&optional regexp max-matches path-list)
  "Interactively search with a consult package grep command.
Search for optional REGEXP up to MAX-MATCHES in PATH-LIST or `hywiki-directory'.

Use ripgrep (rg) if found, otherwise, plain grep.  Initialize search with
optional REGEXP and interactively prompt for changes.  Limit matches
per file to the absolute value of MAX-MATCHES, if given and not 0.  If
0, match to headlines only (lines that start with a '^[*#]+[ \t]+' regexp)."
  (interactive "i\nP")
  (let* ((grep-includes "--include *.org")
	 (ripgrep-globs "--glob *.org"))
    (hsys-consult-grep grep-includes ripgrep-globs
		       regexp max-matches (or path-list (list hywiki-directory)))))

(defun hywiki-convert-words-to-org-links ()
  "Convert all highlighted HyWiki words in current buffer to Org links.
Use when publishing a HyWiki file to another format, e.g. html.

For example, the link:
  \"WikiWord#Multi-Word Section\"
is converted to:
  \"[[hy:WikiWord#Multi-Word Section]]\".

If the reference is within the WikiWord page to which it refers, it
simplifies to:
  \"[[Multi-Word Section]]\".

The finalized Org link is then exported to html format by the Org
publish process."
  (barf-if-buffer-read-only)
  ;; Need to be explicit about the region here so does not use markers
  ;; from a region pointing to another buffer
  (hywiki-maybe-highlight-page-names (point-min) (point-max))
  (let ((make-index (hywiki-org-get-publish-property :makeindex))
	org-link
	wikiword-and-section
	wikiword)
    (hywiki-map-words (lambda (overlay)
			(setq wikiword-and-section
			      (buffer-substring-no-properties
			       (overlay-start overlay)
			       (overlay-end overlay)))
			(goto-char (overlay-start overlay))
			(delete-region (overlay-start overlay)
				       (overlay-end overlay))
			(delete-overlay overlay)
			(if (setq org-link (hywiki-word-to-org-link wikiword-and-section nil))
			    (insert org-link)
			  (message
			   "(hywiki-convert-words-to-org-links): \"%s\" in \"%s\" produced nil org link output"
			   wikiword-and-section (buffer-name)))
			(when make-index
			  (when (string-match (concat hywiki-org-link-type ":")
					      wikiword-and-section)
			    (setq wikiword (substring wikiword-and-section (match-end 0))))
			  (insert "\n#+INDEX: " wikiword "\n"))))))

(defun hywiki-word-to-org-link (link &optional description)
;; \"[[file:<hywiki-directory>/WikiWord.org::Multi-Word Section][WikiWord#Multi-Word Section]]\".
  (let ((resolved-link (hywiki-org-link-resolve link :full-data)))
    (when (stringp (car resolved-link))
      (let* ((path-word-suffix resolved-link)
             (path (file-relative-name (nth 0 path-word-suffix)))
             (path-stem (when path
			  (file-name-sans-extension path)))
             (word (nth 1 path-word-suffix))
             (suffix (nth 2 path-word-suffix))
             (desc (cond (description)
			 (suffix (when word
				   (format "%s%s" word suffix)))
			 (word)))
	     suffix-no-hashmark)
	(unless (and suffix (not (string-empty-p suffix)))
	  (setq suffix nil))
	(setq suffix-no-hashmark (when suffix (substring suffix 1)))
	(when (or (not buffer-file-name)
		  (string-equal path (file-name-nondirectory buffer-file-name)))
	  (setq path nil))
	(cond (desc
	       (if path
		   (if suffix
		       ;; "[[file:path-stem.org::suffix][desc]"
		       (format "[[file:%s.org::%s][%s]]"
			       path-stem suffix-no-hashmark desc)
		     ;; "[[file:path-stem.org][desc]]")
		     (format "[[file:%s.org][%s]]" path-stem desc))
		 (if suffix
		     ;; "[[suffix][desc]]"
		     (format "[[%s][%s]]" suffix desc)
		   ;; "[[desc]]"
		   (format "[[%s]]" desc))))
	      (path
	       ;; "[[file:path-stem.org][word]]"
	       (format "[[file:%s.org][%s]]" path-stem word)))))))

(defun hywiki-maybe-at-wikiword-beginning ()
  "Return non-nil if previous character is one preceding a HyWiki word.
Do not test whether or not a page exists for the HyWiki word.
Use `hywiki-get-referent' to determine whether a HyWiki page exists."
  ;; Ignore wikiwords preceded by any non-whitespace character, except
  ;; any of these: [({<"'`'
  (when (or (bolp)
	    (string-match (regexp-quote (char-to-string (char-before)))
			  "\[\(\{\<\"'`\t\n\r\f "))
    t))

(defun hywiki-directory-edit ()
  "Edit HyWiki pages in current `hywiki-directory'.
Use `dired' unless `action-key-modeline-buffer-id-function' is set to
`smart-treemacs-modeline', then use `treemacs'."
  (interactive)
  (if (eq action-key-modeline-buffer-id-function #'smart-treemacs-modeline)
      (hywiki-directory-treemacs-edit)
    (hywiki-directory-dired-edit)))

(defun hywiki-directory-dired-edit ()
  "Use `dired' to edit HyWiki pages in current `hywiki-directory'."
  (interactive)
  (let ((case-fold-search nil))
    (dired (cons hywiki-directory
		 (directory-files hywiki-directory nil
				  (format "^%s%s$"
					  hywiki-word-regexp
					  (regexp-quote hywiki-file-suffix)))))))

(defun hywiki-directory-treemacs-edit ()
  "Use `treemacs' to edit HyWiki pages in current `hywiki-directory'."
  (interactive)
  (require 'hui-treemacs)
  (smart-treemacs-edit hywiki-directory))

(defun hywiki-directory-get-checksum ()
  "Compute and return the checksum for the current set of HyWiki pages."
  (let ((hywiki-page-files (hywiki-get-page-files)))
    (when hywiki-page-files
      (md5 (apply #'concat hywiki-page-files) nil nil nil t))))

(defun hywiki-directory-get-mod-time ()
  "Return the last mod time for `hywiki-directory' or nil."
  (when (file-readable-p hywiki-directory)
    (time-convert (file-attribute-modification-time
		   (file-attributes hywiki-directory))
		  'list)))

(defun hywiki-directory-modified-p ()
  "Return non-nil if any HyWiki page name change since last read."
  (or (null hywiki--directory-mod-time)
      ;; Both dir mod-time and filename checksum over HyWiki page
      ;; files must have changed for this to be an update to report.
      ;; Don't change this logic as many other dir changes can occur
      ;; that should not be reported here.
      (not (or (equal hywiki--directory-mod-time (hywiki-directory-get-mod-time))
	       (string-equal hywiki--directory-checksum (hywiki-directory-get-checksum))))))

(defun hywiki-directory-set-checksum ()
  "Store the last page name checksum for `hywiki-directory' as a string."
  (setq hywiki--directory-checksum (hywiki-directory-get-checksum)))

(defun hywiki-directory-set-mod-time ()
  "Store the last page mod time for `hywiki-directory'.
Use `time-since' to see the time in seconds since this modification time."
  (setq hywiki--directory-mod-time (hywiki-directory-get-mod-time)))

(defun hywiki-maybe-directory-updated ()
  "When a HyWiki directory is modified, reset its modified time and checksum."
  (hywiki-directory-set-mod-time)
  (hywiki-directory-set-checksum))


;;;###autoload
(defun hywiki-find-referent (&optional wikiword prompt-flag)
  "Display optional HyWiki WIKIWORD referent or if nil, use current buffer.
If called interactively, use the WIKIWORD at point or if none, prompt for
an existing or new one.  With a prefix arg PROMPT-FLAG, prompt for the
type of referent to link to.  See `hywiki-referent-menu' for valid
referent types.

Return the referent if successfully found or nil otherwise.
A valid referent is a cons of (<referent-type> . <referent-value>).

If the referent is a HyWiki page:
    Return a cons of the symbol \\='page and the absolute path
    to any page successfully found.  Return nil if failed or
    if displaying a regular file (read in via a `find-file' call).

    By default, create any non-existent page.  When not in batch
    mode, with optional PROMPT-FLAG t or if this is the first
    HyWiki page in `hywiki-directory', prompt to create if
    non-existent.  If PROMPT-FLAG is :existing or with a prefix
    argument when called interactively, return nil unless the
    page already exists.  After successfully finding a page and
    reading it into a buffer, run `hywiki-display-referent-hook'.

After successfully finding any kind of referent, run
`hywiki-find-referent-hook'."
  (interactive (list (hywiki-word-read-new "Add/Edit HyWikiWord: ")
		     (when current-prefix-arg t)))
  (let ((referent (hywiki-display-referent wikiword prompt-flag)))
    (run-hooks 'hywiki-find-referent-hook)
    referent))

(defun hywiki-highlight-on-yank (_prop-value start end)
  "Used in `yank-handled-properties' called with START and END pos of the text.
Have to add one character to the length of the yanked text so that any
needed word-separator after the last character is included to induce
highlighting any last HyWikiWord."
  ;; When yank only part of a delimited pair, expand the range to
  ;; include the whole delimited pair before re-highlighting
  ;; HyWikiWords therein, so that the whole delimited expression is
  ;; included.
  (cl-destructuring-bind (start end)
      (hywiki--extend-yanked-region start end)
    (hywiki-maybe-highlight-page-names start (min (1+ end) (point-max)))))

(defun hywiki-map-words (func)
  "Apply FUNC across highlighted HyWikiWords in the current buffer and return nil.
FUNC takes 1 argument, the Emacs overlay spanning the start and end buffer
positions of each HyWikiWord and its optional #section."
  (save-excursion
    (save-restriction
      (widen)
      (mapc (lambda (overlay)
	      (when (eq (overlay-get overlay 'face) hywiki-word-face)
		(funcall func overlay)))
	    (overlays-in (point-min) (point-max)))))
  nil)

(defun hywiki-at-range-delimiter ()
  "Immediately before or after a balanced delimiter, return the delimited range.
If no such range, return \\='(nil nil).
This includes the delimiters: (), {}, <>, [] and \"\" (double quotes)."
  (save-excursion
    (save-restriction
      ;; Limit balanced pair checks to the next two lines for speed
      (narrow-to-region (line-beginning-position) (line-end-position 2))
      (let ((result (condition-case nil
			(cond
			 ;; Handle opening delimiters
			 ((memq (char-before) '(?\[ ?\<))
			  (goto-char (1- (point)))
			  (hywiki--get-delimited-range-forward))
			 ((memq (char-after) '(?\[ ?\<))
			  (hywiki--get-delimited-range-forward))
			 ((memq (char-before) '(?\( ?\{))
			  (goto-char (1- (point)))
			  (list (point) (scan-sexps (point) 1)))
			 ((memq (char-after) '(?\( ?\{))
			  (list (point) (scan-sexps (point) 1)))
			 ((and (eq (char-before) ?\")
			       (hypb:in-string-p))
			  (goto-char (1- (point)))
			  (list (point) (scan-sexps (point) 1)))
			 ((and (eq (char-after) ?\")
			       (hypb:in-string-p))
			  (goto-char (1+ (point)))
			  (list (point) (scan-sexps (point) -1)))
			 ;; Handle closing delimiters
			 ((memq (char-before) '(?\] ?\>))
			  (hywiki--get-delimited-range-backward))
			 ((memq (char-after) '(?\] ?\>))
			  (goto-char (1+ (point)))
			  (hywiki--get-delimited-range-backward))
			 ((memq (char-before) '(?\) ?\}))
			  (list (point) (scan-sexps (point) -1)))
			 ((memq (char-after) '(?\) ?\}))
			  (goto-char (1+ (point)))
			  (list (point) (scan-sexps (point) -1)))
			 ((and (eq (char-before) ?\")
			       (not (hypb:in-string-p)))
			  (list (point) (scan-sexps (point) -1)))
			 ((and (eq (char-after) ?\")
			       (not (hypb:in-string-p)))
			  (list (point) (scan-sexps (point) 1))))
		      (error nil))))
	(if (and result (integerp (nth 0 result)) (integerp (nth 1 result)))
	    (sort result #'<)
	  (list nil nil))))))

;;;###autoload
(defun hywiki-insert-link ()
  "Insert at point a link to a HyWiki page."
  (interactive "*")
  (insert (hywiki-word-read "Link to HyWiki page: "))
  (hywiki-maybe-highlight-page-name))

(defun hywiki-maybe-dehighlight-balanced-pairs ()
  "Before or after a balanced delimiter, dehighlight HyWikiWords within.
Include: (), {}, <>, [] and \"\" (double quotes).  Exclude Org links
and radio targets.

Ignore return value; it has no meaning."
  (save-excursion
    (save-restriction
      (if (hywiki--buttonized-region-p)
	  (narrow-to-region hywiki--buttonize-start hywiki--buttonize-end)
	;; Limit balanced pair checks to the next two lines for speed
	(narrow-to-region (line-beginning-position) (line-end-position 2)))

      ;; char-before
      (ignore-errors
	(cond ((memq (char-before) '(?\[ ?\<))
	       (goto-char (1- (point)))
	       ;; Dehighlight HyWikiWords within opening square or angle brackets
	       (hywiki-maybe-dehighlight-org-element-forward))
	      ((memq (char-before) '(?\( ?\{))
	       ;; Dehighlight HyWikiWords within opening parens or braces
	       (goto-char (1- (point)))
	       (hywiki-maybe-dehighlight-sexp 1))
	      ((and (eq (char-before) ?\")
		    (hypb:in-string-p))
	       ;; Dehighlight HyWikiWords in any string following point
	       (goto-char (1- (point)))
	       (hywiki-maybe-dehighlight-sexp 1))
	      ((memq (char-before) '(?\] ?\>))
	       ;; Dehighlight HyWikiWords within closing square or angle brackets
	       (hywiki-maybe-dehighlight-org-element-backward))
	      ((memq (char-before) '(?\) ?\}))
	       ;; Dehighlight HyWikiWords within closing parens or braces
	       (hywiki-maybe-dehighlight-sexp -1))
	      ((and (eq (char-before) ?\")
		    (not (hypb:in-string-p)))
	       ;; Dehighlight HyWikiWords in any string preceding point
	       (hywiki-maybe-dehighlight-sexp -1))))

      ;; char-after
      (ignore-errors
	(cond ((memq (char-after) '(?\[ ?\<))
	       ;; Dehighlight HyWikiWords within opening square or angle brackets
	       (hywiki-maybe-dehighlight-org-element-forward))
	      ((memq (char-after) '(?\( ?\{))
	       ;; Dehighlight HyWikiWords within opening parens or braces
	       (hywiki-maybe-dehighlight-sexp 1))
	      ((and (eq (char-after) ?\")
		    (hypb:in-string-p))
	       ;; Dehighlight HyWikiWords in any string preceding point
	       (goto-char (1+ (point)))
	       (hywiki-maybe-dehighlight-sexp -1))
	      ((memq (char-after) '(?\] ?\>))
	       (goto-char (1+ (point)))
	       ;; Dehighlight HyWikiWords within double closing square
	       ;; or angle brackets, as these may be links or targets
	       (hywiki-maybe-dehighlight-org-element-backward))
	      ((memq (char-after) '(?\) ?\}))
	       ;; Dehighlight any HyWikiWords within closing parens or braces
	       (goto-char (1+ (point)))
	       (hywiki-maybe-dehighlight-sexp -1))
	      ((and (eq (char-after) ?\")
		    (not (hypb:in-string-p)))
	       ;; Dehighlight HyWikiWords in any string following point
	       (hywiki-maybe-dehighlight-sexp 1)))))))

(defun hywiki-maybe-highlight-balanced-pairs ()
  "Before or after a balanced delimiter, highlight HyWikiWords within.
Include: (), {}, <>, [] and \"\" (double quotes).  Exclude Org links
and radio targets.

Return t if no errors and a pair was found, else nil."
  (save-excursion
    (save-restriction
      (if (hywiki--buttonized-region-p)
	  (narrow-to-region hywiki--buttonize-start hywiki--buttonize-end)
	;; Limit balanced pair checks to the next two lines for speed
	(narrow-to-region (line-beginning-position) (line-end-position 2)))

      (let ((result t))
	(condition-case nil
	    ;; char-before
	    (cond ((memq (char-before) '(?\[ ?\<))
		   (goto-char (1- (point)))
		   ;; Highlight any HyWikiWords within single opening
		   ;; square or angle brackets
		   ;; Dehighlight HyWikiWords within double opening square
		   ;; or angle brackets, as these are Org links and targets
		   (hywiki-maybe-highlight-org-element-forward))
		  ((memq (char-before) '(?\( ?\{))
		   ;; Highlight any HyWikiWords within opening parens or braces
		   (goto-char (1- (point)))
		   (hywiki-maybe-highlight-sexp 1))
		  ((and (eq (char-before) ?\")
			(hypb:in-string-p))
		   (goto-char (1- (point)))
		   (hywiki-maybe-highlight-sexp 1))
		  ((memq (char-before) '(?\] ?\>))
		   ;; Dehighlight HyWikiWords within double closing square
		   ;; or angle brackets, as these are Org links and targets
		   (hywiki-maybe-highlight-org-element-backward))
		  ((memq (char-before) '(?\) ?\}))
		   ;; Highlight any HyWikiWords within closing parens or braces
		   (hywiki-maybe-highlight-sexp -1))
		  ((and (eq (char-before) ?\")
			(not (hypb:in-string-p)))
		   ;; Highlight HyWikiWords in any string preceding point
		   (hywiki-maybe-highlight-sexp -1))
		  (t (setq result nil)))
	  (error (setq result nil)))

	(when result
	  (condition-case nil
	      ;; char-after
	      (cond ((memq (char-after) '(?\[ ?\<))
		     ;; Highlight any HyWikiWords within single opening
		     ;; square or angle brackets
		     ;; Dehighlight HyWikiWords within double opening square
		     ;; or angle brackets, as these are Org links and targets
		     (hywiki-maybe-highlight-org-element-forward))
		    ((memq (char-after) '(?\( ?\{))
		     ;; Highlight any HyWikiWords within opening parens or braces
		     (hywiki-maybe-highlight-sexp 1))
		    ((and (eq (char-after) ?\")
			  (hypb:in-string-p))
		     (goto-char (1+ (point)))
		     (hywiki-maybe-highlight-sexp -1))
		    ((memq (char-after) '(?\] ?\>))
		     (goto-char (1+ (point)))
		     ;; Highlight any HyWikiWords within single closing
		     ;; square or angle brackets
		     ;; Dehighlight HyWikiWords within double closing square
		     ;; or angle brackets, as these are Org links and targets
		     (hywiki-maybe-highlight-org-element-backward))
		    ((memq (char-after) '(?\) ?\}))
		     ;; Highlight any HyWikiWords within closing parens or braces
		     (goto-char (1+ (point)))
		     (hywiki-maybe-highlight-sexp -1))
		    ((and (eq (char-after) ?\")
			  (not (hypb:in-string-p)))
		     ;; Highlight HyWikiWords in any string following point
		     (hywiki-maybe-highlight-sexp 1))
		    (t (setq result nil)))
	    (error (setq result nil))))
	(when result t)))))

(defun hywiki-maybe-dehighlight-between-page-names ()
  "Dehighlight any non-Org link HyWiki page#section between point.
If in a programming mode, must be within a comment or string.  Use
`hywiki-word-face' to dehighlight."
  (when (hproperty:char-property-range (point) 'face hywiki-word-face)
    (hproperty:but-clear-all-in-list
     (hproperty:but-get-all-in-region (point) (1+ (point))
				      'face hywiki-word-face)))

  (cond ((cl-destructuring-bind (start end)
	     (hywiki-at-range-delimiter)
	   (when (and start end)
	     (save-excursion
	       (goto-char (1+ start))
	       (and (hproperty:char-property-range (point) 'face hywiki-word-face)
		    (equal (hywiki-referent-exists-p :range)
			   '(nil nil nil))
		    ;; non-existing wikiword
		    (hywiki-maybe-dehighlight-on-page-name)))
	     t)))
	((looking-at "[ \t\n\r\f]")
	 (hywiki-maybe-dehighlight-off-page-name)
	 (hywiki-maybe-dehighlight-on-page-name))))

(defun hywiki-maybe-dehighlight-off-page-name ()
  "Dehighlight any non-Org link HyWiki page#section at or one char before point.
If on a whitespace character or at end of buffer, handle
dehighlighting for any previous word or punctuation.  If
in a programming mode, must be within a comment."
  ;; Dehighlight any page name at point
  (hywiki-maybe-dehighlight-page-name
   ;; Flag on-page-name if on a whitespace character
   (or (= (point) (point-max))
       (= (if (char-after) (char-syntax (char-after)) 0) ? ))))

(defun hywiki-maybe-dehighlight-on-page-name ()
  "Dehighlight any non-Org link HyWiki page#section at or one char before point.
If not on a whitespace character, handle dehighlighting for any
page/section name or punctuation.  If in a programming mode, must
be within a comment."
  ;; Dehighlight any page name at point
  (hywiki-maybe-dehighlight-page-name
   ;; Flag on-page-name if not on a whitespace character
   (and (/= (point) (point-max))
	(/= (if (char-after) (char-syntax (char-after)) 0) ? ))))

;;;###autoload
(defun hywiki-maybe-dehighlight-page-name (&optional on-page-name)
  "Dehighlight any non-Org link HyWiki page#section at or one char before point.
With optional ON-PAGE-NAME non-nil, assume point is within the page or
section name.  Otherwise, if `pre-command-hook' has set
`hywiki--buttonize-start' `hywiki--buttonize-end' global variables,
use these as the region in which to dehighlight.

If in a programming mode, must be within a comment.  Use
`hywiki-word-face' to dehighlight."
  (interactive)
  (setq hywiki--start nil
	hywiki--end   nil)
  (when (and (hywiki-active-in-current-buffer-p)
	     (if (and (derived-mode-p 'prog-mode)
		      (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes)))
		 ;; Non-nil if match is inside a comment or a string
		 (or (nth 4 (syntax-ppss)) (hypb:in-string-p))
	       t)
	     (or on-page-name
		 (string-match (regexp-quote
				(char-to-string (char-syntax last-command-event)))
			  " _()<>$.\"'"))
             (not executing-kbd-macro)
             (not noninteractive))
    (setq hywiki--highlighting-done-flag nil)
    (with-syntax-table hbut:syntax-table
      (save-excursion
	(save-restriction
	  (when (hywiki--buttonized-region-p)
	    (narrow-to-region hywiki--buttonize-start hywiki--buttonize-end)
	    (goto-char hywiki--buttonize-start))

	  (unless on-page-name
	    ;; after page name
	    (skip-syntax-backward ">-"))

	  (hywiki-maybe-dehighlight-balanced-pairs)

	  (unless hywiki--highlighting-done-flag
	    (unless on-page-name
	      ;; May be a closing delimiter that we have to skip past
	      (skip-chars-backward (hywiki-get-buttonize-characters)))
	    ;; Skip past HyWikiWord or section
	    (skip-syntax-backward "^-$()<>._\"\'")
	    (skip-chars-backward "-_*#:[:alnum:]")

	    (setq hywiki--save-case-fold-search case-fold-search
		  case-fold-search nil
		  hywiki--save-org-link-type-required hywiki-org-link-type-required
		  hywiki-org-link-type-required t)
	    (unless (and (hywiki-maybe-at-wikiword-beginning)
			 (looking-at hywiki--word-and-buttonize-character-regexp)
			 (progn
			   (setq hywiki--word-only (match-string-no-properties 2)
				 hywiki--start (match-beginning 1)
				 hywiki--end   (match-end 1))
			   (hywiki-get-referent hywiki--word-only)))
	      ;; Remove any potential earlier highlighting since the
	      ;; previous word may have changed.
	      (skip-syntax-backward "^-$()<>._\"\'"))

	    (hproperty:but-clear-all-in-list
	     (hproperty:but-get-all-in-region (or hywiki--start (point))
					      (or hywiki--end (1+ (point)))
					      'face hywiki-word-face))))))))

;;;###autoload
(defun hywiki-maybe-highlight-page-name (&optional on-page-name)
  "Highlight any non-Org link HyWikiWord#section at or one char before point.
With optional ON-PAGE-NAME non-nil, assume point is within the page or
section name.  Otherwise, if a HyWiki per-character hook has set
`hywiki--buttonize-start' `hywiki--buttonize-end' global variables,
use these as the region to highlight.

If in a programming mode, must be within a comment.  Use
`hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached."
  (interactive)
  (when (and (hywiki-active-in-current-buffer-p)
	     (if (and (derived-mode-p 'prog-mode)
		      (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes)))
		 ;; Non-nil if match is inside a comment or string
		 (or (nth 4 (syntax-ppss)) (hypb:in-string-p))
	       t)
	     ;;  (or on-page-name
	     ;;	 (string-match (regexp-quote (char-to-string (char-syntax last-command-event)))
	     ;;		       " _()<>$.\"'"))
             (not executing-kbd-macro)
             (not noninteractive))
      (setq hywiki--highlighting-done-flag nil)
      (with-syntax-table hbut:syntax-table
	(save-excursion
	  (when (hywiki--buttonized-region-p)
	    (goto-char hywiki--buttonize-start))

	  (unless on-page-name
	    ;; after page name
	    (skip-syntax-backward ">-"))

	  (unless (or hywiki--highlighting-done-flag
 		      (hywiki-maybe-highlight-balanced-pairs))

	    (unless on-page-name
	      ;; May be a HyWikiWord ending character to skip past
	      (skip-chars-backward (hywiki-get-buttonize-characters)
				   (line-beginning-position)))
	    ;; Skip past HyWikiWord or section
	    (skip-syntax-backward "^-$()<>._\"\'")
	    (skip-chars-backward "-_*#:[:alnum:]")

	    (setq hywiki--save-case-fold-search case-fold-search
		  case-fold-search nil
		  hywiki--save-org-link-type-required hywiki-org-link-type-required
		  hywiki-org-link-type-required t
		  hywiki--start nil
		  hywiki--end   nil)

	    (if (and (cl-destructuring-bind (word start end)
			 (hywiki-word-at :range)
		       (setq hywiki--word-only word
			     hywiki--start start
			     hywiki--end end))
		     hywiki--start
		     (hywiki-get-referent hywiki--word-only)
		     (goto-char hywiki--start))
		(progn
		  (setq hywiki--current-page (hywiki-get-buffer-page-name))
		  ;; Don't highlight current-page matches unless they
		  ;; include a #section.
		  (unless (string-equal hywiki--current-page
					(buffer-substring-no-properties
					 hywiki--start hywiki--end))
		    (if (setq hywiki--buts (hproperty:but-get-all-in-region
					    hywiki--start hywiki--end
					    'face hywiki-word-face))
			(if (> (length hywiki--buts) 1)
			    (progn (hproperty:but-clear-all-in-list hywiki--buts)
				   (hywiki-maybe-highlight-page-names
				    hywiki--start hywiki--end))
			  ;; There is only one existing button
			  (setq hywiki--buts (car hywiki--buts)
				hywiki--but-start (hproperty:but-start hywiki--buts)
				hywiki--but-end   (hproperty:but-end hywiki--buts))
			  (unless (and (= hywiki--start hywiki--but-start)
				       (= hywiki--end hywiki--but-end))
			    (hproperty:but-delete hywiki--buts)
			    (hywiki-maybe-highlight-page-names
			     hywiki--start hywiki--end)))
		      (hywiki-maybe-highlight-page-names
		       hywiki--start hywiki--end))))
	      ;; Remove any potential earlier highlighting since the
	      ;; previous word may have changed.
	      (skip-syntax-backward "^-$()<>._\"\'")
	      (when (setq hywiki--buts (hproperty:but-get-all-in-region
					(point) (1+ (point)) 'face hywiki-word-face))
		(if (> (length hywiki--buts) 1)
		    (hproperty:but-clear-all-in-list hywiki--buts)
		  ;; There is only one existing button
		  (setq hywiki--buts (car hywiki--buts)
			hywiki--but-start (hproperty:but-start hywiki--buts)
			hywiki--but-end   (hproperty:but-end hywiki--buts))
		  (hproperty:but-delete hywiki--buts)))))))))

(defun hywiki-maybe-highlight-between-page-names ()
  "Highlight any non-Org link HyWiki page#section names between point.

If in a programming mode, must be within a comment.  Use
`hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached."
  (cond ((hproperty:char-property-range (point) 'face hywiki-word-face))
	((cl-destructuring-bind (word start end)
	     (hywiki-word-at :range)
	   (when (and start end)
	     (save-excursion
	       (goto-char start)
	       (when (hywiki-referent-exists-p word)
		 ;; existing wikiword
		 (hywiki-maybe-highlight-on-page-name)))
	     t)))
	((cl-destructuring-bind (start end)
	     (hywiki-at-range-delimiter)
	   (when (and start end)
	     (save-excursion
	       (goto-char (1+ start))
	       (skip-syntax-forward "-" (line-end-position))
	       (unless (equal (hywiki-referent-exists-p :range)
			      '(nil nil nil))
		 ;; existing wikiword
		 (hywiki-maybe-highlight-on-page-name)))
	     t)))
	((looking-at "[ \t\n\r\f]")
	 (hywiki-maybe-highlight-off-page-name)
	 (hywiki-maybe-highlight-on-page-name))
	(t (hywiki-maybe-highlight-on-page-name))))

(defun hywiki-maybe-highlight-off-page-name ()
  "Highlight any non-Org link HyWiki page#section at or one char before point.
If at bobp or any preceding char is non-whitespace and any following
character is whitespace or at eobp, handle highlighting for any previous
word or punctuation.

If in a programming mode, must be within a comment.  Use
`hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached."
  (hywiki-maybe-highlight-page-name
   ;; flag on-page-name if on a whitespace character
   (and (or (= (point) (point-max))
	    (= (if (char-after) (char-syntax (char-after)) 0) ?\ ))
	(or (= (point) (point-min))
	    (/= (if (char-before) (char-syntax (char-before)) 0) ?\ )))))

(defun hywiki-maybe-highlight-on-page-name ()
  "Highlight any non-Org link HyWiki page#section at or one char before point.
If not on a whitespace character, handle highlighting for any page/section
name or punctuation.

If in a programming mode, must be within a comment.  Use
`hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached."
  (hywiki-maybe-highlight-page-name
   ;; flag on-page-name if not on a whitespace character
   (and (/= (point) (point-max))
	(/= (if (char-after) (char-syntax (char-after)) 0) ? ))))

(defun hywiki-maybe-dehighlight-org-element-backward ()
  "Dehighlight HyWikiWords within a closing double/single square/angle bracket."
  (hywiki--maybe-de/highlight-org-element-backward #'hywiki-maybe-dehighlight-sexp))

(defun hywiki-maybe-highlight-org-element-backward ()
  "Highlight HyWikiWords with point at a single closing square/angle bracket.
Dehighlight HyWikiWords when on a double closing square/angle bracket,
since Org mode highlights those."
  (hywiki--maybe-de/highlight-org-element-backward #'hywiki-maybe-highlight-sexp))

(defun hywiki-maybe-dehighlight-org-element-forward ()
  "Dehighlight HyWikiWords within an opening double/single square/angle bracket."
  (hywiki--maybe-de/highlight-org-element-forward #'hywiki-maybe-dehighlight-sexp))

(defun hywiki-maybe-highlight-org-element-forward ()
  "Highlight HyWikiWords with point at a single opening square/angle bracket.
Dehighlight HyWikiWords when on a double opening square/angle bracket,
since Org mode highlights those."
  (hywiki--maybe-de/highlight-org-element-forward #'hywiki-maybe-highlight-sexp))

(defun hywiki-maybe-dehighlight-sexp (direction-number)
  "Dehighlight any HyWikiWord within single square/angle bracket.
DIRECTION-NUMBER is 1 for forward scanning and -1 for backward scanning."
  ;; Enable dehighlighting in HyWiki pages
  (let ((hywiki-word-highlight-flag))
    (hywiki--maybe-de/highlight-sexp
     #'hywiki-maybe-dehighlight-page-names direction-number)))

(defun hywiki-maybe-highlight-sexp (direction-number)
  "Highlight any HyWikiWord within single square/angle bracket.
DIRECTION-NUMBER is 1 for forward scanning and -1 for backward scanning."
  (hywiki--maybe-de/highlight-sexp
   #'hywiki-maybe-highlight-page-names direction-number))

;;;###autoload
(defun hywiki-maybe-dehighlight-page-names (&optional region-start region-end)
  "Dehighlight any highlighted HyWiki page names in a HyWiki buffer/region.
With optional REGION-START and REGION-END positions (active region
interactively), limit dehighlighting to the region."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  (unless (or (eq hywiki-buffer-highlighted-state 'd)
	      (hywiki-active-in-current-buffer-p))
    (hproperty:but-clear-all-in-list
     (hproperty:but-get-all-in-region
      (if (markerp region-start)
	  (if (marker-position region-start)
	      region-start
	    (point-min))
	(or region-start (point-min)))
      (if (markerp region-end)
	  (if (marker-position region-end)
	      region-end
	    (point-max))
	(or region-end (point-max)))
      'face hywiki-word-face))
    (unless (or region-start region-end)
      (setq hywiki-buffer-highlighted-state 'd))))
;
;;###autoload
(defun hywiki-maybe-highlight-page-names (&optional region-start region-end skip-lookups-update-flag)
  "Highlight each non-Org link HyWiki page#section in a buffer/region.
With optional REGION-START and REGION-END positions or markers (active
region interactively), limit highlight adjustment to the region.  With
optional SKIP-LOOKUPS-UPDATE-FLAG non-nil, HyWiki lookup tables
should have already been updated and this is skipped.

Use `hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached.

Dehighlight buffers other than HyWiki pages when `hywiki-mode' is
disabled.  Highlight/dehighlight HyWiki page buffers whenever the
value of `hywiki-word-highlight-flag' is changed."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  ;; Avoid doing many lets for efficiency.
  ;; Highlight HyWiki words throughout buffers where `hywiki-mode' is enabled
  ;; or HyWiki pages below `hywiki-directory' whenever displayed in a window.
  (if (hywiki-active-in-current-buffer-p)
      (unless (and (or (and (null region-start) (null region-end))
		       (and (markerp region-start) (markerp region-end)
			    (not (and (marker-position region-start)
				      (marker-position region-end)))))
		   (eq hywiki-buffer-highlighted-state 'h)
		   (not (hywiki-directory-modified-p)))
	(unless skip-lookups-update-flag
	  ;; Rebuild lookup tables if any HyWiki page name has changed
	  (hywiki-get-referent-hasht))
	(unwind-protect
	    (save-excursion
	      (save-restriction
		(setq hywiki--save-case-fold-search case-fold-search
		      case-fold-search nil
		      hywiki--save-org-link-type-required hywiki-org-link-type-required
		      hywiki-org-link-type-required t
		      hywiki--current-page (hywiki-get-buffer-page-name))
		(cond ((and (markerp region-start) (markerp region-end))
		       (when (and (marker-position region-start)
				  (marker-position region-end))
			   (narrow-to-region region-start region-end)))
		      ((and region-start region-end)
		       (narrow-to-region region-start region-end)))
		;; Enable dehighlighting in HyWiki pages only when
		;; whole buffer is being processed; this prevents an
		;; error when called from `hywiki-maybe-highlight-sexp'.
		(unless (and region-start region-end)
		  (let ((hywiki-word-highlight-flag))
		    (hywiki-maybe-dehighlight-page-names)))
		(dolist (hywiki-words-regexp hywiki--any-wikiword-regexp-list)
		  (goto-char (point-min))
		  (let ((highlight-in-comments-and-strings-only
			 (and (derived-mode-p 'prog-mode)
			      (not (apply #'derived-mode-p hywiki-highlight-all-in-prog-modes)))))
		    (while (re-search-forward hywiki-words-regexp nil t)
		      (when (if highlight-in-comments-and-strings-only
				;; Non-nil if match is inside a comment or a string
				(or (nth 4 (syntax-ppss)) (hypb:in-string-p))
			      t)
			(setq hywiki--start (match-beginning 1)
			      hywiki--end   (match-end 1))
			(save-excursion
			  (goto-char hywiki--start)
			  ;; Otherwise, highlight any HyWikiWord found, including
			  ;; any #section:Lnum:Cnum.
			  (when (hywiki-maybe-at-wikiword-beginning)
			    (or (unless (hyperb:stack-frame '(hywiki-maybe-highlight-balanced-pairs))
				  (hywiki-maybe-highlight-balanced-pairs))
				(progn (with-syntax-table hbut:syntax-table
					 (skip-syntax-forward "^-\)$\>._\"\'"))
				       (skip-chars-forward "-_*[:alnum:]")
				       (unless (zerop (skip-chars-forward "#:"))
					 (skip-chars-forward (if (and region-start region-end)
								 "-_*: \t[:alnum:]"
							       "-_*:[:alnum:]")))
				       (setq hywiki--end (point))
				       ;; Don't highlight current-page matches unless they
				       ;; include a #section.
				       (unless (string-equal hywiki--current-page
							     (buffer-substring-no-properties hywiki--start hywiki--end))
					 (hproperty:but-add hywiki--start hywiki--end hywiki-word-face))))))))))

		;; Disable dehighlighting of HyWikiWords between [] and <>.
		;;
		;; (let (str-start-end)
		;;   (goto-char (point-min))
		;;   (while (search-forward "[" nil t)
		;;     (when (setq str-start-end (hargs:delimited-p "[" "]" nil nil t))
		;;       (setq hywiki--start (nth 1 str-start-end)
		;; 	    hywiki--end   (nth 2 str-start-end))
		;;       ;; Clear any HyWikiWord highlighting that may
		;;       ;; just be a part of a larger square brackets
		;;       ;; delimited text with multiple words.
		;;       (hproperty:but-clear-all-in-list
		;;        (hproperty:but-get-all-in-region hywiki--start hywiki--end
		;; 					'face hywiki-word-face))
		;;       (goto-char (min (1+ hywiki--end) (point-max)))))

		;;   (goto-char (point-min))
		;;   (while (search-forward "<" nil t)
		;;     (when (setq str-start-end (hargs:delimited-p "<" ">" nil nil t))
		;;       (setq hywiki--start (nth 1 str-start-end)
		;; 	    hywiki--end   (nth 2 str-start-end))
		;;       ;; Clear any HyWikiWord highlighting that may
		;;       ;; just be a part of a larger angle brackets
		;;       ;; delimited text with multiple words.
		;;       (hproperty:but-clear-all-in-list
		;;        (hproperty:but-get-all-in-region hywiki--start hywiki--end
		;; 					'face hywiki-word-face))
		;;       (goto-char (min (1+ hywiki--end) (point-max))))))

		(unless (and region-start region-end
			     (or (/= region-start (point-min))
				 (/= region-end   (point-max))))
		  (setq hywiki-buffer-highlighted-state 'h))))
	  (setq case-fold-search hywiki--save-case-fold-search
		hywiki-org-link-type-required hywiki--save-org-link-type-required)))

    ;; Otherwise, dehighlight HyWikiWords in this buffer when
    ;; 'hywiki-mode' is disabled and this is not a HyWiki page
    ;; buffer. If this is a HyWiki page buffer, then dehighlight
    ;; when `hywiki-word-highlight-flag' is nil.
    (hywiki-maybe-dehighlight-page-names region-start region-end))
  (unless (hyperb:stack-frame '(hywiki-maybe-highlight-wikiwords-in-frame))
    (hywiki-maybe-directory-updated))
  nil)

(defun hywiki-maybe-highlight-wikiwords-in-frame (frame &optional skip-lookups-update-flag)
  "Highlight all non-Org link HyWiki page names displayed in FRAME.
If FRAME is t, then highlight in all windows across all frames, even
invisible ones.  With optional SKIP-LOOKUPS-UPDATE-FLAG non-nil, HyWiki
lookup tables should have already been updated and this is skipped.

Use `hywiki-word-face' to highlight.  Do not highlight references to
the current page unless they have sections attached."
  (walk-windows
   (lambda (window)
     (with-selected-window window
       ;; Display buffer before `normal-mode' triggers possibly
       ;; long-running font-locking
       (sit-for 0)
       (hywiki-maybe-highlight-page-names nil nil skip-lookups-update-flag)))
   nil frame)
  (hywiki-maybe-directory-updated))

(defun hywiki-in-page-p ()
  "Return non-nil if the current buffer is a HyWiki page.
If this is a HyWiki page and `hywiki-word-highlight-flag' is non-nil
\(the default), also enable auto-highlighting of HyWiki words as they
are typed in the buffer."
  (or hywiki-page-flag
      (when (string-prefix-p (expand-file-name hywiki-directory)
			     (or default-directory ""))
	(setq hywiki-page-flag t))))

(defun hywiki-get-buffer-page-name ()
  "Extract the page name from the buffer file name or else buffer name."
  (file-name-sans-extension (file-name-nondirectory
			     (or (hypb:buffer-file-name) (buffer-name)))))

(defun hywiki-get-page-file (file-stem-name)
  "Return possibly non-existent path in `hywiki-directory' from FILE-STEM-NAME.
FILE-STEM-NAME should not contain a directory and may have or may omit
`hywiki-file-suffix' and an optional trailing #section.

No validation of FILE-STEM-NAME is done except an empty string or null
value returns nil."
  (make-directory hywiki-directory t)
  (unless (or (null file-stem-name) (string-empty-p file-stem-name))
    (let (file-name
	  section)
      ;; Remove any suffix from `file-stem-name' and make it singular
      (if (string-match hywiki-word-suffix-regexp file-stem-name)
	  (setq section (match-string 0 file-stem-name)
		file-name (hywiki-get-singular-wikiword
			   (substring file-stem-name 0 (match-beginning 0))))
	(setq file-name file-stem-name))
      (concat (expand-file-name file-name hywiki-directory)
	      (unless (string-suffix-p hywiki-file-suffix file-name)
		hywiki-file-suffix)
	      section))))

(defun hywiki-get-referent (wikiword)
  "Return the referent of HyWiki WIKIWORD or nil if it does not exist.
If it is a pathname, expand it relative to `hywiki-directory'."
  (when (and (stringp wikiword) (not (string-empty-p wikiword))
	     (string-match hywiki-word-with-optional-suffix-exact-regexp wikiword))
    (let* ((suffix (cond ((match-beginning 2)
			   (prog1 (substring wikiword (match-beginning 2))
			     ;; Remove any #section suffix in `wikiword'.
			     (setq wikiword (match-string-no-properties 1 wikiword))))
			  ((match-beginning 3)
			   (prog1 (substring wikiword (match-beginning 3))
			     ;; Remove any :Lnum:Cnum suffix in `wikiword'.
			     (setq wikiword (match-string-no-properties
					     1 wikiword))))))
	   (referent (hash-get (hywiki-get-singular-wikiword wikiword)
			       (hywiki-get-referent-hasht))))
      ;; If a referent type that can include a # or :L line
      ;; number suffix, append it to the referent-value.
      (setq referent (hywiki--add-suffix-to-referent suffix referent)))))

(defun hywiki-get-page-files ()
  "Return the list of existing HyWiki page file names.
These must end with `hywiki-file-suffix'."
  (when (stringp hywiki-directory)
    (make-directory hywiki-directory t)
    (when (file-readable-p hywiki-directory)
      (directory-files
       hywiki-directory nil (concat "^" hywiki-word-regexp
				    (regexp-quote hywiki-file-suffix) "$")))))

(defun hywiki-get-referent-hasht ()
  "Return hash table of existing HyWiki referents.
May recreate the hash table as well as the list of
regexps of wikiwords, if the hash table is out-of-date."
  (prog1
      (if (and (equal hywiki--pages-directory hywiki-directory)
	       ;; If page files changed, have to rebuild referent hash table
	       (not (hywiki-directory-modified-p))
	       (hash-table-p hywiki--referent-hasht)
	       (not (hash-empty-p hywiki--referent-hasht)))
	  hywiki--referent-hasht
	;; Rebuild referent hash table
	(hywiki-make-referent-hasht))
    (unless hywiki--any-wikiword-regexp-list
      ;; Compute these expensive regexps (matching 50
      ;; hywiki words at a time) only if the set of
      ;; HyWikiWords changed in `hywiki-directory'.
      (setq hywiki--any-wikiword-regexp-list
	    (mapcar (lambda (wikiword-sublist)
		      ;; Add plurals to the list
		      (setq wikiword-sublist
			    (delq nil (nconc wikiword-sublist
					     (mapcar #'hywiki-get-plural-wikiword wikiword-sublist))))
		      (concat (regexp-opt wikiword-sublist 'words)
			      "\\(" hywiki-word-section-regexp "??" hywiki-word-line-and-column-numbers-regexp "?" "\\)"
			      hywiki--buttonize-character-regexp))
		    (hypb:split-seq-into-sublists
		     (hash-map #'cdr hywiki--referent-hasht) 25)))
      ;; This may have been called after a HyWiki page is deleted.
      ;; References to it may be highlighted in any frame, so need to
      ;; walk across all frames here, rehighlighting HyWikiWords.
      (hywiki-maybe-highlight-wikiwords-in-frame t t))))

(defun hywiki-get-wikiword-list ()
  "Return a list of the HyWiki page names."
  (hash-map #'cdr (hywiki-get-referent-hasht)))

(defun hywiki-get-plural-wikiword (wikiword)
  "Return the pluralized version of the given WIKIWORD.
`hywiki-allow-plurals-flag' must be non-nil or nil is always returned."
  ;; You add "-es" to make a noun plural when the singular noun ends
  ;; in "s", "x", "z", "sh", or "ch".  However, there are some
  ;; exceptions to this rule, such as words ending in "-ch" that are
  ;; pronounced with a hard "k", like "monarchs" and "stomachs".
  (when hywiki-allow-plurals-flag
    (cond ((let ((case-fold-search t))
	     (string-match-p "\\(es\\|.[^es]s\\)$" wikiword))
	   ;; Already plural
	   wikiword)
	  ((let ((case-fold-search t))
	     (string-match-p "\\(ch\\|sh\\|[sxz]\\)$" wikiword))
	   (concat wikiword (if (string-match-p "[[:lower:]]" wikiword)
				"es"
			      "ES")))
	  (t (concat wikiword (if (string-match-p "[[:lower:]]" wikiword)
				  "s"
				"S"))))))

(defun hywiki-get-singular-wikiword (wikiword)
  "Return the singular version of the given WIKIWORD with any suffix removed.
If `hywiki-allow-plurals-flag' is nil, return unchanged WIKIWORD name
with any suffix removed."
  (setq wikiword (hywiki-word-strip-suffix wikiword))
  (if (or (not hywiki-allow-plurals-flag)
	  (not (stringp wikiword)))
      wikiword
    (or (when (let ((case-fold-search t))
		;; Handle typical pluralized words ending in 's' (not preceded
		;; by an 's') or 'es'
		(string-match-p "\\(ch\\|sh\\|[sxz]\\)es$" wikiword))
	  (substring wikiword 0 -2))
	(when (let ((case-fold-search t))
		(and (string-match-p ".[^eEsS]s$" wikiword)
		     (not (string-match-p "emacs$" wikiword))))
	  (substring wikiword 0 -1))
	wikiword)))

(defun hywiki-kill-buffer-hook ()
  "Delete file attached to HyWiki buffer if the file is zero-sized.
If deleted, update HyWikiWord highlighting across all frames."
  (when (hywiki-in-page-p)
    (when (hypb:empty-file-p)
      (delete-file (hypb:buffer-file-name)))
    (when (hywiki-directory-modified-p)
      ;; Rebuild lookup tables if any HyWiki page name has changed
      (hywiki-get-referent-hasht)
      t)
    nil))

(defun hywiki-clear-referent-hasht ()
  "Clear all elements from the HyWiki referent hash table and return it."
  (setq hywiki--referent-hasht nil
	hywiki--any-wikiword-regexp-list nil))

(eval-and-compile
  '(when (featurep 'company)
     (defun hywiki-company-hasht-backend (command &optional _arg &rest ignored)
       "A `company-mode` backend that completes from the keys of a hash table."
       (interactive (list 'interactive))
       (when (hywiki-word-at)
	 (pcase command
	   ('interactive (company-begin-backend 'company-hash-table-backend))
	   ('prefix (company-grab-word))
	   ('candidates
	    (let ((prefix (company-grab-word)))
	      (when prefix 
		(cl-loop for key being the hash-keys in (hywiki-get-wikiword-list)
			 when (string-prefix-p prefix key)
			 collect key))))
	   ('sorted t))))))

(defvar hywiki-cache-default-file ".hywiki.eld"
  "Standard file name for storing cached data for a HyWiki.")

(defvar hywiki-cache-file nil
  "Current HyWiki cache file, if any.
If nil, use: (expand-file-name hywiki-cache-default-file hywiki-directory).")

(defun hywiki-cache-default-file (&optional directory)
  "Return a HyWiki cache file for optional DIRECTORY or `hywiki-directory'.
The filename is either the string value of `hywiki-cache-file', or else the
value of `hywiki-cache-default-file'.  The filename returned is an
absolute path."
  (expand-file-name (or hywiki-cache-file hywiki-cache-default-file)
		    (or directory hywiki-directory)))

(defun hywiki-cache-edit (cache-file)
  "Read in CACHE-FILE for editing and disable undo and backups within it."
  (prog1 (set-buffer (find-file-noselect cache-file))
    (buffer-disable-undo (current-buffer))
    (make-local-variable 'make-backup-files)
    (make-local-variable 'backup-inhibited)
    (setq make-backup-files nil
	  backup-inhibited t
	  buffer-read-only nil)))

(defun hywiki-cache-save (&optional save-file)
  "Save the modified Environment to a file.
The file is given by optional SAVE-FILE or `hywiki-cache-file'.  Also
save and potentially set `hywiki--directory-mod-time' and
`hywiki--directory-checksum'."
  (when (or (not (stringp save-file)) (equal save-file ""))
    (setq save-file (hywiki-cache-default-file)))
  (setq save-file (expand-file-name save-file hywiki-directory))
  (or (file-writable-p save-file)
      (error "(hywiki-cache-save): Non-writable Environment file, \"%s\"" save-file))
  (let ((buf (get-file-buffer save-file)))
    (when buf
      (if (buffer-modified-p buf)
	  (save-buffer)
	;; (error "(hywiki-cache-save): Attempt to kill modified Environment file failed to save, \"%s\"" save-file)
	(kill-buffer buf))))
  (let ((dir (or (file-name-directory save-file)
		 default-directory)))
    (or (file-writable-p dir)
	(error "(hywiki-cache-save): Non-writable Environment directory, \"%s\"" dir)))
  (save-window-excursion
    (let ((standard-output (hywiki-cache-edit save-file)))
      (with-current-buffer standard-output
	(erase-buffer)
	(princ ";; -*- mode:lisp-data; coding: utf-8-emacs; -*-\n")

	(princ (format "\n(setq\nhyperb:version %S\n" hyperb:version))

	(princ (format "\nhywiki-directory %S\n" hywiki-directory))

	;; Save last `hywiki-directory' mod time and checksum, nil if none.
	(princ (format "\nhywiki--directory-mod-time '%S\n" (hywiki-directory-set-mod-time)))

	(princ (format "\nhywiki--directory-checksum %S\n"
		       (hywiki-directory-set-checksum)))

	(princ "\nhywiki--referent-alist\n'")
	(hash-prin1 (hywiki-get-referent-hasht) nil t)
	(princ ")\n")

	(save-buffer)
	(if (buffer-modified-p)
	    (error "(hywiki-cache-save): Attempt to kill modified Environment file failed to save, \"%s\"" save-file)
	  (kill-buffer standard-output))))))

(defun hywiki-make-referent-hasht ()
  "Rebuld referent hasht from list of HyWiki page files and non-page entries."
  (setq hywiki--any-wikiword-regexp-list nil
	hywiki--pages-directory hywiki-directory)
  ;; Try to load from a .hywiki.eld cache file if up-to-date
  (let* ((cache-file (hywiki-cache-default-file))
	 (cache-buffer (when (file-readable-p cache-file)
			 (find-file-noselect cache-file)))
	 (hywiki-loaded-flag (when cache-buffer
			       (with-current-buffer cache-buffer
				 (widen)
				 (goto-char (point-min))
				 ;; Skip past initial comments
				 (when (re-search-forward "^(" nil t)
				   (goto-char (1- (point)))
				   (condition-case ()
				       (progn (eval (read (buffer-string)))
					      t)
				     (error nil)))))))
    (if (and hywiki-loaded-flag (not (hywiki-directory-modified-p)))
	;; Rebuild from loaded data
        (prog1 (setq hywiki--referent-hasht (hash-make hywiki--referent-alist t))
	  (setq hywiki--referent-alist nil))
      ;; Read `hywiki-directory' for current page files and merge with
      ;; non-page referents
      (let* ((page-files (hywiki-get-page-files))
	     (non-page-elts (when (hash-table-p hywiki--referent-hasht)
			      (delq nil
				    (hash-map 'hywiki-non-page-elt
					      hywiki--referent-hasht))))
	     (non-page-hasht (hash-make non-page-elts))
	     (key)
	     (page-elts (delq nil (mapcar (lambda (file)
					    (setq key (file-name-sans-extension file))
					    (unless (hash-get key non-page-hasht)
					      (cons (cons 'page file) key)))
					  page-files))))
	(setq hywiki--referent-hasht
	      (if non-page-elts
 		  (hash-merge non-page-hasht
			      (hash-make page-elts))
		(hash-make page-elts)))))))

(defun hywiki-non-page-elt (val-key)
  (unless (eq (caar val-key) 'page) val-key))

(defun hywiki--sitemap-file ()
  "Return file name for the sitemap file."
  (expand-file-name
   (org-publish-property :sitemap-filename (hywiki-org-get-publish-project))
   (org-publish-property :base-directory (hywiki-org-get-publish-project))))

(defun hywiki-org-export-function (&rest _)
  "Add to `write-contents-functions' to convert HyWikiWord links to Org links.
This is done automatically by loading HyWiki."
  (require 'org-element)
  (when (and (derived-mode-p 'org-mode)
             (not (string= (hywiki--sitemap-file) (buffer-file-name)))
	     (hyperb:stack-frame '(org-export-copy-buffer)))
    (hywiki-convert-words-to-org-links)
    (hywiki-org-maybe-add-title)))

(defun hywiki-org-get-publish-project ()
  "Return the HyWiki Org publish project, a named set of properties.
If not found, set it up and return the new project properties."
  (require 'ox-publish)
  (let ((project (assoc "hywiki" org-publish-project-alist)))
    (if (and project hywiki-org-publish-project-alist)
	project
      (hywiki-org-set-publish-project))))

(defun hywiki-org-get-publish-property (property)
  "Return the value of HyWiki Org publish PROPERTY symbol."
  (require 'ox-publish)
  (org-publish-property property (hywiki-org-get-publish-project)))

(defun hywiki-org-link-complete (&optional _arg)
  "Complete HyWiki page names for `org-insert-link'."
  (concat
   (when hywiki-org-link-type-required
     (concat hywiki-org-link-type ":"))
   (hywiki-word-read)))

;;; Next two functions derived from the denote package.
;;;###autoload
(defun hywiki-org-link-export (link description format)
  "Export a HyWikiWord Org-format `hy:' link to various formats.
The LINK, DESCRIPTION, and FORMAT are provided by the export
backend."
  (let* ((path-word-suffix (hywiki-org-link-resolve link :full-data))
         (path (when path-word-suffix
		 (file-relative-name (nth 0 path-word-suffix))))
         (path-stem (when path
		      (file-name-sans-extension path)))
         (word (nth 1 path-word-suffix))
         (suffix (nth 2 path-word-suffix))
         (desc (cond (description)
                     (suffix (when word
			       (format "%s%s" word suffix)))
                     (word)
		     (t ""))))
    (if path
	(pcase format
	  (`ascii (format "[%s] <%s:%s>" hywiki-org-link-type desc path))
	  (`html (format "<a href=\"%s.html%s\">%s</a>"
			 path-stem
			 (hpath:spaces-to-dashes-markup-anchor
			  (or suffix ""))
			 desc))
	  (`latex (format "\\href{%s.latex}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path-stem) desc))
	  (`md (format "[%s](%s.md%s)" desc path-stem
		       (hpath:spaces-to-dashes-markup-anchor
			(or suffix ""))))
	  (`texinfo (format "@uref{%s.texi,%s}" path-stem desc))
	  (_ path))
      link)))

(defun hywiki-org-link-resolve (link &optional full-data)
  "Resolve HyWikiWord LINK to its referent file or other type of referent.
If the referent is not a file type, return (referent-type . referent-value).

Otherwise:
Link may end with optional suffix of the form: (#|::)section:Lnum:Cnum.
With optional FULL-DATA non-nil, return a list in the form of (pathname
word suffix); otherwise, with a section, return pathname::section, with
just line and optionally column numbers, return pathname:Lnum:Cnum and
without any suffix, return just the pathname."
  (when (stringp link)
    (when (string-match (concat "\\`" hywiki-org-link-type ":") link)
      ;; Remove hy: link prefix
      (setq link (substring link (match-end 0))))
    (let* ((suffix-type (and (string-match hywiki-word-suffix-regexp link)
			     (match-string 1 link)))
	   (suffix (and suffix-type (match-string 2 link)))
           (word (if (and suffix (not (string-empty-p suffix)))
                     (substring link 0 (match-beginning 0))
		   link))
           (referent (and word (hywiki-get-referent word)))
	   (referent-type (car referent))
           (pathname (when (memq referent-type '(page path-link))
		       (cdr referent))))
      (if (stringp pathname)
	  (cond
	   (full-data
	    (list pathname word (concat suffix-type suffix)))
	   ((and suffix (not (string-empty-p suffix)))
	    (if (equal suffix-type ":L")
		(concat pathname suffix-type suffix)
	      (concat pathname "::" suffix)))
	   (t pathname))
	referent))))

(defun hywiki-org-link-store ()
  "Store a link to a HyWiki word at point, if any."
  (when (hywiki-word-at)
    (let* ((page-name (hywiki-word-at))
	   (link (concat
		  (when hywiki-org-link-type-required
		    (concat hywiki-org-link-type ":"))
		  page-name)))
      (org-link-store-props
       :type hywiki-org-link-type
       :link link
       :description page-name))))

(defun hywiki-org-maybe-add-title ()
  "Add a title to an Org buffer if it doesn't have one."
  (save-excursion
    (unless (and (re-search-forward "^#\\+TITLE:[ \t]\\|^$" nil t)
		 (not (looking-at "^$")))
      (goto-char (point-min))
      (insert "#+TITLE: "
	      (if (hypb:buffer-file-name)
		  (file-name-base (hypb:buffer-file-name))
		(buffer-name))
	      "\n"))))

(defun hywiki-org-set-publish-project ()
  "Setup and return the HyWiki Org publish project, a named set of properties.
Sets the `org-publish-project-alist' and `hywiki-org-publish-project-alist'
variables."
  (require 'ox-publish)
  (prog1 (hywiki-org-make-publish-project-alist)
    ;; Remove "hywiki" entry from `org-publish-project-alist', then update it.
    (setf (alist-get "hywiki" org-publish-project-alist nil 'remove #'equal) nil)
    (add-to-list 'org-publish-project-alist hywiki-org-publish-project-alist t)))

(eval-after-load "org"
  '(org-link-set-parameters hywiki-org-link-type
                            :complete #'hywiki-org-link-complete
			    :export #'hywiki-org-link-export
			    :follow #'hywiki-find-referent
			    :htmlize-link #'hywiki-section-to-headline-reference
			    :store #'hywiki-org-link-store))

(defun hywiki-word-strip-suffix (page-name)
  "Return PAGE-NAME with any optional #section:Lnum:Cnum stripped off.
If an empty string or not a string, return nil."
  (when (and (stringp page-name) (not (string-empty-p page-name)))
    (if (and (string-match hywiki-word-with-optional-suffix-exact-regexp page-name)
	     (or (match-beginning 2) (match-beginning 4)))
	;; Remove any #section:Lnum:Cnum suffix in PAGE-NAME.
	(match-string-no-properties 1 page-name)
      page-name)))

(defun hywiki-publish-to-html (&optional all-pages-flag)
  "Publish/export updated HyWiki pages to html.
With an optional prefix arg, ALL-PAGES-FLAG, regenerate all html
pages rather than only those HyWiki pages which have changed
since a prior publish.

Files are saved in:
    (hywiki-org-get-publish-property :publishing-directory)
Customize this directory with:
    {M-x customize-variable RET hywiki-org-publishing-directory RET}."
  (interactive "P")
  ;; Export Org to html with useful link ids.
  ;; Instead of random ids like "orga1b2c3", use heading titles with
  ;; spaces replaced with dashes, made unique when necessary.
  (unwind-protect
      (progn
	(advice-add #'org-export-get-reference :override #'hywiki--org-export-get-reference)
	(org-publish-project "hywiki" all-pages-flag))
    (advice-remove #'org-export-get-reference #'hywiki--org-export-get-reference)))

(defun hywiki-referent-exists-p (&optional word start end)
  "Return the HyWikiWord at point or optional HyWiki WORD, if has a referent.
If no such referent exists, return nil.

Word may be of form:
 1. HyWikiWord#section with an optional #section.
 2. If WORD is the symbol, :range, and there is a HyWikiWord at point
    with an existing referent, return the tuple of values: (word
    word-start word-end) instead of the word; otherwise, return the tuple
    \='(nil nil nil).

When using the word at point, a call to `hywiki-active-in-current-buffer-p'
at point must return non-nil or this function will return nil."
  (let ((save-input-word word))
    (when (stringp word)
      (setq word (hywiki-strip-org-link word)))
    (if (or (stringp word)
	    (setq word (hywiki-word-at word)))
	(unless (hywiki-get-referent (if (stringp word) word (nth 0 word)))
	  (setq word nil))
      (setq word nil))
    (when (and (listp word) (= (length word) 3))
      (setq start (nth 1 word)
	    end   (nth 2 word)
	    ;; `word' must be set last so list version can be referenced
	    ;; first above
	    word  (nth 0 word)))
    (if (eq save-input-word :range)
	(list word start end)
      word)))

(defun hywiki-section-to-headline-reference ()
  "Replace file#section dashes with spaces to match to an Org headline.
Does replacement only when not in a programming mode and section
contains no spaces."
 (let ((link (get-text-property (point) 'org-link)))
   (if (and link (string-match "#" link))
       (let* ((file (substring link 0 (match-beginning 0)))
              (section (substring link (match-beginning 0))))
	 (concat file (hpath:dashes-to-spaces-markup-anchor section)))
     link)))

(defun hywiki-strip-org-link (link-str)
  "Return the hy:HyWikiWord#section part of an Org link string.
Strip any square bracket delimiters, description and leading or
trailing whitespace, and type prefix.  Return nil, if no match."
  (when (and (stringp link-str) (not (string-empty-p link-str)))
    (string-remove-prefix
     (concat hywiki-org-link-type ":")
     (let ((blank "[[:blank:]\r\n]+"))
       (string-trim (car (delete ""
				 (mapcar (lambda (str)
					   (string-trim (replace-regexp-in-string blank " " str t t)
							blank blank))
					 (split-string link-str "\\[\\[\\|\\]\\[\\|\\]\\]")))))))))

;;;###autoload
(defun hywiki-tags-view (&optional todo-only match view-buffer-name)
  "Prompt for colon-separated Org tags and display matching HyWiki page sections.
With optional prefix arg TODO-ONLY, limit matches to HyWiki Org
todo items only.  With optional MATCH, an Org tags match selector
string, e.g. \":tag1:tag2:tag3:\", match to sections that contain
or inherit all of these tags, regardless of tag order.  With
optional VIEW-BUFFER-NAME, use that rather than the default,
\"*HyWiki Tags*\"."
  (interactive "P")
  (require 'org-agenda)
  (let* ((org-agenda-files (list hywiki-directory))
	 (org-agenda-buffer-name (or view-buffer-name "*HyWiki Tags*"))
	 ;; `org-tags-view' is mis-written to require setting this next
	 ;; tmp-name or it will not properly name the displayed buffer.
	 (org-agenda-buffer-tmp-name org-agenda-buffer-name))
    ;; This prompts for the tags to match and uses `org-agenda-files'.
    (org-tags-view todo-only match)
    (when (equal (buffer-name) org-agenda-buffer-name)
      ;; Set up {C-u r} redo cmd
      (let (buffer-read-only)
	(put-text-property (point-min) (point-max) 'org-redo-cmd
			   `(hywiki-tags-view
			       ,todo-only
			       nil
			       ,org-agenda-buffer-name)))
      (forward-line 2))))

(defun hywiki-validate-referent (referent)
  "Return t if REFERENT is valid, otherwise trigger an error."
  (if (and (consp referent)
	   (symbolp (car referent))
	   (cdr referent))
      t
    (error (concat "(hywiki-add-referent): Invalid referent"
		   "\n  must be a cons of (<type-symbol) . <value>)"
		   "\n  not %S")
	   referent)))

(defun hywiki-word-activate (&optional arg)
  "Display HyWiki referent for wikiword at point.
If referent is a non-existent HyWiki page, create it.  When this
is the first HyWiki page, prompt before creating in case this is
not what was intended.

If found, return the referent.

If not on a HyWikiWord and optional prefix ARG is null, emulate an
Action Key press; with a prefix ARG, emulate an Assist Key press."
  (interactive "P")
  (let ((word (hywiki-word-at)))
    (if word
	(hywiki-find-referent word)
      (hkey-either arg))))

(defun hywiki-word-highlighted-at-p (&optional range-flag)
  "Return highlighted HyWikiWord and optional #section:Lnum:Cnum at point or nil.
If the HyWikiWord is delimited, point must be within the delimiters.

With optional RANGE-FLAG, return a list of (HyWikiWord start-position
end-position); the positions include the entire
HyWikiWord#section:Lnum:Cnum string but exclude any delimiters.

This does not test whether a referent exists for the HyWiki word; call
`hywiki-referent-exists-p' without an argument for that.

A call to `hywiki-active-in-current-buffer-p' at point must return non-nil
or this will return nil."
  (when (hywiki-active-in-current-buffer-p)
    (if (setq hywiki--range
	      (hproperty:char-property-range (point) 'face hywiki-word-face))
	(let ((wikiword (buffer-substring-no-properties (car hywiki--range) (cdr hywiki--range))))
	  (if (string-match hywiki-word-with-optional-suffix-exact-regexp wikiword)
	      (if range-flag
		  (list wikiword (car hywiki--range) (cdr hywiki--range))
		wikiword)
	    (when range-flag
	      '(nil nil nil)))))))

(defun hywiki-word-at (&optional range-flag)
  "Return potential HyWikiWord and optional #section:Lnum:Cnum at point or nil.
If the HyWikiWord is delimited, point must be within the delimiters.
This works regardless of whether the HyWikiWord has been highlighted
or not.

With optional RANGE-FLAG, return a list of (HyWikiWord start-position
end-position); the positions include the entire
HyWikiWord#section:Lnum:Cnum string but exclude any delimiters.

This does not test whether a referent exists for the HyWiki word; call
`hywiki-referent-exists-p' without an argument for that.

A call to `hywiki-active-in-current-buffer-p' at point must return non-nil
or this will return nil."
  (if (hywiki-active-in-current-buffer-p)
      (save-excursion
	;; Don't use `cl-destructuring-bind' here since the `hargs:delimited' call
	;; can return nil rather than the 3 arg list that would be required
	(let* ((wikiword-start-end
		(let ((start-regexp (concat "\\[\\[\\(" hywiki-org-link-type ":\\)?")))
		  (save-excursion
		    (skip-chars-backward (concat hywiki-org-link-type ":["))
		    (when (looking-at start-regexp)
		      (goto-char (match-end 0)))
		    (hargs:delimited (concat "\\[\\[\\(" hywiki-org-link-type ":\\)?")
				     "\\(\\]\\[\\|\\]\\]\\)" t t t))))
	       (wikiword (nth 0 wikiword-start-end))
	       (start    (nth 1 wikiword-start-end))
	       (end      (nth 2 wikiword-start-end)))
	  (with-syntax-table hywiki--org-mode-syntax-table
	    (if (and (cond (wikiword
		       ;; Handle an Org link [[HyWikiWord]] [[hy:HyWikiWord]]
		       ;; or [[HyWikiWord#section][Description Text]].
		       ;; Get the HyWikiWord link reference, ignoring any
		       ;; description given in the link
		       ;; Don't use next line so don't have to load all of Org
		       ;; mode just to check for HyWikiWords; however, disables
		       ;; support for Org mode aliases.
		       ;; (setq wikiword (org-link-expand-abbrev (org-link-unescape (string-trim wikiword))))
		       (setq wikiword (hywiki-strip-org-link wikiword))
		       (when (and wikiword end)
			 ;; Update start and end to newly stripped
			 ;; string positions
			 (save-excursion
			   (save-restriction
			     (narrow-to-region start end)
			     (goto-char (point-min))
			     (when (search-forward wikiword nil t)
			       (setq start (match-beginning 0)
				     end   (match-end 0))))))
		       (hywiki-word-is-p wikiword))

		      ;; Handle delimited HyWikiWord references with
		      ;; multiple words in their sections,
		      ;; e.g. (MyWikiWord WikiWord#one two three)
		      ((let ((case-fold-search nil)
			     (bol (line-beginning-position))
			     opoint)
			 ;; May be a HyWikiWord ending character to skip past
			 (skip-chars-backward (hywiki-get-buttonize-characters) bol)
			 (setq opoint (point))
			 (when (hywiki-delimited-p)
			   (unless (progn
				     ;; Skip past HyWikiWord or section with
				     ;; possible whitespace
				     (skip-syntax-backward "^$()<>._\"\'" bol)
				     (unless (= (or (char-before) 0) ?#)
				       (goto-char opoint)
				       (skip-syntax-backward "^-$()<>._\"\'" bol))
				     ;; Move to start of wikiword reference
				     (skip-chars-backward "-_*#:[:alnum:]" bol)
				     (skip-syntax-backward "-" bol)
				     ;; Preceding char must now be the
				     ;; opening delimiter or else there may
				     ;; be multiple non-section words within
				     ;; the delimiters, so reprocess and do
				     ;; not allow spaces in the #section part
				     (memq (char-syntax (or (char-before) 0))
					   '(?\( ?\< ?\")))
			     (goto-char opoint)
			     (skip-syntax-backward "^-$()<>._\"\'" bol)
			     ;; Move to start of wikiword reference
			     (skip-chars-backward "-_*#:[:alnum:]" bol)
			     (skip-syntax-backward "-" bol))
			   (when (and
				  ;; (or (bolp)
				  ;;     (string-match (regexp-quote
				  ;; 		     (char-to-string (char-before)))
				  ;; 		    "\[\(\{\<\""))
				  (progn
				    (skip-chars-forward " \t")
				    (hywiki-maybe-at-wikiword-beginning))
				  (looking-at (concat
					       hywiki-word-regexp
					       "\\(#[^][#()<>{}\"\n\r\f]+\\)?"
					       hywiki-word-line-and-column-numbers-regexp "?"))
				  ;; Can't be followed by a # character
				  (/= (or (char-after (match-end 0)) 0)
				      ?#)
				  (progn (goto-char (match-end 0))
					 (skip-syntax-forward "-")))
			     (setq start (match-beginning 0)
				   end   (match-end 0)
				   ;; No following char
				   wikiword (string-trim
					     (buffer-substring-no-properties start end)))))))

		      ;; Handle non-delimited HyWikiWord references
		      ;; with multiple dash-separated words in their sections,
		      ;; e.g. WikiWord#one-two-three.
		      ((let ((case-fold-search nil)
			     (bol (line-beginning-position))
			     opoint)
			 ;; May be a HyWikiWord ending character to skip past
			 (skip-chars-backward (hywiki-get-buttonize-characters) bol)
			 (setq opoint (point))
			 (goto-char opoint)
			 (skip-syntax-backward "^-$()<>._\"\'" bol)
			 ;; Move to start of wikiword reference
			 (skip-chars-backward "-_*#:[:alnum:]" bol)
			 (skip-syntax-backward "-" bol)
			 (when (and (or (bolp)
					(string-match (regexp-quote
						       (char-to-string (char-before)))
						      "\[\(\{\<\""))
				    (progn
				      (skip-chars-forward " \t")
				      (hywiki-maybe-at-wikiword-beginning))
				    (looking-at (concat
						 hywiki-word-regexp
						 "\\(#[^][#()<>{}\" \t\n\r\f]+\\)?"
						 hywiki-word-line-and-column-numbers-regexp "?"))
				    ;; Can't be followed by a # character
				    (/= (or (char-after (match-end 0)) 0)
					?#)
				    (goto-char (match-end 0)))
			   (setq start (match-beginning 0)
				 end   (match-end 0)
				 ;; No following char
				 wikiword (string-trim
					   (buffer-substring-no-properties start end))))))

		      ;; Handle a non-delimited HyWikiWord with optional
		      ;; #section:Lnum:Cnum; if it is an Org link, it may
		      ;; optionally have a hy: link-type prefix.  Ignore
		      ;; wikiwords preceded by any non-whitespace
		      ;; character, except any of these: "([\"'`'"
		      (t (let ((case-fold-search nil))
			   (skip-chars-forward " \t")
			   (when (hywiki-maybe-at-wikiword-beginning)
			     (when (looking-at (concat hywiki-org-link-type ":"))
			       (goto-char (match-end 0)))
			     (cond ((looking-at hywiki--word-and-buttonize-character-regexp)
				    (setq start (match-beginning 1)
					  end (match-end 1)
					  wikiword (string-trim
						    (buffer-substring-no-properties start end))))
				   ((and (looking-at hywiki-word-with-optional-suffix-regexp)
					 ;; Can't be followed by a # character
					 (/= (or (char-after (match-end 0)) 0)
					     ?#))
				    (setq start (match-beginning 0)
					  end   (match-end 0)
					  ;; No following char
					  wikiword (string-trim
						    (buffer-substring-no-properties start end)))))))))
		     ;; If `wikiword' has a #section, ensure there are
		     ;; no invalid chars
		     (if (and (stringp wikiword) (string-match "#" wikiword))
			 (string-match "#[^][#()<>{}\"\n\r\f]+\\'" wikiword)
		       t))
		(if range-flag
		    (progn
		      ;; Ensure wikiword is highlighted before returning it
		      (and wikiword start end
			   (not (hproperty:but-get start 'face hywiki-word-face))
			   (hywiki-referent-exists-p wikiword)
			   (hproperty:but-add start end hywiki-word-face))
		      (list wikiword start end))
		  wikiword)
	      (when range-flag
		'(nil nil nil))))))
    (when range-flag
      '(nil nil nil))))

(defun hywiki-word-at-point ()
  "Return singular HyWikiWord at point with its suffix stripped or nil.
Point should be on the HyWikiWord itself.  Suffix is anything after
the # symbol.

This does not test whether a referent exists for the HyWiki word; call
`hywiki-referent-exists-p' without an argument for that.

A call to `hywiki-active-in-current-buffer-p' at point must return non-nil
or this will return nil."
  (hywiki-get-singular-wikiword (hywiki-word-strip-suffix (hywiki-word-at))))

(defun hywiki-delimited-p (&optional pos)
  "Return non-nil if optional POS or point is surrounded by matching delimiters.
The delimited range must be two lines or less.

Use `hywiki-word-at', which calls this, to determine whether there is
a HyWikiWord at point."
  (save-excursion
    (when (natnump pos)
      (goto-char pos))
    (or (hypb:in-string-p)
	(let ((range (hargs:delimited-p "[\[<\(\{]" "[\]\}\)\>]" t t t)))
	  (when range
	    ;; Ensure closing delimiter is a match for the opening one
	    (= (matching-paren (char-before (nth 1 range)))
	       (char-after (nth 2 range))))))))

(defun hywiki-word-face-at-p (&optional pos)
  "Non-nil if but at point or optional POS has `hywiki-word-face' property."
  ;; Sometimes this can return a left over button/overlay that points
  ;; to no buffer.  Ignore this case.
  (hproperty:but-get (or pos (point)) 'face hywiki-word-face))

;;;###autoload
(defun hywiki-word-consult-grep (word)
  "Use `hywiki-consult-grep' to show occurrences of a prompted for HyWikiWord.
Default to any HyWikiWord at point."
  (interactive (list (hywiki-word-read)))
  (if (and (stringp word) (not (string-empty-p word)))
      (hywiki-consult-grep (concat "\\b" (regexp-quote word) "\\b"))
    (user-error "(hywiki-word-consult-grep): Invalid HyWikiWord: '%s'; must be capitalized, all alpha" word)))

(defun hywiki-word-grep (wikiword)
  "Grep for occurrences of WIKIWORD with `consult-grep' or normal-grep'.
Search across `hywiki-directory'."
  (if (fboundp 'consult-grep) ;; allow for autoloading
      (hywiki-word-consult-grep wikiword)
    (grep (string-join (list grep-command (format "'%s'" wikiword)
			     (concat (file-name-as-directory hywiki-directory)
				     "*" hywiki-file-suffix))
		       " "))))

(defun hywiki-word-is-p (word)
  "Return non-nil if WORD is a HyWikiWord and optional #section:Lnum:Cnum.
WORD may not yet have a referent (non-existent).  Use `hywiki-get-referent'
to determine whether a HyWikiWord referent exists. 

Return nil if WORD is a prefixed, typed hy:HyWikiWord, since
these are handled by the Org mode link handler."
  (and (stringp word) (not (string-empty-p word))
       (let (case-fold-search)
	 (and (or (string-match hywiki-word-with-optional-suffix-exact-regexp word)
		  ;; For now this next version allows spaces and tabs in
		  ;; the suffix part
		  (eq 0 (string-match
			 hywiki-word-with-optional-suffix-exact-regexp
			 word)))
	      ;; If has a #section, ensure there are no invalid chars
	      (if (string-match "#" word)
		  (string-match "#[^][#()<>{}\"\n\r\f]+\\'" word)
		t)))))

(defun hywiki-word-read (&optional prompt)
  "Prompt with completion for and return an existing HyWikiWord.
If point is on one, press RET immediately to use that one."
  (let ((completion-ignore-case t))
    (completing-read (if (stringp prompt) prompt "HyWikiWord: ")
		     (hywiki-get-referent-hasht)
		     nil t nil nil (hywiki-word-at-point))))

(defun hywiki-word-read-new (&optional prompt)
  "Prompt with completion for and return a new HyWikiWord.
If point is on one, press RET immediately to use that one."
  (let ((completion-ignore-case t))
    (completing-read (if (stringp prompt) prompt "HyWikiWord: ")
		     (hywiki-get-referent-hasht)
		     nil nil nil nil (hywiki-word-at-point))))

(defun hywiki-word-highlight-flag-changed (symbol set-to-value operation _where)
  "Watch function for variable ``hywiki-word-highlight-flag'.
Function is called with 4 arguments: (SYMBOL SET-TO-VALUE OPERATION WHERE).
Highlight/dehighlight HyWiki page names across all frames on change."
  (unless (memq operation '(let unlet)) ;; not setting global value
    (set symbol set-to-value)
    (hywiki-word-set-auto-highlighting set-to-value)))

(defun hywiki-word-set-auto-highlighting (arg)
  "With a prefix ARG, turn on HyWikiWord auto-highlighting.
Otherwise, turn it off.

Auto-highlighting uses pre- and post-command hooks.  If an error
occurs with one of these hooks, the problematic hook is removed.
Invoke this command with a prefix argument to restore the
auto-highlighting."
  (interactive "P")
  (if arg
      ;; enable
      (progn
	(when hywiki-word-highlight-flag
	  (add-hook 'pre-command-hook      'hywiki-debuttonize-non-character-commands 95)
          (add-hook 'post-command-hook     'hywiki-buttonize-non-character-commands 95)
	  (add-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
	  (add-hook 'window-buffer-change-functions
		    'hywiki-maybe-highlight-wikiwords-in-frame)
	  (add-to-list 'yank-handled-properties
		       '(hywiki-word-face . hywiki-highlight-on-yank))
	  (hywiki-maybe-highlight-wikiwords-in-frame t))
	(when (called-interactively-p 'interactive)
	  (if hywiki-word-highlight-flag
	      (message "HyWikiWord page auto-highlighting enabled")
	    (message "`hywiki-word-highlight-flag' must first be set to t to enable auto-highlighting"))))
    ;; disable
    (remove-hook 'pre-command-hook      'hywiki-debuttonize-non-character-commands)
    (remove-hook 'post-command-hook     'hywiki-buttonize-non-character-commands)
    (remove-hook 'post-self-insert-hook 'hywiki-buttonize-character-commands)
    (hywiki-mode 0) ;; also dehighlights HyWiki words outside of HyWiki pages
    (remove-hook 'window-buffer-change-functions
		 'hywiki-maybe-highlight-wikiwords-in-frame)
    (hywiki-maybe-highlight-wikiwords-in-frame t)
    (setq yank-handled-properties
	  (delete '(hywiki-word-face . hywiki-highlight-on-yank)
		  yank-handled-properties))
    (when (called-interactively-p 'interactive)
      (message "HyWikiWord page auto-highlighting disabled"))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hywiki--buttonized-region-p ()
  "Return non-nil when hywiki--buttonize-start/end point to the current buffer."
  (and (marker-position hywiki--buttonize-start)
       (eq (marker-buffer hywiki--buttonize-start) (current-buffer))
       (marker-position hywiki--buttonize-end)
       (eq (marker-buffer hywiki--buttonize-end) (current-buffer))))

(defun hywiki--add-suffix-to-referent (suffix referent)
  "Add SUFFIX to REFERENT's value and return REFERENT.
SUFFIX includes its type prefix, e.g. #.  Return nil if any input is
invalid.  Appended only if the referent-type supports suffixes."
  (if (or (null suffix) (and (stringp suffix) (string-empty-p suffix)))
      referent
    (when (consp referent)
      (let ((referent-type (car referent))
	    (referent-value (cdr referent)))
	(when (and (symbolp referent-type) referent-value)
	  (if (and (stringp suffix)
		   (stringp referent-value)
		   (memq referent-type hywiki-allow-suffix-referent-types)
		   (not (seq-contains-p referent-value ?# #'=)))
	      ;; Need to insert #suffix into referent's value
	      (progn
		(setq referent-value
		      (if (string-match hpath:line-and-column-regexp referent-value)
			  (concat (substring 0 (match-beginning 0))
				  suffix
				  (match-string 0 referent-value))
			(concat referent-value suffix)))
		(cons referent-type referent-value))
	    referent))))))

(defun hywiki--extend-yanked-region (start end)
  "Extend range (START END) with any delimited regions and return the new range.
Typically used to extend a yanked region to fully include any strings
or balanced pair delimiters."
  (let ((delim-distance 0)
	(result (list start end))
	opoint)

    ;; Skip past all delimited ranges and extend `end' as needed
    (save-excursion
      (goto-char start)
      (while (and (<= (point) end)
		  (not (zerop (setq delim-distance (skip-syntax-forward "^\(" end)))))
	(condition-case nil
	    (progn (goto-char (+ (point) delim-distance))
		   (setq opoint (point))
		   (setq end (max end (goto-char (scan-sexps (point) 1)))
			 result (list start end)))
	  (error (goto-char (min (1+ opoint) end))))))

    ;; Skip past all double-quoted ranges and extend `start' and `end' as needed
    (save-excursion
      (goto-char start)
      (while (and (<= (point) end)
		  (not (zerop (setq delim-distance (skip-syntax-forward "^\"" end)))))
	(condition-case nil
	    (progn (goto-char (+ (point) delim-distance))
		   (setq opoint (point))
		   (if (hypb:in-string-p)
		       (progn (goto-char (1+ (point)))
			      (setq start (min start (goto-char (scan-sexps (1+ (point)) -1))))
			      (goto-char (min (1+ opoint) end)))
		     ;; before a string
		     (setq end (max end (goto-char (scan-sexps (point) 1)))))
		   (setq result (list start end)))
	  (error (goto-char (min (1+ opoint) end))))))

    ;; Skip past closing delimiter and extend `start' if needed
    (save-excursion
      (goto-char start)
      (while (and (<= (point) end)
		  (not (zerop (setq delim-distance (skip-syntax-forward "^\)" end)))))
	(condition-case nil
	    (progn (goto-char (+ (point) delim-distance))
		   (setq opoint (point))
		   (setq start (min start (goto-char (scan-sexps (1+ (point)) -1)))
			 result (list start end))
		   (goto-char (min (1+ opoint) end)))
	  (error (goto-char (min (1+ opoint) end))))))
      result))

(defun hywiki--get-delimited-range-backward ()
  "Return a list of (start end) if not between/after end ]] or >>.
Delimiters are included in the range.  Point must be on or after the
closing delimiter.  Otherwise, return nil."
  (save-excursion
    (unless (or (eq (char-before) (char-before (1- (point))))
		(and (char-after)
		     (goto-char (1+ (point)))
		     (eq (char-before) (char-before (1- (point))))))
      (nreverse (list (point) (scan-sexps (point) -1))))))

(defun hywiki--get-delimited-range-forward ()
  "Return a list of (start end) if not between/before opening [[ or <<.
Delimiters are included in the range.  Point must be on or after the
opening delimiter.  Otherwise, return nil."
  (save-excursion
    (unless (or (eq (char-after) (char-after (1+ (point))))
		(and (char-before)
		     (goto-char (1- (point)))
		     (eq (char-after) (char-after (1+ (point))))))
      (list (point) (scan-sexps (point) 1)))))

(defun hywiki--maybe-de/highlight-org-element-backward (func)
  "De/Highlight HyWikiWords with FUNC on: ], ]], >, or >> brackets.
Func must take a single numeric argument of -1 to process one
delimited grouping."
  (ignore-errors
    (unless (save-excursion
	      (when (or (eq (char-before) (char-before (1- (point))))
			(and (char-after)
			     (goto-char (1+ (point)))
			     (eq (char-before) (char-before (1- (point))))))
		;; double delimiters - dehighlight
		(let* ((sexp-end (point))
		       (sexp-start (scan-sexps sexp-end -1)))
		  (when sexp-start
		    (hproperty:but-clear-all-in-list
		     (hproperty:but-get-all-in-region
		      sexp-start sexp-end 'face hywiki-word-face))
		    (setq hywiki--highlighting-done-flag t)))))
      ;; single delimiters - highlight
      (funcall func -1))))

(defun hywiki--maybe-de/highlight-org-element-forward (func)
  "De/Highlight HyWikiWords with FUNC on: [, [[, <, or << brackets.
Func must take a single numeric argument of 1 to process one
delimited grouping."
  (ignore-errors
    (unless (save-excursion
	      (when (or (eq (char-after) (char-after (1+ (point))))
			(and (char-before)
			     (goto-char (1- (point)))
			     (eq (char-after) (char-after (1+ (point))))))
		;; double delimiters - dehighlight
		(let* ((sexp-start (point))
		       (sexp-end (scan-sexps sexp-start 1)))
		  (when sexp-end
		    (hproperty:but-clear-all-in-list
		     (hproperty:but-get-all-in-region
		      sexp-start sexp-end 'face hywiki-word-face))
		    (setq hywiki--highlighting-done-flag t)))))
      ;; single delimiters - highlight
      (funcall func 1))))

(defun hywiki--maybe-de/highlight-sexp (func direction-number &optional sexp-start sexp-end)
  "De/highlight HyWikiWord with FUNC on a single paired delimiter char.
DIRECTION-NUMBER is 1 for forward scanning and -1 for backward scanning."
  (setq sexp-start (or sexp-start (point))
	sexp-end (or sexp-end (scan-sexps sexp-start direction-number)))
  (when (and sexp-start sexp-end)
    (cl-destructuring-bind (start end)
	;; Point may be at end of sexp, so start and end may
	;; need to be reversed.
	(list (min sexp-start sexp-end) (max sexp-start sexp-end))
      ;; Increment sexp-start so regexp matching excludes the
      ;; delimiter and starts with the HyWikiWord.  But include any
      ;; trailing delimiter or regexp matching will not work.
      (save-restriction
	(narrow-to-region (1+ start) end)
	(prog1 (funcall func (1+ start) end)
	  (setq hywiki--highlighting-done-flag nil))))))

(defun hywiki--maybe-dehighlight-at-point ()
  "Dehighlight any existing HyWikiWord when needed.
That is, only if the editing command has changed the word-only part of
the HyWikiWord reference."
  (when (and hywiki--word-pre-command
	     (not (equal hywiki--word-pre-command
			 (hywiki-get-singular-wikiword
			  (or (car hywiki--range)
			      (when (hywiki--buttonized-region-p)
				(buffer-substring hywiki--buttonize-start
						  hywiki--buttonize-end))
			      (when (and (setq hywiki--range (hywiki-word-at :range))
					 (nth 1 hywiki--range))
				(prog1 (nth 1 hywiki--range)
				  (setq hywiki--range nil)))
)))))
    ;; Dehighlight if point is on or between a HyWikiWord
    (hywiki-maybe-dehighlight-between-page-names)))

(defun hywiki--maybe-rehighlight-at-point ()
  "Dehighlight any existing HyWikiWord when needed.
That is, only if the editing command has changed the word-only part of
the HyWikiWord reference.

This must be called within a `save-excursion' or it may move point."

  (hywiki--maybe-dehighlight-at-point)

  ;; Highlight wikiwords around point as needed
  (when hywiki--range
    (hywiki-maybe-highlight-on-page-name))

  (when (hywiki--buttonized-region-p)
    (hywiki--maybe-de/highlight-sexp
     #'hywiki-maybe-highlight-page-names 1
     hywiki--buttonize-start hywiki--buttonize-end))

  (cond ((= (char-syntax (or (char-before) 0)) ?\ )
	 (goto-char (1- (point)))
	 (hywiki-maybe-highlight-between-page-names))
	((= (char-syntax (or (char-after) 0)) ?\ )
	 (hywiki-maybe-highlight-between-page-names))))

;;; ************************************************************************
;;; Private Org export override functions
;;; ************************************************************************

;; Thanks to alphapapa for the GPLed code upon which these hywiki--org
;; functions are based.  These change the html ids that Org export
;; generates to use the text of headings rather than randomly
;; generated ids.

(require 'cl-extra) ;; for `cl-some'
(require 'ox)       ;; for `org-export-get-reference'
(require 'url-util) ;; for `url-hexify-string'

(defun hywiki--org-export-get-reference (datum info)
  "Return a unique reference for DATUM, as a string.
Like `org-export-get-reference' but uses modified heading strings as
link ids rather than generated ids.  To form an id, spaces in headings
are replaces with dashes and to make each id unique, heading parent
ids are prepended separated by '--'.

DATUM is either an element or an object.  INFO is the current
export state, as a plist.

References for the current document are stored in
`:internal-references' property.  Its value is an alist with
associations of the following types:

  (REFERENCE . DATUM) and (SEARCH-CELL . ID)

REFERENCE is the reference string to be used for object or
element DATUM.  SEARCH-CELL is a search cell, as returned by
`org-export-search-cells'.  ID is a number or a string uniquely
identifying DATUM within the document.

This function also checks `:crossrefs' property for search cells
matching DATUM before creating a new reference."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before reusing any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (hywiki--org-export-new-title-reference datum cache))
			(cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
			(org-export-format-reference
                         (org-export-new-reference cache))))
	       (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun hywiki--org-export-new-title-reference (datum cache)
  "Return new heading title reference for DATUM that is unique in CACHE."
  (let* ((title (org-element-property :raw-value datum))
         (ref (hywiki--org-format-reference title))
         (parent (org-element-property :parent datum))
	 raw-parent)
    (while (cl-some (lambda (elt) (equal ref (car elt)))
                    cache)
      ;; Title not unique: make it so.
      (if parent
          ;; Append ancestor title.
          (setq raw-parent (org-element-property :raw-value parent)
		title (if (and (stringp raw-parent) (not (string-empty-p raw-parent)))
			  (concat raw-parent "--" title)
			title)
                ref (hywiki--org-format-reference title)
                parent (org-element-property :parent parent))
        ;; No more ancestors: add and increment a number.
        (when (string-match "\\`\\([[:unibyte:]]\\)+?\\(--\\([0-9]+\\)\\)?\\'"
			    ref)
          (let ((num (match-string 3 ref)))
            (setq parent (match-string 1 ref)
		  parent (if (stringp parent) (concat parent "--") "")
		  num (if num
                          (string-to-number num)
                        0)
		  num (1+ num)
		  ref (format "%s%s" parent num))))))
    ref))

(defun hywiki--org-format-reference (title)
  "Format TITLE string as an html id."
  (url-hexify-string
   (replace-regexp-in-string "\\[\\[\\([a-z]+:\\)?\\|\\]\\[\\|\\]\\]" ""
			     (subst-char-in-string
			      ?\  ?- 
			      (substring-no-properties title)))))

;;; ************************************************************************
;;; Private initializations
;;; ************************************************************************

;; Must be set after `hywiki-get-buttonize-characters' is defined
(unless hywiki--buttonize-characters
  (setq hywiki--buttonize-characters
	(concat "[]()<>{}\"' \t\r\n" (hywiki-get-buttonize-characters))
	hywiki--buttonize-character-regexp
	(concat "\\([]["
		(regexp-quote (substring hywiki--buttonize-characters 2))
		"]\\|$\\)")
	hywiki--word-and-buttonize-character-regexp
	(concat "\\(" hywiki-word-with-optional-suffix-regexp "\\)"
		hywiki--buttonize-character-regexp)))

;;; ************************************************************************
;;; Public initializations
;;; ************************************************************************

(add-hook 'kill-buffer-hook 'hywiki-kill-buffer-hook)

(eval-after-load "org-element"
  '(advice-add 'org-element--generate-copy-script :before #'hywiki-org-export-function))

;; Use for its side effects, setting variables
(eval-after-load "ox-publish" '(hywiki-org-get-publish-project))

(add-variable-watcher 'hywiki-word-highlight-flag
		      'hywiki-word-highlight-flag-changed)

;; Set HyWiki page auto-HyWikiWord highlighting and `yank-handled-properties'
(hywiki-word-highlight-flag-changed 'hywiki-word-highlight-flag
				    hywiki-word-highlight-flag 'set nil)

;; Ensure HyWiki referent lookup table is initialized as are HyWiki Org
;; Publish settings.
(hywiki-set-directory 'hywiki-directory hywiki-directory)

(provide 'hywiki)

;;; hywiki.el ends here
