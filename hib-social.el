;;; hib-social.el --- Implicit button type for social media/git hashtag and username references
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    20-Jul-16 at 22:41:34
;;
;; Copyright (C) 2016-2017  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This defines an implicit button type, social-reference, that displays 
;;   information (often a web page) associated with the given hashtag or username.
;;   When the referent is a web page, this calls the function given by
;;   `hibtypes-social-display-function' to display it, initially set to `browse-url'.
;;
;;   A hashtag reference is either: [facebook|github|gitlab|git|instagram|twitter]#<hashtag>
;;   or using 2-letter service abbreviations: [fb|gh|gl|gt|in|tw]#<hashtag>.
;;
;;   A username reference is either: [facebook|github|gitlab|instagram|twitter]@<username>
;;   or [fb|gh|gl|in|tw]@<username>.
;;
;;   If the social media service is not given, it defaults to the value of
;;   `hibtypes-social-default-service', initially set to \"twitter\".
;;
;;   Below are a list of examples; simply press the Action Key on each one
;;   to test it; use the Assist Key to see what it will do.  The git
;;   examples require that you have a local git clone of the Hyperbole
;;   repository.

;;     facebook@zuck                             Display user's home page
;;     github@rswgnu
;;     gitlab@seriyalexandrov
;;     instagram@lostart
;;     twitter@nytimestravel

;;     fb#technology                             Display page of hashtag matches
;;     in#art
;;     tw#travel

;;   Git (local) reference links
;;
;;     git#branches                              List branches in current repo/project
;;     git#commits                               List and browse commits for current project
;;     git#tags                                  List tags in current project
;;
;;     git#/hyperbole                            From any buffer, dired on the top
;;                                               directory of the local hyperbole
;;                                               project (notice no =)
;; 
;;     git#/hyperbole/55a1f0 or                  From any buffer, display hyperbole
;;     git#hyperbole/55a1f0                      local git commit diff
;;                                               
;;     git#55a1f0                                Based on current default-directory,
;;                                               display current repo's local git
;;                                               commit diff; works when default-directory
;;                                               is inside a git project with commit
;;                                               hashtag 55a1f0
;;
;;     commit 55a1f0                             Commits listed in 'git log' output
;;                                               also display diffs.
;;
;;     (setq hibtypes-git-default-project "hyperbole")
;;     git#55a1f0                                From any buffer, once the above default
;;                                               is set, display current project's local
;;                                               git commit diff
;;     git#master                                Show latest commit entry and diff for branch
;;     git#hyperbole-7.0.0                       From any buffer, show the commit diff
;;                                               for tag `hyperbole-7.0.0'
;;
;;     When you want to be more explicit, use:
;;       
;;       git#commit/55a1f0
;;       git#branch/master
;;       git#tag/hyperbole-7.0.0
;;
;;     To edit and view git managed files (note the =):
;;
;;       git#=hibtypes.el                        Edit any local git-versioned file
;;                                               in another window; file must match
;;                                               to the last part of a pathname
;;       git#=partial-path/file
;;       git#=/path/file                         Both work, constraining the lookup more.
;;       git#=hyperbole.pdf                      Typically displays Hyperbole manual
;;                                               in an external viewer
;;
;;       git#=master:hyperbole.el                View a file or other entity from a specific branch
;;       git#=master:kotl/kview.el               View a branch file located in a project subdirectory
;;


;;   Github (remote) reference links
;;
;;     gh@rswgnu                                 Display user's home page & projects
;;
;;     github#rswgnu/hyperbole                   Display user's project
;;     gh#rswgnu/helm/global_mouse               Display user project's branch
;;     gh#rswgnu/hyperbole/55a1f0                Display user project's commit diff
;;
;;     gh#orgs/github/people (or staff)          List the org, github's staff
;;     gh#/github/fetch/contributors             List contributors to github's fetch project
;;
;;     (setq hibtypes-github-default-user "rswgnu")
;;     github#/hyperbole                         Display default user's project
;;
;;
;;     Once you set the default user and project variables, you can leave
;;     them off any reference links:
;;
;;       (setq hibtypes-github-default-user "emacs-helm")
;;       (setq hibtypes-github-default-project "helm")
;;
;;     like so:
;;
;;       gh#pulls                                List project's open pull requests (PRs)
;;       gh#pull/1871                            Display a specific project pull request
;;
;;       gh#issues                               List emacs-helm/helm's open issues
;;       gh#1878                                 Display a specific project issue (or PR)
;;
;;       gh#branches                             List project's branches
;;       gh#branch/global_mouse                  List files in a specific branch
;;       gh#global_mouse                         You can even leave off the `branch' keyword
;;
;;       gh#tags                                 List project's tagged commits, typically releases
;;       gh#tag/v2.8.4 or gh#v2.8.4              List files in a specific tagged commit
;;
;;       gh#commits                              List project's commits
;;       gh#898e55c                              Display default user and default
;;                                               project commit diff

;;   Gitlab (remote) reference links support the same reference types as Github (but
;;   substitute the gl# prefix) plus these additional reference types:
;;
;;     gl#/libertybsd/libertybsd-status          Group and project
;;
;;     gl#gitlab-org/gitlab-ce/activity          Summarize user's project activity
;;     gl#gitlab-org/gitlab-ce/analytics         Display user project's cycle_analytics
;;     gl#gitlab-org/gitlab-ce/boards            Display user project's kanban-type issue boards
;;
;;     Once you set the default user and project variables, you can leave
;;     them off any reference links:
;;
;;       (setq hibtypes-gitlab-default-user "gitlab-org")
;;       (setq hibtypes-gitlab-default-project "gitlab-ce")
;;
;;     gl#issues or gl#list                      Display default project's issue list
;;     gl#jobs                                   Display default project's computing jobs
;;     gl#labels                                 Display default project's issue categories
;;     gl#members                                Display default project's staff list
;;     gl#contributors                           Show contributor push frequency charts
;;     gl#merge_requests or gl#pulls             Display default project's pull requests
;;     gl#milestones                             Display default project's milestones status
;;     gl#pages                                  Display default project's web pages
;;     gl#pipelines                              List build and test sequences
;;     gl#pipeline_charts                        Graphical view of pipeline run results across time
;;     gl#schedules                              Display schedules for project pipelines
;;     gl#snippets                               Project snippets, diffs and text with discussion
;;
;;     gl#groups                                 List all available groups of projects
;;     gl#projects                               List all available projects
;;
;;     gl#milestone=38                           Show a specific project milestone
;;     gl#snippet/1689487                        Show a specific project snippet

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (require 'browse-url))
(require 'hbut)
(require 'hargs)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hibtypes-social-default-service "twitter"
  "Lowercase string matching the name of the default social media service to use when none is specified."
  :type '(radio (const "facebook")
		(const "git")
		(const "github")
		(const "gitlab")
		(const "instagram")
		(const "twitter"))
  :group 'hyperbole-button)

(defcustom hibtypes-social-display-function #'browse-url
  "Function of one argument, a url, to display when a social media reference is activated."
  :type 'function
  :group 'hyperbole-button)

(defcustom hibtypes-git-default-project nil
  "Default project name to associate with any local git commit link."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-github-default-project nil
  "Default project name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-github-default-user nil
  "Default user name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-gitlab-default-project nil
  "Default project name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-gitlab-default-user nil
  "Default user name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hibtypes-social-hashtag-alist
  '(("\\`\\(fb\\|facebook\\)\\'"  . "https://www.facebook.com/hashtag/%s")
    ("\\`\\(gh\\|github\\)\\'"    . "https://github.com/%s/%s/%s%s")
    ("\\`\\(gl\\|gitlab\\)\\'"    . "https://www.gitlab.com/%s/%s/%s%s")
    ("\\`\\(gt\\|git\\)\\'"       . "(cd %s; git %s %s)")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/explore/tags/%s/")
    ("\\`\\(tw\\|twitter\\)\\'"   . "https://twitter.com/search?q=%%23%s&src=hashtag")
)
  "Alist of (social-media-service-regexp  . expression-to-display-hashtag-reference) elements.")

(defconst hibtypes-social-username-alist
  '(("\\`\\(fb\\|facebook\\)\\'"  . "https://www.facebook.com/%s")
    ("\\`\\(gh\\|github\\)\\'"    . "https://github.com/%s/")
    ("\\`\\(gl\\|gitlab\\)\\'"    . "https://www.gitlab.com/%s/")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/%s/")
    ("\\`\\(tw\\|twitter\\)\\'"   . "https://twitter.com/search?q=@%s")
    )
  "Alist of (social-media-service-regexp  . url-with-%s-for-username) elements.")

;; Assume at least a 2-character project name
(defconst hibtypes-git-project-regexp "/?[[:alnum:]]+[-=._/[:alnum:]]*[-=_[:alnum:]]")
(defconst hibtypes-git-file-regexp "=[-=.:_/[:alnum:]]*[-=_/[:alnum:]]")

(defconst hibtypes-social-regexp
  (concat "\\([[:alpha:]]*\\)\\([#@]\\)"
	  "\\(" hibtypes-git-project-regexp "\\|" hibtypes-git-file-regexp "\\)")
  "Regular expression that matches a social media/git hashtag or username reference.
See `ibtypes::social-reference' for format details.")

(defvar hibtypes-social-inhibit-modes '(texinfo-mode para-mode)
  "*List of major modes in which to inhibit any possible social media tag matches.")

;;; ************************************************************************
;;; Public Button Types
;;; ************************************************************************

(defib social-reference ()
  "Display the web page associated with a social hashtag or username reference at point.
Reference format is:
  [facebook|git|github|gitlab|instagram|twitter]?[#@]<reference> or
  [fb|gt|gh|gl|in|tw]?[#@]<reference>.

The first part of the label for a button of this type is the social
service name.  The service name defaults to the value of
`hibtypes-social-default-service' (default value of \"twitter\")
when not given, so #hashtag would be the same as twitter#hashtag.

Local git references allow hashtags only, not username references.

This will not match within any single line, single or
double-quoted strings or within any buffer whose major mode is
listed in `hibtypes-social-inhibit-modes'."
  (when (and (not (or (memq major-mode hibtypes-social-inhibit-modes)
		      (hargs:delimited "\"" "\"")
		      (hargs:delimited "[\`\']" "\'" t)
		      ;; Avoid Markdown parenthesized hash links
		      (and (eq major-mode 'markdown-mode)
			   (hargs:delimited "(" ")"))))
	     (save-excursion
	       (if (looking-at "[-#@=/.:_[:alnum:]]")
		   (skip-chars-backward "-#@=/.:_[:alnum:]"))
	       (and (looking-at hibtypes-social-regexp)
		    ;; Ensure prefix matches to a social web service
		    (save-match-data
		      (let ((ref (match-string-no-properties 1)))
			(delq nil (mapcar (lambda (regexp) (string-match regexp ref))
					  (mapcar #'car hibtypes-social-hashtag-alist)))))
		    ;; Heuristic to ensure this is not an email address
		    (save-match-data
		      (not (and (looking-at mail-address-regexp)
				(let ((case-fold-search t))
				  (string-match mail-address-tld-regexp
						(match-string-no-properties 1)))))))))

    (save-match-data
      (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0)))
    (let ((ref (match-string-no-properties 0))
	  (service (match-string-no-properties 1))
	  (ref-kind-str (match-string-no-properties 2))
	  (after-hash-str (match-string-no-properties 3)))
      (cond ((string-match "\\`\\(gt\\|git\\)#" ref)
	     (hact 'git-reference after-hash-str))
	    ((string-match "\\`\\(gh\\|github\\)#" ref)
	     (hact 'github-reference after-hash-str))
	    ((string-match "\\`\\(gl\\|gitlab\\)#" ref)
	     (hact 'gitlab-reference after-hash-str))
	    (t (hact 'social-reference service ref-kind-str after-hash-str))))))

;; Don't make this a defact or its arguments may be improperly expanded as pathnames.
(defun social-reference (service ref-kind-str hashtag-or-username)
  "Display the web page at social media SERVICE for REF-KIND-STR and HASHTAG-OR-USERNAME.
REF-KIND-STR is either \"#\" for a hashtag reference or \"@\" for a username reference."
  (if (or (null service) (equal service "")) (setq service hibtypes-social-default-service))
  (let ((case-fold-search t)
	expr-to-format)
    (when (or (and (equal ref-kind-str "#")
		   (setq expr-to-format
			 (assoc-default service hibtypes-social-hashtag-alist #'string-match)))
	      (and (equal ref-kind-str "@")
		   (setq expr-to-format
			 (assoc-default service hibtypes-social-username-alist #'string-match))))
      (if expr-to-format
	  (funcall hibtypes-social-display-function (format expr-to-format hashtag-or-username))
	(error "(social-reference): Service `%s' does not support reference format, `%s%s'"
	       service ref-kind-str hashtag-or-username)))))

;;; Remote Github commit references

;; Don't make this a defact or its arguments may be improperly expanded as pathnames.
(defun github-reference (reference &optional user project)
  "Display the Github entity associated with REFERENCE and optional USER and PROJECT.
REFERENCE is a string of one of the following forms:
    <ref-item>
    <user>/<project>/<ref-item>
    <project>/<ref-item>
or  /<project>.

<ref-item> is one of these:
  one of the words: branches, commits, contributors, issues, people or staff,
  pulls, status or tags; the associated items are listed;

  one of the words: branch, commit, issue, pull or tag followed by a '/' or '=' and 
  an item-id; the item is shown;

  an issue reference given by a positive integer, e.g. 92 or prefaced with GH-, e.g. GH-92;
  the issue is displayed;

  a commit reference given by a hex number, 55a1f0; the commit diff is displayed;

  a branch or tag reference given by an alphanumeric name, e.g. hyper20; the
  files in the branch are listed.

USER defaults to the value of `hibtypes-github-default-user'.
If given, PROJECT overrides any project value in REFERENCE.  If no
PROJECT value is provided, it defaults to the value of
`hibtypes-github-default-project'."
  (cond ((or (null reference) (equal reference ""))
	 (error "(github-reference): Github reference must not be empty"))
	((equal reference "status")
	 (funcall hibtypes-social-display-function "https://status.github.com"))
	(t (let ((case-fold-search t)
		 (url-to-format (assoc-default "github" hibtypes-social-hashtag-alist #'string-match))
		 (ref-type))
	     (when url-to-format
	       (cond ((string-match "\\`\\(branch\\|commit\\|issue\\|pull\\|tag\\)[/=]" reference)
		      ;; [branch | commit | issue | pull | tag]/ref-item
		      nil)
		     ((string-match "\\`/?\\(\\([^/#@]+\\)/\\)\\([^/#@]+\\)\\'" reference)
		      ;; /?user/project
		      (setq user (or user (match-string-no-properties 2 reference))
			    project (or project (match-string-no-properties 3 reference))
			    reference nil))
		     ((string-match "\\`/?\\(\\([^/#@]+\\)/\\)?\\([^/#@]+\\)/\\([^#@]+\\)\\'" reference)
		      ;; /?[user/]project/ref-item
		      (setq user (or user (match-string-no-properties 2 reference))
			    project (or project (match-string-no-properties 3 reference))
			    reference (match-string-no-properties 4 reference)))
		     ((string-match "\\`/\\([^/#@]+\\)\\'" reference)
		      ;; /project
		      (setq project (or project (match-string-no-properties 1 reference))
			    reference nil)))
	       (when (or (and project (string-match "\\`\\(members\\|people\\|staff\\)\\'" project))
			 ;; Change <org-name>/[members|people|staff] to /orgs/<org-name>/people.
			 (and reference (string-match "\\`\\(members\\|people\\|staff\\)\\'" reference)))
		 ;; Change <org-name>/project/[people|staff] to /orgs/<org-name>/people.
		 (setq project user
		       user "orgs"
		       reference "people"))
	       (when (equal reference "contributors")
		 ;; Change /user/project/contributors to /user/project/graphs/contributors.
		 (setq ref-type "graphs/"
		       reference "contributors"))
	       (unless (stringp user) (setq user hibtypes-github-default-user))
	       (unless (stringp project) (setq project hibtypes-github-default-project))
	       (when reference
		 (cond ((member reference '("branches" "commits" "contributors" "issues" "people" "pulls" "tags"))
			;; All branches, commits, contributors, open issues, people, pull requests or commit tags reference
			(setq ref-type reference
			      reference ""))
		       ((and (< (length reference) 8) (string-match "\\`\\([gG][hH]-\\)?[0-9]+\\'" reference))
			;; Issue ref-id reference
			(setq ref-type "issues/"
			      reference (substring reference (match-end 1) (match-end 0))))
		       ((string-match "\\`\\(commit\\|issue\\|pull\\)[/=]" reference)
			;; Specific reference preceded by keyword branch, commit,
			;; issue, or pull
			(setq ref-type (substring reference 0 (match-end 1))
			      reference (substring reference (match-end 0))
			      ref-type (concat ref-type (if (string-equal ref-type "issue") "s/" "/"))))
		       ((string-match "\\`[0-9a-f]+\\'" reference)
			;; Commit reference
			(setq ref-type "commit/"))
		       (t
			;; Specific branch or commit tag reference
			(setq ref-type "tree/")
			(when (string-match "\\`\\(branch\\|tag\\)/" reference)
			  ;; If preceded by optional keyword, remove that from the reference.
			  (setq reference (substring reference (match-end 0)))))))
	       (if (and (stringp user) (stringp project))
		   (funcall hibtypes-social-display-function
			    (if reference
				(format url-to-format user project ref-type reference)
			      ;; Remove trailing /
			      (substring (format url-to-format user project "" "") 0 -1)))
		 (cond ((and (null user) (null project))
			(error "(github-reference): Set `hibtypes-github-default-user' and `hibtypes-github-default-project'"))
		       ((null user)
			(error "(github-reference): Set `hibtypes-github-default-user'"))
		       (t
			(error "(github-reference): Set `hibtypes-github-default-project'")))))
	     (unless url-to-format
	       (error "(github-reference): Add an entry for github to `hibtypes-social-hashtag-alist'"))))))

;;; Remote Gitlab commit references

;; Don't make this a defact or its arguments may be improperly expanded as pathnames.
(defun gitlab-reference (reference &optional user project)
  "Display the Gitlab entity associated with REFERENCE and optional USER and PROJECT.
REFERENCE is a string of one of the following forms:
    <ref-item>
    <user>/<project>/<ref-item>
    <project>/<ref-item>
    /<group>/<project>
or  /<project-or-group> (where a group is a collection of projects).

<ref-item> is one of these:
  one of the words: activity, analytics, boards or kanban, branches, commits, contributors,
  groups, issues or list, jobs, labels, merge_requests, milestones, pages, pipelines,
  pipeline_charts, members or people or staff, projects, pulls, schedules, snippets,
  status or tags; the associated items are listed;

  one of the words: branch, commit(s), issue(s), milestone(s), pull(s), snippet(s) or
  tag(s) followed by a '/' or '=' and an item-id; the item is shown;

  an issue reference given by a positive integer, e.g. 92 or prefaced with GL-, e.g. GL-92;
  the issue is displayed;

  a commit reference given by a hex number, 55a1f0; the commit diff is displayed;

  a branch or tag reference given by an alphanumeric name, e.g. hyper20; the
  files in the branch are listed.

USER defaults to the value of `hibtypes-gitlab-default-user'.
If given, PROJECT overrides any project value in REFERENCE.  If no
PROJECT value is provided, it defaults to the value of
`hibtypes-gitlab-default-project'."
  (cond ((or (null reference) (equal reference ""))
	 (error "(gitlab-reference): Gitlab reference must not be empty"))
	((equal reference "status")
	 (funcall hibtypes-social-display-function "https://status.gitlab.com"))
	(t (let ((case-fold-search t)
		 (url-to-format (assoc-default "gitlab" hibtypes-social-hashtag-alist #'string-match))
		 (ref-type))
	     (when url-to-format
	       (cond ((string-match "\\`\\(branch\\|commits?\\|issues?\\milestones?\\|pulls?\\|snippets?\\|tags?\\)[/=]" reference)
		      ;; Reference to a specific ref-item
		      nil)
		     ((string-match "\\`/?\\(\\([^/#@]+\\)/\\)\\([^/#@]+\\)\\'" reference)
		      ;; /?user/project
		      (setq user (or user (match-string-no-properties 2 reference))
			    project (or project (match-string-no-properties 3 reference))
			    reference nil))
		     ((string-match "\\`/?\\(\\([^/#@]+\\)/\\)?\\([^/#@]+\\)/\\([^#@]+\\)\\'" reference)
		      ;; /?[user/]project/ref-item
		      (setq user (or user (match-string-no-properties 2 reference))
			    project (or project (match-string-no-properties 3 reference))
			    reference (match-string-no-properties 4 reference)))
		     ((string-match "\\`/\\([^/#@]+\\)\\'" reference)
		      ;; /project
		      (setq project (or project (match-string-no-properties 1 reference))
			    reference nil)))
	       (when (and (null (and user project)) (string-match "\\`\\(groups\\|projects\\)\\'" reference))
		 ;; List all available groups of projects or projects.
		 (setq user "explore"
		       project (match-string-no-properties 1 reference)
		       ref-type nil
		       reference nil))
	       (unless (stringp user) (setq user hibtypes-gitlab-default-user))
	       (unless (stringp project) (setq project hibtypes-gitlab-default-project))
	       (when (equal project "pages")
		 ;; Project web pages use a reverse pages/<project> URL format
		 (setq project user
		       user "pages"
		       ref-type nil
		       reference nil))
	       (when reference
		 (cond  ((string-match "\\`\\(analytics\\|cycle_analytics\\)\\'" reference)
			;; Project analytics
			(setq ref-type "cycle_analytics"
			      reference ""))
			((string-match "\\`\\(boards\\|kanban\\)\\'" reference)
			;; Kanban-type Issue Stage Boards
			(setq ref-type "boards"
			      reference ""))
		       ((equal reference "jobs")
			;; Manual/automated project-related jobs that run
			(setq ref-type "-/jobs"
			      reference ""))
 		       ((equal reference "list")
			;; List all issues
			(setq ref-type "issues"
			      reference ""))
	               ((equal reference "contributors")
			(setq ref-type "graphs/master"
			      reference ""))
		       ((string-match "\\`\\(members\\|people\\|staff\\)\\'" reference)
			(setq ref-type "project_members"
			      reference ""))
		       ((equal reference "pipeline_charts")
			;; Continuous Integration Pipeline Charts
			(setq ref-type "pipelines/charts"
			      reference ""))
 		       ((equal reference "pulls")
			;; Merge requests for the project
			(setq ref-type "merge_requests"
			      reference ""))
		       ((equal reference "schedules")
			;; Schedules for CI Pipelines
			(setq ref-type "pipeline_schedules"
			      reference ""))
		       ((string-match "\\`\\(service\\|service_desk\\)\\'" reference)
			;; Project help desk 
			(setq ref-type "issues/service_desk"
			      reference ""))
		       ((member reference '("activity" "branches" "commits" "issues" "labels"
					    "merge_requests" "milestones" "pages" "pipelines"
					    "snippets" "tags"))
			;; All activity, branches, commits, cycle analytics, open issues, issue labels,
			;; members, merge requests, milestones, web pages, pull requests, code snippets
			;; or commit tags reference
			(setq ref-type reference
			      reference ""))
		       ((and (< (length reference) 8) (string-match "\\`\\([gG][lL]-\\)?[0-9]+\\'" reference))
			;; Issue ref-id reference
			(setq ref-type "issues/"
			      reference (substring reference (match-end 1) (match-end 0))))
		       ((string-match "\\`label[/=]" reference)
			;; Labeled category of issues
			(setq ref-type "issues?label_name%5B%5D="
			      reference (substring reference (match-end 0))))
		       ((string-match "\\`\\(commit\\|issues\\|milestones\\|pull\\|snippets\\|tags\\)[/=]" reference)
			;; Ref-id preceded by a keyword
			(setq ref-type (concat (substring reference 0 (match-end 1)) "/")
			      reference (substring reference (match-end 0))))
		       ((string-match "\\`\\(issue\\|milestone\\|snippet\\|tag\\)[/=]" reference)
			;; Ref-id preceded by a singular keyword that must be converted to plural
			(setq ref-type (concat (substring reference 0 (match-end 1)) "s/")
			      reference (substring reference (match-end 0))))
		       ((string-match "\\`\\(commit\\|pull\\)s[/=]" reference)
			;; Ref-id preceded by a plural keyword that must be converted to singular
			(setq ref-type (concat (substring reference 0 (match-end 1)) "/")
			      reference (substring reference (1+ (match-end 0)))))
		       ((string-match "\\`[0-9a-f]+\\'" reference)
			;; Commit reference
			(setq ref-type "commit/"))
		       (t
			;; Specific branch or commit tag reference
			(setq ref-type "tree/")
			(when (string-match "\\`\\(branch\\|tag\\)[/=]" reference)
			  ;; If preceded by optional keyword, remove that from the reference.
			  (setq reference (substring reference (match-end 0)))))))
	       (if (and (stringp user) (stringp project))
		   (funcall hibtypes-social-display-function
			    (setq a (if reference
				(format url-to-format user project ref-type reference)
				;; Remove trailing /
				(substring (format url-to-format user project "" "") 0 -1))))
		 (cond ((and (null user) (null project))
			(error "(gitlab-reference): Set `hibtypes-gitlab-default-user' and `hibtypes-gitlab-default-project'"))
		       ((null user)
			(error "(gitlab-reference): Set `hibtypes-gitlab-default-user'"))
		       (t
			(error "(gitlab-reference): Set `hibtypes-gitlab-default-project'")))))
	     (unless url-to-format
	       (error "(gitlab-reference): Add an entry for gitlab to `hibtypes-social-hashtag-alist'"))))))

;;; Local git repository commit references

(defib git-commit-reference ()
  "Display the diff for a git commit reference, e.g. \"commit a55e21\", typically produced by git log."
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\s-*commit \\([0-9a-f]+\\)$"))
      (hact #'git-reference (match-string-no-properties 1))))

(defvar hibtypes-git-repos-cache 
  (expand-file-name "Local-Git-Repos" hbmap:dir-user)
  "Filename of cache of local git repository directories found by `locate-command'.")

(defun hibtypes-git-get-locate-command ()
  (require 'locate)
  (let ((cmd (if (string-match "locate" locate-command) locate-command "locate")))
    (if (executable-find cmd)
	cmd
      (error "(git-reference): \"locate\" command required but not found; see its man page for setup instructions"))))

(defun hibtypes-git-build-repos-cache (&optional prompt-flag)
  "Store cache of local git repo directories in `hibtypes-git-repos-cache'.
With optional prompt-flag non-nil, prompt user whether to build the cache before building.
Return t if built, nil otherwise."
  (when (or (not prompt-flag)
	    (y-or-n-p "Find all local git repositories (will take some time)?"))
    (message "Please wait while all local git repositories are found...")
    (unless (zerop (shell-command (format "%s -r '/\.git$' | sed -e 's+/.git$++' > %s"
					  (hibtypes-git-get-locate-command)
					  hibtypes-git-repos-cache)))
      (error "(hibtypes-git-build-repos-cache): Cache build failed; `locate-command' must accept `-r' argument for regexp matching"))
    (message "Please wait while all local git repositories are found...Done")
    t))

(defun hibtypes-git-add-project-to-repos-cache (project)
  "Locate PROJECT directory and add to the cache of local git repo directories in `hibtypes-git-repos-cache'.
Return the project directory found or nil if none."
  (message "Please wait while %s's local git repository is found..." project)
  (let ((project-dir (shell-command-to-string
		      (format "%s -l1 /%s/.git | sed -e 's+/.git++' | tr -d '\n'"
			      (hibtypes-git-get-locate-command)
			      project))))
    (message "")
    (when (and (> (length project-dir) 0) (= ?/ (aref project-dir 0)))
      ;; project-dir a directory, prepend it to the cache file...
      (shell-command-to-string (format "echo -e \"%s\n$(cat %s)\" > %s"
				       project-dir hibtypes-git-repos-cache
				       hibtypes-git-repos-cache))
      ;; ...and return it.
      project-dir)))

(defun hibtypes-git-build-or-add-to-repos-cache (project &optional prompt-flag)
  "Store cache of local git repo directories in `hibtypes-git-repos-cache'.
With optional prompt-flag non-nil, prompt user whether to build the cache before building.
Return t if built, nil otherwise."
  (if (and (file-readable-p hibtypes-git-repos-cache)
	   ;; Non-zero file size
	   (not (zerop (nth 7 (file-attributes hibtypes-git-repos-cache)))))
      (hibtypes-git-add-project-to-repos-cache project)
    (hibtypes-git-build-repos-cache t)))

(defun hibtypes-git-project-directory (project)
  "Given git PROJECT name, return local git repository directory or nil if none found."
  (if (or (and (file-readable-p hibtypes-git-repos-cache)
	       ;; Non-zero file size
	       (not (zerop (nth 7 (file-attributes hibtypes-git-repos-cache)))))
	  (hibtypes-git-build-repos-cache t))
      ;; echo -n deletes trailing newline
      (shell-command-to-string (format "grep -m1 '/%s$' %s | tr -d '\n'" project hibtypes-git-repos-cache))
    (message "")
    nil))

;; Pseudo-code for next action definition:
;;  1. If within a git repo directory, use that repo unless specified in path
;;  2. If project name is given or is default, see if assocated repo dir is in cache and use it.
;;  3. Prompt to rebuild locate db and then goto 2 if yes else quit
;;  4. Run: (cd <dir-found>; git <cmd> <item>)
;;  5. Otherwise, do nothing.
;;
;; Don't make this a defact or its arguments may be improperly expanded as pathnames.
(defun git-reference (reference &optional project)
  "Display the git entity associated with REFERENCE and optional PROJECT.
REFERENCE is a string of one of the following forms:
    <ref-item>
    /?<project>/<ref-item>
or  /<project>.

<ref-item> is one of these:
  one of the words: branches, commits, or tags; the associated items are listed;

  one of the words: branch, commit, or tag followed by a '/' and item id; the item is shown;

  a commit reference given by a hex number, 55a1f0; the commit diff is displayed;

  a branch or tag reference given by an alphanumeric name, e.g. hyper20; the
  files in the branch are listed.

If given, PROJECT overrides any project value in REFERENCE.  If no
PROJECT value is provided, it defaults to the value of
`hibtypes-git-default-project'."
  (cond ((or (null reference) (equal reference ""))
	 (error "(git-reference): Git commit hashtag must not be empty"))
	((string-match "\\`=\\([^:#@]+\\)\\'" reference)
	 ;; =file
	 (git-find-file (match-string-no-properties 1 reference)))
	(t (let ((case-fold-search t)
		 (shell-cmd-to-format (assoc-default "git" hibtypes-social-hashtag-alist #'string-match)))
	     (when shell-cmd-to-format
	       (cond ((string-match "\\`\\(=\\)\\|\\(branch\\|commit\\|tag\\)/" reference)
		      ;; [branch | commit | tag]/ref-item
		      nil)
		     ((string-match "\\`/?\\([^/#@]+\\)/\\([0-9a-f]+\\)\\'" reference)
		      ;; /?project/hashtag
		      (setq project (or project (match-string-no-properties 1 reference))
			    reference (match-string-no-properties 2 reference)))
		     ((string-match "\\`/\\([^/#@]+\\)\\'" reference)
		      ;; /project
		      (setq project (or project (match-string-no-properties 1 reference))
			    reference nil))
		     ((string-match "/.*/" reference)
		      ;; Invalid user/project/hashtag
		      (error "(git-reference): Username or path not allowed, only <project>/<commit hashtag>")))
	       (let ((cmd)
		     (ref-type)
		     ;; `project' now may be a project directory or a project name.
		     ;; If a project name:
		     ;;   If reference is within a git project, use its project directory.
		     ;;   Otherwise, look up the project in Hyperbole's local git repo directory cache;
		     ;;   the user is prompted to have it built when necessary.
		     (project-dir (or (and project (file-readable-p project) (file-directory-p project) project)
				      (locate-dominating-file default-directory ".git"))))
		 (unless (or (stringp project) (= (aref reference 0) ?=))
		   (unless (setq project (cond (project-dir (file-name-nondirectory (directory-file-name project-dir)))
					       ((stringp hibtypes-git-default-project)
						hibtypes-git-default-project)))
		     (error "(git-reference): Set `hibtypes-git-default-project' to a default project name")))
		 (unless project-dir
		   (setq project-dir (and project (hibtypes-git-project-directory project))))
		 (when reference
		   (cond ((member reference '("branches" "commits" "tags"))
			  ;; All branches, commits or commit tags reference
			  (setq ref-type reference
				reference ""))
			 ((string-match "\\`=?\\(commit\\)/" reference)
			  ;; Specific reference preceded by keyword commit.
			  (setq ref-type "commit"
				reference (substring reference (match-end 0))))
			 ((string-match "\\`=?[0-9a-f]+\\'" reference)
			  ;; Commit reference
			  (setq ref-type "commit"))
			 ((string-match "\\`\\(=?\\(branch\\|tag\\)/\\)\\|=" reference)
			  ;; Specific branch or commit tag reference
			  (setq ref-type "tree"
				reference (substring reference (match-end 0)))
			  ;; reference now might be branch-name:subpath or just branch-name.
			  ;; (subpath by itself was handled by git-find-file up above).
			  ;; If reference contains subpath, expand it with hibtypes-git-find.
			  (let (branch-name
				file
				path)
			    (if (string-match ":" reference)
				(setq branch-name (substring reference 0 (match-beginning 0))
				      file (substring reference (match-end 0))
				      path (hibtypes-git-find file)
				      reference (concat branch-name ":" file))
			      (setq path default-directory))
			    (setq project-dir (or project-dir (and path (locate-dominating-file path ".git")))
				  project (or project (and project-dir (file-name-nondirectory project-dir))
					      hibtypes-git-default-project))))
			 (t
			  (setq ref-type "tree"))))
		 (when (or (null project-dir) (equal project-dir ""))
		   (if (and project
			    ;; Maybe the Hyperbole git project cache is
			    ;; out-of-date and needs to be rebuilt or added
			    ;; to.  Prompt user and if rebuilt or added to,
			    ;; continue.
			    (hibtypes-git-build-or-add-to-repos-cache project t))
		       (setq project-dir (and project (hibtypes-git-project-directory project)))
		     (error "(git-reference): No git directory found for project `%s'" project)))
		 (when (equal project-dir "") (setq project-dir nil))
		 (cond ((and project-dir (file-readable-p project-dir) (file-directory-p project-dir))
			(if reference
			    (if (and (equal ref-type "commits") (fboundp 'vc-print-root-log))
				(let ((default-directory project-dir))
				  (vc-print-root-log))
			      ;; Display commit diffs in a help buffer
			      ;; Ensure these do not invoke with-output-to-temp-buffer a second time.
			      (let ((temp-buffer-show-hook)
				    (temp-buffer-show-function))
				(setq cmd
				      (pcase ref-type
					("branches" (format shell-cmd-to-format project-dir "branch -la" ""))
					("commits"  (format shell-cmd-to-format project-dir "log --abbrev-commit --pretty=oneline" ""))
					("tags"     (format shell-cmd-to-format project-dir "tag -l" ""))
					(_          (format shell-cmd-to-format project-dir "show" reference))))
				(with-help-window (format "*git%s%s %s%s%s*"
							  (if (equal project "") "" " ")
							  project ref-type
							  (if (equal reference "") "" " ")
							  reference)
				  (princ (format "Command: %s\n\n" cmd))
				  (princ (shell-command-to-string cmd)))))
			  ;; Project-only reference, run dired on the project home directory
			  (hpath:display-buffer (dired-noselect
						 (file-name-as-directory project-dir)))))
		       (t (if project-dir
			      (error "(git-reference): git project `%s' directory is unreadable or invalid: \"%s\""
				     project project-dir)
			    (error "(git-reference): No git project found for `%s'" project))))))))))

(defun hibtypes-git-find-execute (format-prefix find-dir file)
  "Build and execute a shell command to find a matching git-versioned file.
Return nil if no matching file is found."
  (let ((path
	 (shell-command-to-string
	  (format (concat
		   ;; Ignore any errors for non-existing paths
		   "%s %s -path '*/.git' -prune -o -path '*%s' -print 2> /dev/null"
		   ;; Choose the shortest matching path which is usually the best guess.
		   " | awk '{ print length($0) \"\t\" $0 }' - | sort -n | head -n 1 | cut -f2- | tr -d '\n'")
		  format-prefix find-dir file))))
    (and (stringp path) (> (length path) 0) path)))

(defun hibtypes-git-find (file)
  "Return the shortest pathname matching git-versioned FILE name.

Search for matches in this order: (1) the git repository of the current
directory, if any; (2) the git repository of project `hibtypes-git-default-project'
if not nil;  (3) the list of locally cached git repositories in `hibtypes-git-repos-cache'.

Return nil if no match is found."
  (let (root)
    (cond
     ;; Try to find in current directory tree first...
     ((and (fboundp 'locate-dominating-file)
	   (setq root (locate-dominating-file default-directory ".git"))
	   (hibtypes-git-find-execute "find" root file)))
     ;; then in default project tree...
     ((and hibtypes-git-default-project
	   (setq root (hibtypes-git-project-directory hibtypes-git-default-project))
	   (hibtypes-git-find-execute "find" root file)))
     ;; then in any of list of cached project trees...
     ((or (and (file-readable-p hibtypes-git-repos-cache)
	       (not (zerop (nth 7 (file-attributes hibtypes-git-repos-cache))))) ; Non-zero file size
	  (hibtypes-git-build-repos-cache t))
      (hibtypes-git-find-execute (format "cat '%s' | xargs -I{} find" hibtypes-git-repos-cache)
				 "'{}'"
				 file))
     ;; otherwise, fail.
     (t (message "") ; Clear any potential message from building the cache.
	nil))))

(defun git-find-file (file)
  "Locate and edit the named FILE with the shortest git-versioned pathname, typically in another window.
Uses `hpath:find' to display the FILE.  FILE must not have any path component.

If the current directory is in a git repository, search only that one;
otherwise, search all known local repositories.  Signal an error if no match
is found."
  (interactive "sFind git-versioned file: ")
  (let ((path (hibtypes-git-find file)))
    (if path
	(progn (message path)
	       (hpath:find path))
      (error "(git-find-file): `%s' not found in any local git repository" file))))


(provide 'hib-social)
