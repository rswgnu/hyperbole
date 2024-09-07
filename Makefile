# Makefile --- Build GNU Hyperbole directories and package distributions
#
# Author:       Bob Weiner
#
# Orig-Date:    15-Jun-94 at 03:42:38
# Last-Mod:      7-Sep-24 at 16:29:00 by Mats Lidell
#
# Copyright (C) 1994-2023  Free Software Foundation, Inc.
# See the file HY-COPY for license information.
#
# This file is part of GNU Hyperbole.

# Commentary:
#
#   **********
#   READ THIS:
#   **********
#
#   GNU Hyperbole should be installed for use via the Emacs package system;
#   see the "INSTALL" file for installation instructions and the Info node,
#   "(emacs)Packages", if you are unfamiliar with the Emacs package system.
#
#   Only Hyperbole developers (those who develop the source code) and testers
#   need to use this file for building Hyperbole distributions.  Others
#   may ignore it.
#
#   **********
#
#   Before doing your first make, edit the CONFIGURABLE SECTION below.
#   Make any needed changes now and save the file.  Then select from the
#   USAGE lines immediately following.
#
#   USAGE:	For those installing GNU Hyperbole, display your options with:
#   	             make help
#
#               To build only the output formats of the Hyperbole Manual:
#		     make doc
#
#		Note: Releasing to ELPA is automatic in that the
#		master branch on savannah is automatically synced
#		daily by ELPA. The pkg and release targets are for
#		making and uploading a tar ball to ftp.gnu.org.
#
#               To assemble a Hyperbole Emacs package for testing:
#		     make pkg
#
#               To release a Hyperbole Emacs package to ftp.gnu.org:
#		     make release
#
#		Generate the website sources and upload them:
#		    make website - generate web site in folder $(HYPB_WEB_REPO_LOCATION)"
#
#               Lint all Hyperbole code files:
#                   make lint
#
#               To setup Hyperbole to run directly from the latest test source
#               code, use:
#                    git clone http://git.savannah.gnu.org/r/hyperbole.git
#                    cd hyperbole
#                 Then use either:
#                    make src     - setup to run directly from .el files
#                  or
#                    make bin     - setup to build and run from .elc files
#                  or
#                    make eln     - setup to build and run from .eln natively compiled files
#
#               The Hyperbole Manual is included in the package in four forms:
#                  "man/hyperbole.info"   - GNU browsable version
#                  "man/hyperbole.html"   - Web browsable version
#                  "man/hyperbole.pdf"    - Printable version
#                  "man/hyperbole.texi"   - source form
#
#               * Developer targets
#
#               To run unit tests:
#                   make batch-tests             - run non-interactive tests in batch mode
#                   make all-tests               - run all tests starting an interactive Emacs
#                   make test test=<test-name>   - run a single test or tests matching the name
#
#               To interactively run a docker version of Emacs with Hyperbole:
#                   make docker-run              - default to running master
#                   make docker-run version=27.2 - run Emacs V27.2
#
#               To build and test a dockerized version of Emacs with Hyperbole:
#                   make docker                  - defaults: version=master targets='clean bin test'
#
#                   make docker version=28.2 targets='clean bin' - byte-compile Hyperbole with Emacs 28.2
#
#                   For CI/CD Emacs versions see macro DOCKER_VERSIONS below.
#
#		    make docker-batch-tests      - run all non-interactive tests in docker for all CI/CD Emacs versions
#		    make docker-all-tests        - run all tests in docker for all CI/CD Emacs versions
#
#               Verify hyperbole installation using different sources:
#                   make install-<source>
#               Where source can be 'elpa', 'elpa-devel', 'tarball' (tarball from elpa-devel),
#               'straight' (git master from savannah) or 'all'.

# Code:
##########################################################################
#                         CONFIGURABLE SECTION                           #
##########################################################################

# This ver setup won't work under any make except GNU make, so set it manually.
#HYPB_VERSION = "`head -3 hversion.el | tail -1 | sed -e 's/.*|\(.*\)|.*/\1/'`"
HYPB_VERSION = 9.0.2pre

# Emacs executable used to byte-compile .el files into .elc's.
# To override which executable is used from the commandline, do something like this:
#  make EMACS=/Applications/Emacs28-nativecomp.app/Contents/MacOS/Emacs bin
EMACS = \emacs

# Site-specific Emacs Lisp libraries to load before byte-compiling any files
# from this package.  Typically the only reason to set this is to get Emacs
# to include the directory of this package into its load-path variable, which
# determines where it will find Lisp library files to load.  This is now
# handled automatically by Hyperbole for most modern versions of Emacs.
#
# You must include the .el or .elc file suffix on each library name and each
# must be preceded by the `-l' command-line flag.  If the directory in which
# the library is stored will not be in your Emacs load-path when Emacs
# attempts to load the library, you must include the full pathname to the
# library.  Here is an example setting.
#
# SITE_PRELOADS = -l ~/.emacs -l set-load-path.el
#
SITE_PRELOADS =

# Command used to build the .info version of the user manual.
# Ensure punctuation generated by makeinfo is ascii, not unicode (requires makeinfo 7.0 or above).
TEXI2INFO = makeinfo --no-split -c ASCII_PUNCTUATION=1
# Command used to build the .html version of the user manual.
# TEXI2HTML = id-texi2html -html_only -number -split_chapter # InfoDock-specific command
# TEXI2HTML = makeinfo --html --split=chapter # Chapter splitting doesn't seem to work in 6.0
TEXI2HTML = makeinfo --html --no-split --css-ref="hyperbole.css"

# Command used to build the .pdf version of the user manual.
TEXI2PDF = makeinfo --pdf --no-split

# Where to find the parent tree of the Hyperbole source directory.
id_dir = $(HOME)/sw-dev/emacs
# Where to find the .texi source of the user manual.
man_dir := $(shell pwd)/man
# Where to install the Hyperbole mouse key help file
data_dir = $(id_dir)/id-etc
# Where to install the Info version of the user manual.
info_dir = $(id_dir)/id-info
# Where to install the HTML version of the user manual.
html_dir = $(id_dir)/id-html

# Shell used to process this Makefile.  Bourne shell syntax is required.
SHELL = /bin/sh

# Shell commands you may want to change for your particular system.
CP = \cp -p
ETAGS = \etags
GNUFTP = \gnupload --to ftp.gnu.org:hyperbole --replace
GPG = \gpg
GZIP = \gzip -c
INSTALL = \install -m 644 -c
MKDIR = \mkdir -p
MAKE = \make
RM = \rm -f
TAR = \tar
ZIP = \zip -qry
CVS = \cvs

# Directory in which to create new package distributions of Hyperbole.
pkg_parent = /tmp
pkg_hyperbole = $(pkg_parent)/hyperbole-$(HYPB_VERSION)

# Temp file to use to build .elc files.
ELISP_TO_COMPILE = $(pkg_parent)/elc-${USER}

# Path to dir where the web repository is located i.e. hypb:web-repo-location
HYPB_WEB_REPO_LOCATION = "../hyweb/hyperbole/"

# CI/CD Emacs versions for local docker based tests
DOCKER_VERSIONS=27.2 28.2 29.4 master

##########################################################################
#                     NO CHANGES REQUIRED BELOW HERE.                    #
##########################################################################

# Libraries that must be pre-loaded before trying to byte-compile anything.
PRELOADS = $(SITE_PRELOADS) -l ./hload-path.el -l ./hversion.el -l ./hyperbole.el 

# Compile in batch mode.  Load site-lisp/site-start.el, which may set load-path.
# Show complete expression; do not abbreviate any exprs in batch logs with ...
BATCHFLAGS = --batch --quick --eval "(progn (setq debug-on-error t) (setq backtrace-line-length 0) \
                                 (message \"  emacs-version = %s\n  system-configuration = %s\n  emacs = %s%s\n  load-path = %s\" emacs-version system-configuration invocation-directory invocation-name load-path))"

EMACS_BATCH=$(EMACS) $(BATCHFLAGS) $(PRELOADS)
EMACS_PLAIN_BATCH=$(EMACS) $(BATCHFLAGS)

# Directories other than the current directory in which to find files.
# This doesn't seem to work in all versions of make, so we also add kotl/
# explicitly to those files which need it.
VPATH = kotl man

EL_COMPILE = hact.el hactypes.el hargs.el hbdata.el hbmap.el hbut.el \
	     hgnus.el hhist.el hib-debbugs.el hib-doc-id.el hib-kbd.el \
	     hib-social.el hibtypes.el \
	     hinit.el hload-path.el hmail.el hmh.el hmoccur.el hmouse-info.el \
	     hmouse-drv.el hmouse-key.el hmouse-mod.el hmouse-sh.el hmouse-tag.el \
	     hpath.el hproperty.el hrmail.el hsettings.el hsmail.el hsys-consult.el \
             hsys-flymake.el \
             hsys-org.el hsys-org-roam.el hsys-www.el hsys-xref.el hsys-youtube.el htz.el \
	     hycontrol.el hui-jmenu.el hui-menu.el hui-mini.el hui-mouse.el hui-select.el \
	     hui-treemacs.el hui-window.el hui.el hvar.el hversion.el hynote.el hypb.el hyperbole.el \
	     hyrolo-demo.el hyrolo-logic.el hyrolo-menu.el hyrolo.el hywconfig.el hywiki.el \
             hasht.el set.el hypb-ert.el hui-dired-sidebar.el hypb-maintenance.el \
             hui-register.el

EL_SRC = $(EL_COMPILE)

EL_KOTL = kotl/kexport.el kotl/kfile.el kotl/kfill.el kotl/kimport.el kotl/klabel.el \
	  kotl/klink.el kotl/kmenu.el kotl/kotl-mode.el kotl/kotl-orgtbl.el \
          kotl/kcell.el kotl/kproperty.el kotl/kview.el kotl/kvspec.el

ELC_COMPILE = $(EL_COMPILE:.el=.elc)

ELC_KOTL = $(EL_KOTL:.el=.elc)

HY-TALK  = HY-TALK/.hypb HY-TALK/HYPB HY-TALK/HY-TALK.org HY-TALK/HYPERAMP.org HY-TALK/HYPERORG.org

HYPERBOLE_FILES = dir info html $(EL_SRC) $(EL_KOTL) \
	$(HY-TALK) .mailmap ChangeLog COPYING Makefile HY-ABOUT HY-ANNOUNCE \
        HY-CONCEPTS.kotl HY-NEWS HY-WHY.kotl $(wildcard hywiki/*.org) \
        INSTALL DEMO DEMO-ROLO.otl FAST-DEMO MANIFEST README.md TAGS _hypb \
        .hypb hyrolo.py smart-clib-sym topwin.py hyperbole-banner.png \
	$(man_dir)/hkey-help.txt $(man_dir)/hyperbole.texi $(man_dir)/hyperbole.css

TEST_ERT_FILES = $(wildcard test/*tests.el) $(wildcard test/hy-test-*.el)

EL_TAGS = $(EL_SRC) $(EL_KOTL) $(TEST_ERT_FILES)

.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .el .elc   # Define the list of file suffixes to match to rules

help:
	@echo "Use the Emacs Package Manager to build and install the latest release of GNU Hyperbole."
	@echo "For help with Emacs packages, see the GNU Emacs Info Manual section, \"(emacs)Packages\"."
	@echo "See \"$(shell pwd)/INSTALL\" for detailed installation instructions,"
	@echo "including how to install Hyperbole pre-releases from the git repo tip."
	@echo ""

	@echo "To setup Hyperbole to run directly from the latest test source code, use:"
	@echo "     git clone http://git.savannah.gnu.org/r/hyperbole.git"
	@echo "     cd hyperbole"
	@echo "  Then use either:"
	@echo "     make src     - setup to run directly from .el files"
	@echo "   or"
	@echo "     make bin     - setup to build and run from .elc files"
	@echo ""

	@echo "For Hyperbole maintainers only:"
	@echo "  To run unit tests:"
	@echo "     make all-tests          - run all tests with Emacs under a window system"
	@echo "     make batch-tests        - run non-interactive tests with Emacs in batch mode"
	@echo "  Using docker and the macro DOCKER_VERSIONS for selected Emacs versions to test against"
	@echo "     make docker-all-tests   - run all tests"
	@echo "     make docker-batch-tests - run non-interactive tests"
	@echo "  To selectively run make targets in docker:"
	@echo "     make docker version=<emacs-version> targets=<make targets>"
	@echo "  To verify hyperbole installation using different sources:"
	@echo "     make install-<source>"
	@echo "   where <source> can be 'elpa', 'elpa-devel', 'tarball' (tarball from elpa-devel),"
	@echo "   'straight' (git master from savannah) or 'all'."
	@echo "  To build the Hyperbole distribution package:"
	@echo "     make pkg"
	@echo "  To build documentation formats only:"
	@echo "     make doc"
	@echo "  To generate and upload the public Hyperbole website:"
	@echo "     make website"
	@echo "  To release a Hyperbole Emacs package to ELPA and ftp.gnu.org:"
	@echo "     make release"
	@echo ""

	@echo "The Hyperbole Manual is included in the package in four forms:"
	@echo "    man/hyperbole.info    - GNU browsable version"
	@echo "    man/hyperbole.html    - Web browsable version"
	@echo "    man/hyperbole.pdf     - Printable version"
	@echo "    man/hyperbole.texi    - source form"
	@echo ""

all: help

echo:
	@echo "Emacs: $(shell which ${EMACS})"
	@echo "Version: $(shell ${EMACS} --version)"
	@echo "TERM: $(TERM)"
	@echo "DISPLAY: $(DISPLAY)"

install: elc install-info install-html $(data_dir)/hkey-help.txt

install-info: $(info_dir)/hyperbole.info
$(info_dir)/hyperbole.info: $(man_dir)/hyperbole.info
	$(MKDIR) $(info_dir)/im; \
	  cd $(man_dir); $(INSTALL) hyperbole.info* $(info_dir); \
	  $(INSTALL) im/*.{png,eps} $(info_dir)/im

install-html: $(html_dir)/hyperbole.html
$(html_dir)/hyperbole.html: $(man_dir)/hyperbole.html $(man_dir)/hyperbole.css
	$(MKDIR) $(html_dir)/im; \
	  cd $(man_dir); $(INSTALL) hyperbole.html* hyperbole.css $(html_dir); \
	  $(INSTALL) im/*.{png,eps} $(html_dir)/im

$(data_dir)/hkey-help.txt: $(man_dir)/hkey-help.txt
	$(INSTALL) hkey-help.txt $(data_dir)

.PHONY: src new-bin remove-elc bin eln
# Setup to run Hyperbole from .el source files
src: autoloads tags

# Byte compile files but apply a filter for either including or
# removing warnings.  See variable {C-hv byte-compile-warnings RET} for
# list of warnings that can be controlled.  Default is set to suppress
# warnings for long docstrings.
#
# Example for getting warnings for obsolete functions and variables
#   HYPB_WARNINGS="free-vars" make bin
# Example for surpressing the free-vars warnings
#   HYPB_WARNINGS="not free-vars" make bin
ifeq ($(origin HYPB_WARNINGS), undefined)
HYPB_BIN_WARN =
else ifeq ($(origin HYPB_WARNINGS), environment)
HYPB_BIN_WARN = --eval "(setq-default byte-compile-warnings '(${HYPB_WARNINGS}))"
endif

curr_dir = $(shell pwd)
ifeq ($(HYPB_NATIVE_COMP),yes)
%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) --batch --quick \
	--eval "(progn (add-to-list 'load-path \"$(curr_dir)\") (add-to-list 'load-path \"$(curr_dir)/kotl\"))" \
	${HYPB_BIN_WARN} \
	-f batch-native-compile $<
else
%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) --batch --quick \
	--eval "(progn (add-to-list 'load-path \"$(curr_dir)\") (add-to-list 'load-path \"$(curr_dir)/kotl\"))" \
	${HYPB_BIN_WARN} \
	-f batch-byte-compile $<
endif

new-bin: autoloads $(ELC_KOTL) $(ELC_COMPILE)

remove-elc:
	$(RM) *.elc kotl/*.elc

# Remove and then rebuild all byte-compiled .elc files, even those .elc files
# which do not yet exist, plus build TAGS file.
bin: src remove-elc new-bin

# Native compilation (Requires Emacs built with native compilation support.)
eln: echo src
	HYPB_NATIVE_COMP=yes make new-bin

tags: TAGS
TAGS: $(EL_TAGS)
	$(ETAGS) --regex='{lisp}/(ert-deftest[ \t]+\([^ \t\n\r\f()]+\)/' $(EL_TAGS)

clean:
	$(RM) hyperbole-autoloads.el kotl/kotl-autoloads.el $(ELC_COMPILE) $(ELC_KOTL) TAGS test/*.elc

version:
	@echo ""
	@fgrep -L $(HYPB_VERSION) Makefile HY-ABOUT HY-ANNOUNCE HY-NEWS README.md hversion.el hyperbole.el man/hyperbole.texi > WRONG-VERSIONS
	@# If any file(s) have wrong version number, print them and exit with code 1
	@if [ -s WRONG-VERSIONS ]; then \
	  echo "The following files do not have the proper Hyperbole version number, $(HYPB_VERSION):"; \
	  cat WRONG-VERSIONS; rm -f WRONG-VERSIONS; exit 1; \
	fi
	@rm -f WRONG-VERSIONS
	@echo ""

# Build the README.md.html and Info, HTML and Postscript versions of the user manual
doc: version README.md.html manual

# Build the Info, HTML and Postscript versions of the user manual
manual: info html pdf

TEXINFO_SRC = $(man_dir)/hyperbole.texi $(man_dir)/hkey-help.txt $(man_dir)/im/*.png

info: $(man_dir)/hyperbole.info
$(man_dir)/hyperbole.info: $(TEXINFO_SRC)
	cd $(man_dir) && $(TEXI2INFO) hyperbole.texi

html: $(man_dir)/hyperbole.html
$(man_dir)/hyperbole.html: $(TEXINFO_SRC) $(man_dir)/hyperbole.css
	cd ${man_dir} && $(TEXI2HTML) hyperbole.texi

pdf: $(man_dir)/hyperbole.pdf
$(man_dir)/hyperbole.pdf: $(TEXINFO_SRC)
	cd $(man_dir) && $(TEXI2PDF) hyperbole.texi

# The `md_toc' table-of-contents generator program is available from:
#   https://github.com/frnmst/md-toc
#
# `pandoc' is available from:
#    https://github.com/jgm/pandoc
README.md.html: README.md README.toc.md
	cp -p README.md README.toc.md && md_toc -p -m [TOC] github README.toc.md \
	  && sed -i -e 's/^\[TOC\]//g' README.toc.md \
	  && pandoc --from=gfm-tex_math_dollars --to=html+gfm_auto_identifiers -o README.md.html README.toc.md

# website maintenance: "https://www.gnu.org/software/hyperbole/"
# Locally update Hyperbole website
website-local: README.md.html
	$(EMACS_BATCH) --debug -l hypb-maintenance --eval '(let ((hypb:web-repo-location $(HYPB_WEB_REPO_LOCATION))) (hypb:web-repo-update))'

# Push to public Hyperbole website
website: website-local
	cd $(HYPB_WEB_REPO_LOCATION) && $(CVS) commit -m "Hyperbole release $(HYPB_VERSION)"
	@echo "Website for Hyperbole $(HYPB_VERSION) is updated."

# Generate a Hyperbole package suitable for distribution via the Emacs package manager.
pkg: package
package: tags doc $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.sig

# Generate and distribute a Hyperbole release to ftp.gnu.org.
# One step in this is to generate an autoloads file for the Koutliner, kotl/kotl-autoloads.el.
release: git-pull git-verify-no-update package $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.gz ftp website git-tag-release
	@echo "Hyperbole $(HYPB_VERSION) is released."

# Ensure local hyperbole directory is synchronized with master before building a release.
git-pull:
	@echo "If this step fails check your work directory for not committed changes"
	git checkout master && git pull
	git diff-index --quiet master

git-verify-no-update:
	@echo "If this step fails check your work directory for updated docs and push these to savannah"
	git diff-index --quiet master

git-tag-release:
	git tag -a hyperbole-$(HYPB_VERSION) -m "Hyperbole release $(HYPB_VERSION)"
	git push origin hyperbole-$(HYPB_VERSION)
	@echo "Hyperbole $(HYPB_VERSION) is tagged as hyperbole-$(HYPB_VERSION)."

# Send compressed tarball for uploading to GNU ftp site; this must be done from the directory
# containing the tarball to upload.
ftp: package $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.gz
	cd $(pkg_parent) && $(GNUFTP) hyperbole-$(HYPB_VERSION).tar.gz
	@echo "Hyperbole $(HYPB_VERSION) uploaded to ftp.gnu.org."

# Autoloads
autoloads: kotl/kotl-autoloads.el hyperbole-autoloads.el

hyperbole-autoloads.el: $(EL_COMPILE)
	$(EMACS_BATCH) --debug --eval "(progn (setq generated-autoload-file (expand-file-name \"hyperbole-autoloads.el\") backup-inhibited t) (let (find-file-hooks) (hload-path--make-directory-autoloads \".\" generated-autoload-file)))"

kotl/kotl-autoloads.el: $(EL_KOTL)
	$(EMACS_PLAIN_BATCH) --debug --eval "(let ((autoload-file (expand-file-name \"kotl/kotl-autoloads.el\")) (backup-inhibited t) (find-file-hooks)) (if (functionp (quote make-directory-autoloads)) (make-directory-autoloads \"kotl/\" autoload-file) (progn (setq generated-autoload-file autoload-file) (update-directory-autoloads \"kotl/\"))))"

# Used for ftp.gnu.org tarball distributions.
$(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.gz:
	cd $(pkg_parent) && $(GZIP) hyperbole-$(HYPB_VERSION).tar > hyperbole-$(HYPB_VERSION).tar.gz

$(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.sig: $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar
	$(RM) $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.sig && \
	cd $(pkg_parent) && $(GPG) -ba -o hyperbole-$(HYPB_VERSION).tar.sig hyperbole-$(HYPB_VERSION).tar && \
	echo &&  echo "Hyperbole package built successfully:" && \
	ls -l $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar*

$(pkg_parent)/hyperbole-$(HYPB_VERSION).tar: version $(HYPERBOLE_FILES)
	$(RM) -fr $(pkg_hyperbole) $(pkg_hyperbole).tar
        # git archive --format=tar --prefix=hyperbole-$(HYPB_VERSION)/ HEAD | (cd $(pkg_parent) && tar xf -)
	(mkdir -p $(pkg_hyperbole) && git ls-files | tar Tzcf - - | (cd $(pkg_hyperbole) && tar zxf -)) && \
	cd $(pkg_hyperbole) && make autoloads && chmod 755 topwin.py && \
	COPYFILE_DISABLE=1 $(TAR) -C $(pkg_parent) -clf $(pkg_hyperbole).tar hyperbole-$(HYPB_VERSION)

pkgclean: packageclean
packageclean:
	if [ -d $(pkg_hyperbole) ]; then \
	  cd $(pkg_hyperbole) && $(RM) -r .git* videos ChangeLog.* *autoloads.* *.elc TODO* HY-ANNOUNCE-* .DS_Store \
	    core .place* ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore GNUmakefile.id \
	    && gsed '/\f/,/\f/{/\f/!d}' .hypb | tail +2 > .hypb2 && rm -f .hypb && mv .hypb2 .hypb; fi # Filter out unneeded TODO file hbut data from .hypb
	if [ -d $(pkg_hyperbole)/kotl ]; then \
	  cd $(pkg_hyperbole)/kotl && $(RM) -r *autoloads.* *.elc TODO* .DS_Store \
	    core .place* ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(pkg_hyperbole)/man ]; then \
	  cd $(pkg_hyperbole)/man && $(RM) -r .DS_Store core .place* hyperbole.{log,aux,cp*,fn*,ky*,toc,vr*} \
	    ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(pkg_hyperbole)/man/im ]; then \
	  cd $(pkg_hyperbole)/man/im && $(RM) -r .DS_Store core .place* ._* .*~ *~ \
	    *.ps *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi

# ERT test
.PHONY: tests test test-ert all-tests test-all batch-tests
,PHONY: docker-all-tests docker-batch-tests
batch-tests: test
tests: test
test: test-ert

# enable-local-variables setting needed so local variables set in files like
# FAST-DEMO are automatically obeyed without prompting when testing.
LET_VARIABLES = (auto-save-default) (enable-local-variables :all)
LOAD_TEST_ERT_FILES=$(patsubst %,(load-file \"%\"),${TEST_ERT_FILES})

# Run make test test=<ert-test-selector> to limit batch test to
# tests specified by the selector. See "(ert)test selectors"
ifeq ($(origin test), command line)
HYPB_ERT_BATCH = (ert-run-tests-batch-and-exit \"${test}\")
HYPB_ERT_INTERACTIVE = (ert-run-tests-interactively \"${test}\")
else
HYPB_ERT_BATCH = (ert-run-tests-batch-and-exit)
HYPB_ERT_INTERACTIVE = (ert-run-tests-interactively t)
endif

# For full backtrace run make test FULL_BT=<anything or even empty>
ifeq ($(origin FULL_BT), command line)
HYPB_ERT_BATCH_BT = (ert-batch-backtrace-line-length t) (backtrace-line-length nil)
else
HYPB_ERT_BATCH_BT = (ert-batch-backtrace-line-length 256) (backtrace-line-length 5000)
endif

# Run non-interactive tests in batch mode
test-ert:
	@echo "# Tests: $(TEST_ERT_FILES)"
	$(EMACS_BATCH) --eval "(load-file \"test/hy-test-dependencies.el\")" \
	--eval "(let ((auto-save-default) (ert-batch-print-level 10) \
	              (ert-batch-print-length nil) \
	              $(HYPB_ERT_BATCH_BT) (ert-batch-backtrace-right-margin 2048)) \
	           $(LOAD_TEST_ERT_FILES) $(HYPB_ERT_BATCH))"

# Run all tests by starting an Emacs that runs the tests interactively in windowed mode.
all-tests: test-all
test-all:
	@echo "# Tests: $(TEST_ERT_FILES)"
ifeq ($(TERM), dumb)
ifneq (,$(findstring .apple.,$(DISPLAY)))
        # Found, on MacOS
	TERM=xterm-256color $(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ((auto-save-default)) $(LOAD_TEST_ERT_FILES) $(HYPB_ERT_INTERACTIVE))"
else
        # Not found, set TERM so tests will at least run within parent Emacs session
	TERM=vt100 $(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ($(LET_VARIABLES)) $(LOAD_TEST_ERT_FILES) $(HYPB_ERT_INTERACTIVE))"
endif
else
        # Typical case, run emacs normally
	$(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ($(LET_VARIABLES)) $(LOAD_TEST_ERT_FILES) $(HYPB_ERT_INTERACTIVE))"
endif

test-all-output:
	@output=$(shell mktemp); \
	$(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ($(LET_VARIABLES) (ert-quiet t)) $(LOAD_TEST_ERT_FILES) (ert-run-tests-batch t) (with-current-buffer \"*Messages*\" (append-to-file (point-min) (point-max) \"$$output\")) (kill-emacs))"; \
	sed -n -E '/^Ran [0123456789]+ tests/,/^make:/p' $$output; \
	rm $$output

# Target to be used in docker
internal-docker-all-tests-ert-output:
	@$(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ($(LET_VARIABLES) (ert-quiet t)) $(LOAD_TEST_ERT_FILES) (ert t) (with-current-buffer \"*ert*\" (write-region (point-min) (point-max) \"/hypb-tmp/ERT-OUTPUT-ERT\")) (kill-emacs))"

docker-all-tests:
	@total_summary=$(shell mktemp); \
	for i in $(DOCKER_VERSIONS); do printf "=== Emacs $$i ===\n" | tee -a $$total_summary; \
		make docker version=$$i targets='clean bin internal-docker-all-tests-ert-output'; \
		cat /tmp/ERT-OUTPUT-ERT | tee -a $$total_summary; \
	done; \
	printf "\n\n=== Summary ===\n"; cat $$total_summary; \
	rm $$total_summary

docker-batch-tests:
	@total_summary=$(shell mktemp); build_summary=$(shell mktemp); \
	for i in $(DOCKER_VERSIONS); do printf "=== Emacs $$i ===\n" | tee -a $$total_summary; \
		make docker version=$$i targets='clean bin test' | tee $$build_summary; \
		sed -n -E '/^Ran [0123456789]+ tests/,/^make:/p' $$build_summary | head -n-1 | tee -a $$total_summary; \
	done; \
	printf "\n\n=== Summary ===\n"; cat $$total_summary; \
	rm $$total_summary $$build_summary

# Hyperbole install tests - Verify that hyperbole can be installed
# using different sources. See folder "install-test"
.PHONY: install-elpa install-elpa-devel install-tarball install-straight install-all install-local
install-all: install-elpa install-elpa-devel install-melpa install-tarball install-straight install-local

install-elpa install-elpa-devel install-tarball install-melpa install-straight install-elpaca:
	@echo "Install Hyperbole using $@"
	(cd ./install-test/ && ./local-install-test.sh $(subst install-,,$@))

install-local:
	@echo "Install Hyperbole using $@"
	(cd ./install-test/ && \
	./local-install-test.sh $(subst install-,,$@) $(shell pwd) $(shell git rev-parse --abbrev-ref HEAD 2>/dev/null))

lint:
	$(EMACS_BATCH) \
	--eval "(setq package-lint-main-file \"hyperbole.el\")" \
	--eval "(load-file \"test/hy-test-dependencies.el\")" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	--eval "(hy-test-ensure-package-installed 'package-lint)" \
	-l package-lint.el -f package-lint-batch-and-exit \
	$(EL_KOTL) $(EL_SRC)

# Docker versions of Emacs for interactive running and test execution

# Specify version and targets to run
ifeq ($(origin targets), command line)
DOCKER_TARGETS = ${targets}
else
DOCKER_TARGETS = clean bin test
endif
ifeq ($(origin version), command line)
DOCKER_VERSION = ${version}-ci
else
DOCKER_VERSION = master-ci
endif

docker: docker-update
	docker run -v $$(pwd):/hypb -v /tmp:/hypb-tmp -it --rm silex/emacs:${DOCKER_VERSION} bash -c "cp -a /hypb /hyperbole && make -C hyperbole ${DOCKER_TARGETS}"

docker-run: docker-update
	docker run -v $$(pwd):/hypb -v /tmp:/hypb-tmp -it --rm silex/emacs:${DOCKER_VERSION}

# Update the docker image for the specified version of Emacs
docker-update:
	docker pull silex/emacs:${DOCKER_VERSION}

# Run with coverage. Run tests given by testspec and monitor the
# coverage for the specified file.
#
# Usage:
#    make coverage file=<file> testspec=<testspec>

# Specify file to inspect for coverage while running tests given by testspec
COVERAGE_FILE = ${file}
ifeq ($(origin testspec), command line)
COVERAGE_TESTSPEC = ${testspec}
else
COVERAGE_TESTSPEC = t
endif
coverage:
	$(EMACS) --quick $(PRELOADS) \
	--eval "(load-file \"test/hy-test-dependencies.el\")" \
	--eval "(load-file \"test/hy-test-coverage.el\")" \
	--eval "(hy-test-coverage-file \"${file}\" \"${COVERAGE_TESTSPEC}\")"
