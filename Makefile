# Makefile --- Build GNU Hyperbole directories and package distributions
#
# Author:       Bob Weiner
#
# Orig-Date:    15-Jun-94 at 03:42:38
# Last-Mod:     11-Oct-22 at 22:22:16 by Mats Lidell
#
# Copyright (C) 1994-2022  Free Software Foundation, Inc.
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
#		making and uploading tar ball to ftp.gnu.org.
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
#                   make test           - run not interactive tests in batch mode
#                   make test-all       - run all tests startin an emacs in windowed mode
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
HYPB_VERSION = 8.0.1pre

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
TEXI2INFO = makeinfo --no-split

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

##########################################################################
#                     NO CHANGES REQUIRED BELOW HERE.                    #
##########################################################################

# Libraries that must be pre-loaded before trying to byte-compile anything.
PRELOADS = $(SITE_PRELOADS) -l ./hload-path.el -l ./hversion.el -l ./hyperbole.el 

# Compile in batch mode.  Load site-lisp/site-start.el, which may set load-path.
# Show complete expression; do not abbreviate any exprs in batch logs with ...
BATCHFLAGS = -batch -Q --eval "(progn (setq debug-on-error t) (setq backtrace-line-length 0) \
                                 (message \"  emacs-version = %s\n  system-configuration = %s\n  emacs = %s%s\" emacs-version system-configuration invocation-directory invocation-name))"

EMACS_BATCH=$(EMACS) $(BATCHFLAGS) $(PRELOADS)

# Directories other than the current directory in which to find files.
# This doesn't seem to work in all versions of make, so we also add kotl/
# explicitly to those files which need it.
VPATH = kotl man

EL_COMPILE = hact.el hactypes.el hargs.el hbdata.el hbmap.el hbut.el \
	     hgnus.el hhist.el hib-debbugs.el hib-doc-id.el hib-kbd.el \
	     hib-social.el hibtypes.el \
	     hinit.el hload-path.el hmail.el hmh.el hmoccur.el hmouse-info.el \
	     hmouse-drv.el hmouse-key.el hmouse-mod.el hmouse-sh.el hmouse-tag.el \
	     hpath.el hrmail.el hsettings.el hsmail.el hsys-org.el hsys-www.el hsys-youtube.el htz.el \
	     hycontrol.el hui-jmenu.el hui-menu.el hui-mini.el hui-mouse.el hui-select.el \
	     hui-treemacs.el hui-window.el hui.el hvar.el hversion.el hvm.el hypb.el hyperbole.el \
	     hyrolo-demo.el hyrolo-logic.el hyrolo-menu.el hyrolo.el hywconfig.el set.el hypb-ert.el \
	     hui-dired-sidebar.el hypb-maintenance.el hui-em-but.el hui-register.el

EL_KOTL = kotl/kexport.el kotl/kfile.el kotl/kfill.el kotl/kimport.el kotl/klabel.el \
	  kotl/klink.el kotl/kmenu.el kotl/kotl-mode.el kotl/kotl-orgtbl.el \
          kotl/kcell.el kotl/kproperty.el kotl/kview.el kotl/kvspec.el

ELC_COMPILE =  hactypes.elc hibtypes.elc hib-debbugs.elc hib-doc-id.elc hib-kbd.elc \
	     hib-social.elc hact.elc \
	     hargs.elc hbdata.elc hbmap.elc hbut.elc hgnus.elc hhist.elc \
	     hinit.elc hload-path.elc hmail.elc hmh.elc hmoccur.elc hmouse-info.elc \
	     hmouse-drv.elc hmouse-key.elc hmouse-mod.elc hmouse-sh.elc hmouse-tag.elc \
	     hpath.elc hrmail.elc hsettings.elc hsmail.elc hsys-org.elc hsys-www.elc hsys-youtube.elc htz.elc \
	     hycontrol.elc hui-jmenu.elc hui-menu.elc hui-mini.elc hui-mouse.elc hui-select.elc \
	     hui-treemacs.elc hui-window.elc hui.elc hvar.elc hversion.elc hvm.elc hypb.elc hyperbole.elc \
	     hyrolo-demo.elc hyrolo-logic.elc hyrolo-menu.elc hyrolo.elc hywconfig.elc \
	     set.elc hypb-ert.elc hui-dired-sidebar.elc hypb-maintenance.elc hui-register.elc

ELC_KOTL = kotl/kexport.elc kotl/kfile.elc kotl/kfill.elc kotl/kimport.elc kotl/klabel.elc \
	   kotl/klink.elc kotl/kmenu.elc kotl/kotl-mode.elc kotl/kotl-orgtbl.elc \
           kotl/kcell.elc kotl/kproperty.elc kotl/kview.elc kotl/kvspec.elc

HY-TALK  = HY-TALK/.hypb HY-TALK/HYPB HY-TALK/HY-TALK.org

HYPERBOLE_FILES = dir info html $(EL_COMPILE) $(EL_KOTL) \
	$(ELC_COMPILE) $(HY-TALK) ChangeLog COPYING Makefile HY-ABOUT HY-ANNOUNCE HY-NEWS \
	HY-WHY.kotl INSTALL DEMO DEMO-ROLO.otl FAST-DEMO MANIFEST README README.md _hypb \
        .hypb smart-clib-sym topwin.py hyperbole-banner.png $(man_dir)/hkey-help.txt \
	$(man_dir)/hyperbole.texi $(man_dir)/hyperbole.css $(man_dir)/version.texi

TEST_ERT_FILES = $(wildcard test/*tests.el) $(wildcard test/hy-test-*.el)

EL_TAGS = $(EL_COMPILE) $(EL_KOTL) $(TEST_ERT_FILES)

.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .el .elc   # Define the list of file suffixes to match to rules

help: 
	@ echo "Use the Emacs Package Manager to build and install the latest release of GNU Hyperbole."
	@ echo "For help with Emacs packages, see the GNU Emacs Info Manual section, \"(emacs)Packages\"."
	@ echo "See \"$(shell pwd)/INSTALL\" for detailed installation instructions,"
	@ echo "including how to install Hyperbole pre-releases from the git repo tip."
	@ echo ""

	@ echo "To setup Hyperbole to run directly from the latest test source code, use:"
	@ echo "     git clone http://git.savannah.gnu.org/r/hyperbole.git"
	@ echo "     cd hyperbole"
	@ echo "  Then use either:"
	@ echo "     make src     - setup to run directly from .el files"
	@ echo "   or"
	@ echo "     make bin     - setup to build and run from .elc files"
	@ echo ""

	@ echo "For Hyperbole maintainers only:"
	@ echo "  To run unit tests:"
	@ echo "     make test-all       - run all tests with Emacs under a window system"
	@ echo "     make test           - run non-interactive tests with Emacs in batch mode"
	@ echo "  To verify hyperbole installation using different sources:"
	@ echo "     make install-<source>"
	@ echo "   where <source> can be 'elpa', 'elpa-devel', 'tarball' (tarball from elpa-devel),"
	@ echo "   'straight' (git master from savannah) or 'all'."
	@ echo "  To build the Hyperbole distribution package:"
	@ echo "     make pkg"
	@ echo "  To build documentation formats only:"
	@ echo "     make doc"
	@ echo "  To generate and upload the public Hyperbole website:"
	@ echo "     make website"
	@ echo "  To release a Hyperbole Emacs package to ELPA and ftp.gnu.org:"
	@ echo "     make release"
	@ echo ""

	@ echo "The Hyperbole Manual is included in the package in four forms:"
	@ echo "    man/hyperbole.info    - GNU browsable version"
	@ echo "    man/hyperbole.html    - Web browsable version"
	@ echo "    man/hyperbole.pdf     - Printable version"
	@ echo "    man/hyperbole.texi    - source form"
	@ echo ""

all: help

echo:
	which emacs; echo $(TERM); echo "$(DISPLAY)"

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

# Record any .el files that need to be compiled.
.el.elc:
	@ echo $< >> $(ELISP_TO_COMPILE)

# Compile all recorded .el files.
elc: elc-init $(ELC_KOTL) $(ELC_COMPILE)
	@- \test ! -f $(ELISP_TO_COMPILE) \
            || (echo "These files will be compiled: " \
                 && echo "`cat $(ELISP_TO_COMPILE)`" \
                 && $(EMACS_BATCH) -f batch-byte-compile `cat $(ELISP_TO_COMPILE)`)
	@ $(RM) $(ELISP_TO_COMPILE)

elc-init:
	@ $(RM) $(ELISP_TO_COMPILE)

# Setup to run Hyperbole from .el source files
src: autoloads tags

# Remove and then rebuild all byte-compiled .elc files, even those .elc files
# which do not yet exist, plus build TAGS file.
#
# Use this to suppress docstring warnings.
#	$(EMACS_BATCH) --eval="(setq-default byte-compile-warnings '(not docstrings))" \
#		-f batch-byte-compile $(EL_KOTL) $(EL_COMPILE)
bin: src
	$(RM) *.elc kotl/*.elc
	$(EMACS_BATCH) -f batch-byte-compile $(EL_KOTL) $(EL_COMPILE)

# Create -l "file.el" load-file command-line args for each Hyperbole .el file for use in
# eln native compile target below.
LOAD_EL = $(shell echo "$(EL_KOTL) $(EL_COMPILE)" | sed - -e 's+ +" -l "./+g' -e 's+^+-l "+')"

load-hyperbole:
	$(EMACS_BATCH) \
          $(LOAD_EL)

# Use this to suppress docstring warnings.
# 	$(EMACS_BATCH) \
#           $(LOAD_EL) \
#           --eval="(setq-default byte-compile-warnings '(not docstrings))" \
# 	    -f batch-native-compile $(EL_KOTL) $(EL_COMPILE)
eln: src
	$(EMACS_BATCH) \
          $(LOAD_EL) \
	  -f batch-native-compile $(EL_KOTL) $(EL_COMPILE)

# Byte compile files but apply a filter for either including or
# removing warnings.  See variable {C-hv byte-compile-warnings RET} for
# list of warnings that can be controlled.  Default is set to suppress
# warnings for long docstrings.
#
# Example for getting warnings for obsolete functions and variables
#   HYPB_WARNINGS="free-vars" make bin-warn
# Example for surpressing the free-vars warnings
#   HYPB_WARNINGS="not free-vars" make bin-warn
ifeq ($(origin HYPB_WARNINGS), undefined)
HYPB_BIN_WARN = not docstrings
else ifeq ($(origin HYPB_WARNINGS), environment)
HYPB_BIN_WARN = ${HYPB_WARNINGS}
endif
bin-warn: src
	$(RM) *.elc kotl/*.elc
	$(EMACS_BATCH) --eval="(setq-default byte-compile-warnings '(${HYPB_BIN_WARN}))" \
		-f batch-byte-compile $(EL_KOTL) $(EL_COMPILE)

tags: TAGS
TAGS: $(EL_TAGS)
	$(ETAGS) --regex='/(ert-deftest[ \t]+\([^ \t\n\r\f()]+\) ?/' $(EL_TAGS)

clean:
	$(RM) hyperbole-autoloads.el kotl/kotl-autoloads.el $(ELC_COMPILE) $(ELC_KOTL) TAGS

version: doc
	@ echo ""
	@ echo "Any fgrep output means the version number has not been updated in that file."
	test 0 -eq $$(fgrep -L $(HYPB_VERSION) hyperbole-pkg.el Makefile HY-ABOUT HY-NEWS README.md hversion.el hyperbole.el man/hyperbole.texi man/version.texi | wc -c) || exit 1
	@ echo ""

# Build the Info, HTML and Postscript versions of the user manual and README.md.html.
doc: README.md.html info html pdf

TEXINFO_SRC = $(man_dir)/hyperbole.texi $(man_dir)/version.texi $(man_dir)/hkey-help.txt $(man_dir)/im/*.png

info: $(man_dir)/hyperbole.info
$(man_dir)/hyperbole.info: $(TEXINFO_SRC)
	cd $(man_dir) && $(TEXI2INFO) hyperbole.texi

html: $(man_dir)/hyperbole.html
$(man_dir)/hyperbole.html: $(TEXINFO_SRC) $(man_dir)/hyperbole.css
	cd ${man_dir} && $(TEXI2HTML) hyperbole.texi

pdf: $(man_dir)/hyperbole.pdf
$(man_dir)/hyperbole.pdf: $(TEXINFO_SRC)
	cd $(man_dir) && $(TEXI2PDF) hyperbole.texi

# md2html is a Python package that comes from the md2html-phuker repo on github.
#   Documentation is here: https://github.com/Phuker/md2html
#   Need the GNU sed call below because md2html generates ids with the wrong case and leaves URL encoded chars in ids.
#   To test links in the generated html:
#     Run a Python directory web browser in this directory: python -m http.server 8000
#     Open the page in a web browser:                       http://localhost:8000/README.md.html
#
# Used to use github-markdown is an npm, installed with: npm install markdown-to-html -g
#   But then it's links broke.  Documentation is here: https://www.npmjs.com/package/markdown-to-html
#	github-markdown README.md > README.md.html
README.md.html: README.md
	md2html README.md -f -o - | sed - -e 's/\(id="[^%]*\)\(%[A-Z0-9][A-Z0-9]\)/\1/g' -e 's/\(id="[^"]*"\)/\L\1/g' > README.md.html
	md2html README.md -f -o README.md.html

# website maintenance: "https://www.gnu.org/software/hyperbole/"
# Locally update Hyperbole website
website-local: README.md.html
	$(EMACS_BATCH) --debug -l hypb-maintenance --eval '(let ((hypb:web-repo-location $(HYPB_WEB_REPO_LOCATION))) (hypb:web-repo-update))'

# Push to public Hyperbole website
website: website-local
	cd $(HYPB_WEB_REPO_LOCATION) && $(CVS) commit -m "Hyperbole release $(HYPB_VERSION)"

# Generate a Hyperbole package suitable for distribution via the Emacs package manager.
pkg: package
package: doc $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.sig

# Generate and distribute a Hyperbole release to ftp.gnu.org.
# One step in this is to generate an autoloads file for the Koutliner, kotl/kotl-autoloads.el.
release: git-pull git-verify-no-update package $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.gz ftp website git-tag-release
	@ echo; echo "Hyperbole $(HYPB_VERSION) released to ftp.gnu.org successfully."

# Ensure local hyperbole directory is synchronized with master before building a release.
git-pull:
	echo "If this step fails check your work directory for not committed changes"
	git checkout master && git pull
	git diff-index --quiet master

git-verify-no-update:
	echo "If this step fails check your work directory for updated docs and push these to savannah"
	git diff-index --quiet master

git-tag-release:
	git tag -a hyperbole-$(HYPB_VERSION) -m "Hyperbole release $(HYPB_VERSION)"
	git push origin hyperbole-$(HYPB_VERSION)

# Send compressed tarball for uploading to GNU ftp site; this must be done from the directory
# containing the tarball to upload.
ftp: package $(pkg_parent)/hyperbole-$(HYPB_VERSION).tar.gz
	cd $(pkg_parent) && $(GNUFTP) hyperbole-$(HYPB_VERSION).tar.gz

# Autoloads
autoloads: hyperbole-autoloads.el kotl/kotl-autoloads.el

hyperbole-autoloads.el: $(EL_COMPILE)
	$(EMACS_BATCH) --debug --eval "(progn (setq generated-autoload-file (expand-file-name \"hyperbole-autoloads.el\") backup-inhibited t) (let (find-file-hooks) (hload-path--make-directory-autoloads \".\" generated-autoload-file)))"

kotl/kotl-autoloads.el: $(EL_KOTL)
	$(EMACS_BATCH) --debug --eval "(progn (setq generated-autoload-file (expand-file-name \"kotl/kotl-autoloads.el\") backup-inhibited t) (let (find-file-hooks) (hload-path--make-directory-autoloads \"kotl/\" generated-autoload-file)))"

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
	$(CP) hyperbole-pkg.el $(pkg_hyperbole) && cd $(pkg_hyperbole) && make autoloads && chmod 755 topwin.py && \
	COPYFILE_DISABLE=1 $(TAR) -C $(pkg_parent) -clf $(pkg_hyperbole).tar hyperbole-$(HYPB_VERSION)

pkgclean: packageclean
packageclean:
	if [ -d $(pkg_hyperbole) ]; then \
	  cd $(pkg_hyperbole) && $(RM) -r .git* videos ChangeLog.* *autoloads.* *.elc TAGS TODO* HY-ANNOUNCE-* .DS_Store \
	    core .place* ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore GNUmakefile.id \
	    && gsed '/\f/,/\f/{/\f/!d}' .hypb | tail +2 > .hypb2 && rm -f .hypb && mv .hypb2 .hypb; fi # Filter out unneeded TODO file hbut data from .hypb
	if [ -d $(pkg_hyperbole)/kotl ]; then \
	  cd $(pkg_hyperbole)/kotl && $(RM) -r *autoloads.* *.elc TAGS TODO* .DS_Store \
	    core .place* ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(pkg_hyperbole)/man ]; then \
	  cd $(pkg_hyperbole)/man && $(RM) -r .DS_Store core .place* hyperbole.{log,aux,cp*,fn*,ky*,toc,vr*} \
	    ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(pkg_hyperbole)/man/im ]; then \
	  cd $(pkg_hyperbole)/man/im && $(RM) -r .DS_Store core .place* ._* .*~ *~ \
	    *.ps *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi

# Ert test
.PHONY: tests test test-ert all-tests test-all
tests: test
test: test-ert

LOAD_TEST_ERT_FILES=$(patsubst %,(load-file \"%\"),${TEST_ERT_FILES})

test-ert:
	@echo "# Tests: $(TEST_ERT_FILES)"
	$(EMACS_BATCH) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ((auto-save-default)) $(LOAD_TEST_ERT_FILES) (ert-run-tests-batch-and-exit))"

all-tests: test-all
test-all:
	@echo "# Tests: $(TEST_ERT_FILES)"
ifeq ($(TERM), dumb)
ifneq (,$(findstring .apple.,$(DISPLAY)))
        # Found, on MacOS
	TERM=xterm-256color $(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ((auto-save-default)) $(LOAD_TEST_ERT_FILES) (ert-run-tests-interactively t))"
else
        # Not found, set TERM so tests will at least run within parent Emacs session
	TERM=vt100 $(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ((auto-save-default)) $(LOAD_TEST_ERT_FILES) (ert-run-tests-interactively t))"
endif
else
        # Typical case, run emacs normally
	$(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ((auto-save-default)) $(LOAD_TEST_ERT_FILES) (ert-run-tests-interactively t))"
endif

batch-tests: test-all-output
test-all-output:
	$(EMACS) --quick $(PRELOADS) --eval "(load-file \"test/hy-test-dependencies.el\")" --eval "(let ((auto-save-default) (ert-quiet t)) $(LOAD_TEST_ERT_FILES) (ert-run-tests-interactively t) (with-current-buffer \"*ert*\" (append-to-file (point-min) (point-max) \"ERT-OUTPUT\")) (kill-emacs))"
	@echo "# Results written to file: ERT-OUTPUT"

# Hyperbole install tests - Verify that hyperbole can be installed
# using different sources. See folder "install-test"
.PHONY: install-elpa install-elpa-devel install-tarball install-straight install-all
install-all: install-elpa install-elpa-devel install-tarball install-straight

install-elpa install-elpa-devel install-tarball install-straight:
	@echo "Install Hyperbole using $@"
	(cd ./install-test/ && ./local-install-test.sh $(subst install-,,$@))

package-lint:
	$(EMACS_BATCH) \
	--eval "(setq package-lint-main-file \"hyperbole.el\")" \
	--eval "(load-file \"test/hy-test-dependencies.el\")" \
	-l package-lint.el -f package-lint-batch-and-exit \
	$(EL_KOTL) $(EL_COMPILE)
