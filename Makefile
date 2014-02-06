# Makefile --- top-level Makefile for my init files
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-06-02
# Public domain

# $Id: Makefile,v 1.8 2006/03/11 22:02:24 friedman Exp $

# Commentary:
# Code:

CP = cp # -f
LN = ln
MV = mv -f
RM = rm -f
SHELL = /bin/sh

MKTAR = $(sinit)/bin/init-mktar

#TARDIR = $(HOME)/src/archive
TARDIR = /home/ftp/pub/users/friedman/inits

all:
	@echo Choose one of: dist terminfo termcap

dist: distclean incr-minor-version cvs-tag tar-all

tar-all: tar tar-emacs #tar-es

tar: force
	@$(MKTAR) $(TARDIR)/init-VERSION *

tar-emacs: force
	@$(MKTAR) $(TARDIR)/init-emacs-VERSION emacs

tar-es: force
	@$(MKTAR) $(TARDIR)/init-es-VERSION es

# Increment minor version number by 1.
incr-minor-version: force
	awk -F. 'BEGIN { OFS = "." } { $$2++; print $$0 ; }'\
	   < $$sinit/share/=version > $$sinit/share/=version.new
	test -s $$sinit/share/=version.new
	$(MV) $$sinit/share/=version.new $$sinit/share/=version
	chmod 666 $$sinit/share/=version

# Increments major version number, resets minor number to -1 (this way when
# you do "make dist", the minor version will be properly incremented to 0.
incr-major-version: force
	awk -F. 'BEGIN { OFS = "." } { $$1++; $$2 = -1; print $$0 ; }'\
	   < $$sinit/share/=version > $$sinit/share/=version.new
	$(MV) $$sinit/share/=version.new $$sinit/share/=version
	chmod 644 $$sinit/share/=version

cvs-tag: force
	cvs tag -F init-`cat $$sinit/share/=version | sed 'y/./-/'`

clean: force
	@echo 'Use "make veryclean" if you really mean it.'

veryclean: force
	$(RM) *~ #*# .nfs* core

distclean: force
	find . \( -name "*~" -o -name "#*#" -o -name ".#*" -o -name ".nfs*" -o -name "core" \) -print -exec $(RM) "{}" \;

TERMINFO_DIR = $(HOME)/lib/terminfo
terminfo: force
	-mkdir -p $(TERMINFO_DIR)
	TERMINFO=$(TERMINFO_DIR) tic share/terminfo.src

TERMCAP_DIR = $(TERMINFO_DIR)
TERMCAP_DST = $(TERMCAP_DIR)/termcap
termcap: force
	-mkdir -p $(TERMCAP_DIR)
	sed -n -e '/^[A-Za-z0-9	]/!d' -e '/:\\$$/{H;d;}' -e '/:$$/{H;s/.*//;x;s/\n//g;s/:\\[	 ]*:/:/g;p;}' share/termcap.src > $(TERMCAP_DST)

.PHONY: force
force:

# Makefile ends here
