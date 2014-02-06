# fedora.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 2010-02-18
# Public domain

# $Id: fedora.bash,v 1.2 2010/03/23 07:21:32 friedman Exp $

# Commentary:

# Get rid of system-installed cruft Fedora decided I need, that I don't want.

# Code:

unset  KDEDIRS
#unset KDE_IS_PRELINKED

case ${USER_LS_COLORS:-undefined} in
  undefined ) unset  LS_COLORS ;;

  *         ) LS_COLORS=$USER_LS_COLORS
              export LS_COLORS ;;
esac

unset  PERL5LIB

unset  LOADEDMODULES
unset  MODULEPATH
unset  MODULESHOME
unset  -f module

unset  QTDIR
unset  QTINC
unset  QTLIB

# eof
