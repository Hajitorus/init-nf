# os.bash --- load os-specific hacks
# Author: Noah Friedman <friedman@splode.com>
# Created: 1999-06-01
# Public domain

# $Id: os.bash,v 1.3 2010/02/18 11:04:11 friedman Exp $

# Commentary:
# Code:

dom=$OS

case "$OS" in
  aix* )      dom=aix      ;;
  hpux* )     dom=hpux7    ;;
  irix* )     dom=irix     ;;
  solaris2* ) dom=solaris2 ;;
  sunos4.* )  dom=sunos4.1 ;;
  redhat* )   dom=redhat   ;;
  fedora* )   dom=fedora   ;;
esac

source_bash_init_file os/$dom
unset dom

# os.bash ends here
