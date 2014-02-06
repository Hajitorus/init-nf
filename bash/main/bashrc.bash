# bashrc.bash --- start of initialization for all bash sessions
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-07-23
# Public domain

# $Id: bashrc.bash,v 1.3 2010/02/22 19:47:38 friedman Exp $

# Commentary:
# Code:

case $PS1$XSESSION in
  '' ) return 0 ;;
esac

sinit=$HOME/etc/init
sinit_local=$sinit/local
export sinit sinit_local

. "$sinit/bash/main/startup.simple.bash"

# bashrc.bash ends here
