#!/bin/sh
# $Id: 20-xbindkeys.sh,v 1.3 2012/03/05 16:59:37 friedman Exp $

# Run this in backgrounded "foreground" mode so that stdout/stderr will go
# to a log file.
xbindkeys -n -fg $sinit/share/xbindkeysrc.scm &

# eof
