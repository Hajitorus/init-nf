#!/bin/sh
# $Id: 15-xset.sh,v 1.4 2011/07/29 00:46:11 friedman Exp $

xset b 100

# Not all systems support setting the repeat rate (e.g. x86-pc-solaris10)
xset r rate 250 30 2> /dev/null

xset m 5/1 15

xset dpms 1800 1800 1800

# sometimes necessary to enable dpms with buggy drivers.
xset dpms force on 2> /dev/null

# eof
