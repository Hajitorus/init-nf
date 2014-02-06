# 300h.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1993-05-18
# Public domain

# $Id: 300h.bash,v 1.2 2002/08/09 11:54:07 friedman Exp $

# Commentary:
# Code:

LINES=49
COLUMNS=128
export LINES COLUMNS

stty erase '^H' rows $LINES columns $COLUMNS 2> /dev/null

# 300h.bash ends here
