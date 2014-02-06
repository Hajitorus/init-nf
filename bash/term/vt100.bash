# vt100.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1991-12-11
# Public domain

# $Id: vt100.bash,v 1.2 2002/08/09 11:54:07 friedman Exp $

# Commentary:

# Functions which should only be loaded when running shells on vt100,
# vt102, or vt220 terminals.  (For maintenance convenience, these files are
# probably all hard-linked to each other)

# Code:

#LINES=24
#COLUMNS=80
#export LINES COLUMNS
#stty erase '^?' rows $LINES columns $COLUMNS 2> /dev/null

# Change terminal to black-on-white
function bow ()
{
  echo -n "${e}[?5l"
}

# Change terminal to white-on-black
function wob ()
{
  echo -n "${e}[?5h"
}

# vt100.bash ends here
