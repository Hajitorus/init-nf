# sun.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1991-12-11
# Public domain

# $Id: sun.bash,v 1.2 2002/08/09 11:54:07 friedman Exp $

# Commentary:
# Code:

LINES=34
COLUMNS=80
export LINES COLUMNS
stty erase '^?' rows $LINES columns $COLUMNS 2> /dev/null

# Make terminal black-on-white
function bow ()
{
  echo -n "${e}[p"
}

# Make terminal white-on-black
function wob ()
{
  echo -n "${e}[q"
}

# echoing a C-l clears the screen on suns
function cls ()
{
  echo -ne "\014"
}

# Function to fix screwy hiking on the console, which can cause the screen
# to be scrolled halfway even if only one line of scrolling is needed.
# Echoing this sequence on the console seems to make it sane again.
function fixhike ()
{
  echo -n "${e}[1r"
}

# sun.bash ends here
