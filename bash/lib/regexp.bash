# regexp.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1991-10-11
# Public domain

# $Id: regexp.bash,v 1.2 1995/01/25 19:26:41 friedman Exp $

# Commentary:
# Code:

#:docstring regexp-quote:
# Usage: regexp-quote [string]
#
# Quote STRING so that [e]grep and sed will match exactly that string (i.e.
# quote special characters so that they lose their specialness). 
# The character `/' is quoted also, so that filenames can be quoted for sed
# literals.
# If no string is provided on the command line, the string is read from
# stdin instead.
#:end docstring:

###;;;autoload
function regexp-quote ()
{
  local re='s/\([][*.\\\/?+|^$]\)/\\\1/g'

  if [ $# -eq 0 ]; then
    sed -e "$re"
  else
    echo ${1+"$@"} | sed -e "$re"
  fi
}

provide regexp

# regexp.bash ends here
