# meta.bash --- meta key frobnications
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-06-28
# Public domain

# $Id: meta.bash,v 1.2 2010/02/22 19:47:38 friedman Exp $

# Commentary:
# Code:

require bash_version

#:docstring meta:
# Usage: meta [on|off]
#
# An argument of "on" will make bash use the 8th bit of any input from
# a terminal as a "meta" bit, i.e bash will be able to use a real meta
# key.
#
# An argument of "off" causes bash to disregard the 8th bit, which is
# assumed to be used for parity instead.
#:end docstring:

###;;;autoload
meta()
{
  case $1 in
    on  ) bind 'set eight-bit-input On'  ;;
    off ) bind 'set eight-bit-input Off' ;;
    *   ) echo "Usage: meta [on|off]" 1>&2
          return 1 ;;
  esac
}

provide meta

# meta.bash ends here
