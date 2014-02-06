# misc.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-03-18
# Public domain

# $Id: misc.bash,v 1.11 2005/06/13 14:59:35 friedman Exp $

# Commentary:
# Code:

# By temporarily setting HISTSIZE to zero, we can zap all the saved history
# lines
function clear_history ()
{
  local saved_HISTSIZE="${HISTSIZE}"
  HISTSIZE=0
  HISTSIZE="${saved_HISTSIZE}"
}

# Call the real pager, regardless of the name.
#function more ()
#{
# command "${PAGER:-more}" "$@";
#}

# Print just variables and their values, but do not include function
# definitions.
function variables ()
{
  set | sed -ne '
   / ()[ 	]*$/{
     h;n
     /^{[ 	]*$/!{
       x;p;x;p;d
     }
     /^{[ 	]*$/{
       :l
       n
       /^}[ 	]*$/!b l
      d
     }
   }
   p'
}


# When XSESSION is loading, there's no reason to run stty and in some
# cases it actually hangs the terminal.
{
  if [ -z "$XSESSION" ]; then
    stty-canon
  fi
} 2> /dev/null

raise-limits

if [ $EUID -eq 0 ]; then
   umask 022
else
   umask 000
fi

if [ -n "$TTY" -a -O "$TTY" ]; then
   chmod 622 "$TTY"
fi

# If we're in an automounted directory for $HOME and the prompt is buggered
# as a result, fix it.
if [ "$(pwd)" = "$(set -o physical; cd $HOME; pwd)" ]; then
   cd "$HOME"
fi

source_local_bash_init_file misc

# misc.bash ends here
