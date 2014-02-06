# xterm.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1993-05-18
# Public domain

# $Id: xterm.bash,v 1.5 2011/03/01 07:50:46 friedman Exp $

# Commentary:
# Code:

alias qekrpat='xdvorak'
alias resize='eval $(command resize)'

# These are obsolesced by xterm-set
#function wob    () { echo -ne "\E[?5h"; }     # Make window white-on-black
#function bow    () { echo -ne "\E[?5l"; }     # Make window black-on-white
#function ititle () { echo -ne "\E]1;$*\a"; }  # Change icon title
#function wtitle () { echo -ne "\E]2;$*\a"; }  # Change window title
#function xtitle () { echo -ne "\E]0;$*\a"; }  # Change both i&w titles
#function xterm-set-font () { echo -ne "\E]50;$*\a"; }

stty-canon
stty erase '^?' 2> /dev/null
eval 'resize 2> /dev/null'

function init-screen-term ()
{
  xterm-set title "${1-screen}" geometry ${2-66} 80
  export TERM=xterm-256color
  load paranoid-exit
  prompt uhsw
}

# xterm.bash ends here
