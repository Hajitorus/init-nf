# startup.simple.bash --- simple initialization process for bash shells
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-07-23
# Public domain

# $Id: startup.simple.bash,v 1.34 2011/04/14 00:14:20 friedman Exp $

# Commentary:
# Code:

# Function to start hairy initialization
# If `time' is a builtin keyword, use it to provide more accuracy reporting
# startup time.  Otherwise the startup routines will use $SECONDS to print
# a low-precision time.
function startup ()
{
  local hairy=$sinit/bash/main/startup.hairy.bash

  case `type -type time` in
    keyword )
      time . "$hairy"
      case ${startup_TIMEFORMAT:+set} in
        set ) TIMEFORMAT=$startup_TIMEFORMAT ;;
        *   ) unset TIMEFORMAT ;;
      esac
      unset startup_TIMEFORMAT ;;
    * ) . "$hairy" ;;
  esac
}

if [ -n "$XSESSION" -o -f "$HOME/.bash_startup" ]; then
  startup
  return $?
fi

. "$sinit/bash/main/options.bash"

case $LOGGED in
  t ) : ;;
  * )
    umask 000
    PATH="$HOME/bin/local:$HOME/bin/share:$HOME/bin/misc:$sinit/bin:$PATH:/usr/sbin:/sbin"
    HISTFILE=/dev/null
    EDITOR=ed
    VISUAL=ed
    export PATH HISTFILE EDITOR VISUAL

    case $LANG in
      en_US* ) unset LANG ;;
    esac
   ;;
esac

BASH_LOAD_PATH="$sinit/bash/lib:$HOME/lib/bash"
source "$sinit/bash/lib/require.bash"
require load

# Do not let RedHat 6.x override INPUTRC.
case $INPUTRC in /etc/inputrc ) unset INPUTRC ;; esac

require defvar

PS1='\u@\h[$SHLVL]: \w \$ '
notify=
ignoreeof=10

# Turn off history; I never use it, but I do sometimes type filenames with
# `!' in them, and I'm sick of getting bitten.
set +o histexpand

# Make sure these are not read-only (aix does this in /etc/profile).
# Doh!  It turns out that you can't make a variable un-readonly in bash
# once it's been declared readonly.  Of course, a manually-set readonly
# property of a variable is not propagated to subshells...
#declare +r LOGNAME USER

function li ()  { ls -la "$@"; }
function lif () { ls -la "$@" | fmtcols -Nwi '^total' ; }

function login_mail_check ()
{
  local mail=/var/mail
  local d

  for d in /var/mail /usr/spool/mail /usr/mail ; do
    if [ -d "$d" ]; then
      mail="$d"
      break
    fi
  done

  mail="$mail/${LOGNAME:-${USER:-$(whoami)}}"
  if [ -s "$mail" ]; then
    set fnord $(grep '^From ' "$mail" | wc -l)
    shift
    case $1 in
      1 ) echo There is $1 message in $mail ;;
      * ) echo There are $1 messages in $mail ;;
    esac
  fi
}

# Raise all soft limits to hard limits
function raise-limits ()
{
  local ulimit

  for ulimit in c d m s t f p n u v ; do
    ulimit -S$ulimit $(ulimit -H$ulimit 2> /dev/null) 2> /dev/null
  done
}

function stty-canon ()
{
  stty icanon tabs

  stty intr '^C' kill '^U' quit '^\\' eof '^D'
  stty susp '^Z'
  stty cs8 -istrip -iexten -parenb -ixon -ixoff -ixany ${1+"$@"}
  stty onlcr -ocrnl -onlret ${1+"$@"}

  case $TERM in
    emacs ) stty -icrnl -inlcr -onlcr ;;
    xterm ) stty erase '^?' ;;
  esac
}

function set-path ()
{
  require defvar
  require path-list

  # First set a temporary path so we can set some needed variables.
  export PATH="$sinit/bin:$PATH"

  # Useful to have set before initializing real path.
  defenv_cmd SINITDOMAIN    nil get_sinit_domain
  defenv_cmd SINIT_MACHTYPE nil hosttype
  defvar OS "${SINIT_MACHTYPE##*-}"

  PATH=$(print-site-paths $HOME/bin | cprintpath - "$sinit/share/paths/path")
}

function get_sinit_domain ()
{
  for f in ~/.sinitdomain ~/etc/misc/.sinitdomain ; do
    if [ -f "$f" ]; then
      sed -n -e 's/[ 	]*#.*//' \
             -e '/^$/d' \
             -e H \
             -e '${' \
             -e   'x' \
             -e   's/\n/:/g' \
             -e   's/[ 	,;]/:/g' \
             -e   's/:::*/:/g' \
             -e   's/^://p' \
             -e  '}' "$f"
      break
    fi
  done
}

alias ZZ=suspend

# Do not use stty-canon here because the cs8 etc. settings there might make
# some terminals unusable.  These are pretty safe options.
{
  stty icanon tabs
  stty intr '^C' kill '^U' quit '^\\' eof '^D'
  stty susp '^Z'
  stty -ixon -ixoff -ixany
  stty onlcr -ocrnl -onlret

  case $TERM in
    emacs ) stty -icrnl -inlcr -onlcr ;;
  esac

} 2> /dev/null

case $EMACS in
  t )
    case $TERM in
      xterm )
        unset EMACS
        stty opost 2> /dev/null
       ;;
      * ) TERM=emacs ;;
    esac
esac
case $TERM in
  emacs)
    export PAGER=cat
    unset MAIL MAILPATH MAILCHECK MAIL_WARNING
    stty -echo 2> /dev/null
   ;;
esac

if [ ".$LOGGED" != '.t' -a "${login_mail_check+set}" = set -a -f ~/.hushlogin ]
then
   login_mail_check
fi

raise-limits

if [ -f "$sinit_local/bash/startup.simple.bash" ]; then
   . "$sinit_local/bash/startup.simple.bash"
fi

# If we're in an automounted directory for $HOME and the prompt is buggered
# as a result, fix it.
if [ "$PWD" = "$(set -o physical; cd $HOME; pwd)" ]; then
   cd "$HOME"
fi

:

# startup.simple.bash ends here
