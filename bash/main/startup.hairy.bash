# startup.hairy.bash --- hairy initialization process for bash shells
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-07-23
# Public domain

# $Id: startup.hairy.bash,v 1.12 2010/03/02 22:58:35 friedman Exp $

# Commentary:
# Code:

# Null function.  Provided only so that we can put page breaks in source
# files without any ill effects.  \\014 == C-l
#eval "function $(echo -e \\014) () { :; }"
#() { :; }

test -f "$HOME/.bash_debug" && set -x


function verbose_startup ()
{
  local indent_padding="  "
  local i=${verbose_startup_indent_level:-0}
  local padding

  if [ -n "${verbose_startup+set}" ]; then
    if [ $# -eq 0 ]; then
      echo -n ")"
      verbose_startup_indent_level=$[ verbose_startup_indent_level - 1 ]
      if [ $verbose_startup_indent_level -eq 0 ]; then
        echo
        case `type -type time` in
          keyword ) startup_TIMEFORMAT=$TIMEFORMAT
                    TIMEFORMAT="Shell initialized in %3R seconds." ;;
          * ) echo Shell initialized in \
                   $[ $SECONDS - verbose_startup_start_seconds ] seconds. ;;
        esac
        unset verbose_startup_indent_level verbose_startup_start_seconds
      fi
    else
      while [ $i -gt 0 ]; do
        padding="$padding$indent_padding"
        i=$[ i - 1 ]
      done
      if [ "$verbose_startup_indent_level" != "0" ]; then
        echo
      fi
      echo -n "$padding($*"
      verbose_startup_indent_level=$[ verbose_startup_indent_level + 1 ]
    fi
  fi
}

function defenv_PAGER ()
{
  for pg in pager less most more ; do
    defenv_cmd PAGER which which $pg && break
  done
}

function get_termcap ()
{
  local term="${1:-${TERM:=dumb}}"
  local capfiles="$sinit/share/paths/termcapfiles"
  local file

  require path-list
  for file in $(path-list-verify path-list-name-file-p "$capfiles"); do
    if egrep "(^$term)|(\|$term\|)|(\|$term:)" "$file" > /dev/null 2>&1 ; then
      if [ -n "$1" ]; then
        echo "$file"
      else
        export TERMCAP="$file"
      fi
      return 0
    fi
  done
  return 1
}

function get_terminfo ()
{
  local term="${1:-${TERM:=dumb}}"
  local letter=$(echo "$term" | sed -e 's/^\(.\).*/\1/')
  local tinfodirs="$sinit/share/paths/terminfodirs"

  require path-list
  for dir in $(path-list-verify path-list-name-dir-p "$tinfodirs"); do
    if [ -f "$dir/$letter/$term" ] ; then
      if [ -n "$1" ]; then
        echo "$dir"
      else
        export TERMINFO="$dir"
      fi
      return 0
    fi
  done
  return 1
}

function tset ()
{
  case ${TERMCAP+set}  in set ) : ;; * ) get_termcap  ;; esac
  case ${TERMINFO+set} in set ) : ;; * ) get_terminfo ;; esac
  command tset "$@"
}

function sourceif ()
{
  local f

  for f in "$@" ; do
    if [ -r "$f" -a -f "$f" ]; then
      builtin source "$f"
    fi
  done
}

function source_bash_init_file ()
{
  if [ -f "$sinit/bash/$1.bash" ]; then
    verbose_startup "$1"
    source "$sinit/bash/$1.bash"
    verbose_startup
  fi
}

function source_local_bash_init_file ()
{
  if [ -f "$sinit_local/bash/$1.bash" ]; then
    verbose_startup "local/$1"
    source "$sinit_local/bash/$1.bash"
    verbose_startup
  fi
}

function startup_load_libraries ()
{
  local BASH_LOAD_PATH="$sinit/bash/lib"
  local lib

  source "$BASH_LOAD_PATH/require.bash"
  for lib in "$@"; do
     verbose_startup "lib/$lib"
     require "$lib"
     verbose_startup
  done
}


{
  if [ -n "${verbose_startup+set}" ]; then
     verbose_startup=t
     verbose_startup_indent_level=${verbose_startup_indent_level:-0}
     verbose_startup_start_seconds="$SECONDS"
     verbose_startup "main/startup.hairy"
  fi

  startup_load_libraries path-list chdir defvar fileutils help \
                         load prompt prompt_command source which \
                         check_exit_status

  add_prompt_command check_exit_status

  if (uname) > /dev/null 2>&1 ; then
    un_m=`uname -m`
    un_r=`uname -r`
    un_s=`uname -s`
    un_v=`uname -v`
  fi

  for f in  env vars aliases misc  ; do
     source_bash_init_file "main/$f"
  done
  unset f

  source_bash_init_file term/$TERM
  source_bash_init_file os/os
  source_bash_init_file main/domain

  if [ $SHLVL -eq 1 -a -z "${LOGGED+set}" ]; then
     if test -n "${LOGFILE+set}" ; then
        require recordlog
        recordlog in
        test -n "${loghost_in_project+set}" \
         && record_loghost_in_project
     fi
     trap 'sourceif "$sinit/bash/main/logout.bash"' EXIT
     export LOGGED=t
  fi

  verbose_startup

  prompt w
}

:

# startup.hairy.bash ends here
