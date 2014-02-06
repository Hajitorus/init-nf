# fileutils.bash --- interactive file manipulation functions
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-01-15
# Public domain

# $Id: fileutils.bash,v 1.12 2010/03/02 22:58:35 friedman Exp $

# Commentary:
# Code:

ls_page()
{
  case $TERM in
    emacs | emacs-virtual ) command ls "$@" ;;
    * ) command ls "$@" | ${LS_PAGER-more} ;;
  esac
}

# Make sure there are no aliases defined for these functions.
# If there are, their definitions will get expanded inside the function
# definitions (aliases are expanded in functions when the functions are
# read, not when they are executed.  Yuck!
unalias ls lf lh li lis ll 2>/dev/null

###;;;autoload
ls()
{
  case ${GNU_LS_P-unknown} in
    unknown )
      if { command ls -d --version . \
            | egrep -i 'fileutils|coreutils'; } > /dev/null 2>&1
      then GNU_LS_P=t
      else GNU_LS_P=f
      fi ;;
  esac

  case $GNU_LS_P in
    t ) set fnord -C $GNU_LS_OPTIONS "$@"
        shift

        case $EMACS in
          t ) set fnord --show-control-chars "$@"; shift ;;
        esac

        case ${LSCLEAN+t} in
          t ) set fnord -B -I '*.o' "$@" ; shift ;;
        esac ;;
  esac

  ls_page "$@"
}

###;;;autoload
li() { ls -la "$@"; }

provide fileutils

# fileutils.bash ends here
