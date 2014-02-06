# printpath.bash --- functions for parsing path files
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-05-11
# Public domain

# $Id: printpath.bash,v 1.3 1995/01/25 18:50:43 friedman Exp $

# Commentary:

# This file is deprecated in favor of path-list.bash.

# Code:

#:docstring cprintpath:
# Usage: cprintpath {-d delimiter} [file1] {files...}
# 
# Like printpath, but don't include directories that don't actually exist.
#:end docstring:

###;;;autoload
function cprintpath ()
{
  local delim=:
  local newlist=
  local dir=
  local testop=-d

  while : ; do
    case "$1" in
      -d ) 
        delim=$2
        shift
        shift
       ;;
      -f )
        testop=-f
        shift
       ;;
      * )
        break
       ;;
    esac
  done      

  case $# in
     0 )
       echo "Usage: cprintpath {-f} {-d delimiter} [file1] {files...}" 1>&2
       exit 1
      ;;
  esac

  eval set fnord `sed -e '/^[	 ]*[#].*/d' ${1+"$@"}`
  shift

  for dir in ${1+"$@"} ; do
     # Always allow "." and ".."
     if test "z$dir" = 'z.' -o "$testop" "$dir" ; then
        case "$newlist" in
          '') newlist="$dir" ;;
          * ) newlist="$newlist$delim$dir" ;;
        esac
     fi
  done

  echo "$newlist"
}

#:docstring printpath:
# Usage: printpath {-d delimiter} [file1] {files...}
#
# Parse {FILE1 FILES...}, which is assumed to contain a list of directories
# separated by whitespace (tabs, spaces, or newlines), and construct a
# string consisting of those directories separated by DELIMITER (`:' by
# default).  Comments in the file (lines beginning with `#') are ignored,
# but directories and comments cannot exist on the same line.
#:end docstring:

###;;;autoload
function printpath ()
{
 local delim=":"

    case "z$1" in
       z-d ) delim="$2"; shift 2 ;;
    esac

    case $# in
       0 )
          echo "Usage: printpath {-d delimiter} [file1] {files...}" 1>&2
          return 1
         ;;
    esac

    eval echo \"`sed -ne '
             /^[	 ]*[#].*/!H;
             ${x;
               s/  */'"${delim}"'/g;
               s/\n\n*/'"${delim}"'/g;
               s/^://;s/:$//;
               p;
              }' ${1+"$@"}`\"
}

printpathif_internal ()
{
 local cmd="${1}"
 local delim
 local arg
 
    shift; 

    case "z${1}" in
       z-d ) delim="-d $2"; shift 2 ;;
    esac

    for arg in "$@" ; do
       test -f "${arg}" || return 1
    done

    "${cmd}" ${delim} "$@"
}

###;;;autoload
function cprintpathif ()
{
    printpathif_internal cprintpath "$@"
}

###;;;autoload
function printpathif ()
{
    printpathif_internal printpath "$@"
}

provide printpath

# printpath.bash ends here
