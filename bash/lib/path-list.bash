# path-list.bash --- functions for parsing path files
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-05-11
# Public domain

# $Id: path-list.bash,v 1.6 2000/01/24 13:04:34 friedman Exp $

# Commentary:
# Code:

#:docstring path-list:
# Usage: path-list [file1] {files...}
#
# Parse {FILE1 FILES...}, which is assumed to contain a list of names
# separated by whitespace (tabs, spaces, or newlines), and construct a list
# consisting of those names.  Comments in the file (lines beginning
# with `#') are ignored, but names and comments cannot exist on the
# same line.
#:end docstring:

###;;;autoload
function path-list
{
  eval echo `sed -e '/^[	 ]*[#].*/d' ${1+"$@"} 2> /dev/null`
}

#:docstring path-list-verify:
# Usage: path-list-verify [predicate] [file1] {files...}
#
# Like path-list, but don't return names that don't satisfy predicate.
# Some standard predicates functions available include:
#     path-list-name-exists-p
#     path-list-name-file-p
#     path-list-name-dir-p
#:end docstring:

###;;;autoload
function path-list-verify
{
  local predicate="$1"

  shift
  for dir in `path-list ${1+"$@"}`; do
    if "$predicate" "$dir"; then
      echo "$dir"
    fi
  done
}

#:docstring path-list-verify-first:
# Usage: path-list-verify [predicate] [file1] {files...}
#
# Like path-list-verify, but return only the first name that satisfies
# predicate.
#:end docstring:

###;;;autoload
function path-list-verify-first
{
  local predicate="$1"

  shift
  for dir in `path-list ${1+"$@"}`; do
    if "$predicate" "$dir"; then
      echo "$dir"
      break
    fi
  done
}

###;;;autoload
function path-list-colon
{
  ${1+"$@"} \
   | sed -ne '1h
              1!H
              ${x
                /^ /s/^  *//
                / $/s/  *$//
                s/\n/:/g
                / /s/  */:/g
                p
               }
             '
}

function path-list-name-exists-p { test -e "$1"; }
function path-list-name-file-p   { test -f "$1"; }
function path-list-name-dir-p    { test -d "$1"; }

function verify-dir-list
{
  path-list-colon path-list-verify path-list-name-dir-p ${1+"$@"}
}

function verify-file-list
{
  path-list-colon path-list-verify path-list-name-file-p ${1+"$@"}
}

function verify-first-file
{
  path-list-verify-first path-list-name-file-p ${1+"$@"}
}

function verify-first-dir
{
  path-list-verify-first path-list-name-dir-p ${1+"$@"}
}

function verify-first-file-in-list
{
  for e in ${1+"$@"}; do
    echo "$e"
  done | verify-first-file
}

function verify-first-dir-in-list
{
  for e in ${1+"$@"}; do
    echo "$e"
  done | verify-first-dir
}

provide path-list

# path-list.bash ends here
