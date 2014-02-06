# help.bash --- help system for runtime environment
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-06-18
# Last modified: 1993-02-03
# Public domain

# Commentary:

# TODO: Perhaps add a way of storing docstrings in variables so that future
# lookups in same session will be faster. 

# Code:

export -n DOCSTRING_FILE="${DOCSTRING_FILE:-${INIT}/lib/.docstrings}"

function docstring_print ()
{
 local docvar="$1"
 local docstring
 local docstring_dta="${DOCSTRING_FILE}.dta"
 local docstring_idx="${DOCSTRING_FILE}.idx"
 local address
 local defining_docstrings

    set -- $(sed -n "/^${docvar} /{
                     s/^${docvar}  *\([0-9][0-9]*\)  *\([0-9][0-9]*\)/\1 \2/p;
                     q;
                   }" "${docstring_idx}")

    if [ $# -ne 2 ]; then
       return 1
    fi

    # To work around GNU sed bug where 'addr,addr' loses if beginning and
    # ending addr are the same (whole file is printed).  This was probably
    # fixed in 1.09, but there's no point in asking for trouble. 
    if [ "$1" = "$2" ]; then
       address="$1"
    else
       address="$1,$2"
    fi

    set -- "$(sed -n "${address}p" ${docstring_dta})"

    if [ -z "$1" ]; then
       return 1
    fi

    echo "$1"
}

#:docstring help:
# Provide help for bash builtins or documented shell functions.
#:end docstring:

###;;;autoload
function help ()
{
 local func="$1"

   if [ $# -ne 0 ]; then
      case "${func}" in
         builtins )
            builtin help
           ;;
         * )
            if ! docstring_print "${func}" ; then
               builtin help "${func}"
            fi
          ;;
      esac
      return 
   fi
   echo 'To get help on shell builtins, try "help builtins".  For other functions,
try "help func" (where "func" is the name of the function).'
}

###;;;autoload
function mkdocstrings ()
{
  command mkdocstrings --docstrings-file="$DOCSTRING_FILE" --verbose -- $sinit/bash/lib/*.bash
}

provide help

# help.bash ends here
