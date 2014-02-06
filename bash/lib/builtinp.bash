# builtinp.bash --- determine if a command is a builtin
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-06-06
# Last modified: 1993-02-03
# Public domain

# Commentary:

# This function is mainly for use by `external', but there are a few other
# useful applications (like determining whether `getopts' or `bind' are
# available in a particular version of bash).  What is does is tell you if
# a particular keyword is currently enabled as a shell builtin.  It does
# NOT tell you if invoking that keyword will necessarily run the builtin.
# For that, do something like
#
#       test "$(builtin type -type [keyword])" = "builtin"

# Code:

#:docstring builtinp:
# Usage: builtinp symbol
#
# Returns zero exit status if NAME is currently enabled as a shell builtin,
# nonzero otherwise. 
# 
# This function does not tell you if invoking that keyword will necessarily
# run the builtin.  Note that disabling a builtin with "enable -n" will
# make builtinp return false, since the builtin is no longer available.
#:end docstring:

###;;;autoload
function builtinp ()
{
    set -- $(builtin type -all -type "$1")

    case " $* " in
       *" builtin "* )
          return 0
         ;;
    esac

    return 1
}

#:docstring subrp:
# See builtinp
#:end docstring:

# An alternative name, but not really very desirable. 
###;;;autoload
function subrp ()
{
    return $(builtinp "$@")
}

provide builtinp

# builtinp.bash ends here
