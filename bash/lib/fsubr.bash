# fsubr.bash --- function-related predicates and other operations
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-05-24
# Last modified: 1993-09-29
# Public domain

# Commentary:
# Code:

#:docstring fsubr:
# Usage: fboundp symbol
#
# Returns true if SYMBOL is presently defined as a shell function.  
#
# Note that just because fboundp returns true doesn't mean invoking command
# will get you the function definition.  Aliases have precedence.
#:end docstring:

###;;;autoload
function fboundp ()
{
    # declare returns 1 if symbol is not defined as a function
    builtin declare -f "$1" > /dev/null 2>&1
}


#:docstring fset:
# Copies the definition of a function to that of another.  That is, 
# 
#        fset foo bar
#
# defines function foo to be the the same as the current definition of
# function bar (recursive calls are not resolved, however, so foo might
# still call bar if bar was recursive)
#:end docstring:

###;;;autoload
function fset ()
{
 local newname="$1"
 local oldname="$2"
 local definition 
    
    # symbol_function returns nonzero exit status if symbol isn't defined
    # as a function. 
    definition="$(symbol_function ${oldname})" \
    && eval "function ${newname} () { ${definition} }"
}


#:docstring symbol_function:
# Usage: symbol_function func
#
# Pretty-prints definition of FUNC without function name header or
# top-level opening and closing braces. 
#:end docstring:

###;;;autoload
function symbol_function ()
{
 local funcname="$1"

    if ! fboundp "${funcname}" ; then
       echo "symbol_function: ${funcname}: not a function" 1>&2
       return 1
    fi

    builtin declare -f "${funcname}" | sed '1,2d;$d'
}


#:docstring functions:
# Usage: functions symbol ...
#
# Pretty-print the function definitions of SYMBOLs in a form that is
# re-parseable by bash.
#:end docstring:

###;;;autoload
function functions ()
{
    if [ $# -eq 0 ]; then
       declare -f | sed 's/^declare -f //'
    else
       # `declare -f' doesn't get printed in front of function names if
       # they are listed explicitly, so no need for sed. 
       declare -f "$@"
    fi
}

provide fsubr

# fsubr.bash ends here
