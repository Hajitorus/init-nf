# type_pred.bash --- misc type predicates for symbols
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-06-04
# Last modified: 1993-01-26
# Public domain

# Commentary:
# Code:

#:docstring alphabeticp:
# Usage: alphabeticp EXPR
#
# Returns zero exit status if EXPR consists entirely of alphabetic
# characters (either upper or lower case), nonzero otherwise.
#:end docstring:

###;;;autoload
function alphabeticp ()
{
    case "${1}" in
       "" | *[!a-zA-Z]* )  
          return 1
         ;;
    esac

    return 0
}

#:docstring alphanumericp:
# Usage: alphanumericp EXPR
#
# Returns zero exit status if EXPR consists entirely of alphabetic
# characters (either upper or lower case) and/or numeric characters.
# Otherwise function returns nonzero exit status.
#:end docstring:

###;;;autoload
function alphanumericp ()
{
    case "${1}" in
       "" | *[!a-zA-Z0-9]* )  
          return 1
         ;;
    esac

    return 0
}

#:docstring lowercasep:
# Usage: lowercasep STRING
#
# Returns zero exit status if STRING consists entirely of lower case
# alphabetic characters, nonzero otherwise.
#:end docstring:

###;;;autoload
function lowercasep ()
{
    case "${1}" in
       "" | *[!a-z]* )  
          return 1
         ;;
    esac

    return 0
}

#:docstring numericp:
# Usage: numericp EXPR
#
# Returns zero exit status if EXPR consists entirely of numeric characters,
# nonzero otherwise.
#:end docstring:

###;;;autoload
function numericp ()
{
    case "${1}" in
       "" | *[!0-9]* )  
          return 1
         ;;
       *)
    esac

    return 0 
}

#:docstring punctuationp:
# Usage: punctuationp STRING
#
# Returns zero exit status if STRING consists entirely of punctuation
# characters---that is, any characters in the set
#
#        []{}()<>!*~@#%^=_+-*$&!\`\|;:'\",./?
#
# A nonzero exit status is returned if any other characters appear in
# STRING. 
#:end docstring:

###;;;autoload
function punctuationp ()
{
    case "${1}" in
       *"[!]~@#%^=_+-{}()*$&!\`\|;:'\",.<>/?[]"* )  
          return 1
         ;;
    esac

    return 0 
}

#:docstring uppercasep:
# Usage: uppercasep STRING
#
# Returns zero exit status if STRING consists entirely of upper case
# alphabetic characters, nonzero otherwise.
#:end docstring:

###;;;autoload
function uppercasep ()
{
    case "${1}" in
       "" | *[!A-Z]* )  
          return 1
         ;;
    esac

    return 0
}

#:docstring whitespacep:
# Usage: whitespacep STRING
#
# Returns zero exit status if STRING consists entirely of whitespace
# characters.  Whitespace is defined to be spaces, tabs, newlines, or BEL
# characters (C-g).  A nonzero exit status is returned if STRING contains
# any other characters.
#:end docstring:

###;;;autoload
function whitespacep ()
{
   case "${1}" in
      *"[! 	]"* )
         return 1
        ;;
   esac

   return 0
}

provide type_pred

# type_pred.bash ends here
