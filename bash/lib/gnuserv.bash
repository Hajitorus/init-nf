# gnuserv.bash --- front end for `gnuserv' package
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-03-06
# Last modified: 1993-02-03
# Public domain

# Commentary:
# Code:

require expand_file_name

# Wrapper to run emacsclient or gnuclient, whichever is appropriate (or
# just run emacs)
function emacs ()
{
    if [ "${GNU_HOST}" ]; then
       if [ "${GNU_HOST}" = "${HOSTNAME}.${DOMAINNAME}" ]; then
         gnuclient "$@"
       else
         set -- $(emacs_parse_gnuclient_args "$@")
         gnuclient "$@"
       fi
    elif [ "${EDITOR##*/}" = "emacsclient" ]; then
       "${EDITOR}" "$@"
    else
       command emacs "$@"
    fi
}

# I'm not really happy with how this works but I want to preserve
# positional parameters very carefully. 
function emacs_parse_gnuclient_args ()
{
 local arg
 local kill_args=1
 local found_dash_dash=0
 local prefix="/$(whoami)@${HOSTNAME}.${DOMAINNAME}:"

    for arg in "$@" ; do
       if [ ${kill_args} -eq 1 ]; then
          kill_args=0
          shift $#
       fi
       case z${arg} in 
          z-* )
             set -- "$@" "${arg}"
            ;;
          * )
             set -- "$@" "${prefix}$(expand_file_name "${arg}")"
            ;;
       esac
    done
    echo "$@"
}

# Evaluate a lisp on a remote emacs
function elisp-eval ()
{
    gnudoit "$@"
}

provide gnuserv

# gnuserv.bash ends here
