# repeat.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-05-13
# Public domain

# $Id: repeat.bash,v 1.2 1999/10/27 15:11:13 friedman Exp $

# Commentary:
# Code:

#:docstring repeat:
# Usage: repeat n command ...
#
# Repeat COMMAND n number of times.
#:end docstring:

# This function isn't hygienic, but there's too much overhead required to
# fix this.  Let's just hope people don't use variable names that will
# cause name conflicts with the local variables here.

###;;;autoload
function repeat ()
{
 local repeat_n="$1"
 local repeat_i=0;

    shift
    while [ ${repeat_i} -lt ${repeat_n} ]; do
       "$@"
       repeat_i=$[ repeat_i + 1 ]
    done
}

#:docstring repeat-eval:
# Usage: repeat-eval n command ...
#
# Repeat COMMAND n number of times.  COMMAND is `eval'ed.
#:end docstring:

###;;;autoload
function repeat-eval ()
{
 local repeat_n="$1"
 local repeat_i=0;

    shift
    while [ ${repeat_i} -lt ${repeat_n} ]; do
       eval "$@"
       repeat_i=$[ repeat_i + 1 ]
    done
}

provide repeat

# repeat.bash ends here
