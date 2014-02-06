# which.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1991-12-11
# Last modified: 1993-02-03
# Public domain

# Commentary:
# Code:

#:docstring which:
# Usage: which PROG
#
# Like `where', but prints only first occurence. 
#:end docstring:

###;;;autoload
function which () 
{ 
 local path

    set -- $(builtin type -all -path "$1")

    for path in "$@" ; do
       if [ -x "${path}" -a -f "${path}" ]; then
          echo "${path}"
          return 0
       fi
    done

    return 1
}

#:docstring where:
# Usage: where PROG
#
# Print the paths of the all occurances of PROG as an executable program
# (i.e. not a directory, character/block special device, etc) in PATH.
# Returns 0 if at least one program is found, 1 otherwise.
#:end docstring:

###;;;autoload
function where () 
{ 
 local path
 local failure_p=1;

    set -- $(builtin type -all -path "$1")

    for path in "$@" ; do
       if [ -x "${path}" -a -f "${path}" ]; then
          echo "${path}"
          failure_p=0;
       fi
    done

    return ${failure_p}
}

provide which

# which.bash ends here
