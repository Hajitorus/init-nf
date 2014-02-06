# dir_empty_p.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-01-13
# Last modified: 1993-09-29
# Public domain

# Commentary:
# Code:

#:docstring dir_empty_p:
# Usage: dir_empty_p directory 
#
# Returns 0 (true) if DIRECTORY is empty (i.e. has no files in it), 1 if
# DIRECTORY is not empty, or 2 if DIRECTORY was not accessible.
#:end docstring:

###;;;autoload
function dir_empty_p ()
{
 local dir="${1}"
 local files
    
    # This doesn't win if you can cd to the directory but can't read it
    # (e.g. the directory is mode 111)
    files=$(set +f; 
            glob_dot_filenames=t; 
            cd "${dir}" 2> /dev/null || exit 2; 
            echo *; 
           )

    # If you can't even cd to the directory I guess it's an unusual error
    # condition.
    test $? -ne 0 && return 2

    if [ "${files}" = ". .." ]; then
       return 0
    fi

    return 1
}

provide dir_empty_p

# dir_empty_p.bash ends here
