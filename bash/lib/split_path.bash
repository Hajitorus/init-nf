# split_path.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-02-29
# Last modified: 1993-02-03
# Public domain

# Commentary:
# Code:

#:docstring split_path:
# A good generic function for splitting a PATH-style string into a list of
# directories or files, which can then be manipulated with `set'.  Typical
# use of this function goes like:
#
#        for dir in $(split_path ${PATH}) ; do 
#           ...
#
#:end docstring:

###;;;autoload
function split_path ()
{
 local p
 local IFS=':'

    set -- ${1}
    for p in "$@" ; do
       echo -n "${p:-.} "
    done
    echo 
}

provide split_path

# split_path.bash ends here
