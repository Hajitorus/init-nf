# logname.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-01-15
# Last modified: 1993-09-29
# Public domain

# Commentary:
# Code:

#:docstring logname:
# Reports login name, or if that fails, the user name associated with the
# effective user id. 
#:end docstring:

###;;;autoload
function logname () 
{ 
 local uname;

    uname="$(command logname 2> /dev/null)"
    echo "${uname:-$(command whoami)}"
}

provide logname

# logname.bash ends here
