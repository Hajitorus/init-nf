# shell_time.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1991-11-13
# Last modified: 1993-09-29
# Public domain

# Commentary:
# Code:

#:docstring shell_time:
# A function of questionable use that returns the elapsed time since
# invocation of the current shell in days, hours, minutes, and seconds.
#:end docstring:

###;;;autoload
shell_time ()
{
 local seconds=${1:-SECONDS}
 local minutes
 local hours
 local days
 local output
 
    days=$[ seconds / 86400 ];
    seconds=$[ seconds - days * 86400 ];
    [ ${days} -gt 0 ] && output="${days}d"

    hours=$[   seconds / 3600 ];
    seconds=$[ seconds - hours * 3600 ];
    [ ${days} -gt 0 -o ${hours} -gt 0 ] \
       && output="${output} ${hours}h"

    minutes=$[ seconds / 60];
    [ ${days} -gt 0 -o ${hours} -gt 0 -o ${minutes} -gt 0 ] \
       && output="${output} ${minutes}m"

    seconds=$[ seconds - minutes * 60 ];
    output="${output} ${seconds}s"
    
    echo ${output}
}

provide shell_time

# shell_time.bash ends here
