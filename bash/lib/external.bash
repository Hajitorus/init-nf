# external.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1991-12-07
# Last modified: 1993-09-29
# Public domain

# Commentary:
# Code:

require builtinp

#:docstring external:
# Usage: external ...
#
# To avoid using a function in bash, you can use the `builtin' or `command'
# builtins, but neither guarantees that you use an external program instead
# of a bash builtin if there's a builtin by that name.  So this function
# can be used like `command' except that it guarantees the program is
# external by first disabling any builtin by that name.  After the command
# is done executing, the state of the builtin is restored.
#:end docstring:

###;;;autoload
function external ()
{
 local state=""
 local exit_status
  
    if builtinp "$1"; then
       state="builtin"
       enable -n "$1"
    fi

    command "$@"
    exit_status=$?

    if [ "$state" = "builtin" ]; then
       enable "$1"
    fi

    return ${exit_status}
}

provide external

# external.bash ends here
