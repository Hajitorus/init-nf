# paranoid-exit.bash --- avoid exiting shells accidentally.
# Author: Noah Friedman <friedman@splode.com>
# Created 1998-02-19
# Public domain

# $Id: paranoid-exit.bash,v 1.1 1998/02/19 14:42:03 friedman Exp $

# Commentary:
# Code:

function make-paranoid-exit-functions ()
{
  local name

  for name in ${1+"$@"}; do
    eval "function $name ()
          {
            echo \"Use \\\`builtin $name' if you really mean it.\" 1>&2
            return 1;
          }"
  done
}

make-paranoid-exit-functions bye exec exit logout

provide paranoid-exit

# paranoid-exit.bash ends here
