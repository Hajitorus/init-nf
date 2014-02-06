# check_exit_status.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-01-15
# Last modified: 1993-09-29
# Public domain

# Commentary:
# Code:

#:docstring check_exit_status:
# If set to be run by the PROMPT_COMMAND variable, this function will check
# the exit status of the last executed command and report on its status if
# it was nonzero.  If the process exited with a signal, the name of the
# signal is reported also.
#:end docstring:

###;;;autoload
function check_exit_status () 
{ 
 local status="${exit_status:-$?}"
 local signal=""

   if [ ${status} -ne 0 ]; then
      if [ ${status} -gt 128 ]; then
         signal="$(builtin kill -l $[ status - 128 ] 2> /dev/null)"
         [ "${signal}" ] && signal=" ${signal}"
      fi

      echo "[Exit ${status}${signal}]" 1>&2
   fi

   return 0;
}

provide check_exit_status

# check_exit_status.bash ends here
