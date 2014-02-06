# logout.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-07-23
# Public domain

# $Id: logout.bash,v 1.2 2002/08/09 11:54:07 friedman Exp $

# Commentary:
# Code:

source_local_bash_init_file logout

if [ -n "${LOGFILE+set}" ]; then
   require recordlog && recordlog out
fi

if consolep && [ ${SHLVL} -eq 1 ]; then
   clear
fi

# logout.bash ends here
