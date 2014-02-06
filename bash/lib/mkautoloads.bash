# mkautoloads.bash --- bash front end for mkautoloads script
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1993-09-26
# last modified: 1993-09-29
# Public domain

# Commentary:
# Code:

mkautoloads_file_name="$sinit/bash/lib/.autoloads.bash"

#:docstring mkautoloads:
# Usage: mkautoloads
#
# Front end for the `mkautoloads' shell script.  This function calls the
# shell script with the appropriate arguments for bash.
#:end docstring:

###;;;autoload
function mkautoloads ()
{
  command mkautoloads --autoload-file="$mkautoloads_file_name" --verbose -- $sinit/bash/lib/*.bash
}

provide mkautoloads

# mkautoloads.bash ends here
