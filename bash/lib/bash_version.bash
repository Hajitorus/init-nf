# bash_version.bash --- get major and minor components of bash version number
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1993-01-26
# Last modified: 1993-01-26
# Public domain

# Commentary:
# Code:

#:docstring bash_version:
# Usage: bash_version {major|minor}
#
# Echo the major or minor number of this version of bash on stdout, or
# just echo $BASH_VERSION if no argument is given. 
#:end docstring:

###;;;autoload
function bash_version ()
{
 local major
 local minor

    case "$1" in 
       major )
          echo "${BASH_VERSION%%.*}"
         ;;
       minor )
          major="${BASH_VERSION%%.*}"
          minor="${BASH_VERSION#${major}.}"
          echo "${minor%%.*}"
         ;;
       * )
          echo "${BASH_VERSION}"
         ;;
    esac
}

provide bash_version

# bash_version.bash ends here
