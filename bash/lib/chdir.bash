# chdir.bash --- interactive cwd-manipulation frobs
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-01-15
# Last modified: 1993-09-29
# Public domain

# Commentary:

# Most of these have merely been made silent (except for stderr) so that
# skipping to a directory through CDPATH doesn't print the directory on
# stdout.
#
# Docstrings only given for commands that are not normally built-in

# Code:

#:docstring chdir:
# Equivalent to "cd"
#:end docstring:

###;;;autoload
function chdir ()
{
    builtin cd "$@" > /dev/null
}

###;;;autoload
function cd () 
{ 
    builtin cd "$@" > /dev/null 
}

#:docstring ..:
# Usage: .. [n] [dir]
#   Equivalent to "cd .." n times, then "cd ./dir"
#:end docstring:

###;;;autoload
function .. ()
{
 local n="${1:-1}"
 local oPWD="${PWD}"

    while [ ${n} -gt 0 ]; do
      cd ..
      n=$[ n - 1 ]
    done
    cd "${PWD}/${2}" || cd "${oPWD}"
}

###;;;autoload
function popd () 
{ 
    builtin popd  "$@" > /dev/null 
}

###;;;autoload
function pushd () 
{ 
    builtin pushd "$@" > /dev/null 
}

#:docstring [d:
# Equivalent to pushd.
#:end docstring:

###;;;autoload
function [d () 
{ 
    pushd "$@" 
}

#:docstring ]d:
# Equivalent to popd.
#:end docstring:

###;;;autoload
function ]d () 
{
    popd "$@"
}

#:docstring back:
# Changes current-working directory to the most recently visited one (other
# than the present one). 
#:end docstring:

###;;;autoload
function back ()
{
    cd "${OLDPWD}"
}

provide chdir

# chdir.bash ends here
