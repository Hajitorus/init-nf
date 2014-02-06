# defvar.bash --- define variables according to certain criteria
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-09-24
# Last modified: 1993-01-27
# Public domain

# Commentary:
# Code:

#:docstring boundp:
# Usage: boundp [variable]
#
# Returns true if VARIABLE is presently set.  Returns false otherwise.
#:end docstring:

###;;;autoload
function boundp ()
{
    eval test \""\${${1}+set}"\" = "set"
}

#:docstring setq:
# Usage: setq VARIABLE1 VALUE2 ... VARIABLEn VALUEn
# 
# Binds the variable VARIABLEx to value VALUEx for each x
#:end docstring:

###;;;autoload
function setq ()
{
    while [ $# -gt 0 ] ; do
       eval "$1"=\'"$2"\'
       shift 2
    done
    return 0
}

#:docstring defvar:
# Usage: defvar variable value
#
# Set value of VARIABLE to VALUE only if VARIABLE is currently unset or
# set to a null value (if VARIABLE is null, unset it).
#:end docstring:

###;;;autoload
function defvar ()
{
 local variable="$1"
 local current_value 
 local new_value="$2"
 
    eval current_value="\${${variable}}"
    if [ -z "${current_value}" -a -n "${new_value}" ]; then
       export -n "${variable}"
       eval "${variable}"=\""${new_value}"\"
       current_value="${new_value}"
    fi

    if [ -z "${current_value}" ]; then
       unset "${variable}"
       return 1
    fi

    return 0
}

#:docstring defvar_cmd:
# Usage: defvar_cmd variable package {command {args ...}}
#
# Execute COMMAND and assign output to VARIABLE if variable is currently
# unset or set to a null value.  Use `require' to get definition of command
# from PACKAGE if it doesn't exist already.  If PACKAGE is "nil" or empty
# string, assume function (or command) exists.  Rest of args after $3 are
# arguments to command.
#:end docstring:

###;;;autoload
function defvar_cmd ()
{
 local variable="$1"
 local package="${2:-nil}"
 local cmd="$3"
 local current_value 

    shift 3
    eval current_value="\${${variable}}"
    if [ -z "${current_value}" ]; then
       if [ "${package}" = "nil" ] || require "${package}" ; then
          eval defvar "${variable}" \"$(${cmd} "$@")\" && export -n "${variable}"
          return $?
       else
	  unset "${variable}"
          return 1
       fi
    fi
    return 0
}

#:docstring defvar_declare:
# Usage: defvar_declare [declare params] [variable] [value]
#
# Like defvar, but allows one to modify some characteristics of the
# variable at the same time with the "declare" command (see documentation
# for "declare"). 
#:end docstring:

###;;;autoload
function defvar_declare ()
{
 local declare_args="$1"
 local variable="$2"
 local new_value="$3"
 local current_value 

    # This is going to make any variable declared local, so it won't get
    # set properly.  TODO: think of a workaround.
    declare +x ${declare_args} ${variable}
    eval current_value="\${${variable}}"
    if [ -z "${current_value}" ]; then
       eval export "${variable}"=\""${new_value}"\"
    fi
    return 0
}

# Variations which export variable after defining them. 

#:docstring defenv:
# Like defvar, but export variable (variable is not exported if it remains
# undefined).
#:end docstring:

###;;;autoload
function defenv () 
{
 local variable="$1"
 local value
 local status

    defvar "$@" && eval value=\""\${${variable}}"\"
    status=$?

    if [ ${status} -eq 0 -a -n "${value}" ]; then
       export "${variable}"
    fi
    
    return ${status}
}

#:docstring defenv_cmd:
# Like defvar_cmd, but export variable (variable is not exported if it
# remains undefined).
#:end docstring:

###;;;autoload
function defenv_cmd ()
{
 local variable="$1"
 local value
 local status

    defvar_cmd "$@" && eval value=\""\${${variable}}"\"
    status=$?

    if [ ${status} -eq 0 -a -n "${value}" ]; then
       export "${variable}"
    fi

    return ${status}
}

#:docstring defenv_declare:
# Like defvar_declare, but export variable (variable is not exported if it
# remains undefined).
#:end docstring:

###;;;autoload
function defenv_declare ()
{
 local declare_args="$1"

    shift
    defvar_declare "${declare_args} -x" "$@"
}

provide defvar

# defvar.bash ends here
