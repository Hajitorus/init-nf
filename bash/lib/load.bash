# load.bash --- load or autoload bash library files
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-05-09
# Public domain

# $Id: load.bash,v 1.2 2004/02/15 02:22:08 friedman Exp $

# Commentary:

# This package uses the `bash_load_path_search' function defined in the
# `requre' package to do searches through BASH_LOAD_PATH.

# Code:

require fsubr

#:docstring autoload:
# Usage: autoload function pathname
#
# Declare a function that does not yet have a definition.  The definition
# is loaded from a file the first time the function is run.
#
# When the function actually needs to be loaded, the environment variable
# `BASH_LOAD_PATH' is searched as a pathlist for a file of the same name as
# the autoloaded function.  First, the file name with a \`.bash' suffix is
# appended and searched for through all of BASH_LOAD_PATH, then the file
# name itself is tried if it hasn't been found already.  The file is then
# loaded and the function is executed.
#
# Note: if 2nd (optional) argument to autoload is given, then autoload will
# expect to be able to load the definition of the function from that file
# (with or without a `.bash' suffix).  For more details consult the
# docstring for bash_load_path_search.
#:end docstring:

###;;;autoload
function autoload ()
{
  local func
  local path
  local force=

  if [ $# -eq 0 ]; then
    echo "Usage: autoload {-f} [function] {filename}" 1>&2
    return 1
  fi

  if [ "z$1" = "z-f" ]; then
    force=t
    shift
  fi

  func=$1
  path=$2

  if [ -z "$force" ] && fboundp "$func" ; then
    #echo "autoload: function \`${func}' is already defined." 1>&2
    return 1
  fi

  eval '
    function '$func' ()
    {
      _autoload_internal '$func' "'${path:+"$path"}'" && '$func' "$@"
    }'
}

# We could put all this in the autoload definition for the function, but
# then we'd have to maintain separate copies of all the stuff.
function _autoload_internal ()
{
  local func=$1
  local path=$2
  local file
  local old_definition=$(builtin declare -f $func)
  local new_definition

  if ! file=$(bash_load_path_search $func ${path:+"$path"}) || ! . "$file" ;
  then
    echo "autoload: $func: could not load definition" 1>&2
    return 1
  fi

  # On the offhand chance that the file failed to define the function (or
  # defined another identical autoload) we don't want to recur forever.
  # This isn't wonderful because the new definition for func might be
  # large, but in that case the function is likely to be reasonably slow
  # also, and so an extra second for this check won't make things
  # substantially worse (this *is* just shell script, after all).
  new_definition=$(builtin declare -f $func)
  if [ "$old_definition" = "$new_definition" ]; then
    echo "autoload: $func: $file did not define function" 1>&2
    return 1
  fi

  return 0
}

#:docstring load:
# Usage: load [library]
#
# Load (source) contents of a bash LIBRARY, searching "BASH_LOAD_PATH" for
# the file (see bash_load_path_search).
#:end docstring:

###;;;autoload
function load ()
{
  local arg
  local name
  local basename
  local failurep=0
  local file

  if [ $# -eq 0 ]; then
    echo "Usage: load [file]" 1>&2
    return 1
  fi

  for arg in "$@" ; do
    name=$arg
    basename=${arg##*/}

    if [ "$name" = "$basename" ]; then
      name=
    fi

    if ! file=$(bash_load_path_search "$basename" ${name:+"$name"}) || ! . "$file" ; then
      echo "load: ${arg}: could not load file" 1>&2
      failurep=1
    fi
  done

  return $failurep
}

provide load

# load.bash ends here
