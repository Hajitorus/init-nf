# require.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-07-08
# Public domain

# $Id: require.bash,v 1.4 2010/02/22 19:47:38 friedman Exp $

# Commentary:

# These functions provide an interface inspired by Emacs for loading
# libraries when they are needed and eliminating redundant loading.  The
# basic idea is that each "package" (or set of routines, even if it is only
# one function) registers itself with a symbol that marks a "feature" as
# being "provided".  If later you "require" a given feature, you save
# yourself the trouble of explicitly loading it again.
#
# At the bottom of each package, put a "provide foobar", so when another
# package has a "require foobar", it gets loaded and registered as a
# "feature" that won't need to get loaded again.  (See warning below for
# reasons why provide should be put at the end.)
#
# The list of provided features are kept in the `BASH_FEATURES' variable,
# which is not exported.  Care should be taken not to munge this in the
# shell.  The search path comes from a colon-separated `BASH_LOAD_PATH'
# variable.  It has no default value and must be set by the user.
#
# Require uses `bash_load_path_search', which works by scanning all of
# BASH_LOAD_PATH for a file named the same as the required symbol but with
# a `.bash' appended to the name.  If that is found, it is loaded.  If it
# is not, BASH_LOAD_PATH is searched again for a file name the same as the
# feature (i.e. without any extension).  bash_load_path_search may be
# useful for doing library filename lookups in other functions (such as a
# `load' or `autoload' function).
#
# Warning: Because require ultimately uses the builtin `source' command to
# read in files, it has no way of undoing the commands contained in the
# file if there is an error or if no provide statement appeared (this
# differs from the Emacs implementation, which normally undoes most of the
# forms that were loaded if the require fails).  Therefore, to minize the
# number of problems caused by requiring a faulty package (such as syntax
# errors in the source file) it is better to put the provide at the end of
# the file, rather than at the beginning.

# Code:

# Do not exporting this variable by default since functions are not
# exported by default.
export -n BASH_FEATURES

#:docstring :
# Null function.  Provided only so that one can put page breaks in source
# files without any ill effects.
#:end docstring:
#
# (\\014 == C-l)
() { :; }

#:docstring featurep:
# Usage: featurep argument
#
# Returns 0 (true) if argument is a provided feature.
# Returns 1 (false) otherwise.
#:end docstring:

###;;;autoload
featurep()
{
  case " $BASH_FEATURES " in
    *" $1 "* ) return 0 ;;
  esac
  return 1
}

#:docstring provide:
# Usage: provide symbol ...
#
# Register a list of symbols as provided features
#:end docstring:

###;;;autoload
provide()
{
  local feature

  for feature in "$@" ; do
    if ! featurep $feature ; then
      BASH_FEATURES="$BASH_FEATURES $feature"
    fi
  done
  return 0
}

#:docstring require:
# Usage: require feature {file}
#
# Load FEATURE if it is not already provided.  Note that require does not
# call `provide' to register features.  The loaded file must do that
# itself.  If the package does not explicitly do a `provide' after being
# loaded, require will complain about the feature not being provided on
# stderr.
#
# Optional argument FILE means to try to load FEATURE from FILE.  If no
# file argument is given, require searches through BASH_LOAD_PATH (see
# bash_load_path_search) for the appropriate file.
#
# If the variable REQUIRE_FAILURE_FATAL is set, require will cause the
# current shell invocation to exit, rather than merely return.  This may be
# useful for a shell script that vitally depends on a package.
#
#:end docstring:

###;;;autoload
require()
{
  local feature=$1
  local path=$2
  local file

  if ! featurep $feature ; then
    file=$(bash_load_path_search $feature ${path:+"$path"})
    if [ -z "$file" ]; then
      echo "require: $feature: module not found" 1>&2
    else
      . "$file"
    fi

    if ! featurep $feature ; then
      echo "require: $feature: feature was not provided." 1>&2
      case ${REQUIRE_FAILURE_FATAL:+t} in
        t ) exit   1 ;;
        * ) return 1 ;;
      esac
    fi
  fi
  return 0
}

#:docstring bash_load_path_search:
# Usage: bash_load_path_search filename {path ...}
#
# Search $BASH_LOAD_PATH for `filename' or, if `path' (a list) is
# specified, search those directories instead of $BASH_LOAD_PATH.  First
# the path is searched for an occurrence of `filename.bash, then a second
# search is made for just `filename'.
#:end docstring:

###;;;autoload
bash_load_path_search()
{
  local path
  local file
  local dir

  for file in "$1.bash" "$1" ; do
    path=${2-$BASH_LOAD_PATH}
    while :; do
      dir=${path%%:*}
      path=${path#*:}

      if [ -f "${dir:-.}/$file" ]; then
        echo "${dir:-.}/$file"
        return 0
      fi
      case $dir in $path ) break ;; esac
    done
  done

  return 1
}

provide require

# require.bash ends here
