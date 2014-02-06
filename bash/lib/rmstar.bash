# rmstar.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-07-15
# Last modified: 1993-03-01
# Public domain

# Commentary:
# Code:

require y_or_n_p 

#:docstring rmstar:
# If this is loaded AND the shell variable `rmstar' is set, then the shell
# will query the user for confirmation whenever a global wildcard is
# supplied to the `rm' command.  User can respond `y' to affirm deletion of
# this file, or `!' to stop asking and assume `y' for everything.  A
# response of `n' or `q' will abort without deleting any files. 
#
# Some people think this is a bad feature.  The reason why is that if you
# get too used to rm asking for confirmation before blowing things away,
# you may become careless about using it, and run "rm *" (or some equally
# disastrous command) someday when it's not idiot-proofed.
#
# The same philosophy applies to aliasing rm to `rm -i'. 
# 
#:end docstring:

alias rm='_rmstar_flags="$-"; set -f; rmstar "${_rmstar_flags}"'

###;;;autoload
function rmstar ()
{
 local flags="$1"
 local arg
 local t="$(echo -e \\011)"  # TAB
 local s=' '                 # SPACE
 local n='
'
 local IFS="${s}${t}${n}"

    shift;
   
    # Only re-enable globbing if it was enabled before.  This is done here
    # even though globbing will be temporarily disabled again later because
    # function might abort prematurely for some reason, and this needs to
    # be preserved.  (There is still a window of time in which a signal
    # might cause the function to abort and globbing will be screwed up,
    # but defining a signal handler to restore this in a transparent way is
    # massively more complicated (and slower).
    case "${flags}" in
       *f* ) : ;;
       * ) set +f ;;
    esac

    if [ "${rmstar+set}" = 'set' ]; then
       for arg in "$@" ; do
          # We really want to use a case statement here so we can do
          # pattern matching on arbitrary things.  That is, do
          #
          #    case "${arg}" in
          #     '*' | *'/*' | *'*'* )
          #     ...
          #
          # However, this is broken in bash 1.12 (which globs everything,
          # even quoted asterisks).  It's fixed in bash 1.13, but that
          # hasn't been released as of 01-Mar-93.
          if [ "z${arg##*/}" = 'z*' ]; then
             dir="${arg%/*}";
             if [ "${dir}" = "${arg}" ]; then
                dir='current directory'
             fi

             y_or_n_p "Remove all files in ${dir} (y/n/"\!"/q)? "
             case $? in
                0 ) continue ;;  # y
                2 ) break ;;     # !
                1 | 3 )          # n or q
                   echo 'rmstar: aborted -- no files deleted.' 1>&2
                   return 1
                  ;;
             esac
          fi
       done
   fi

   # This tries to preserve quoting, etc.  It's not perfect.  If an
   # argument contained globbing characters in quotes, the glob will still
   # get expanded.  There's really nothing that can be done about this
   # because the information about what was in quotes and what wasn't has
   # been lost.  The characters `*' ad `?', and characters in brackets ([])
   # will always cause an expansion to occur.  Unless you regularly use
   # weird file names, these are pathological cases anyway.  You usually
   # want them to be globbed.
   #
   # This *only* affects arguments passed explicitly.  If a glob expands to
   # some perverse file name, that file will be handled correctly.
   set -f
   IFS="${n}" set -- $(for arg in "$@"; do
                          echo "${arg}"
                       done | sed -e 's/\(['"${s}${t}"'${}()]\)/\\\1/g')
   case "${flags}" in
      *f* ) : ;;
      * ) set +f ;;
   esac
   eval command rm "$@"
}

provide rmstar

# rmstar.bash ends here
