# env.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-07-23
# Public domain

# $Id: env.bash,v 1.33 2010/03/02 22:58:35 friedman Exp $

# Commentary:
# Code:

# First set a temporary path so we can set some needed variables.
PATH=$sinit/bin:$PATH
export PATH

# Useful to have set before initializing real path.
defenv_cmd SINIT_MACHTYPE nil hosttype
defvar OS "${SINIT_MACHTYPE##*-}"

case $HOSTNAME in
  *.* ) : ;;
  * )
    unset HOSTNAME
    defenv_cmd HOSTNAME nil hostname-fqdn
   ;;
esac

defvar DOMAINNAME "${HOSTNAME#*.}"
defvar HOSTNICK   "${HOSTNAME%%.*}"

# For use in setting FPATH by share/paths/fpath
export shellname=bash
paths=$sinit/share/paths

# Always reset.
set-path

require path-list

unset BASH_LOAD_PATH # reset from startup_simple
defenv_cmd  BASH_LOAD_PATH  path-list verify-dir-list   "$paths/load_path"

#defenv_cmd HOSTALIASES     path-list verify-first-file "$paths/hostaliases"
#defenv_cmd LD_LIBRARY_PATH path-list verify-dir-list   "$paths/ld_library_path"
#defenv_cmd LD_RUN_PATH     path-list verify-dir-list   "$paths/ld_run_path"
#defenv_cmd MAILCAPS        path-list verify-file-list  "$paths/mailcaps"

case ${un_s-`uname -s`}:${MANPATH+set} in
  Linux: )
    _mpath=`man -w 2> /dev/null` # get linux's computed path from /etc/man.config
    defenv_cmd  MANPATH     path-list verify-dir-list   "$paths/manpath"
    case $_mpath in
      '' ) : ;;
      * ) MANPATH=$MANPATH:$_mpath ;;
    esac
    unset _mpath ;;
  *: )
    defenv_cmd  MANPATH     path-list verify-dir-list   "$paths/manpath" ;;
esac

#defenv_cmd TEXFONTS        path-list verify-dir-list   "$paths/texfonts"
#defenv_cmd TEXFORMATS      path-list verify-dir-list   "$paths/texformats"
#defenv_cmd TEXINPUTS       path-list verify-dir-list   "$paths/texinputs"
#defenv_cmd TEXPOOL         path-list verify-dir-list   "$paths/texpool"

unset paths

defenv_cmd FROMRCPL     path-list verify-first-file-in-list \
                        ~/.fromrc.pl \
                        ~/etc/misc/.fromrc.pl \
                        "$sinit/share/fromrc.pl"

defenv_cmd INPUTRC      path-list verify-first-file-in-list \
                        ~/.inputrc \
                        ~/etc/misc/.inputrc \
                        "$sinit/share/inputrc"

#defenv_cmd MAILRC      path-list verify-first-file-in-list \
#                       ~/.mailrc \
#                       "$sinit/share/mailrc"

defenv_cmd NETRC        path-list verify-first-file-in-list \
                        ~/.netrc \
                        ~/etc/misc/.netrc

defenv_cmd GNUPGHOME    path-list verify-first-dir-in-list \
                        ~/.gnupg \
                        ~/etc/misc/.gnupg \
                        ~/etc/certs/gnupg

#defenv_cmd PGPPATH     path-list verify-first-dir-in-list \
#                       ~/.pgp \
#                       ~/etc/certs/pgp

defenv_cmd SCREENRC     path-list verify-first-file-in-list \
                        ~/.screenrc \
                        "$sinit/share/screenrc"

#defenv_cmd VMINCRC     path-list verify-first-file-in-list \
#                       ~/.vmincrc \
#                       ~/etc/misc/.vmincrc \
#                       "$sinit/share/vmincrc"

defenv_cmd XINITRC      path-list verify-first-file-in-list \
                        ~/.xinitrc \
                        ~/etc/misc/.xinitrc \
                        "$sinit/share/xinitrc"

defenv XAUTHORITY      $HOME/.Xauthority
defenv XDG_CONFIG_HOME $HOME/etc/misc/.config
defenv XCOMPOSEFILE    $sinit/share/xcompose

defenv EDITOR ed
defenv VISUAL "$EDITOR"

defenv P4CONFIG .p4
#defenv PILOTRATE 115200

# formatting variable used by GNU time
# Does anything else use this that might break badly?
defenv TIME '\nreal\t%e\nuser\t%U\nsys\t%S\ncpu\t%P'

# don't defenv this.  tty may change if running under emacs, for example.
TTY=$(tty 2> /dev/null)
export TTY

defenv VERSION_CONTROL numbered    # used by some GNU utils for making backups

# Set USER and LOGNAME environment variables. Since BSD and ATT can't agree
# on one symbol we make sure we have both.
defenv USER "${LOGNAME:-${USER:-$(logname 2> /dev/null || whoami 2> /dev/null)}}"
defenv LOGNAME "$USER"

# Set TERM if necessary.
case $EMACS in
  t ) TERM=emacs ;;
esac
case $TERM in
   '' | unknown | dialup | network | dumb | plugboard )
      if [ -n "${verbose_startup+set}" ]; then
         echo
      fi
      while : ; do
         echo -n "TERM = ($default_term) " 1>&2
         read TERM
         TERM="${TERM:-$default_term}"

         if ! get_terminfo ; then
           echo "warning: no terminfo entry for $TERM" 1>&2
         fi

         if get_termcap ; then
            break
         else
            echo "Unknown terminal type: $TERM" 1>&2
         fi
      done
      export TERM
      tset -Q < /dev/null
     ;;
   EMACS )
      TERM=emacs
      get_terminfo
      get_termcap
     ;;
   emacs | EMACS | emacs-virtual | xsession )
      get_terminfo
      get_termcap
     ;;
   *)
      get_terminfo
      get_termcap
      if [ "$TERM" = "aixterm" ]; then TERM=xterm; fi
      tset -Q < /dev/null
     ;;
esac

defenv_PAGER

for f in ~/.tzenv ~/etc/misc/.tzenv ; do
  if [ -f "$f" ]; then
    . "$f"
    break
  fi
done

source_local_bash_init_file env

# env.bash ends here
