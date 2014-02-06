# vars.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-07-23
# Public domain

# $Id: vars.bash,v 1.10 2010/02/18 11:04:11 friedman Exp $

# Commentary:

# The distinction between exported and unexported variables is not
# fine enough to really warrant a separate file.  Perhaps this should be
# merged with env.bash

# Code:

# Disable history substitution (it's annoying to have to escape `!' in
# filenames, etc), and enable or disable other features.
set \
    +o allexport \
    -o braceexpand \
    +o histexpand \
    -o ignoreeof \
    -o monitor \
    -o notify

# Disable interactive comments (posix me harder!)
# This is listed separately because bash 1.12 and earlier didn't have this
# option.
case "$BASH_VERSION" in
   0.* | 1.[0-9].* | 1.1[0-2].* ) : ;;
   * ) set +o interactive-comments ;;
esac

# Export CDPATH for emacs.
#defenv_cmd CDPATH  path-list verify-dir-list "$sinit/share/paths/cdpath"
#defenv_cmd FIGNORE path-list path-list-colon path-list "$sinit/share/paths/fignore"

defvar FCEDIT ed

export HISTFILE=/dev/null
HISTSIZE=4096

# New in bash 3.0
HISTTIMEFORMAT='%m-%d.%H:%M '

# No notification of new mail will occur if we appear to be running under
# emacs or we appear to be in an X client of any sort, since other means of
# mail notification exist.
# 2010-02-18 for now disable this entirely.
if : || [ -n "$DISPLAY" -o "$TERM" = emacs -o "$TERM" = emacs-virtual ]; then
  unset MAIL MAILCHECK MAILPATH MAIL_WARNING
else
  MAILCHECK=60

  case "${MAIL+set}" in
    set ) : ;;
    * )
      for d in /com/mail /var/mail /var/spool/mail \
               /usr/mail /usr/spool/mail
      do
        if [ -d "$d" ]; then
          export MAIL="$d/$USER"
          break
        fi
      done
     ;;
  esac

  case "${MAIL:+set}" in
    set )
      MAILPATH="${MAIL}?You have new mail."
      MAIL_WARNING=
     ;;
  esac
fi

PS1='\$ '
PS2='> '
PS4='+ '

# This variable is used by the `time' keyword in bash.
# The $'' syntax isn't recognized in very old versions but it's harmless
# there, since this variable isn't used for the `time' keyword in those
# versions anyway.
#TIMEFORMAT=$'[real=%3lR\tuser=%3lU\tsys=%3lS\tcpu=%P%%]'
TIMEFORMAT=$'\nreal\t%3lR\nuser\t%3lU\nsys\t%3lS\ncpu\t%P%%'

#allow_null_glob_expansion=
#auto_resume=
#cdable_vars=
command_oriented_history=
#defvar histchars "!^#"
defvar history_control ignoredups
defvar hostname_completion_file $sinit/share/hosts
#glob_dot_filenames=
no_exit_on_failed_exec=
#nolinks=
#NO_PROMPT_VARS=
#pushd_silent=   # Obsolesced in bash 1.13
#defvar TM 3600  # One hour timeout

# All shells complain about exiting by EOF.
ignoreeof=

# Some variables for common nonprintable characters.
# This makes the rest of the script files and aliases easier to read.
e=$(echo -e \\033)  # ESC
g=$(echo -e \\007)  # BEL
t=$(echo -e \\011)  # TAB

source_local_bash_init_file vars

# vars.bash ends here
