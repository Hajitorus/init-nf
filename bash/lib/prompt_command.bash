# prompt_command.bash --- interface for using PROMPT_COMMAND feature
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-06-06
# Public domain

# $Id: prompt_command.bash,v 1.2 2008/05/02 19:41:00 friedman Exp $

# Commentary:
# Code:

PROMPT_COMMAND=do_prompt_command

#:docstring do_prompt_command:
# Perform a series of commands before prompt is displayed in interactive
# sessions.  This function provides `exit_status' as a means of accessing
# the exit status from the last command.
#
# You should not attempt to change the value of PROMPT_COMMAND directly.
# Use the "add_prompt_command" and "remove_prompt_command" interfaces.
#:end docstring:

###;;;autoload
function do_prompt_command()
{
  local exit_status=$?
  local cmd

  for cmd in $PROMPT_COMMAND_list ; do
    $cmd
  done
  return 0
}

#:docstring add_prompt_command:
# Usage: add_prompt_command cmd
#
# Provides a convenient interface for adding new commands to
# "do_prompt_command".  Commands called by add_prompt_command should be
# shell functions---anything else is likely to break this implementation.
#:end docstring:

###;;;autoload
function add_prompt_command ()
{
  local cmd

  for cmd in "$@"; do
    case " $PROMPT_COMMAND_list " in
      *" $cmd "* ) continue ;;
    esac

    PROMPT_COMMAND_list="$PROMPT_COMMAND_list $cmd"
  done
}

#:docstring remove_prompt_command:
# Usage: remove_prompt_command cmd
#
# Provides a convenient interface for removing commands from
# "do_prompt_command" (see comments for "add_prompt_command").
#:end docstring:

###;;;autoload
function remove_prompt_command()
{
  local arg
  local cmd
  local old

  for arg in "$@"; do
    old=$PROMPT_COMMAND_list
    PROMPT_COMMAND_list=

    for cmd in $old ; do
      case $cmd in
        $arg ) : ;;
        *    ) PROMPT_COMMAND_list="$PROMPT_COMMAND_list $cmd" ;;
      esac
    done
  done
}

provide prompt_command

# prompt_command.bash ends here
