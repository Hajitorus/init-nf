#!/bin/sh
# $Id: 10-S-ssh-agent.sh,v 1.2 2010/11/16 08:00:10 friedman Exp $

case ${SSH_AUTH_SOCK-notset} in
  notset )
    eval `ssh-agent`
    ssh_agent_started_by_xinitrc=t
  ;;
esac

# eof
