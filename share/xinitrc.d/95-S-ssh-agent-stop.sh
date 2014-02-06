#!/bin/sh
# $Id: 95-S-ssh-agent-stop.sh,v 1.1 2010/11/16 08:00:10 friedman Exp $

case $ssh_agent_started_by_xinitrc in
  t ) ssh-agent -k ;;
esac

# eof
