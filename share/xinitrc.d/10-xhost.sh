#!/bin/sh
# $Id: 10-xhost.sh,v 1.4 2011/07/29 00:46:11 friedman Exp $

xhost +si:localuser:${LOGNAME-${USER-`id -un`}}
xhost +si:localuser:root

xhost | while read entry cruft; do
  case $entry in
    # Use IP addresses for these because sometimes they can't be resolved.
    INET:localhost*  ) xhost -INET:127.0.0.1 ;;
    INET6:localhost* ) xhost -INET6:::1      ;;

    INET:*  ) xhost -$entry ;;
    INET6:* ) xhost -$entry ;;
    LOCAL:* ) xhost -$entry ;;
  esac
done

xhost -

# eof
