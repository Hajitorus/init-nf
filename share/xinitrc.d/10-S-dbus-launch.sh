#!/bin/sh
# $Id: 10-S-dbus-launch.sh,v 1.1 2010/02/18 10:13:55 friedman Exp $

case ${DBUS_SESSION_BUS_ADDRESS-notset} in
  notset ) eval `{ dbus-launch --sh-syntax --exit-with-session; } 2> /dev/null` ;;
esac

# eof
