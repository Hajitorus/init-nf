#!/bin/sh
# $Id: 20-thinkpad-buttons.sh,v 1.5 2012/01/11 09:19:51 friedman Exp $

if [ -d /sys/devices/platform/thinkpad_acpi ] &&
   ! pidof -x ntpb > /dev/null
then
  read model < /sys/class/dmi/id/product_version

  case $model in
    *W70*  ) :             ;;  # not needed on W701ds
    *X220* ) :             ;;  # nor on X220 tablet
    *      ) ntpb --daemon ;;
  esac
fi

# eof
