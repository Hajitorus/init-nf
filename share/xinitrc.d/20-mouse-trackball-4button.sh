#!/bin/sh
# $Id: 20-mouse-trackball-4button.sh,v 1.2 2011/07/29 00:46:11 friedman Exp $

case ${BASH_VERSION-notset} in
  notset ) exit 0 ;;
esac

if [ -e /dev/input/mouse.trackball-4button ]; then
  # Fix the button assignments on the usb logitech marblemouse.
  # See /etc/X11/xorg.conf for the gory details.
  n=`xmodmap -pp | sed -ne '/^There are \([0-9]*\) pointer.*/s//\1/p'`
  case $n in
    8 ) xmodmap -e 'pointer = 3 8 1 2 4 5 6 7' ;;
  esac
fi

# eof
