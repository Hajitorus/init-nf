#!/bin/sh
# $Id: 30-init-01-std.sh,v 1.5 2012/01/11 09:19:52 friedman Exp $

with -b xsetroot -solid black
with -b xunclutter -visible
with -b fancy-xearth -once
with -b reset-xkb -q

yres=`xrdb-symbol-value HEIGHT`
if [ $yres -gt 1050 ]; then
  with -b xterm -geometry 80x66-1-1 -ut -font terminus8x14u
elif [ $yres -gt 768 ]; then
  with -b xterm -geometry 80x66-1-1 -ut
elif [ $yres -eq 768 ]; then
  with -b xterm -geometry 80x55-1-1 -ut
else
  with -b xterm -geometry 80x42-1-1 -ut
fi

# eof
