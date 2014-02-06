#!/bin/sh
# $Id: 30-init-03-host.sh,v 1.18 2012/01/13 08:33:59 friedman Exp $

run_unique()
{
  prg=$1
  shift

  case `uname -s` in
    Linux )
      for pid in `pidof ${prg##*/}`; do
        cmd=`perl -pe 's=^[^\0]*/==; s=\0= =g; s=\s*$=\n=' /proc/$pid/cmdline`
        case $cmd in
          "${prg##*/}${1+ $*}" ) return ;;
        esac
      done ;;
  esac

  with -b "$prg" "$@"
}

host_commands()
{
  #utc_timestamp=20111201.060000               # displays sun in distance
  utc_timestamp=`strftime -f "%Y%m%d.060000"`  # 6am UTC

  case ${HOSTNAME-`hostname`} in

    obey-chomsky* | gedanken-donuts* ) # lenovo w701ds
      start-pulseaudio-x11

      # We set the virtual size to 1280x1280 in xorg.conf so that we can
      # rotate this screen between 1280x768 and 768x1280 without xrandr
      # complaining that we're trying to set a resolution larger than
      # allowed in any one direction.
      xrandr --screen 1 -s 1280x768
      xrandr --screen 1 --orientation left

      with -b -d ${DISPLAY%.[0-9]}.1 xterm -geometry 80x66-1-1 -ut -font terminus8x14u

      with -b -d ${DISPLAY%.[0-9]}.1 fancy-xearth -once -date "$utc_timestamp"
      with -b -d ${DISPLAY%.[0-9]}.0 fancy-xearth # update once per hour

      #DISPLAY=${DISPLAY%.[0-9]}.1 run_unique gkrellm
      run_unique gkrellm
    ;;

    unexploded-cow* )
      with -b vncconfig -nowin
    ;;

    doh | doh.* )
      xset m 3/2 10
      xset dpms 3600 3600 5400

      start-pulseaudio-x11
      run_unique gkrellm
      with -b xscreensaver

      with -b -d ${DISPLAY%.[0-9]}.1 fancy-xearth -once -date "$utc_timestamp"
      with -b -d ${DISPLAY%.[0-9]}.0 fancy-xearth # update once per hour
    ;;

    tabula-non-rasa* )
      xbacklight -set 60
      start-pulseaudio-x11
      run_unique gkrellm
    ;;

    * )
      start-pulseaudio-x11
      run_unique gkrellm
    ;;

  esac
}

host_commands

# eof
