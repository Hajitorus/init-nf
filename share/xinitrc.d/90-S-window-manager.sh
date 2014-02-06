#!/bin/sh
# $Id: 90-S-window-manager.sh,v 1.3 2011/07/29 00:46:11 friedman Exp $

run_ctwm()
{
  if test -f $HOME/.ctwmrc ; then
    ctwm
  else
    ctwm -f $sinit/share/twmrc.m4
  fi
}

run_twm()
{
  twmrc=/tmp/twmrc$$

  {
    { echo -DHOME="$HOME"
      echo -DUSER="$USER"
      xrdb -symbols -screen
    } | sed -e 's/^-D/define(`/' \
            -e "s/=/',\`/" \
            -e "s/\$/')dnl/" \
            -e 's/"//g'

    cat $sinit/share/twmrc.m4
  } | ${M4-m4} > $twmrc

  twm -f $twmrc
  rm -f $twmrc
}

run_window_manager()
{
  SHLVL=-1

  while : ; do
    rm -f "$HOME/.wm-restart"

    if type -path ctwm > /dev/null; then
      run_ctwm
    else
      run_twm
    fi

    test -f "$HOME/.wm-restart" || break
  done
}

run_window_manager

# eof
