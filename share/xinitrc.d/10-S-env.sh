#!/bin/sh
# $Id: 10-S-env.sh,v 1.1 2010/02/22 09:35:31 friedman Exp $

XAUTHORITY=$HOME/.Xauthority
export XAUTHORITY

XDG_CONFIG_HOME=$HOME/etc/misc/.config
export XDG_CONFIG_HOME

XCOMPOSEFILE=$sinit/share/xcompose
export XCOMPOSEFILE

# eof
