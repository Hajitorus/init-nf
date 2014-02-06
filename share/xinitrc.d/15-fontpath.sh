#!/bin/sh
# $Id: 15-fontpath.sh,v 1.1 2010/02/18 08:45:55 friedman Exp $

xset fp
for d in bitmap Type1 TrueType; do
  if [ -d "$HOME/lib/fonts/$d" ]; then
    case $d in
      bitmap ) d=bitmap:unscaled ;;
    esac
    xset +fp "$HOME/lib/fonts/$d"
  fi
done
xset fp rehash

# eof
