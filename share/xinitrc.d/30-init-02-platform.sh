#!/bin/sh
# $Id: 30-init-02-platform.sh,v 1.1 2010/02/18 08:45:56 friedman Exp $

case ${SINIT_MACHTYPE-`hosttype`} in
  x86*-*-linux-* ) : ;;
  * )
    xset b 0
  ;;
esac

# eof
