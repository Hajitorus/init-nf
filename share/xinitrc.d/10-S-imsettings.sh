#!/bin/sh
# $Id: 10-S-imsettings.sh,v 1.1 2010/02/22 09:35:31 friedman Exp $

xinputrc=/etc/X11/xinit/xinitrc.d/50-xinput.sh
if [ -f $xinputrc ] ; then
  . $xinputrc
fi

# eof
