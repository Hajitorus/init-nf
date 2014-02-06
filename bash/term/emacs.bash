# emacs.bash
# Author: Noah Friedman <friedman@splode.com>
# Created: 1991-12-11
# Public domain

# $Id: emacs.bash,v 1.5 2004/11/24 00:17:49 friedman Exp $

# Commentary:
# Code:

# If we absolutely *must* invoke an editor under emacs.
EDITOR=ed
VISUAL=ed
export EDITOR VISUAL

PAGER=cat
export PAGER

# Obsolete.  See bash/lib/fileutils.bash
#function ls-page () { command ls ${1+"$@"} ; }

# emacs.bash ends here
