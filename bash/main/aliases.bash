# aliases.bash --- define standard aliases
# Author: Noah Friedman <friedman@splode.com>
# Created: 1992-01-15
# Public domain

# $Id: aliases.bash,v 1.6 2004/02/14 21:28:31 friedman Exp $

# Commentary:
# Code:

alias cont='kill -CONT'
alias stop='kill -STOP'
alias ZZ='suspend'

alias j='jobs -l'
alias rehash='hash -r'
alias unexport='export -n'

source_local_bash_init_file aliases

# aliases.bash ends here
