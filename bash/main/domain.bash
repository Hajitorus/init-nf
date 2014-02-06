# domain.bash --- load domain-specific settings
# Author: Noah Friedman <friedman@splode.com>
# Created: 2002-08-08
# Public domain

# $Id: domain.bash,v 1.10 2006/07/12 23:42:13 friedman Exp $

# Commentary:
# Code:

function domain_init_file_list ()
{
  # The apparently no-op sequence 'x;x' is to avoid a bug in FreeBSD 4.4's
  # sed, where an append to an uninitialized hold space results in no
  # newline separator.  The exchange forces the init of the hold space to
  # an empty string so that the newline appears.
  # FreeBSD 4.10-RC3 and later don't seem to have this bug.
  echo $1 | sed -n -e 'x;x;H;g' \
                   -e 's/$/./' \
                   -e :l1 \
                   -e 's/^\(.*\)\(\n\)\([^.]*\)\./\3.\1\2/' \
                   -e 't l1' \
                   -e 's/\.\n//p' \
                   -e :l2 \
                   -e 's/\.[^.]*$//p' \
                   -e 't l2'
}

function source_domain_init_file ()
{
  local file=$(bash_load_path_search "domain/site-init/$1")
  test -z "$file" && return 1
  verbose_startup "domain/site-init/$1"
  source "$file"
  verbose_startup
  return 0
}

for domain in $(IFS=:; echo ${SINITDOMAIN:-$HOSTNAME}); do
  for sub in $(domain_init_file_list $domain) ; do
    source_domain_init_file "$sub" && break
  done
done
unset sub domain

# domain.bash ends here
