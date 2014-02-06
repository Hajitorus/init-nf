# stat.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-07-18
# Last modified: 1993-02-03
# Public domain

# Commentary:
# Code:

export -n st_{dev,ino,mode,nlink,uid,gid,rdev,size,atime,mtime,ctime,blksize,blocks}

# dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks

###;;;autoload
function _stat ()
{
    perl - "$@" <<- '__EOF__'
	for ($i = 0; $i <= $#ARGV; $i += 1)
	  {
	    @st = stat("$ARGV[$i]");
	    $st[2] = $st[2] & 0777;
	    printf ("%ld %u %o %d %lu %lu %ld %lu %lu %lu %lu %lu %lu\n", @st);
	  }
	__EOF__
}

###;;;autoload
function stat ()
{
 local file="$1"

    if [ $# -ne 1 ]; then
       return 1
    fi

    if [ ! -e "$file" ]; then
       echo -n "stat: " 1>&2
       < "$file"
       return 1
    fi

    stat_set $(_stat "$file")
}

###;;;autoload
function stat_set ()
{
    if [ $# -eq 1 ]; then
       set -- $1
    fi

    st_dev="$1"
    st_ino="$2"
    st_mode="$3"
    st_nlink="$4"
    st_uid="$5"
    st_gid="$6"
    st_rdev="$7"
    st_size="$8"
    st_atime="$9"
    st_mtime="$10"
    st_ctime="$11"
    st_blksize="$12"
    st_blocks="$13"
}

###;;;autoload
function stat_field ()
{
 local file="$1"
 local field="$2"

    if [ $# -ne 2 ]; then
       return 1
    fi

    if [ ! -e "$file" ]; then
       echo -n "stat: " 1>&2
       < "$file"
       return 1
    fi

    set -- $(_stat "$file")
    eval echo \""\${${field}}"\"
}

###;;;autoload
function stat_dev     () { stat_field "$1"  1; }

###;;;autoload
function stat_ino     () { stat_field "$1"  2; }

###;;;autoload
function stat_mode    () { stat_field "$1"  3; }

###;;;autoload
function stat_nlink   () { stat_field "$1"  4; }

###;;;autoload
function stat_uid     () { stat_field "$1"  5; }

###;;;autoload
function stat_gid     () { stat_field "$1"  6; }

###;;;autoload
function stat_rdev    () { stat_field "$1"  7; }

###;;;autoload
function stat_size    () { stat_field "$1"  8; }

###;;;autoload
function stat_atime   () { stat_field "$1"  9; }

###;;;autoload
function stat_mtime   () { stat_field "$1" 10; }

###;;;autoload
function stat_ctime   () { stat_field "$1" 11; }

###;;;autoload
function stat_blksize () { stat_field "$1" 12; }

###;;;autoload
function stat_blocks  () { stat_field "$1" 13; }

provide stat

# stat.bash ends here
