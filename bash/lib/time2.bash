# time2.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1992-07-19
# Last modified: 1993-02-03
# Public domain

# Commentary:
# Code:

export -n _time_seconds

#:docstring time2:
# time2 emulates the "time" system call.  It echoes the current number of
# seconds since the Epoch (Jan 1, 1970 00:00:00 GMT)
#
# WARNING: In order to optimize computations after the first call to this
# function, the `SECONDS' shell variable is used to compute changes from
# the previous value.  If this variable is ever assigned (or unset), time2
# will no longer provide the correct value.
#:end docstring:

###;;;autoload
function time2 ()
{
    # Calling an external program is slow
    if [ "${_time_seconds+set}" != "set" ]; then
       _time_seconds=$(perl -e 'print(time());')
       _time_seconds=$[ _time_seconds - SECONDS ]
    fi

    echo $[ _time_seconds + SECONDS ]
}

#:docstring ctime:
# Usage: ctime {seconds}
#
# This function emulates the "ctime" library routine, formatting according
# to the same conventions.  If the optional argument `seconds' is not
# provided, ctime assumes the current time.  See docstring for `time2' for
# more information about the `seconds' parameter.
#:end docstring:

###;;;autoload
function ctime ()
{
 perl - ${1} << '__EOF__'
    @weekday = ("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat");
    @month = ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

    $time = (length ($ARGV[0]) > 0)? $ARGV[0] : time() ;

    # Determine what time zone is in effect.
    # Use GMT if TZ is defined as null, local time if TZ undefined.
    # There's no portable way to find the system default timezone.
    $TZ = defined($ENV{'TZ'}) ? ( $ENV{'TZ'} ? $ENV{'TZ'} : 'GMT' ) : '';

    ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) =
        ($TZ eq 'GMT') ? gmtime($time) : localtime($time);

    # Hack to deal with 'PST8PDT' format of TZ
    # Note that this can't deal with all the esoteric forms, but it
    # does recognize the most common: [:]STDoff[DST[off][,rule]]

    if ($TZ=~/^([^:\d+\-,]{3,})([+-]?\d{1,2}(:\d{1,2}){0,2})([^\d+\-,]{3,})?/)
      {
        $TZ = $isdst ? $4 : $1;
      }

    if ($TZ ne "") { $TZ .= " "; }

    $year += ($year < 70) ? 2000 : 1900;

    printf ("%s %s %2d %02d:%02d:%02d %s%4d\n",
            $weekday[$wday], $month[$mon], $mday, $hour, $min, $sec, 
            $TZ, $year);
__EOF__
}

#:docstring timefmt_to_seconds:
# Compute the number of seconds specified by a string of the form
#    ...y...M...w...d...h...m...s
# For years, months, weeks, days, hours, minutes, and seconds.  Any given
# unit is optional.  
#
# A year is considered to be 365 days.
# A month is considered to be 30 days.
# A week is considered to be 7 days. 
# A day is considered to be 24 hours.
#:end docstring:

###;;;autoload
function timefmt_to_seconds ()
{
 local arg="$*"
 local val
 local unit
 local total
 
    if [ $# -eq 0 ]; then
       read arg
    fi

    shift $#
    set -- $(echo ${arg} | sed 's/\([0-9]*\)\([yMwdhms]\)/\1 \2 /g')

    while [ $# -gt 0 ]; do
       val="${1}"
       unit="${2}"

       case "${unit}" in
          y ) multiple=31536000 ;; # seconds per year (365 days)
          M ) multiple=2592000  ;; # seconds per month (30 days)
          w ) multiple=604800   ;; # seconds per week (7 days)
          d ) multiple=86400    ;; # seconds per day (24 hours)
          h ) multiple=3600     ;;
          m ) multiple=60       ;;
          s ) multiple=1        ;;
          * )
             echo "Invalid time+unit argument: ${val}${unit}" 1>&2
             return 1
       esac

       total=$[ total + (val * multiple) ];

       if ! shift 2 ; then
          echo "Bad time specification: $*" 1>&2
          return 1
       fi
    done

    echo "${total}"
}

provide time2

# time2.bash ends here
