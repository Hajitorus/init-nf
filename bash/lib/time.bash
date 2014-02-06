# time.bash
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1993-01-25
# Last modified: 1993-01-25
# Public domain

# Commentary:
# Code:

#:docstring time:
# Usage: time [command]
#
# Show verbose time resource statistics about command.
#:end docstring:

function time ()
{
   command time --format '*** Timing Information ***\n
Elapsed time     : %E   		
CPU Time         : %Ss kernel, %Us user
Percent CPU      : %P\n
Page faults      : %F major, %R minor
Page size        : %Z bytes\n
Context switches : %w voluntary, %c involuntary
Swaps            : %W\n
Shared text      : %Xk     
Resident set size: %tk avg, %Mk max
Avg total memory : %Kk
Avg unshared mem : %pk stack, %Dk data\n
I/O block ops    : %I input, %O output
Socket msgs      : %r received, %s sent
Signals received : %k
Exit status: %x' "$@"
}

provide time

# time.bash ends here
