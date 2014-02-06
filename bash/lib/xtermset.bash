# xtermset.bash --- frob xterm settings
# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1991-06-26
# Last modified: 1993-01-02
# Public domain

# Commentary:

# Complete documentation for this function is not available yet.
# 
# Gripes: There's no xterm sequence to toggle the scrollbar.
#         Some features don't actually work in some versions of xterm.
#         There's no documentation on tektronix ctlseqs (just as well.  I
#            never use tek mode).

# Code:

###;;;autoload
function xtermset ()
{
 local ctlseq
 local err=0
 local e="$(echo -e \\033)"   # ESC
 local g="$(echo -e \\007)"   # BEL

    case $1 in
       nmlcursekeys                   ) ctlseq='${e}[?1l'               ;;
       vt52mode                       ) ctlseq='${e}[?2h'               ;;
       132col                         ) ctlseq='${e}[?3h'               ;;
       80col                          ) ctlseq='${e}[?3l'               ;;
       smoothscroll                   ) ctlseq='${e}[?4h'               ;;
       jumpscroll                     ) ctlseq='${e}[?4l'               ;;
       wob                            ) ctlseq='${e}[?5h'               ;;
       bow                            ) ctlseq='${e}[?5l'               ;;
       origin-mode                    ) ctlseq='${e}[?6h'               ;;
       ncurse-mode                    ) ctlseq='${e}[?6l'               ;;
       wraparound                     ) ctlseq='${e}[?7h'               ;;
       nowraparound                   ) ctlseq='${e}[?7l'               ;;
       autorepeat                     ) ctlseq='${e}[?8h'               ;;
       noautorepeat                   ) ctlseq='${e}[?8l'               ;;
       tektronix                      ) ctlseq='${e}[?38h'              ;;
       allow80-132                    ) ctlseq='${e}[?40h'              ;;
       noallow80-132 | disallow80-132 ) ctlseq='${e}[?40l'              ;;
       fixcurses                      ) ctlseq='${e}[?41h'              ;;
       nofixcurses                    ) ctlseq='${e}[?41l'              ;;
       marginbell                     ) ctlseq='${e}[?44h'              ;;
       nomarginbell                   ) ctlseq='${e}[?44l'              ;;
       reversewrap                    ) ctlseq='${e}[?45h'              ;;
       noreversewrap                  ) ctlseq='${e}[?45l'              ;;
       logging                        ) ctlseq='${e}[?46h'              ;;
       nologging                      ) ctlseq='${e}[?46l'              ;;
       altbuf                         ) ctlseq='${e}[?47h'              ;;
       normbuf                        ) ctlseq='${e}[?47l'              ;;
       char                           )
          case $2 in
             normal                   ) ctlseq='${e}[0m'                ;;
             blink                    ) ctlseq='${e}[1m'                ;;
             underscore               ) ctlseq='${e}[4m'                ;;
             bold                     ) ctlseq='${e}[5m'                ;;
             inverse                  ) ctlseq='${e}[7m'                ;;
             *                        ) 
                ctlseq='xtermset: char: invalid parameter \"$2\"'; err=1  ;;
                                                                        
           esac                                                         ;;
       xtitle                         ) ctlseq='${e}]0\;$*${g}'; shift  ;;
       ititle                         ) ctlseq='${e}]1\;$*${g}'; shift  ;;
       wtitle                         ) ctlseq='${e}]2\;$*${g}'; shift  ;;
       logfilename                    ) ctlseq='${e}]46\;$*${g}'; shift ;;
       # Roland McGrath Memorial option
       frobme_baby | frobme-baby      ) ctlseq='${e}\(0${e}\)0${e}\*0${e}+B' ;;
       unfrob* | ascii                ) ctlseq='${e}\(B${e}\)B${e}\*B${e}+B' ;;
       *                              ) 
          ctlseq='xtermset: invalid option \"$1\"'; err=1                 ;;
    esac ;

    echo -en $(eval echo -en ${ctlseq});
    if [ ${err} -eq 1 ]; then echo ""; fi
}

provide xtermset

# xtermset.bash ends here
