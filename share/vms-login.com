$ vf = f$verify (0)
$ ! $Id: vms-login.com,v 1.3 2006/12/16 01:48:37 friedman Exp $
$ ! Author: Noah Friedman <friedman@splode.com>
$ ! Created: 2006-10-09
$ ! Public domain
$
$ ! Create user$home logical device to which directories can be appended.
$ call define_logical_device user$home sys$login
$
$ call define_logical_device gnu sys$sysroot:[gnv]
$ gnv_setup :== @gnu:[lib]gnv_setup.com
$
$ !----------------------------------------
$
$ ! Image search path
$ define dcl$path sys$system,user$home:[bin]
$
$ !----------------------------------------
$
$ default_privs := (s:rwed,o:rwed,g:rwed,w:rwed)
$ set protection='default_privs' /default
$
$ !----------------------------------------
$
$ ls    :== dir /versions=1
$ li    :== dir /size=(units=bytes,used)	-
		/date=modified			-
		/time=modified			-
		/security			-
		/nowrap				-
		/width=(display=0,filename=32,owner=10,size=7)
$
$ cd    :== set default
$ pwd   :== show default
$ up    :== pipe set default [-] ; show default
$
$ mv    :== rename
$ cp    :== copy
$ mkdir :== create /directory /protection='default_privs'
$
$ echo  :== write sys$output
$ rwed  :== set security /protection='default_privs'
$ su    :== set process/privilege=sysprv
$ ! chmod := set security 'P1'.dir /protection=('P2')
$
$ cls   :== type/page nla0:
$
$ !----------------------------------------
$
$ set terminal /nowrap
$
$ exit 1 .or. f$verify(vf)
$
$ !----------------------------------------
$ ! Subroutines
$ !----------------------------------------
$
$ define_logical_device: subroutine
$   ! The hairy concatenation is there to handle a variety of edge cases
$   ! for directory specifications.
$   expn = f$parse (p2,,, "device", "no_conceal")                 -
           + f$parse (p2, "[000000]",, "directory", "no_conceal") -
           - "]["                                                 -
           - "000000]"                                            -
           - ".]"                                                 -
           + "*" - ".*" - "*"                                     -
           - "]"                                                  -
           + ".]"                                                 -
           - "[.]"
$   ! The following definition can be removed via:
$   !     deassign /table=lnm$job 'p1'
$   define /table=lnm$job                                         -
           /translation_attributes=(concealed,terminal)           -
           'p1' 'expn'
$ endsubroutine
$
$ !----------------------------------------
$ ! End of commands
$ !----------------------------------------
$
$ ! Directory syntax notes:
$ ! Top-level directory: dev:[000000]
$ ! parent directory:    [-]
$ ! recursion:           [...]  (not all commands support this)
$ ! absolute directory:  [foo.bar]
$ ! relative directory:  [.bar]
$ ! directory file:      [foo]bar.dir;1
$ !
$ ! to run a command not in path, with args: MCR command args...
$
$ ! eof
