divert(-1)
# twmrc.m4 --- m4 preprocessor input file for twm/ctwm
# Author: Noah Friedman <friedman@splode.com>
# Created: 1991-10-23
# Public domain

# $Id: twmrc.m4,v 1.174 2012/03/05 16:59:25 friedman Exp $

# Commentary:

# This is insane.

# Code:


# repeats the expansion of text with any occurence of var substituted with
# the value of any remaining args, i.e.
#   foreach(`VAR',`arg is VAR\n', arg1, arg2, ...)
#   => arg is arg1
#      arg is arg2
#      arg is ...
define(`foreach',
  `ifelse(eval($# < 3),1,,
          $#`$3',3,,
          `pushdef(`$1',`$3')`'$2`'popdef(`$1')`'$0(`$1',`$2',shift(shift(shift($@))))')')

# usage: join(`separator', `arg1', `arg2', ...)
define(`join',
       `ifelse(`$#',0,,
               `$#',1,,
               `$#',2,`$2',
               `$2`'$1`'join(`$1',shift(shift($@)))')')

# conditional def: only define if not already defined
define(`cdef',`ifdef(`$1',,`define(`$1',`$2')')')

# quotes quotation marks (and backslash) for embedding in strings
define(`qq',`patsubst(`$@',`\(["\\]\)',`\\\1')')

# Surrounds args with double quotes if any whitespace is present
define(`qs',`ifelse(regexp(`$@',`[ 	]'),-1,`$@',`"$@"')')

define(`listlength',`$#')

define(`NEWLINE', `
')


define(`TWM_VERSION_MAJOR',`0')
define(`TWM_VERSION_MINOR',`0')
regexp(TWM_VERSION, `^\([0-9]+\)\.\([0-9]+\)',
       `define(`TWM_VERSION_MAJOR',\1)
        define(`TWM_VERSION_MINOR',\2)')

define(`IF_CTWM', `ifelse(TWM_TYPE,`ctwm', `$1',`$2')')

# args: majornum, minornum, true-action, false-action, not-ctwm-action
define(`IF_CTWM_MINVER',
       `ifelse(TWM_TYPE, `ctwm',
               `ifelse(eval((TWM_VERSION_MAJOR > `$1')
                            || (TWM_VERSION_MAJOR == `$1'
                                && TWM_VERSION_MINOR >= `$2')), 1,
                       `$3', `$4')',
               `$5')')

IF_CTWM(`define(`menu_separator',`"" f.separator')',
        `define(`menu_separator',`')')

IF_CTWM(`define(`pin_menu',
                `menu_separator
                 "[Pin Menu]" f.pin')',
        `define(`pin_menu',`')')

IF_CTWM(`define(`map_window_color_parameters',
                `MapWindowBackground    "black"
                 MapWindowForeground    "white"
                ')',
        `define(`map_window_color_parameters',`')')

define(`workspace_names',`1,2,3,4,5,6,7,8')


cdef(`SHARE', `HOME/etc/init/share')

ifdef(`ignore__WIDTH',
      `define(`max_window_size',`WIDTH`'x`'HEIGHT')',
      `define(`max_window_size',`8192x8192')')

dnl define(`defaultfont', `variable')
dnl define(`defaultfont', `-*-helvetica-medium-r-normal-*-*-120-*-*-*-*-*-*')
define(`defaultfont', `-*-helvetica-medium-r-normal--12-*-*-*-*-*-*-*')


define(`system',`f.exec "qq($@)"')
define(`spawn', `f.exec "qq($@) &"')

define(`xapp',`"`$1'"			spawn(join(` ', $@))')

define(`spawn_misc', `spawn(run-misc.sh qq($@))')
define(`xapp_misc',  `"`$1'"			spawn_misc(join(` ', $@))')

# Aliased here in case we want to change later.
define(`spawn_gnome',`spawn_misc(qq($@))')
define(`xapp_gnome', `xapp_misc(qq($@))')

# Aliased here in case we want to change later.
define(`spawn_kde',`spawn_misc(qq($@))')
define(`xapp_kde', `xapp_misc(qq($@))')

# args: label, font, title, extra
define(`mkxterm',
  `join(`	',
        "qq(`$1')",
        spawn(join(, `xterm -ls -ut',
                     ifelse(`$2',,,` -fn qs(`$2') -fb qs(`$2')'),
                     ifelse(`$3',,,` -T qs(`$3') -n qs(`$3')'),
                     ifelse(`$4',,,` $4'))))NEWLINE')

define(`hostnick', `ifelse(index(`$1',`.'),-1,`$1',substr(`$1',0,index(`$1',`.')))')
define(`rxterm',  `"qq(hostnick($1))"	spawn(`rxterm' join(` ',shift($@)))'NEWLINE)

divert(0)dnl
#
# Global parameters
#

NoDefaults

# Various *Function variables must be set after the fonts have been
# assigned, so assign the fonts early.
IconFont                        "defaultfont"
IconManagerFont                 "defaultfont"
MenuFont                        "defaultfont"
ResizeFont                      "defaultfont"
TitleFont                       "defaultfont"

#AutoRelativeResize
BorderWidth                     1
ButtonIndent                    0
#ClientBorderWidth              1           # Not in all twm implementations
ConstrainedMoveTime             400
DecorateTransients
DefaultFunction                 f.nop
DontMoveOff
#ForceIcons
FramePadding                    2
MaxWindowSize                   "max_window_size"
MoveDelta                       0
NoBackingStore
NoCaseSensitive
NoGrabServer
NoMenuShadows
#NoRaiseOnDeiconify
#NoRaiseOnResize
#NoRaiseOnMove
#NoRaiseOnWarp
NoSaveUnders
#NoTitleFocus
NoTitleHighlight
#OpaqueMove
RandomPlacement
RestartPreviousState
TitleButtonBorderWidth          0
TitlePadding                    5
UsePPosition                    "on"
WarpUnmapped
#WindowFunction                 f.deiconify
#Zoom 16

ifelse(PLANES,`8',`
# This only seems necessary for 8bpp displays; the rest of the time,
# the default algorithm works fine.
XORvalue 1
')dnl

#WindowRing {}

# X11R5 and later has a NoStackMode list (rtfm)
ifelse(eval(RELEASE > 4), 1,
`NoStackMode
{
  "oclock"
  "dclock"
  "xbiff"
  "xclock"
  "xload"
  "TWM Icon Manager"
}')

dnl  `"Untitled"',
define(`notitle_list',
  `ifelse(eval(HEIGHT < 1024), 1,`"Mozilla Firefox",')
  `"WorkSpaceManager"',
  `"Basilisk II"',
  `"SheepShaver"',
  `"dclock"',
  `"gkrellm"',
  `"GQradio"',
  `"msgs"',
  `"notitle"',
  `"oclock"',
  `"panel"',
  `"screen"',
  `"ssystem 1.6"',
  `"TWM Icon Manager"',
  `"xbiff"',
  `"xclock"',
  `"xconsole"',
  `"xdaliclock"',
  `"xeyes"',
  `"xgif"',
  `"xload"',
  `"wmwave"',

  `"MPlayer"',
  `"MPlayer - Video"',

  foreach(`PLAYER',
          `"PLAYER",
           "PLAYER Equalizer",
           "PLAYER Playlist",
           "PLAYER Playlist Editor",',
          `Audacious', `BMP', `XMMS')

  `"Songbird"',
  `"Music - Songbird"',

  `"xine Control Window"',
  `"xine Event Sender"',
  `"xine MRL Browser"',
  `"xine Panel"',
  `"xine Playlist Editor"',
  `"xine setup"',
  `"xiTK Window"'')

IF_CTWM(`define(`winbordercolors',
                `{
                   foreach(`TITLE',
                           `TITLE "black"NEWLINE',
                           notitle_list)dnl
                 }')',
        `define(`winbordercolors',`')')

Notitle
{
  join(`NEWLINE', notitle_list)
}

# SqueezeTitle {}

IconifyByUnmapping
IconDirectory                   "HOME/lib/bitmaps/xbm"
IconManagerGeometry             "97x1-0+0"
IconRegion                      "256x256-4+0" North East 1 1
ShowIconManager

# In ctwm, use f.sorticonmgr menu option, to avoid focus screwage from
# title renames
IF_CTWM(`',`
SortIconManager
')
# UnknownIcon                   "generic.icon"

# Don't unmap anything that doesn't show up in the window manager.
#
# Actually you can uncomment the DontIconifyByUnmapping line if you really
# want this behavior.  Since I can get to any window with the TwmWindows
# menu, there's no need for lots of icons floating around.
define(`icon_manager_dont_show_list',
  `"dclock"
   "gkrellm"
   "msgs"
   "oclock"
   "xbiff"
   "xclock"
   "xconsole"
   "xdaliclock"
   "xeyes"
   "xload"
   "XMMS"
   "Audacious"
   "TWM Icon Manager"')
IconManagerDontShow    { icon_manager_dont_show_list }
#DontIconifyByUnmapping { icon_manager_dont_show_list }

#IconManagers {}
#Icons {}

Cursors
{
    Frame                       "right_ptr"
    Title                       "left_ptr"
    Icon                        "right_ptr"
    IconMgr                     "right_ptr"
    Move                        "plus"
    Resize                      "fleur"
    Menu                        "hand1"
    Button                      "hand2"
    Wait                        "watch"
    Select                      "dot"
    Destroy                     "pirate"
}

define(`monochrome_defaults',`
  BorderColor                   "white"  winbordercolors
  BorderTileBackground          "black"
  BorderTileForeground          "white"  winbordercolors
  DefaultBackground             "black"
  DefaultForeground             "white"
  IconBackground                "black"
  IconBorderColor               "white"
  IconForeground                "white"
  IconManagerBackground         "black"
  IconManagerForeGround         "white"
  IconManagerHighlight          "white"
  MenuBackground                "black"
  MenuForeground                "white"
  MenuShadowColor               "black"
  MenuTitleBackground           "white"
  MenuTitleForeground           "black"
  TitleBackground               "black"
  TitleForeground               "white"
')

Color
{
  monochrome_defaults
  map_window_color_parameters
}

# Too many versions of twm/ctwm don't recognize GreyScale.
#Greyscale { monochrome_defaults }

Monochrome
{
  monochrome_defaults
  map_window_color_parameters
}

IF_CTWM(`
AlwaysShowWindowWhenMovingFromWorkspaceManager
AutoFocusToTransients
BorderResizeCursors
#CenterFeedbackWindow
#ChangeWorkspaceFunction        f.change_workspace_function
#ClientBorderWidth
#DeIconifyFunction              f.deiconify_function
DontPaintRootWindow
DontWarpCursorInWMap
#IconifyFunction                f.iconfify_function
IF_CTWM_MINVER(3,6,`
IgnoreCaseInMenuSelection
')
MoveOffResistance               100
MovePackResistance              20
#NoIconManagerFocus
NoShowOccupyAll
#OpaqueResize
OpenWindowTimeout               10
RaiseDelay                      10
#RandomPlacement
ReallyMoveInWorkspaceManager
#SchrinkIconTitles
#ShowWorkSpaceManager           # Dont show workspace manager by default
#StartInMapState                # Use button state for WorkSpaceMgr
StayUpMenus
TitleJustification              "left"  # "center" "right"
# Need to make transients have occupation or else one cannot reassign
# workspaces for most gtk apps.  (Why are gtk apps transient?)
TransientHasOccupation
TransientOnTop 			0  # minimum transient/leader ratio (percent)
#UseThreeDBorders
#UseThreeDIconManagers
#UseThreeDMenus
#UseThreeDTitles
#UseThreeDWMap
#WindowFunction                 f.window_function
XMoveGrid 			1
YMoveGrid			1

IF_CTWM_MINVER(3,7,`
#AlwaysSqueezeToGravity {}
')

#IF_CTWM_MINVER(3,7,`ifdef(`EXT_XINERAMA',`
#VirtualScreens
#{
#  "1600x1200+0+0"
#  "1600x1200+1600+0"
#}')')

MapWindowCurrentWorkSpace  {  "white" "black" "white" }

WorkSpaces
{
  dnl name bg-button fg-button bg-root fg-root pixmap-root
  foreach(`X',
          `"X" { "black" "white" "black" "white" }NEWLINE',
          workspace_names)dnl
}

WorkSpaceManagerGeometry "-0-0" listlength(workspace_names)

OccupyAll
{
  "TWM Icon Manager"
}

')  dnl end of if_ctwm section

divert(-1)dnl

# Once you use any (m)odifier keys, you have to specify the behavior for
# every combination.  Since NumLock (mod2) should impact only a very small
# number of keys, we have to define both the case where m2 is or is not
# defined.
#
# This kinda sucks.
pushdef(`m2button',`
`$1' = `$2' : `$3' : `$4'
`$1' = ifelse(`$2',`',`m2',`$2|m2') : `$3' : `$4'
')

divert(0)dnl
# Button definitions
# Modifiers: (c)ontrol  (m)eta   (s)hift  (l)ock  m[1-5]|mod[1-5]
# Contexts:  (w)indow   (t)itle  (i)con   (r)oot  (f)rame  icon(m)gr  all

#Button			= Mod	: Context	: Function
#m2button(Button, 	Mod, 	Context, 	Function)

m2button(Button1,	, 	m|i,	f.iconify)
m2button(Button1,	, 	f|t,	f.function "default_move")
m2button(Button1,	, 	f,	f.resize)
m2button(Button1,	, 	r,	f.menu "main")

# Raw
IF_CTWM(`
m2button(Button2,	,	f|i,	f.function "default_move")
m2button(Button2,	,	t,	f.squeeze)
',`
m2button(Button2,	,	f|t|i,	f.function "default_move")
')
m2button(Button2,	,	r,	f.menu "window_ops")

m2button(Button3,	,	i,	f.move)
m2button(Button3,	,	r,	f.menu "TwmWindows")
m2button(Button3,	,	f|t|m,	f.menu "window_ops")
m2button(Button3,	c,	f|t|m,	f.menu "dangerous_window_ops")

# Control
m2button(Button1,	c,	f|t|m,	f.identify)

# Function key mappings
#m2button("F1",		,	all,	f.nop)
#m2button("F2",		,	all,	f.refresh)
#m2button("F3",		,	all,	f.nop)
#m2button("F4",		,	all,	f.nop)
#m2button("F5",		,	all,	f.nop)
#m2button("F6",		,	all,	f.nop)
#m2button("F7",		,	all,	f.nop)
#m2button("F8",		,	all,	f.nop)
#m2button("F9",		,	all,	f.nop)
#m2button("F10",	,	all,	f.nop)
#m2button("F11",	,	all,	f.nop)
#m2button("F12",	,	all,	f.nop)

m2button("Up",		c|m1,	all,	f.menu "main")

m2button("Up",		c,	all,	f.function "up_iconmgr")
m2button("Down",	c,	all,	f.function "down_iconmgr")
#m2button("Left",	c,	all,	f.function "left_iconmgr")
#m2button("Right",	c,	all,	f.function "right_iconmgr")
m2button("Insert",	c,	all,	f.iconify)

IF_CTWM(`
#m2button("Up",		c|s,	all,	f.function "up_iconmgr")
#m2button("Down",	c|s,	all,	f.function "down_iconmgr")
#m2button("Left",	c|s,	all,	f.function "prev_iconmgr")
#m2button("Right",	c|s,	all,	f.function "next_iconmgr")

#m2button("Up",		m1,	all,	f.upworkspace)
#m2button("Down",	m1,	all,	f.downworkspace)
m2button("Left",	m1,	all,	f.leftworkspace)
m2button("Right",	m1,	all,	f.rightworkspace)

m2button("Left",	m1|s,	w,	f.movetoprevworkspaceandfollow)
m2button("Right",	m1|s,	w,	f.movetonextworkspaceandfollow)

') dnl end of ctwm keymaps

divert(-1)dnl

popdef(`m2button')
pushdef(`m2button',`
  Button`$1' =    : `$2'
  Button`$1' = m2 : `$2'
')

divert(0)dnl

# titlebar mappings

IF_CTWM(`
LeftTitleButton    ":iconify"
{
  m2button(1, f.menu "title_ops")
  m2button(2, f.menu "title_ops")
  m2button(3, f.menu "title_ops")
}
',`
LeftTitleButton    ":iconify"       = f.menu "title_ops"
')dnl

IF_CTWM(`
RightTitleButton   "minimize.xbm"
{
  m2button(1, f.iconify)
  m2button(2, f.iconify)
  m2button(3, f.iconify)
}

RightTitleButton   "maximize.xbm"
{
  m2button(1, f.fullzoom)
  m2button(2, f.fullzoom)
  m2button(3, f.fullzoom)
}

RightTitleButton   "close.xbm"
{
  m2button(1, f.delete)
  m2button(2, f.delete)
  m2button(3, f.menu "dangerous_window_ops")
}
',`
#RightTitleButton   "lower.xbm"      = f.lower
#RightTitleButton   "raise.xbm"      = f.raise
#RightTitleButton   "resize.xbm"     = f.resize   # Nicer than ":resize"
RightTitleButton    "minimize.xbm"   = f.iconify
RightTitleButton    "maximize.xbm"   = f.fullzoom
RightTitleButton    "close.xbm"      = f.delete
')dnl

divert(-1)dnl
popdef(`m2button')


divert(0)dnl
# function definitions

Function "preprocess_and_restart_wmgr"
{
 f.exec ": > HOME/.wm-restart"
 f.quit
}

Function "preprocess_and_rehash_xresources"
{
 f.exec "xrdb -load -cpp xrdb-m4 SHARE/xresources.m4 &"
}

Function "up_iconmgr"
{
  f.warptoiconmgr ""
  f.deltastop
  f.upiconmgr
}

Function "down_iconmgr"
{
  f.warptoiconmgr ""
  f.deltastop
  f.downiconmgr
}

Function "left_iconmgr"
{
  f.warptoiconmgr ""
  f.deltastop
  f.lefticonmgr
}

Function "right_iconmgr"
{
  f.warptoiconmgr ""
  f.deltastop
  f.righticonmgr
}

Function "next_iconmgr"
{
  f.warptoiconmgr ""
  f.deltastop
  f.nexticonmgr
}

Function "prev_iconmgr"
{
  f.warptoiconmgr ""
  f.deltastop
  f.previconmgr
}

Function "default_move"
{
IF_CTWM(`f.movepack',`f.move')
}

divert(-1)dnl

divert(0)dnl
# menu specifications
# Note that the TwmWindows menu is a builtin hack in twm.
# TwmAllWindows and TwmWorkSpaces are builtin ctwm hacks.

Menu                            "main"
{
  "Main Menu"                   f.title
  "Applications"                f.menu "applications"
  "Managerial"                  f.menu "managerial"
  "Miscellaneous"               f.menu "misc"
  "Hosts"                       f.menu "hosts"
  "Current Windows"             f.menu "TwmWindows"
IF_CTWM(`
# "All Windows"                 f.menu "TwmAllWindows"
  "Workspaces"                  f.menu "TwmWorkspaces"
')
dnl IF_CTWM_MINVER(3,7,`
dnl  "CTWM Keybindings"            f.menu "TwmKeys"
dnl ')
  "Quit"                        f.menu "quit"
  pin_menu
}

Menu                            "hosts"
{
  "Hosts"                       f.title
  xapp_misc(vncviewer)
  xapp_misc(vinagre)

  menu_separator
  "splode.com"                  f.menu "hosts:splode.com"
}

Menu                            "applications"
{
  "Applications"                f.title
  "XTerm"                       f.menu "app:terminals:xterm"
  "Emacs"                       f.menu "app:emacs"
  "WWW"                         f.menu "app:www"
  "Mail"                        f.menu "app:mail"
  "Ftp"                         f.menu "app:ftp"
  menu_separator
  "Gnome"                       f.menu "app:gnome"
  "KDE"                         f.menu "app:kde"
  menu_separator
  "Astronomical"                f.menu "app:astronomical"
  "Audio/Video"                 f.menu "app:audio_video"
  "Chat"                        f.menu "app:chat"
  "Circuit"                     f.menu "app:circuit"
  "Debuggers"                   f.menu "app:debuggers"
  "Doc Viewers"                 f.menu "app:document_viewers"
  "Emulators"                   f.menu "app:emulators"
  "Games"                       f.menu "app:games"
  "Image View/Edit"             f.menu "app:image"
  "LDAP"                        f.menu "app:ldap"
  "Office Tools"                f.menu "app:office"
  "Palm"                        f.menu "app:palm"
  "iPod"                        f.menu "app:ipod"
  "Sys Monitors"                f.menu "app:system_monitors"
  "Torrent Clients"             f.menu "app:torrent"
  "Utilities"                   f.menu "app:utils"
  "Version Control"             f.menu "app:scm"
  "X Screen Saver"              f.menu "app:xscreensaver"
  pin_menu
}

Menu                            "app:astronomical"
{
  "Astronomical"                f.title
  xapp_misc(celestia)
  xapp_misc(stellarium)
  xapp_misc(xephem)
  xapp_misc(xtide)
  menu_separator
  "open universe"               spawn_misc(openuniverse)
  xapp_misc(ssystem)
  xapp_misc(tkxplanet)
  pin_menu
}

Menu                            "app:audio_video"
{
  "Frequent"                    f.title
  xapp_misc(audacious)
  xapp_misc(xmms)
  xapp_misc(xine)

  "Audio/Video"                 f.title
  "Mixers"                      f.menu "app:audio_video:mixers"
  "Audio"                       f.menu "app:audio_video:audio"
  "Video"                       f.menu "app:audio_video:video"
  pin_menu
}

Menu                            "app:audio_video:mixers"
{
  "Mixers"                      f.title
  mkxterm(`alsamixer',,,`-geometry 160x34 -e alsamixer')
  xapp_misc(alsamixergui)

  xapp_misc(gnome-alsamixer)
  xapp_misc(xaumix)
  xapp_misc(xmmix)
  pin_menu
}

Menu                            "app:audio_video:audio"
{
  "MP3 Players"                 f.title
  xapp_misc(amarok)
  xapp_misc(amarokFS)
  xapp_misc(audacious)
  "bmp"                         spawn_misc(beep-media-player)
  "bmp2 (bmpx)"                 spawn_misc(beep-media-player-2)
  xapp_misc(exaile)
  xapp_misc(listen)
  xapp_misc(songbird)
  xapp_misc(xmms)
  #menu_separator
  #xapp(mxaudio)
  #xapp(x11amp)
  #xapp(xmcd)

  "Edit/Encode"                 f.title
  xapp_misc(audacity)
  xapp_misc(glame)
  xapp_misc(grip)
  xapp_misc(snd)
  #menu_separator
  #xapp(audiotool)
  #xapp(xwave)

  "Radio"                       f.title
  xapp_misc(gqradio)
  pin_menu
}

Menu                            "app:audio_video:video"
{
  "Video"                       f.title
  xapp_misc(gmplayer)
  xapp_misc(gxine)
  xapp_misc(xine)
  xapp_misc(mtv)
  "helix player"                spawn_misc(hxplay)
  xapp_misc(realplay)
  xapp_misc(totem)
  xapp_misc(vlc)

  "TV"                          f.title
  xapp_misc(kdetv)
  xapp_misc(tvtime)
  xapp_misc(motv)
  xapp_misc(xawtv)
  pin_menu
}

Menu                            "app:chat"
{
  "Chat (multi-protocol)"       f.title
  xapp_misc(empathy)
  xapp_misc(pidgin)
  #xapp_misc(gaim)
  xapp_misc(kopete)

  "Jabber (XMPP)"               f.title
  xapp_misc(gajim)
  xapp_misc(psi)

  "Yahoo"                       f.title
  xapp_misc(gyachi)
  xapp_misc(ymessenger)

  "IRC"                         f.title
  xapp_misc(xchat)
  xapp_misc(xchat-gnome)
  pin_menu
}

Menu                            "app:circuit"
{
  "Circuit Simulators"          f.title
  xapp_misc(ktechlab)
  xapp_misc(qucs)
  pin_menu
}

Menu                            "app:debuggers"
{
  "Debuggers"                   f.title
  xapp_misc(nemiver)
  xapp(ddd)
  xapp(kdbg)
  xapp(xxgdb)
  menu_separator
  mkxterm(`gdb',,,`-e gdb')
  mkxterm(`dbx',,,`-e dbx')
  pin_menu
}

Menu                            "app:document_viewers"
{
  "Doc Viewers"                 f.title
  xapp_misc(acroread)
  xapp_misc(epdfview)
  xapp_misc(xpdf)
  xapp_misc(gpdf)
  menu_separator
  xapp_misc(gv)
  xapp_misc(ggv)
  xapp_misc(ghostview)
  menu_separator
  xapp_misc(evince)
  xapp_misc(fbreader)
  xapp_misc(xdvi)
  menu_separator
  xapp_misc(chmsee)
  xapp_misc(xchm)
  menu_separator
  xapp_misc(comix)
  xapp_misc(qcomicbook)
  pin_menu
}

Menu                            "app:emacs"
{
  "EMACS"                       f.title
  xapp(emacs)
  xapp(yemacs)
  xapp(xemacs)
  xapp(lemacs)
  xapp(mule)
  "edwin"                       spawn(scheme -edwin -edit > /dev/null 2>&1 0<&1)

  menu_separator
  "emacs -q"                    spawn(emacs   -q)
  "yemacs -q"                   spawn(yemacs  -q)
  "xemacs -q"                   spawn(xemacs  -q)
  "lemacs -q"                   spawn(lemacs  -q)
  "mule -q"                     spawn(mule    -q)

  menu_separator
  xapp(emacs-18.59)
  xapp(emacs-19.34)
  xapp(emacs-20.7)
  xapp(emacs-21.3)

  menu_separator
  xapp(emacs-18.59 -q)
  xapp(emacs-19.34 -q)
  "emacs-20.7 -q"               spawn(emacs-20.7  -q)
  "emacs-21.3 -q"               spawn(emacs-21.3  -q)
  pin_menu
}

Menu                            "app:emulators"
{
  "Emulators"                   f.title
  "VMware Player"               spawn_misc(vmplayer)
  "VMware Workstation"          spawn_misc(vmware)
  "VMware Remote Console"       spawn_misc(vmware-vmrc)
  "VMware Server Console"       spawn_misc(vmware-server-console)

  menu_separator
  xapp_misc(qemu-launcher)

  menu_separator
  xapp_misc(BasiliskII)
  xapp_misc(SheepShaver)

  menu_separator
  mkxterm(`Commodore PET',      ,, `-e xpet')
  mkxterm(`Commodore Vic-20',   ,, `-e xvic')
  mkxterm(`Commodore 64',       ,, `-e x64')
  mkxterm(`Commodore 128',      ,, `-e x128')
  mkxterm(`C1541 disk drive',   ,, `-e c1541')

  menu_separator
  "Altair 88006"                spawn(xaltair)
  "IMSAI 8080"                  spawn(ximsai)

  pin_menu
}

Menu                            "app:ftp"
{
  "FTP"                         f.title
  foreach(`PROG',
          `mkxterm(`PROG',,,`-e PROG')',
          `ftp',`fftp',`lukemftp',`tnftp',`yafc')
  menu_separator
  xapp(gftp)
  pin_menu
}

Menu                            "app:games"
{
  "Games"                       f.title
  xapp_misc(gweled)
  xapp_misc(gnome-sudoku)
  xapp_misc(penguin-command)
  xapp_misc(pingus)
  "solitaire (PySol)"           spawn_misc(pysol --nosound)
  xapp_misc(supertux)
  menu_separator
  mkxterm(`rogue',,,   `-e rogue')
  mkxterm(`nethack',,, `-e nethack')
  xapp(xnethack)
  "nethack (vultures eye)"      spawn_misc(vultureseye)
  "nethack (vultures claw)"     spawn_misc(vulturesclaw)
  menu_separator
  "chess (glchess)"             spawn_misc(glchess)
  "chess (eboard)"              spawn_misc(eboard)
  "chess (xboard)"              spawn_misc(xboard)
  menu_separator
  "go (cgoban)"                 spawn_misc(cgoban)
  "go (qgo)"                    spawn_misc(qgo)
  menu_separator
  "More Games"                  f.menu "app:games:infrequent"
}

Menu                            "app:games:infrequent"
{
  "More Games"                  f.title
  xapp_misc(asteroids)
  xapp_misc(gattax)
  xapp_misc(lincity-ng)
  xapp_misc(pente)
  xapp_misc(pipepanic)
  "solitaire (Sol)"             spawn_misc(sol)
  "space invaders"              spawn_misc(xinvaders)
  xapp_misc(xaos)
  xapp_misc(xataxx, -n)
  "xgalaxy (galaga)"            spawn_misc(xgalaxy -window)
}

Menu                            "app:image"
{
  "Image Viewers"               f.title
  xapp_misc(eog)
  xapp_misc(feh)
  xapp_misc(gliv)
  xapp_misc(geeqie)
  xapp_misc(gqview)
  xapp_misc(gwenview)
  xapp_misc(gthumb)
  xapp_misc(mirage)
  xapp_misc(qiv)
  xapp_misc(showimg)
  xapp_misc(xv)
  xapp_misc(xzgv)

  "Image Editors"               f.title
  xapp_misc(gimp)
  xapp_misc(xpaint)

  menu_separator
  xapp_misc(dia)
  xapp_misc(kivio)
  xapp_misc(xfig)

  menu_separator
  xapp_misc(blender)
  xapp_misc(inkscape)
  xapp_misc(qcad)
  xapp_misc(scribus)
  xapp_misc(skencil)
  xapp_misc(sodipodi)

  menu_separator
  xapp_misc(gtkam)
  xapp_misc(gphoto)
  pin_menu
}

Menu                            "app:ldap"
{
  "LDAP"                        f.title
  xapp_misc(ApacheDirectoryStudio)
  xapp_misc(edsadmin)
  xapp_misc(gq)
  pin_menu
}

Menu                            "app:office"
{
  "Office Tools"                f.title
  "OpenOffice"                  f.menu "app:openoffice"
  "KOffice"                     f.menu "app:koffice"

  menu_separator
  xapp_misc(gnumeric)
  xapp_misc(oleo)

  "Financial"                   f.title
  xapp_misc(gnucash)
  xapp_misc(grisbi)
  xapp_kde(kmymoney)

  pin_menu
}

Menu                            "app:openoffice"
{
  "OpenOffice"                  f.title
  "Main"                        spawn_misc(soffice)
  "Database"                    spawn_misc(soffice -base)
  "Calc"                        spawn_misc(soffice -calc)
  "Draw"                        spawn_misc(soffice -draw)
  "Impress"                     spawn_misc(soffice -impress)
  "Math"                        spawn_misc(soffice -math)
  "Viewdoc"                     spawn_misc(soffice -view)
  "Writer"                      spawn_misc(soffice -writer)
  "(Install)"                   spawn_misc(ooffice)
  pin_menu
}

Menu                            "app:koffice"
{
  "KOffice"                     f.title
  "Shell"                       spawn_kde(koshell)
  xapp_kde(karbon)
  xapp_kde(kchart)
  xapp_kde(kformula)
  xapp_kde(kivio)
  xapp_kde(kpresenter)
  xapp_kde(krita)
  xapp_kde(kplato)
  xapp_kde(kspread)
  xapp_kde(kthesaurus)
  xapp_kde(kudesigner)
  xapp_kde(kugar)
  xapp_kde(kword)
  pin_menu
}

Menu                            "app:palm"
{
  "Palm Tools"                  f.title
  xapp_misc(jpilot)
  xapp_misc(pose)
}

Menu                            "app:ipod"
{
  "iPod Tools"                  f.title
  xapp_misc(gtkpod)
  xapp_misc(YamiPod)
  menu_separator
  xapp_misc(gpodder)
}

Menu                            "app:system_monitors"
{
  "System Monitors"             f.title
  xapp_misc(gkrellm)
  xapp_misc(gmemusage)
  xapp_misc(procmeter)
  xapp_misc(xcpustate)
  xapp_misc(xidle)
  xapp_misc(xload)
  xapp_misc(xmem)
  xapp_misc(xosview)

  menu_separator
  xapp_misc(mtr)
  xapp_misc(xmtr)
}

Menu                            "app:terminals:xterm"
{
  "XTerm"                       f.title
  "Terminus"                    f.menu "app:terminals:xterm:terminus"
  "xterm"                       spawn(xterm -ls)
  "xterm -ut"                   spawn(xterm -ls -ut)
  "xterm (console)"             spawn(xterm -C -geometry 80x5 -T msgs -n msgs -ut)

  menu_separator
  foreach(`FONT',
          `mkxterm(`xterm -ut (FONT)',`FONT')',
          `5x8',`6x10',`6x13',`8x13',`9x15',`10x20',`12x24')dnl

  menu_separator
  foreach(`FONT',
          `mkxterm(`xterm -ut (FONT) notitle',`FONT',`notitle')',
          `6x13',`9x15',`10x20')dnl

  menu_separator
  "gnome terminal"              spawn(gnome-terminal)
  xapp(putty)

  menu_separator
  xapp(Eterm)
  xapp(kterm)
  xapp(nxterm)
  xapp(rxvt)
  xapp(x3270)
  xapp(xiterm)

  menu_separator
  foreach(`FONT',
          `mkxterm(`screen -x (xterm FONT)',`FONT',`screen',`-e screen -x')',
          `6x13',`9x15',`10x20')dnl
  "screen -x (gnome)"           spawn(gnome-terminal -t screen -e screen -x)
  pin_menu
}

Menu                            "app:terminals:xterm:terminus"
{
  "XTerm (Terminus font)"       f.title
  foreach(`FONT',
          `mkxterm(`xterm -ut (FONT)',`terminus`'FONT`'u')',
          `6x12',`8x14',`8x16',`10x20',`12x24',`14x28',`16x32')dnl

  menu_separator
  foreach(`FONT',
          `mkxterm(`xterm -ut (FONT) notitle',`terminus`'FONT`'u',`notitle')',
          `6x12',`8x14',`8x16',`10x20',`12x24',`14x28',`16x32')dnl
  pin_menu
}

Menu                            "app:torrent"
{
  "Torrent Clients"             f.title
  xapp_misc(azureus)
  xapp_misc(bittorrent)
  xapp_misc(deluge)
  xapp_misc(gtorrentviewer)
  xapp_misc(ktorrent)
  xapp_misc(transmission)
  menu_separator
  xapp_misc(gtk-gnutella)
}

Menu                            "app:utils"
{
  "Utilities"                   f.title
  xapp_misc(gucharmap)
  xapp_misc(gnome-character-map)
  xapp_misc(gnome-font-properties)

  menu_separator
  xapp_misc(xar)
  xapp_misc(xarchiver)

  menu_separator
  "xca (cert tool)"             spawn_misc(xca)

  menu_separator
  xapp(xmaxima)
  xapp(wxmaxima)

  menu_separator
  xapp_misc(galculate)
  xapp_misc(gcalctool)
  xapp_misc(speedcrunch)
  mkxterm(`qalculate (xterm)',,,`-e qalc')
  xapp_misc(qalculate-gtk)
  xapp_misc(x48)
  "xcalc"                       spawn_misc(xcalc -rpn)
  "xcalc (color)"               spawn_misc(xcalc -xrm "*customization: -color" -rpn)
  xapp(xabacus)

  menu_separator
  xapp_misc(vncviewer)

  menu_separator
  xapp(xbiff)
  xapp(xclock)
  xapp(xconsole)
  xapp(xeyes)
  xapp(xfontsel)
  xapp(xkeycaps)
  xapp(xlock, -mode random -duration 600 -nice 10)
  xapp(xunclutter, -visible)

  pin_menu
}

Menu                            "app:www"
{
  "WWW"                         f.title
  "Profile Mgr"                 f.menu "app:gecko:profmgr"
  xapp(firefox)
  xapp_misc(epiphany)
  xapp_misc(midori)

  menu_separator
  xapp_misc(dillo)
  xapp_misc(galeon)
  xapp_misc(google-chrome)
  xapp_misc(konqueror)
  xapp_misc(opera)

  menu_separator
  xapp(seamonkey)
  xapp(mozilla)
  xapp(netscape)

  menu_separator
  xapp_misc(kazehakase)
  xapp_misc(arena)
  xapp_misc(chimera)
  xapp_misc(mosaic)
  xapp_misc(xmosaic)

  menu_separator
  foreach(`PROG',
          `mkxterm(`PROG',,`PROG',`-geometry 80x48 -e PROG')',
          `elinks',`links',`lynx',`w3m')dnl
  "w3 (emacs)"                  spawn(emacs -f w3-fetch)
  pin_menu
}

Menu                            "app:mail"
{
  "Mail"                        f.title
  "Profile Mgr"                 f.menu "app:gecko:profmgr"
  xapp(thunderbird)
  xapp(seamonkey)
  xapp(mozilla)
  xapp(netscape)

  menu_separator
  xapp_misc(balsa)
  xapp_misc(evolution)
  xapp_misc(sylpheed)
  xapp_misc(sylpheed-claws)

  menu_separator
  foreach(`PROG',
          `mkxterm(`PROG',,`PROG',`-geometry 80x48 -e PROG')',
          `abook',`cone',`mail',`mailx',`mutt',`nail',`nmh',`pine')dnl
  pin_menu
}

Menu                            "app:gecko:profmgr"
{
  "Profile Mgr"                 f.title
  xapp(firefox,                 -ProfileManager)
  xapp(thunderbird,             -ProfileManager)
  xapp(seamonkey,               -ProfileManager)
  xapp(mozilla,                 -ProfileManager)
  xapp(netscape,                -ProfileManager)

  "Safe Mode"                   f.title
  xapp(firefox,                 -safe-mode)
  xapp(thunderbird,             -safe-mode)
  xapp(seamonkey,               -safe-mode)
  xapp(mozilla,                 -safe-mode)
  xapp(netscape,                -safe-mode)

  pin_menu
}

Menu                            "app:scm"
{
  "Perforce"                    f.title
  xapp_misc(p4v)

  "Mercurial"                   f.title
  # These have to be started inside a repository.
  #"hgk"                         spawn_misc(hg view)
  #"hgtk"                        spawn_misc(hgtk log)
  #xapp_misc(qct)
  xapp_misc(gwsmhg)

  "Git"                         f.title
  xapp_misc(git-gui)
  xapp_misc(gitk)
  xapp_misc(qgit)

  "Bazaar"                      f.title
  xapp_misc(olive-gtk)

  "Subversion"                  f.title
  xapp_misc(rapidsvn)
  xapp_misc(subcommander)
  xapp_misc(submerge)
  xapp_misc(viewvc)

  "CVS"                         f.title
  xapp_misc(cervisia)
  xapp_misc(lincvs)
  xapp_misc(tkcvs)

  "Misc"                        f.title
  xapp_misc(diffuse)

  pin_menu
}

Menu                            "app:xscreensaver"
{
  "X Screen Saver"              f.title
  "Start"                       spawn_misc(nice -19 xscreensaver)
  "Restart"                     spawn_misc(xscreensaver-command -restart)
  "Activate"                    spawn_misc(xscreensaver-command -activate)
  "Exit"                        spawn_misc(xscreensaver-command -exit)
  menu_separator
  "Next"                        spawn_misc(xscreensaver-command -next)
  "Prev"                        spawn_misc(xscreensaver-command -prev)
  menu_separator
  "Throttle"                    spawn_misc(xscreensaver-command -throttle)
  "Unthrottle"                  spawn_misc(xscreensaver-command -unthrottle)
  menu_separator
  "Prefs"                       spawn_misc(xscreensaver-command -prefs)
  "Demo"                        spawn_misc(xscreensaver-command -demo)
  "Lock"                        spawn_misc(xscreensaver-command -lock)
  pin_menu
}

Menu                            "managerial"
{
  "Managerial"                  f.title
  "Window Ops"                  f.menu "window_ops"
IF_CTWM(`
  "CTWM Window Ops"             f.menu "ctwm_window_ops"
  "Workspace Manager"           f.menu "ctwm_workspace_ops"
  "Pixmap Animation"            f.menu "ctwm_animation"
')
  menu_separator
  "Show Icon MGR"               f.showiconmgr
  "Hide Icon MGR"               f.hideiconmgr
IF_CTWM_MINVER(3,7,`
  "Sort Icon MGR"               f.sorticonmgr
')
  menu_separator
  "Refresh root"                f.refresh
  menu_separator
  "Rehash xresources"           f.function "preprocess_and_rehash_xresources"
  "Reset window mgr"            f.restart
  "Restart window mgr"          f.function "preprocess_and_restart_wmgr"
  pin_menu
}

Menu                            "title_ops"
{
  "Iconify      "               f.iconify
  "Raise"                       f.raise
  "Lower"                       f.lower
  "Move"                        f.move
  "Resize"                      f.resize
  menu_separator
  "Close"                       f.delete
}

Menu                            "window_ops"
{
  "Window Menu"                 f.title
IF_CTWM(`
  "CTWM Window Ops"             f.menu "ctwm_window_ops"
')
  "Iconify"                     f.iconify
IF_CTWM(`
  "Squeeze"                     f.squeeze
')
  "Raise"                       f.raise
  "Lower"                       f.lower
  "Toggle AutoRaise"            f.autoraise
  menu_separator
  "Move (onscreen)"             f.move
  "Move (anywhere)"             f.forcemove
IF_CTWM(`
  "Move (tile)"                 f.movepack
  "Move (tile, push)"           f.movepush
  "Hypermove (between ctwms)"   f.hypermove
')
  menu_separator
  "Resize"                      f.resize
  "Full screen"                 f.fullzoom
  "Tall screen"                 f.zoom
  "Wide screen"                 f.horizoom
IF_CTWM(`
  "Original size"               f.initsize
  menu_separator
  "Fill Top"                    f.fill "top"
  "Fill Bottom"                 f.fill "bottom"
  "Fill Left"                   f.fill "left"
  "Fill Right"                  f.fill "right"
IF_CTWM_MINVER(3,7,`
  "Fill Vertical"               f.fill "vertical"
')')
  menu_separator
  "Identify"                    f.identify
IF_CTWM(`
  "Refresh"                     f.winrefresh
')
  "Focus keyboard"              f.focus
  "Unfocus keyboard"            f.unfocus
  "Save"                        f.saveyourself
  "Delete"                      f.menu "delete_window"
  "Destroy"                     f.menu "destroy_window"
  pin_menu
}

Menu                            "dangerous_window_ops"
{
  "Dangerous Operations"        f.title
  ""                            f.nop
  ""                            f.nop
  "Delete this window"          f.delete
  ""                            f.nop
  ""                            f.nop
  "Destroy this process"        f.destroy
}

IF_CTWM(`
Menu                            "ctwm_window_ops"
{
  "CTWM Window Ops"             f.title
  "Adopt Window"                f.adoptwindow
  "Unoccupy Workspace"          f.vanish
  "Occupy Workspace"            f.occupy
  "Occupy All Workspaces"       f.occupyall
  menu_separator
  "Default Colormap"            f.colormap "default"
  "Next    Colormap"            f.colormap "next"
  "Prev    Colormap"            f.colormap "prev"
  pin_menu
}

Menu                            "ctwm_workspace_ops"
{
  "WorkSpace Manager Ops"       f.title
  "Show Workspace Mgr"          f.showworkspacemgr
  "Hide Workspace Mgr"          f.hideworkspacemgr
  menu_separator
  "Mgr Button State"            f.setbuttonsstate
  "Mgr Map State"               f.setmapstate
  "Mgr Toggle State"            f.togglestate
}

Menu                            "ctwm_animation"
{
  "Animation Ops"               f.title
  "Start animations"            f.startanimation
  "Freeze animations"           f.stopanimation
  "Increment speed"             f.speedupanimation
  "Decrement speed"             f.slowdownanimation
}')  dnl end ctwm menus

Menu                            "delete_window"
{
  "Really Delete?"              f.title
  ""                            f.nop
  "If you wish to"              f.nop
  "close just one"              f.nop
  "selected window,"            f.nop
  "confirm below."              f.nop
  ""                            f.nop
  ""                            f.nop
  ""                            f.nop
  "   CONFIRM"                  f.delete
}

Menu                            "destroy_window"
{
  "Really Destroy?"             f.title
  ""                            f.nop
  "This operation will"         f.nop
  "close all windows"           f.nop
  "associated with this"        f.nop
  "process and probably"        f.nop
  "terminate the process."      f.nop
  ""                            f.nop
  ""                            f.nop
  ""                            f.nop
  "   CONFIRM"                  f.destroy
}

Menu                            "misc"
{
  "Miscellaneous"               f.title
  "PulseAudio"                  f.menu "pulseaudio"
  "X Preferences"               f.menu "xpreferences"
  "Keyboard modes"              f.menu "kbd_mode"
  "BG menu"                     f.menu "BGMenu"
}

Menu                            "pulseaudio"
{
  "PulseAudio"                  f.title
  "Volume Control"              spawn_misc(pavucontrol)
  "Volume Meter"                spawn_misc(pavumeter)
  "Equalizer"                   spawn_misc(pulseaudio-equalizer-gtk)
  menu_separator
  "Preferences"                 spawn_misc(paprefs)
  "Manager"                     spawn_misc(paman)
  "Device Chooser"              spawn_misc(padevchooser)
  menu_separator
  mkxterm(`pacmd',,,`-e pacmd')
  pin_menu
}

Menu                            "xpreferences"
{
  "X Preferences"               f.title
  "Autorepeat On"               spawn(xset r on)
  "Autorepeat Off"              spawn(xset r off)
  menu_separator
  "Bell 100%"                   spawn(xset b 100)
  "Bell 75%"                    spawn(xset b 75)
  "Bell 50%"                    spawn(xset b 50)
  "Bell 25%"                    spawn(xset b 25)
  "Bell Off"                    spawn(xset b off)
  menu_separator
  "Bug Compat On"               spawn(xset  bc)
  "Bug Compat Off"              spawn(xset -bc)
  menu_separator
  "DPMS On"                     spawn(xset -dpms)
  "DPMS Off"                    spawn(xset +dpms)
  menu_separator
  "Key Click 100%"              spawn(xset c 100)
  "Key Click 50%"               spawn(xset c 50)
  "Key Click Off"               spawn(xset c off)
  menu_separator
  "Mouse Fast"                  spawn(xset m 5 1)
  "Mouse Semi-Fast"             spawn(xset m 4 1)
  "Mouse Normal"                spawn(xset m 3 1)
  "Mouse Semi-Slow"             spawn(xset m 2 1)
  "Mouse Slow"                  spawn(xset m 1 1)
  "Mouse Really Slow"           spawn(xset m 1/3 1)
  menu_separator
  "Led 1 (Caps Lock) on"        spawn(xset  led 1)
  "Led 1 (Caps Lock) off"       spawn(xset -led 1)
  menu_separator
  "Led 2 (Num Lock) on"         spawn(xset  led 2)
  "Led 2 (Num Lock) off"        spawn(xset -led 2)
  menu_separator
  "Led 3 (Scroll Lock) on"      spawn(xset  led 3)
  "Led 3 (Scroll Lock) off"     spawn(xset -led 3)
  pin_menu
}

Menu                            "kbd_mode"
{
  "Keyboard modes"              f.title
  "Dvorak"                      spawn(xdvorak)
  "Qwerty"                      spawn(xdvorak)
  menu_separator
  "[-a] Ascii (xlate)"          spawn(kbd_mode -a)
  "[-k] Keycode (medium raw)"   spawn(kbd_mode -k)
  "[-s] Scancode (raw)"         spawn(kbd_mode -s)
  "[-u] UTF-8 (unicode)"        spawn(kbd_mode -u)
ifdef(`SUN',`
  # I have no idea what these actually mean, but sun keyboards get
  # wedged sometimes, so here they all are on a menu where I can get at them.
  # According to the kb(4s) man page, they have the following meanings for
  # the kb streams module.
  # TR_ASCII (-a): ISO 8859/1 codes are reported.
  # TR_NONE  (-n): Keyboard translation is turned off and up/down key codes
  #   are reported.
  # TR_EVENT (-e): firm_events are reported (see SunView 1 Programmers
  #   Guide).
  # TR_UNTRANS_EVENT (-u): firm_events containing unencoded keystation
  #   codes are reported for all input events within the window system.
  "Sun kbd modes"               f.title
  "[-a] Ascii (Console)"        spawn(kbd_mode -a)
  "[-e] Firm events (Sunview)"  spawn(kbd_mode -e)
  "[-n] Up/Down"                spawn(kbd_mode -n)
  "[-u] Undecoded (X)"          spawn(kbd_mode -u)
')
  menu_separator
  xapp(xkeycaps)
}

Menu                            "BGMenu"
{
  "BG Menu"                     f.title
  "Default background"          spawn(xsetroot)
  "Black background"            spawn(xsetroot -solid black)
  "Grey background"             spawn(xsetroot -solid grey )
  "White background"            spawn(xsetroot -solid white)
  menu_separator
  "Space background"            spawn(xspace)
  "Earth background"            spawn(fancy-xearth)
  "Moon  background"            spawn(xphoon -t 720)
}

Menu                            "quit"
{
  "Really Quit?"                f.title
  ""                            f.nop
  "If you wish to"              f.nop
  "quit, select"                f.nop
  "the ``confirm''"             f.nop
  "button below."               f.nop
  ""                            f.nop
  ""                            f.nop
  ""                            f.nop
  "   CONFIRM"                  f.quit
}

divert(-1)dnl

######
## Gnome menus
######

divert(0)dnl

Menu                            "app:gnome"
{
  "Gnome"                       f.title
  "Config"                      f.menu "app:gnome:config"
  "Games"                       f.menu "app:gnome:games"
}

divert(-1)dnl

######
## Gnome menus
######

divert(0)dnl

Menu                            "app:gnome:config"
{
  "Gnome Config"                f.title
  "Control Center"              spawn_gnome(gnome-control-center)
  ""                            f.nop
  "Appearance"                  spawn_gnome(gnome-appearance-properties)
  "Preferred Apps"              spawn_gnome(gnome-default-applications-properties)
  ""                            f.nop
  menu_separator
  xapp_gnome(gconf-editor)
}

Menu                            "app:gnome:games"
{
  "Gnome Games"                 f.title
  xapp_gnome(blackjack)
  xapp_gnome(glchess)
  xapp_gnome(glines)
  xapp_gnome(gnect)
  xapp_gnome(gnibbles)
  xapp_gnome(gnobots2)
  xapp_gnome(gnome-falling-blocks)
  xapp_gnome(gnome-sudoku)
  xapp_gnome(gnomine)
  xapp_gnome(gnotravex)
  xapp_gnome(gnotski)
  xapp_gnome(gtali)
  xapp_gnome(iagno)
  xapp_gnome(mahjongg)
  xapp_gnome(same-gnome)
  xapp_gnome(sol)
}

divert(-1)dnl

######
## KDE menus
######

divert(0)dnl

Menu                            "app:kde"
{
  "KDE"                         f.title
  "Admin"                       f.menu "app:kde:admin"
  "Network"                     f.menu "app:kde:network"
  "PIM"                         f.menu "app:kde:pim"
  "Util"                        f.menu "app:kde:util"
  "KOffice"                     f.menu "app:koffice"
  "Games"                       f.menu "app:kde:games"
}

Menu                            "app:kde:admin"
{
  "KDE Admin"                   f.title
  "KCron"                       spawn_kde(kcron)
  "KDat (Tape Backup Tool)"     spawn_kde(kdat)
  "KPackage"                    spawn_kde(kpackage)
  "KSysV (SysV Init Editor)"    spawn_kde(ksysv)
  "KUser (User Manager)"        spawn_kde(kuser)
}

Menu                            "app:kde:network"
{
  "KDE Network"                 f.title
  "fileshare"                   spawn_kde(kcmshell fileshare)
  xapp_kde(kget)
  "kio_lan"                     spawn_kde(kcmshell kcmkiolan)
  "knewsticker"                 spawn_kde(appletproxy knewsticker.desktop)
  xapp_kde(kopete)
  xapp_kde(kppp)
  xapp_kde(kppplogview)
  xapp_kde(krdc)
  "krfb"                        spawn_kde(kcmshell kcmkrfb)
  "lisa"                        spawn_kde(kcmshell kcmlisa)
  "ResLISa"                     spawn_kde(kcmshell kcmreslisa)
  "sambaconf"                   spawn_kde(kcmshell kcmsambaconf)
}

Menu                            "app:kde:pim"
{
  "KDE PIM"                     f.title
  "KOrganizer"                  spawn_kde(korganizer)
  "Kontact"                     spawn_kde(kontact)
  "KAddressBook"                spawn_kde(kaddressbook)
  "Groupware Wizard"            spawn_kde(groupwarewizard)
  menu_separator
  "KMail"                       spawn_kde(kmail)
  "Mail Alert"                  spawn_kde(korn)
  menu_separator
  "KNode (news reader)"         spawn_kde(knode)
  "Akregator (RSS Feed Reader)" spawn_kde(akregator)
  menu_separator
  "KAlarm"                      spawn_kde(kalarm)
  "KArm (time tracker)"         spawn_kde(karm)
  menu_separator
  xapp_kde(knotes)
  "KTnef"                       spawn_kde(ktnef)
  xapp_kde(kpilot)
}

Menu                            "app:kde:games"
{
  "KDE Games"                   f.title
  xapp_kde(atlantik)
  xapp_kde(bovo)
  xapp_kde(kasteroids)
  xapp_kde(katomic)
  xapp_kde(kbackgammon)
  xapp_kde(kbattleship)
  xapp_kde(kblackbox)
  xapp_kde(kblocks)
  xapp_kde(kbounce)
  xapp_kde(kbreakout)
  xapp_kde(kdiamond)
  xapp_kde(kenolaba)
  xapp_kde(kfouleggs)
  xapp_kde(kfourinline)
  xapp_kde(kgoldrunner)
  xapp_kde(kiriki)
  xapp_kde(kjumpingcube)
  xapp_kde(klickety)
  xapp_kde(klines)
  xapp_kde(kmahjongg)
  xapp_kde(kmines)
  xapp_kde(knetwalk)
  xapp_kde(kolf)
  xapp_kde(kollision)
  xapp_kde(konquest)
  xapp_kde(kpat)
  xapp_kde(kpoker)
  xapp_kde(kreversi)
  xapp_kde(ksame)
  xapp_kde(kshisen)
  xapp_kde(ksirk)
  xapp_kde(ksirtet)
  xapp_kde(ksmiletris)
  xapp_kde(ksnake)
  xapp_kde(ksokoban)
  xapp_kde(kspaceduel)
  xapp_kde(ksquares)
  xapp_kde(ksudoku)
  xapp_kde(ktron)
  xapp_kde(ktuberling)
  xapp_kde(kubrick)
  xapp_kde(kwin4)
  xapp_kde(lskat)
}

Menu                            "app:kde:graphics"
{
  "KDE Graphics"                f.title

  "Color Chooser"               spawn_kde(kcolorchooser)
  "Color Palette Editor"        spawn_kde(kcoloredit)
  "Screen Ruler"                spawn_kde(kruler)

  menu_separator
  "Screen Capture"              spawn_kde(ksnapshot)
  "KolourPaint"                 spawn_kde(kolourpaint)
  "Icon Editor"                 spawn_kde(kiconedit)
  "Kooka (Scan & OCR)"          spawn_kde(kooka)

  menu_separator
  "Kuickshow (Image viewer)"    spawn_kde(kuickshow)
  "KView"                       spawn_kde(kview)

  menu_separator
  "KPDF"                        spawn_kde(kpdf)
  "KGhostView"                  spawn_kde(kghostview)
  "DVI Viewer"                  spawn_kde(kdvi)
}

Menu                            "app:kde:util"
{
  "KDE Utils"                   f.title
  "Character Selector"          spawn_kde(kcharselect)
  "KRegExpEditor"               spawn_kde(kregexpeditor)
  "KTimer"                      spawn_kde(ktimer)
  menu_separator
  "Signature Editor"            spawn_kde(ksig)
  "KGpg"                        spawn_kde(kgpg)
  menu_separator
  "Ark (archiver)"              spawn_kde(ark)
  "KEdit"                       spawn_kde(kedit)
  "KHexEdit"                    spawn_kde(khexedit)
  "KCalc"                       spawn_kde(kcalc)
  "KJots (note taker)"          spawn_kde(kjots)
  menu_separator
  "KDE Wallet"                  spawn_kde(kwalletmanager)
  "KDE Wallet Config"           spawn_kde(kcmshell kwalletconfig)
  menu_separator
  "KDE LIRC Server"             spawn_kde(irkick)
  "KDE LIRC Config"             spawn_kde(kcmshell kcmlirc)
  "PCMCIA status"               spawn_kde(kcmshell pcmcia)
  "IBM Thinkpad Config"         spawn_kde(kcmshell thinkpad)
  menu_separator
  "Kdiskfree"                   spawn_kde(kdf)
  "Kwikdisk"                    spawn_kde(kwikdisk)
  "Storage Devices"             spawn_kde(kcmshell kcmdf)
  "Floppy Formatter"            spawn_kde(kfloppy)
}

divert(-1)

######
## Host menu construction
######

define(`splode_hosts',
  ``bacon-von-munchausen',
   `bride-of-frankenswine.prv',
   `days-of-swine-and-roses',
   `gedanken-donuts.prv',
   `mini-waffles.prv',
   `piglet.prv',
   `obey-chomsky.prv',
   `scones-of-silence.prv',
   `tasty-breakfast-squids.prv',
   `trabanten-schwein.prv',
   `unexploded-cow.prv'')

divert(0)dnl
Menu                            "hosts:splode.com"
{
  "splode.com"                  f.title
  "hosts -ut"                   f.menu "hosts-ut:splode.com"
  foreach(`HOST',
          `rxterm(`HOST',`HOST`'.splode.com')',
          splode_hosts)dnl
}

Menu                            "hosts-ut:splode.com"
{
  "splode.com (-ut)"            f.title
  foreach(`HOST',
          `rxterm(`HOST',`HOST`'.splode.com',`-ut')',
          splode_hosts)dnl
}

divert(-1)

# twmrc.m4 ends here
