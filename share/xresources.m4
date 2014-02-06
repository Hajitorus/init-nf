divert(-1)dnl
! xresources.m4 --- m4 template to generate X resource strings for xrdb
! Author: Noah Friedman <friedman@splode.com>
! Created: 1991-10-28
! Public domain

! $Id: xresources.m4,v 1.61 2011/11/23 05:19:44 friedman Exp $

! Commentary:

! Requires GNU m4.

! Code:

changequote([,])

! Specify `!' as start comment delimiter (to avoid confusing m4 if comments
! contain otherwise bad syntax.
changecom([!])

! This is a GNU extension; if missing, define it so as to always fail.
ifdef([regexp],[],[define([regexp],[-1])])

define([guardcommas],[[$*]])

undefine([include])
! GNU m4 has an include macro.  Disable it. (note that this comment is
! below the actual command so that we can use the word `include' in this
! comment.


divert(0)dnl

! GLOBAL resources

! Correct motif lossage that prevents Delete keysym from being interpreted
! as a delete character in some applications.  It's most notable (and
! irritating) in mosaic.
*XmText.translations: #override\n\
      <Key>osfDelete: delete-previous-character()
*XmTextField.translations: #override\n\
      <Key>osfDelete: delete-previous-character()

*reverseVideo: true

! EMACS
! many of these are only useful for V19
Emacs*reverseVideo:     off
Emacs*Foreground:       white
Emacs*Background:       black
Emacs*borderColor:      black
Emacs.borderWidth:      1
!Emacs*Title:           emacs
!Emacs*iconName:        emacs
Emacs*internalBorder:   1
Emacs*borderWidth:      1

! Emacs 21 face attributes.
! See variable `face-x-resources' for a list of valid attribute tags.
Emacs.fringe.attributeBackground: black

! MoTV, from xawtv package
MoTV.geometry: 320x240-199+21
MoTV.xvport: 62
MoTV.xvVideo: true

! NETSCAPE 4.x
Netscape.Navigator.borderWidth:         1
Netscape.Navigator.geometry:            +0-0
Netscape*dontSaveGeometryPrefs:         True
Netscape*ignoreGeometryPrefs:           False
Netscape*noAboutSplash:                 True
Netscape*sessionManagement:             False
Netscape*dontForceWindowStacking:       True
Netscape*confirmExit:                   True
! This improves the spacing of the buttons on the navigator toolbar in
! Netscape 4.5 and later.
Netscape*toolBar*XmSeparator.width:     1
! Fuckwits.  This ought to be the default.
! An even bigger fuckwit decided no longer to support
! this in Communicator 4.0.  Eat me.
Netscape*blinkingEnabled:               False

Netscape*browserGlobalTranslations:   #override    \n\
  ~Ctrl ~Shift          <Btn4Down>:     LineUp()   \n\
  Ctrl                  <Btn4Down>:     PageUp()   \n\
  Shift                 <Btn4Down>:     LineUp(5)   \n\
  ~Ctrl ~Shift          <Btn5Down>:     LineDown() \n\
  Ctrl                  <Btn5Down>:     PageDown() \n\
  Shift                 <Btn5Down>:     LineDown(5) \n\
  Meta ~Ctrl ~Shift<Key>f:              xfeDoCommand(findInObject)\n\
    Alt ~Ctrl ~Shift<Key>f:             xfeDoCommand(findInObject)\n\
  Meta ~Ctrl ~Shift<Key>g:              xfeDoCommand(findAgain) \n\
   Alt ~Ctrl ~Shift<Key>g:              xfeDoCommand(findAgain) \n\
  Meta ~Ctrl ~Shift<Key>o:              xfeDoCommand(openPage)  \n\
   Alt ~Ctrl ~Shift<Key>o:              xfeDoCommand(openPage)  \n\
  Meta ~Ctrl  Shift<Key>o:              xfeDoCommand(openPageChooseFile)\n\
   Alt ~Ctrl  Shift<Key>o:              xfeDoCommand(openPageChooseFile)\n\
  <Key>Escape:                          xfeDoCommand(stopLoading)\n

rvplayer*reverseVideo: false

! XBIFF
xbiff*flip:             true
xbiff*fullPixmap:       HOME/lib/bitmaps/xbm/spam.d.xbm
xbiff*emptyPixmap:      HOME/lib/bitmaps/xbm/spam-empty.d.xbm
!xbiff*fullPixmap:      /usr/include/X11/bitmaps/flagup
!xbiff*emptyPixmap:     /usr/include/X11/bitmaps/flagdown
xbiff*update:           30
xbiff*volume:           50

! XCLOCK
!xclock*width:          70
!xclock*height:         70
xclock*width:           48
xclock*height:          48
xclock*update:          60
xclock*analog:          true
xclock*padding:         2
xclock*reverseVideo:    true
xclock*render:          false

! Non-render color defaults
xclock*foreground:      white
xclock*background:      black
xclock*hands:           white

! Render color defaults
xclock*hourColor:    white
xclock*minuteColor:  white
xclock*secondColor:  white
xclock*majorColor:   white
xclock*minorColor:   white

! XLOAD
! Some variants seem to use `load' resource strings instead.
define([xload],[
[xload]$1:      $2
[load]$1:       $2])
xload([*width],      [97])
xload([*height],     [40])
xload([*interval],   [10])
xload([*font],       [6x13])
xload([*showLabel],  [true])
xload([*foreground], [white])
xload([*background], [black])
ifdef([COLOR],[
xload([*highlight],  [grey])
])

! XPDF
xpdf*reverseVideo: false


! XSCREENSAVER
xscreensaver*reverseVideo: false

! XTERM
XTerm.minBufSize:                       8192
XTerm.maxBufSize:                       131072
XTerm.scaleHeight:                      1.0
XTerm.toolBar:                          off
XTerm.waitForMap:                       off

XTerm*VT100.geometry:                   80x24

! If backarrowKey is false, backspace key is mapped to DEL
! This might only work for older versions of xterm; see
! backarrowKeyIsErase and ptyInitialErase for newer method?
XTerm*backarrowKey:                     off
XTerm*backarrowKeyIsErase:              true
XTerm*ptyInitialErase:                  on
XTerm.ttyModes:                         erase ^?
! specifies whether the keypad delete key should send DEL
! or VT220 Remove escape sequence.
XTerm*deleteIsDEL:                      true

XTerm*eightBitControl:                  off
XTerm*eightBitInput:                    false
XTerm*eightBitOutput:                   true
! when in utf8 mode, we need to send escape for meta
XTerm*metaSendsEscape:                  true
XTerm*altSendsEscape:                   true
XTerm*altIsNotMeta:                     true

XTerm*allowC1Printable:                 true
XTerm*allowColorOps:                    true
XTerm*allowFontOps:                     true
XTerm*allowScrollLock:                  false
XTerm*allowSendEvents:                  false
XTerm*allowTcapOps:                     true
XTerm*allowTitleOps:                    true
XTerm*allowWindowOps:                   true

XTerm*alwaysHighlight:                  false
XTerm*autoWrap:                         on
XTerm*bellSuppressTime:                 200
XTerm*c132:                             on
XTerm*cutNewline:                       false
XTerm*cutToBeginningOfLine:             true
XTerm*highlightSelection:               true
XTerm*i18nSelections:                   true
XTerm*internalBorder:                   2
XTerm*jumpScroll:                       on
XTerm*fastScroll:                       on
XTerm*keepSelection:                    true
XTerm*loginShell:                       true
XTerm*multiScroll:                      on
XTerm*reverseWrap:                      on
XTerm*rightScrollBar:                   on
XTerm*saveLines:                        1024
XTerm*scrollBar:                        off
XTerm*scrollInput:                      true
XTerm*scrollKey:                        true
XTerm*scrollLines:                      1
XTerm*scrollTtyOutput:                  false
XTerm*selectToClipboard:                true
XTerm*signalInhibit:                    false
XTerm*titeInhibit:                      true
XTerm*trimSelection:                    on
XTerm*visualBell:                       on
XTerm*zIconBeep:                        0
XTerm*VT100.showWrapMarks:              true

XTerm*VT100.pointerMode:                1

XTerm*VT100.cursorBlink:                off
XTerm*VT100.cursorOffTime:              450
XTerm*VT100.cursorOnTime:               450
XTerm*VT100.cursorUnderLine:            false

XTerm*VT100.on2Clicks:                  word
XTerm*VT100.on3Clicks:                  line
XTerm*VT100.on4Clicks:                  group
XTerm*VT100.on5Clicks:                  none

XTerm.menuLocale:                       C
XTerm*locale:                           false
XTerm*VT100.utf8:                       1
XTerm*VT100.utf8Fonts:                  default
XTerm*VT100.utf8Latin1:                 false
XTerm*VT100.utf8Title:                  true
XTerm*VT100.combiningChars:             4
XTerm*VT100.showMissingGlyphs:          true
XTerm*VT100.fontDoublesize:             false

XTerm*SimpleMenu*menuLabel.font:        -*-helvetica-bold-r-normal--12-*-*-*-*-*-iso8859-1
XTerm*SimpleMenu*menuLabel.vertSpace:   100
XTerm*SimpleMenu*HorizontalMargins:     16
XTerm*SimpleMenu*SmeBSB.font:           -*-helvetica-medium-r-normal--12-*-*-*-*-*-iso8859-1
XTerm*SimpleMenu*Sme.height:            16
XTerm*SimpleMenu*borderWidth:           0

ifelse(eval(PLANES > 8),[1],[

XTerm*VT100.colorAttrMode:              on
XTerm*VT100.colorMode:                  on
XTerm*VT100.boldColors:                 on
XTerm*VT100.dynamicColors:              on

! These colors are tuned to match emacs font-lock defaults
XTerm*VT100*color0:                     black
XTerm*VT100*color1:                     red1
XTerm*VT100*color2:                     green
XTerm*VT100*color3:                     goldenrod
XTerm*VT100*color4:                     light steel blue
XTerm*VT100*color5:                     magenta
XTerm*VT100*color6:                     cyan3
XTerm*VT100*color7:                     gray90

! These colors are used when the colors above are combined with the bold
! They are tuned to match various emacs font-lock defaults.
XTerm*VT100*color8:                     gray30
XTerm*VT100*color9:                     light salmon
XTerm*VT100*color10:                    light green
XTerm*VT100*color11:                    light goldenrod
XTerm*VT100*color12:                    light sky blue
XTerm*VT100*color13:                    violet
XTerm*VT100*color14:                    cyan
XTerm*VT100*color15:                    white

! Disabled.
XTerm*VT100*colorBDMode:                off
XTerm*VT100*colorBLMode:                off
XTerm*VT100*colorULMode:                off
XTerm*VT100*colorBD:                    gold
XTerm*VT100*colorBL:                    steel blue
XTerm*VT100*colorUL:                    violet
! This resource is confusing.  In xterm-160 what it means is that if the
! standard font and bold font are the same, do not overstrike.
! However, if the fonts do not match, overstrike anyway.
XTerm*VT100*boldMode:                   off

XTerm*Form.menubar.background:          gray
XTerm*Form.menubar*background:          gray
XTerm*Form.menubar.foreground:          gray15
XTerm*Form.menubar*foreground:          gray15
XTerm*Form.background:                  gray
XTerm*form.background:                  gray

XTerm*VT100.scrollbar.thumb:            vlines2
XTerm*VT100.scrollbar.width:            14
XTerm*VT100.scrollbar.background:       gray60
XTerm*VT100.scrollbar.foreground:       rgb:a/5/5
XTerm*VT100.scrollbar.borderWidth:      0
XTerm*VT100.scrollbar.displayList:\
        foreground      gray90;\
        lines           1,-1,-1,-1,-1,1;\
        foreground      gray60;\
        lines           -1,0,0,0,0,-1

! Athena Widget 7.0 libraries implement gradients, but some derivatives
! (e.g. XawPlus) do not yet.
!xterm*mainMenu*backgroundPixmap:       gradient:vertical?dimension=350&start=gray90&end=gray60
XTerm*mainMenu*foreground:              gray15
!xterm*vtMenu*backgroundPixmap:         gradient:vertical?dimension=445&start=gray90&end=gray60
XTerm*vtMenu*foreground:                gray15
!xterm*fontMenu*backgroundPixmap:       gradient:vertical?dimension=220&start=gray90&end=gray60
XTerm*fontMenu*foreground:              gray15

])dnl  ([PLANES] > 8)

! Define freetype font resources, but don't use them by default.
! This does allow switching using the vtFonts->TrueType Fonts menu option.
XTerm*renderFont:                       false
XTerm*VT100.faceName:                   DejaVu Sans Mono
XTerm*VT100.faceNameDoublesize:         DejaVu Sans Mono
XTerm*VT100.faceSize:                   10
XTerm*VT100.faceSize1:                   8
XTerm*VT100.faceSize2:                  12
XTerm*VT100.faceSize3:                  14
XTerm*VT100.faceSize4:                  16
XTerm*VT100.faceSize5:                  18
XTerm*VT100.faceSize6:                  20

XTerm*VT100.WideFont:                   -efont-fixed-medium-r-normal--12-120-75-75-c-120-iso10646-1
!XTerm*VT100.WideFont:                  -wenquanyi-unibit-medium-r-normal--16-160-75-75-c-80-iso10646-1

XTerm*fontMenu.fontdefault.Label:       Default
XTerm*fontMenu.font1.Label:             Tiny
XTerm*fontMenu.font2.Label:             Small
XTerm*fontMenu.font3.Label:             Medium
XTerm*fontMenu.font4.Label:             Large
XTerm*fontMenu.font5.Label:             Huge
XTerm*fontMenu.font6.Label:             Moby

!xterm*VT100.font:                      fixed
!xterm*VT100.boldFont:                  fixed
XTerm*VT100.font1:                      5x8
XTerm*VT100.font2:                      6x10
XTerm*VT100.font3:                      8x13
XTerm*VT100.font4:                      9x15
XTerm*VT100.font5:                      10x20
XTerm*VT100.font6:                      12x24

XTerm*VT100.std.font:                   fixed
XTerm*VT100.std.boldFont:               fixed
XTerm*VT100.std.font1:                  5x8
XTerm*VT100.std.font2:                  6x10
XTerm*VT100.std.font3:                  8x13
XTerm*VT100.std.font4:                  9x15
XTerm*VT100.std.font5:                  10x20
XTerm*VT100.std.font6:                  12x24

XTerm*VT100.utf8Fonts.font:             terminus6x12u
XTerm*VT100.utf8Fonts.font2:            terminus8x14u
XTerm*VT100.utf8Fonts.font3:            terminus10x20u
XTerm*VT100.utf8Fonts.font4:            terminus12x24u
XTerm*VT100.utf8Fonts.font5:            terminus14x28u
XTerm*VT100.utf8Fonts.font6:            terminus16x32u

define([terminusfnt],[["-*-terminus-medium-r-normal--$2-$2][0-72-72-c-$1][0-iso10646-1"]])dnl
XTerm*VT100.terminus.font:              terminusfnt(6,12)
XTerm*VT100.terminus.boldFont:          terminusfnt(6,12)
XTerm*VT100.terminus.font1:             terminusfnt(8,14)
XTerm*VT100.terminus.font2:             terminusfnt(8,16)
XTerm*VT100.terminus.font3:             terminusfnt(10,20)
XTerm*VT100.terminus.font4:             terminusfnt(12,24)
XTerm*VT100.terminus.font5:             terminusfnt(14,28)
XTerm*VT100.terminus.font6:             terminusfnt(16,32)

define([verafnt],[["-*-bitstream vera sans mono-medium-r-normal--$2-*-*-*-m-$1][0-iso10646-1"]])dnl
XTerm*VT100.vera.font:                  verafnt(6,13)
XTerm*VT100.vera.boldFont:              verafnt(6,13)
XTerm*VT100.vera.font1:                 verafnt(5,8)
XTerm*VT100.vera.font2:                 verafnt(6,10)
XTerm*VT100.vera.font3:                 verafnt(8,13)
XTerm*VT100.vera.font4:                 verafnt(9,15)
XTerm*VT100.vera.font5:                 verafnt(10,20)
XTerm*VT100.vera.font6:                 verafnt(12,24)

!!!!

define([mkxtermKbTrans],[
XTerm*VT100.[$1]font[$2]Keymap.translations:\
  ~Shift Ctrl <KeyPress> KP_Subtract        : set-vt-font(e, $4, $4) keymap([$1]font[$3]) \n\
  ~Shift Ctrl <KeyPress> KP_Add             : set-vt-font(e, $6, $6) keymap([$1]font[$5]) \n
])dnl

define([mkxtermFontCycle],[dnl
  mkxtermKbTrans([$1],[Font1],  [Font7], [$8],    [Font2], [$3])
  mkxtermKbTrans([$1],[Font2],  [Font1], [$2],    [Font3], [$4])
  mkxtermKbTrans([$1],[Font3],  [Font2], [$3],    [Font4], [$5])
  mkxtermKbTrans([$1],[Font4],  [Font3], [$4],    [Font5], [$6])
  mkxtermKbTrans([$1],[Font5],  [Font4], [$5],    [Font6], [$7])
  mkxtermKbTrans([$1],[Font6],  [Font5], [$6],    [Font7], [$8])
  mkxtermKbTrans([$1],[Font7],  [Font6], [$7],    [Font1], [$2])

  ifelse(eval($# > 9),[1],[
XTerm*VT100.[$1]fontKeymap.translations:\
  ~Shift ~Ctrl Meta <KeyPress> KP_Subtract  : set-vt-font(e, $10, $10) keymap([$9]font)      \n\
  ~Shift ~Ctrl Meta <KeyPress> KP_Add       : set-vt-font(e, $12, $12) keymap([$11]font)     \n\
  ~Shift  Ctrl      <KeyPress> KP_Subtract  : set-vt-font(e,  $8,  $8) keymap([$1]fontFont7) \n\
  ~Shift  Ctrl      <KeyPress> KP_Add       : set-vt-font(e,  $3,  $3) keymap([$1]fontFont2) \n
])])dnl

mkxtermFontCycle([std],
                 [5x8],
                 [6x10],
                 [6x13],
                 [8x13],
                 [9x15],
                 [10x20],
                 [12x24],

                 [vera],     [verafnt(6,13)],
                 [terminus], [terminusfnt(8,14)])

mkxtermFontCycle([terminus],
                 [terminusfnt(6,12)],
                 [terminusfnt(8,14)],
                 [terminusfnt(8,16)],
                 [terminusfnt(10,20)],
                 [terminusfnt(12,24)],
                 [terminusfnt(14,28)],
                 [terminusfnt(16,32)],

                 [std],  [6x13],
                 [vera], [verafnt(6,13)])

mkxtermFontCycle([vera],
                 [verafnt(5,8)],
                 [verafnt(6,10)],
                 [verafnt(6,13)],
                 [verafnt(8,14)],
                 [verafnt(9,15)],
                 [verafnt(10,20)],
                 [verafnt(12,24)],

                 [terminus], [terminusfnt(8,14)],
                 [std],      [6x13])

XTerm*VT100.Translations:   #override \n\
  Ctrl                  <Btn4Down>          : scroll-back(1,halfpage) \n\
  Shift                 <Btn4Down>          : scroll-back(1,line)     \n\
                        <Btn4Down>          : scroll-back(5,line)     \n\
  Ctrl                  <Btn5Down>          : scroll-forw(1,halfpage) \n\
  Shift                 <Btn5Down>          : scroll-forw(1,line)     \n\
                        <Btn5Down>          : scroll-forw(5,line)     \n\
  Meta              <KeyPress> Return       : string(0x1b) string(0x0d) \n\
  ~Shift ~Ctrl Meta <KeyPress> KP_Subtract  : set-vt-font(e, verafnt(6,13),     verafnt(6,13))     keymap(verafont)     \n\
  ~Shift ~Ctrl Meta <KeyPress> KP_Add       : set-vt-font(e, terminusfnt(8,14), terminusfnt(8,14)) keymap(terminusfont) \n\
  ~Shift  Ctrl      <KeyPress> KP_Subtract  : set-vt-font(e, 6x10, 6x10) keymap(stdfontFont2) \n\
  ~Shift  Ctrl      <KeyPress> KP_Add       : set-vt-font(e, 8x13, 8x13) keymap(stdfontFont4) \n\
   Shift       Meta <KeyPress> slash        : dabbrev-expand()      \n\
  ~Shift       Meta <KeyPress> slash        : insert-eight-bit()    \n\
ifdef([SUN],[
  ~Shift        <KeyPress> F23              : set-scrollbar(toggle) \n\
  ~Shift        <KeyPress> F27              : scroll-back(100,page) \n\
  ~Shift        <KeyPress> R13              : scroll-forw(100,page) \n\
  ~Shift  Meta  <KeyPress> F29              : scroll-back(1,page)   \n\
  ~Shift  Meta  <KeyPress> F35              : scroll-forw(1,page)   \n\
  ~Shift        <KeyPress> F29              : scroll-back(1,line)   \n\
  ~Shift        <KeyPress> F35              : scroll-forw(1,line)   \n\
   Shift        <KeyPress> KP_0             : string("0") \n\
])dnl
  ~Shift  Meta  <KeyPress> F12              : secure() \n\
  ~Shift  Meta  <KeyPress> Insert           : insert-selection(PRIMARY, CUT_BUFFER0, CLIPBOARD) \n

! This lets you pick up URLs, file names and e-mail address (99.9% of them
! anyhow) with a double left mouse click.
XTerm*VT100*charClass: 33:48,35:48,37-38:48,43:48,45-47:48,58-59:48,61:48,63-64:48,95:48,126:48
! Alternatively,
!XTerm*on2Clicks: regex [[:alpha:]]+://([[:alnum:]!#+,./=?@_~-]|(%[[:xdigit:]][[:xdigit:]]))+

! End of xterm resources

xv*reverseVideo:        false
Fig*reverseVideo:       false

Xcursor.size:           24
Xcursor.theme:          Bluecurve
Xcursor.theme_core:     true

Xft.dpi:                96.0
Xft.antialias:          1
Xft.hinting:            1
Xft.hintstyle:          hintfull
Xft.rgba:               rgb

! local variables:
! mode: conf-xdefaults
! comment-mode-alist: ((conf-xdefaults-mode ?! nil " "))
! end:

! xresources.m4 ends here
