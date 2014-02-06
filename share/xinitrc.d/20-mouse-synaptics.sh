#!/bin/sh
# $Id: 20-mouse-synaptics.sh,v 1.10 2012/02/18 08:36:23 friedman Exp $

start_syndaemon()
{
  if ! pidof syndaemon > /dev/null; then
    syndaemon -i 1 -d -k -R
  fi
}

dmi=/sys/class/dmi/id
if [ -f $dmi/product_version ]; then
  read product_version < $dmi/product_version
fi

case $product_version in
  ThinkPad* )
    # Use Fn-F8 (XF86TouchpadToggle) instead.
    #start_syndaemon

    synflags='
        FingerHigh=40               TapButton1=1
        FingerLow=24                TapButton2=2
        FingerPress=255             TapButton3=3

        MaxTapTime=0                ClickFinger1=1
        MaxTapMove=229              ClickFinger2=2
        MaxDoubleTapTime=180        ClickFinger3=3
        SingleTapTimeout=180
        ClickTime=100               PalmDetect=1
        CoastingSpeed=0             PalmMinWidth=10
                                    PalmMinZ=200
        VertEdgeScroll=1            LockedDrags=0
        VertScrollDelta=50          LockedDragTimeout=5000
        VertTwoFingerScroll=1       TapAndDragGesture=1

        HorizEdgeScroll=1           EmulateTwoFingerMinZ=20
        HorizScrollDelta=50         EmulateTwoFingerMinW=5
        HorizTwoFingerScroll=1      EmulateMidButtonTime=75

        RTCornerButton=0            LTCornerButton=0
        RBCornerButton=0            LBCornerButton=0
    '
    # MaxTapTime=0 disables tapping on thinkpad trackpad
    synclient $synflags ;;

  *"Ideapad S10"* )
    start_syndaemon

    synflags='
        FingerHigh=50               TapButton1=1
        FingerLow=40                TapButton2=2
        FingerPress=255             TapButton3=3

        MaxTapTime=180              ClickFinger1=1
        MaxTapMove=229              ClickFinger2=2
        MaxDoubleTapTime=200        ClickFinger3=3
        SingleTapTimeout=180
        ClickTime=100               PalmDetect=1
                                    PalmMinZ=100
        VertEdgeScroll=1            LockedDrags=1
        VertScrollDelta=120         LockedDragTimeout=10000
        VertTwoFingerScroll=0       TapAndDragGesture=1

        HorizEdgeScroll=0           EmulateTwoFingerMinZ=20
        HorizScrollDelta=120        EmulateTwoFingerMinW=5
        HorizTwoFingerScroll=1      EmulateMidButtonTime=75

        LBCornerButton=1
        LTCornerButton=2
        RBCornerButton=3
    '
    synclient $synflags ;;
esac

# eof
