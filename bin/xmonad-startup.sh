#!/bin/bash

# Startup apps for xmonad
# http://supernerd0.blogspot.com/2010/07/xmonad-xmobar-lxpanel.html

function s {
    ISRUNNING=`ps ax | grep -v grep | grep $1`
    if [ "" = "$ISRUNNING" ]
    then
        START="$1 $2"
        echo starting $START
        `$START > /dev/null &`
    fi
}

s "xscreensaver" "-no-splash"
#s "gnome-keyring-daemon" "--start --components="
s "nm-applet" "--sm-disable"

s "dropboxd"


