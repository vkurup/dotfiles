#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xrdb -merge .Xresources
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --alpha 0 --tint 0x333333 --height 17 &
xsetroot -solid black
xsetroot -cursor_name left_ptr
xcompmgr -c -f -D 5 &
unclutter &
xbindkeys
exec ck-launch-session xmonad
