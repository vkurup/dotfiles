#!/bin/bash

str=`amixer get Master | grep 'Front Left:'`
level=`echo $str | sed 's/^.*\[\(.*\)%\].*/\1/'`
mute=`echo $str | grep off`

if [ "$mute" ]; then
    echo "${level}%M"
else
    echo "${level}% "
fi

