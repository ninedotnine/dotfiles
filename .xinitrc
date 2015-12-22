#!/bin/bash
. ~/.fehbg

# set x resources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
urxvtd -q -o -f

dwmstatus --daemon

tilda > /dev/null 2>&1 &

# compositing manager, makes shit transparent
xcompmgr & 

# hides the mouse when not in use
unclutter -noevents -idle 5 -not "tilda" &

# remap caps lock to be an escape key
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'

# turn off the touchpad on multivac
# FIXME: only do this if a mouse is plugged in
if [[ "$HOSTNAME" = "multivac" ]]; then
    xinput --disable "DLL064D:00 06CB:2985"
fi

# turn off default screensaver
#xset s off
# turn on screensaver after 25 minutes
#xset s 1500

# i always start this anyway...
if [[ "$HOSTNAME" = "arch" ]]; then
    thunderbird &
fi

urxvtc &

firefox &

while true; do
    dwm >> /tmp/dwm.log 2>&1 
done 
# exec dwm >> /tmp/dwm.log 2>&1
