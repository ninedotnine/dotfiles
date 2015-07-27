#!/bin/bash
. ~/.fehbg

# set x resources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
urxvtd -q -o -f

# dwm-status >> /tmp/dwm-status.log 2>&1 &
dwmstatus --daemon 2>> /tmp/dwmstatus.log

tilda > /dev/null 2>&1 &

# compositing manager, makes shit transparent
# xcompmgr & 

# hides the mouse when not in use
unclutter -noevents -idle 5 &

# remap caps lock to be an escape key
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'

# turn off default screensaver
#xset s off
# turn on screensaver after 25 minutes
#xset s 1500

# i always start this anyway...
if [[ "$HOSTNAME" = "arch" ]]; then
    thunderbird &
fi

while true; do
    dwm >> /tmp/dwm.log 2>&1 
done 
# exec dwm >> /tmp/dwm.log 2>&1
