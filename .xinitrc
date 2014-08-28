#!/bin/bash
. ~/.fehbg

# set x resources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# dwm-status >> /tmp/dwm-status.log 2>&1 &
# ~/bin/c/dwm-status.d/testing-dwmstatus --daemon
dwm-status --daemon

#sudo opendns &

# hexchat &
# firefox &

export BROWSER=firefox

# don't need to loop this, i just changed a tilda setting
tilda > /dev/null 2>&1 &
# tilda > /dev/null 2>&1 &

# compositing manager, makes shit transparent
# xcompmgr & 

# hides the mouse when not in use
unclutter -noevents -idle 5 &

# remap caps lock to be an escape key
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'

# add terminus to fonts
# does this do anything? 
# xset +fp /usr/share/fonts/local
xset fp rehash

# turn off default screensaver
#xset s off
# turn on screensaver after 25 minutes
#xset s 1500

# i haven't noticed this to be useful
# thunar isn't even installed
# thunar --daemon &

#/home/dan/bin/dwm/pkg/usr/bin/dwm

while true; do
    dwm >> /tmp/dwm.log 2>&1 
done 

# exec dwm >> /tmp/dwm.log 2>&1
