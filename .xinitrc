#!/bin/bash
. ~/.fehbg

# set x resources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
urxvtd -q -o -f

~/devel/dwmstatus/bin/dwmstatus --daemon >> /tmp/dwmstatus.log 2>&1

# compositing manager, makes windows transparent
xcompmgr & 

# hides the mouse when not in use
unclutter -noevents -idle 5 -not "tilda" &

# remap keys
xmodmap dotfiles/xmodmap

xcape -t 130 -e "Shift_L=parenleft;Shift_R=parenright"

# turn off default screensaver
#xset s off
# turn on screensaver after 25 minutes
#xset s 1500

urxvtc

autocutsel -fork -buttonup &
autocutsel -selection PRIMARY -fork -buttonup &

tilda >> /tmp/tilda.log 2>&1 &

# load scripts from /etc/X11/xinit/xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

exec builds/dwm_statuscolors/dwm/dwm >> /tmp/dwm.log 2>&1
