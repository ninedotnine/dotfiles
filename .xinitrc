. ~/.fehbg

dwm-status >> /tmp/dwm-status.log 2>&1 &
# pacman-update &
sudo opendns &
xchat &

# remap caps lock to be an escape key
# xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'

while true; do
    tilda > /dev/null 2>&1 
done &

# add terminus to fonts
xset +fp /usr/share/fonts/local
xset fp rehash

# set x resources
# [[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# turn off default screensaver
#xset s off
# turn on screensaver after 25 minutes
xset s 1500

# this is to make terminal start and quit asap
# terminal -x echo '' &

# i haven't noticed this to be useful
# thunar --daemon &

#/home/dan/bin/dwm/pkg/usr/bin/dwm

#while true; do
    #dwm >> /tmp/dwm.log 2>&1 
#done 

exec dwm >> /tmp/dwm.log 2>&1
