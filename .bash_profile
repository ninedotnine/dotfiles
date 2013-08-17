#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

PATH=$PATH:/home/dan/bin
export PATH
export EDITOR=vim
export BROWSER=surf

# start mpd, transmission-daemon if not already running
#pidof mpd &> /dev/null || mpd &
pidof transmission-daemon &> /dev/null || transmission-daemon &
pidof bitlbee &> /dev/null || bitlbee -D &

# connect to a wireless network
# if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
host google.com || sudo netcfg bell500 &

# start x on tty1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    startx
fi
