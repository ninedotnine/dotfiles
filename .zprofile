# ~/.zprofile

# echo "[1;31m >[1;33m(.)__ [1;31m >[1;33m(.)__ [1;31m >[1;33m(.)__ [1;39m quack!"
# echo "[1;33m  (___/   (___/   (___/  [1;35m"
# echo "in progress:" 
# echo "git dmenu-launcher" 
echo -n "[1;36m"


# start x server on tty1, also other things
# basically i use this for stuff i only want to run once,
# even if i log in multiple times
if [ $(tty) = '/dev/tty1' ]; then
    # echo "hello on tty1"
    # connect to wireless network
    # sudo wifi-menu
    # sudo connect
    startx
    # run gpg daemon for pass
    eval $(gpg-agent --daemon)
fi

# coloured man pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;34m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;30;03;36m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;35m'
