# ~/.zprofile

# start x server on tty1, also other things
# basically i use this for stuff i only want to run once,
# even if i log in multiple times
if [ $(tty) = '/dev/tty1' ]; then
    # echo "hello on tty1"
    # connect to wireless network
    # sudo wifi-menu
    sudo connect
    startx
    # run gpg daemon for pass
    eval $(gpg-agent --daemon)
fi

