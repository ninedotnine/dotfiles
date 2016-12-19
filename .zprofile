# ~/.zprofile

# coloured man pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;34m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;30;03;36m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;35m'

export LESS="--quit-at-eof --quit-on-intr --LONG-PROMPT --RAW-CONTROL-CHARS --jump-target=2 --ignore-case"

export PATH="/home/dan/bin:$PATH"
export EDITOR="vim"
export PAGER="less"
export BROWSER="firefox"

# start x server on tty1, also other things
# basically i use this for stuff i only want to run once,
# even if i log in multiple times
if [ $(tty) = '/dev/tty1' ]; then
#         eval "$(gpg-agent --daemon)"
    startx
fi

