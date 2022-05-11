# ~/.zprofile

shuf -n 1 ~/life_tips/quotes

# coloured man pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;34m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;30;03;36m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;35m'
export LESS="--quit-at-eof --quit-on-intr --LONG-PROMPT --RAW-CONTROL-CHARS --jump-target=2 --ignore-case"
export GCC_COLORS="error=01;37:warning=01;34:note=01;33:caret=01;32:locus=01;36:quote=36"
export PASSWORD_STORE_X_SELECTION="primary"
export MPC_FORMAT="[%position%: ][[%artist%|%performer%][ / %composer%] - ][%title%|%album%|%track%|%file%]"
export EDITOR="vim"
export PAGER="less"
export BROWSER="firefox"
export TERMCMD="urxvtc"
export TRASH_D_DIR="/media/trash"

export PATH="$PATH:/home/dan/bin"


if [ $(tty) = '/dev/tty1' ]; then
    startx >> /tmp/xorg.log 2>&1
fi
