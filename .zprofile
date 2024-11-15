# ~/.zprofile

shuf -n 1 ~/life_tips/quotes

# create files for myself only
umask 027

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# coloured man pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;34m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;30;03;36m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;35m'
# export LESS="--quit-at-eof --quit-on-intr --quit-if-one-screen --LONG-PROMPT --RAW-CONTROL-CHARS --jump-target=2 --ignore-case"
export LESS="--quit-on-intr --quit-if-one-screen --LONG-PROMPT --RAW-CONTROL-CHARS --jump-target=2 --shift=8 --ignore-case"
export GCC_COLORS="error=01;37:warning=01;34:note=01;33:caret=01;32:locus=01;36:quote=36"
export PASSWORD_STORE_X_SELECTION="primary"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export GRIM_DEFAULT_DIR="/home/danso/pics/scrots"
export MPC_FORMAT="[%position%: ][[%artist%|%performer%][ / %composer%] - ][%title%|%album%|%track%|%file%]"
export EDITOR="nvim"
export PAGER="less"
export BROWSER="firefox"
export TERMCMD="kitty"
export TRASH_D_DIR="$HOME/.trash"

export PATH="$PATH:$HOME/bin"
