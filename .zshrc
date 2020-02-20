# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

# don't have duplicates in history
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS

setopt autocd notify
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/dan/.zshrc'

# append lines to history as they are entered
setopt INC_APPEND_HISTORY
setopt HIST_REDUCE_BLANKS

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U colors && colors

# disable software flow control, prevents ^S from blocking until ^Q is sent
if [ "$TERM" != "linux" ]; then
    stty -ixon
fi

# allow ctrl+n to work like tab, similar to vim
bindkey "^N" expand-or-complete
# allow backspace to work even after command mode
bindkey '^?' backward-delete-char

source "$HOME/dotfiles/.aliases"

# show ls on every dir change
function chpwd() {
    emulate -L zsh
    ls 
}

# highlighting while typing in zsh, pretty neat
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# gpg says this is required: 
# https://www.gnupg.org/documentation/manuals/gnupg/Invoking-GPG_002dAGENT.html
if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
fi
GPG_TTY=$(tty)
export GPG_TTY

# $STY will be set if zsh is running in an instance of screen
if [ "$STY" ]; then
    screenprompt=" %{$fg_bold[blue]%}(screen)$fg_bold[magenta]"
    screentitle="[screen] "
fi


setopt prompt_subst

# %F{xxx} generates any of 256 colours

. ~/dotfiles/.zsh_git_prompt

name_and_host="%{$fg_bold[magenta]%}%n@%m"
interface="$(tty | cut -c 6-)$screenprompt"
smile="%(?.%{$fg_no_bold[green]%}:^).%{$fg_no_bold[red]%}:v()"
current_dir="%{$fg_bold[green]%}[%{$fg_no_bold[magenta]%}%~%{$fg_bold[green]%}]"
prompt_arrow="%(1j.[%{%F{40}%}%j%{$fg_bold[green]%}] .)%{$fg[blue]%}»%{$reset_color%} "
prompt_dolla="%(1j.[%{%F{40}%}%j%{$fg_bold[green]%}] .)%{$fg[blue]%}$%{$reset_color%} "

PROMPTINS='$name_and_host $interface $smile $(git_prompt_string) $current_dir
$prompt_arrow'
PROMPTCMD='$name_and_host $interface $smile $(git_prompt_string) $current_dir
$prompt_dolla'

# 10ms for key sequences, less delay on switching from ins to cmd mode 
export KEYTIMEOUT=1

# this is changes the prompt when the vi input mode changes
function zle-line-init zle-keymap-select {
    PS1="${${KEYMAP/vicmd/$PROMPTCMD}/(main|viins)/$PROMPTINS}"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# set window title to show currently running command
precmd () {
    print -Pn "\e]0;$screentitle%~ »\a"
}
preexec () {
    print -Pn "\e]0;$screentitle%~: $1\a"
}

eval $(thefuck --alias osti)
