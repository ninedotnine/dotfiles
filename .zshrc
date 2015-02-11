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

autoload -Uz compinit
compinit
# End of lines added by compinstall

# source ~/.zprofile

autoload -U colors && colors
#autoload -U promptinit
##promptinit
#prompt walters
#PROMPT="[%n@%m %1~]%# "
setopt prompt_subst
#PROMPT='%{$fg[blue]%}[%D{%d/%m/%y} %T]%{$reset_color%} %(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m%{$reset_color%} %{$fg[magenta]%}[%(!.%1~.%~)]%{$reset_color%} 
#PROMPT='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
#%{$fg[blue]%}>>%{$reset_color%} '

RPROMPT="%(?.%{$fg[magenta]%}♥0♥.%S%{$fg[red]%}ψ☭%?☭ψ%s)%{$reset_color%}"
# this one is red whether exit is zero or nonzero
#RPROMPT="%{$fg[red]%} %(?.♥0♥.%Sψ☭%?☭ψ%s)%{$reset_color%}"
# RPROMPT="%{$fg[red]%} ❤%?❤♞⌫λ♫☺☹☻ツ♔♕♚♛♡♥☭✔%{$reset_color%}"
# RPROMPT="%{$fg[red]%}%?%{$reset_color%}"

# no respect for tables
# RPROMPT="%(?.%{$fg[magenta]%}♥0♥.%S%{$fg[red]%}ψ(╯°□°）╯︵ ┻━┻☭%?☭ψ%s)%{$reset_color%}"
# face instead of 0 exit status
# RPROMPT="%(?.%{$fg[magenta]%}♥(° ͜ʖ°)♥.%S%{$fg[red]%}ψ☭%?☭ψ%s)%{$reset_color%}"

# disable software flow control, prevents ^S from blocking until ^Q is sent
if [ "$TERM" != "linux" ]; then
    stty -ixon
fi

# allow ctrl+n to work like tab, similar to vim
bindkey "^N" expand-or-complete
# allow backspace to work even after command mode
bindkey '^?' backward-delete-char


# aliases

alias ls="ls -hFA --color=auto"
alias mv="mv -i"
alias rm="rm -I"
alias view="vim -R"
alias vi="vim"

alias strings="strings --all"

alias irc="exec screen -raAd"

alias gcc99="gcc -std=c99 -Wall -pedantic"
# once more, with optimizations
alias ogcc99="gcc -std=c99 -Wall -pedantic -O2 -s"
alias oghc="ghc -Wall -O2" # this should also strip but that's harder

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
    SCREEN="-screen"
else
    SCREEN=""
fi

interface=$(tty | cut -c 6-)

. ~/.zsh_git_prompt

PROMPTINS='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m$SCREEN $interface $(git_prompt_string) [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
%{$fg[blue]%}»%{$reset_color%} '
PROMPTCMD='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m$SCREEN $interface $(git_prompt_string) [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
%{$fg[blue]%}$%{$reset_color%} '

# PROMPTINS='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m$SCREEN [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
# %{$fg[blue]%}»%{$reset_color%} '
# PROMPTCMD='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m$SCREEN [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
# %{$fg[blue]%}$%{$reset_color%} '

# this is changes the prompt when the vi input mode changes
function zle-line-init zle-keymap-select {
    PS1="${${KEYMAP/vicmd/$PROMPTCMD}/(main|viins)/$PROMPTINS}"
#     PS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# set window title to show currently running command
# show screen differently
# if [ "$TERM" = "rxvt-unicode-256color" ]; then
precmd () {
    # vcs_info # what does this even do?
    # print -Pn "\e]0;[%n@%M][%~]%#\a"
    print -Pn "\e]0;%~ »\a"
}
preexec () {
    # print -Pn "\e]0;[%n@%M][%~]%# ($1)\a"
    print -Pn "\e]0;%~: $1\a"
}
if [ "$STY" ]; then 
# elif [ "$TERM" = "screen-256color" ]; then 
    precmd () {
        print -Pn "\e]0;[screen] %~\a"
    }
    preexec () {
        print -Pn "\e]0;[screen] %~: $1\a"
    }
fi

# append lines to history as they are entered
setopt INC_APPEND_HISTORY
setopt HIST_REDUCE_BLANKS

# . ~/.zsh_git_prompt
# RPS1='$(git_prompt_string)'
# PROMPTINS="$PROMPTINS $(git_prompt_string)"
# PROMPTCMD="$PROMPTCMD $(git_prompt_string)"

