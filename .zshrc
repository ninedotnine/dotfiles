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

autoload -U colors && colors
#autoload -U promptinit
##promptinit
#prompt walters
#PROMPT="[%n@%m %1~]%# "
setopt prompt_subst
#PROMPT='%{$fg[blue]%}[%D{%d/%m/%y} %T]%{$reset_color%} %(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m%{$reset_color%} %{$fg[magenta]%}[%(!.%1~.%~)]%{$reset_color%} 
#PROMPT='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
#%{$fg[blue]%}>>%{$reset_color%} '
# $STY will be set if zsh is running in an instance of screen
if [ "$STY" ]; then
    PROMPT='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m-screen [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
%{$fg[blue]%}»%{$reset_color%} '
else
    PROMPT='%(!.%{$fg_bold[red]%}.%{$fg_bold[green]%}%n@)%m [%{$reset_color%}%{$fg[magenta]%}%(!.%1~.%~)%{$reset_color%}%{$fg_bold[red]%}%{$fg_bold[green]%}]
%{$fg[blue]%}»%{$reset_color%} '
fi
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

# aliases

alias ls="ls -h --color=auto"
alias mv="mv -i"
alias rm="rm -I"
alias view="vi -R"

alias irc="exec screen -raAd"

alias gcc99="gcc -std=c99 -Wall -pedantic"
# once more, with optimizations
alias ogcc99="gcc -std=c99 -Wall -pedantic -O2 -s"

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
