#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# change prompt colour
# PS1='[\u@\h \W]\$ '
# PS1='\[\e[1;35m\][\u@\h \W]\$\[\e[0m\] '
PS1='\[\e[1;35m\]\u@\h:\W\$\[\e[0m\] '

###########
# aliases #
###########

alias ls='ls --color=auto'
# alias constantine='ssh d67-193-33-160.home3.cgocable.net'

export GREP_COLOR="1;33"
alias grep="grep --color=auto"

alias vi='vim'

# this allows us to use aliases while sudoing
# it really doesn't work at all, whatever
alias sudo="sudo "

alias harper='traceroute 72.20.28.248'
# alias router='ping 192.168.37.231'

alias mv="mv -i"
alias rm="rm -I"
