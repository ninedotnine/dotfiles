#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# remap caps lock to be an escape key
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
