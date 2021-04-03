#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# Start X server
[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1
