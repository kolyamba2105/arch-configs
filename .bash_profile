#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# This will trigger startx right after login
[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1
