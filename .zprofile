[[ -f ~/.zshrc ]] && . ~/.zshrc

PATH=$PATH:/home/kolyamba/.local/bin

# Start X server
[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1
