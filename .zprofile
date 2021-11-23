[[ -f ~/.zshrc ]] && . ~/.zshrc

PATH=$PATH:$HOME/.local/bin
export PATH="$PATH:$HOME/.yarn/bin"

# Start X server
[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1 &> /dev/null
