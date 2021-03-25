set fish_greeting

fish_vi_key_bindings

alias ls "exa"
alias lsa "exa -la"

alias cat "bat"

alias rm "rm -i"
alias mv "mv -i"

set -x STARSHIP_CONFIG ~/.config/starship/config.toml
starship init fish | source
