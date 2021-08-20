#!/bin/sh

# Remove all existing configs first
rm ~/.fehbg

rm ~/.gitconfig

rm ~/.xinitrc
rm ~/.screenlayout/1-laptop.sh
rm ~/.screenlayout/2-monitor.sh
rm ~/.screenlayout/3-dual-monitor.sh

rm -rf ~/.xmonad

rm ~/.config/mimeapps.list

rm ~/.config/alacritty/alacritty.yml
rm ~/.config/alacritty/alacritty-gruvbox.yml

rm ~/.config/conky/conky.conf

rm ~/.config/dunst/dunstrc
rm ~/.config/dunst/notification-critical.png
rm ~/.config/dunst/notification-low.png
rm ~/.config/dunst/notification-normal.png

rm ~/.config/htop/htoprc

rm ~/.config/nvim/coc-settings.json

rm ~/.config/nvim/init.vim
rm ~/.config/nvim/init-coc.vim
rm ~/.config/nvim/init-lsp.vim

rm ~/.config/nvim/lua/lsp/common.lua
rm ~/.config/nvim/lua/lsp/diagnostic.lua
rm ~/.config/nvim/lua/lsp/haskell.lua
rm ~/.config/nvim/lua/lsp/json.lua
rm ~/.config/nvim/lua/lsp/init.lua
rm ~/.config/nvim/lua/lsp/typescript.lua

rm ~/.config/nvim/lua/completion.lua
rm ~/.config/nvim/lua/fuzzy-finder.lua
rm ~/.config/nvim/lua/status-line.lua
rm ~/.config/nvim/lua/toggle-tree.lua
rm ~/.config/nvim/lua/treesitter.lua

rm ~/.config/nvim/vim/basics.vim
rm ~/.config/nvim/vim/buffer-line.vim
rm ~/.config/nvim/vim/coc.vim
rm ~/.config/nvim/vim/color-scheme.vim
rm ~/.config/nvim/vim/fugitive.vim
rm ~/.config/nvim/vim/mappings.vim
rm ~/.config/nvim/vim/nvim-tree.vim
rm ~/.config/nvim/vim/term.vim

rm ~/.config/ranger/commands.py
rm ~/.config/ranger/commands_full.py
rm ~/.config/ranger/rc.conf
rm ~/.config/ranger/rifle.conf
rm ~/.config/ranger/scope.sh

rm ~/.config/starship/config.toml

rm ~/.zprofile
rm ~/.zshrc

# ZSH
ln -s $PWD/.zprofile ~/.zprofile
ln -s $PWD/.zshrc ~/.zshrc

# Set wallpaper with feh
ln -s $PWD/.fehbg ~/.fehbg

# Git config
ln -s $PWD/.gitconfig ~/.gitconfig

# X init
ln -s $PWD/.xinitrc ~/.xinitrc

# Screen layout (monitors settings)
mkdir -p ~/.screenlayout
ln -s $PWD/.screenlayout/1-laptop.sh ~/.screenlayout/1-laptop.sh
ln -s $PWD/.screenlayout/2-monitor.sh ~/.screenlayout/2-monitor.sh
ln -s $PWD/.screenlayout/3-dual-monitor.sh ~/.screenlayout/3-dual-monitor.sh

# XMonad WM
ln -s $PWD/.xmonad ~/.xmonad

# Create config directory if it doesn't exist
mkdir -p ~/.config

# Default apps
ln -s $PWD/.config/mimeapps.list ~/.config/mimeapps.list

# Alacritty terminal
mkdir -p ~/.config/alacritty
ln -s $PWD/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml
ln -s $PWD/.config/alacritty/alacritty-gruvbox.yml ~/.config/alacritty/alacritty-gruvbox.yml

# Conky system monitor
mkdir -p ~/.config/conky
ln -s $PWD/.config/conky/conky.conf ~/.config/conky/conky.conf

# Dunst notification-daemon
mkdir -p ~/.config/dunst
ln -s $PWD/.config/dunst/dunstrc ~/.config/dunst/dunstrc
ln -s $PWD/.config/dunst/notification-critical.png ~/.config/dunst/notification-critical.png
ln -s $PWD/.config/dunst/notification-low.png ~/.config/dunst/notification-low.png
ln -s $PWD/.config/dunst/notification-normal.png ~/.config/dunst/notification-normal.png

# HTOP
mkdir -p ~/.config/htop
ln -s $PWD/.config/htop/htoprc ~/.config/htop/htoprc

# Neovim
mkdir -p ~/.config/nvim
mkdir -p ~/.config/nvim/lua
mkdir -p ~/.config/nvim/lua/lsp
mkdir -p ~/.config/nvim/vim

ln -s $PWD/.config/nvim/coc-settings.json ~/.config/nvim/coc-settings.json

ln -s $PWD/.config/nvim/init-coc.vim ~/.config/nvim/init-coc.vim
ln -s $PWD/.config/nvim/init-lsp.vim ~/.config/nvim/init-lsp.vim

ln -s ~/.config/nvim/init-lsp.vim ~/.config/nvim/init.vim

ln -s $PWD/.config/nvim/lua/lsp/common.lua ~/.config/nvim/lua/lsp/common.lua
ln -s $PWD/.config/nvim/lua/lsp/diagnostic.lua ~/.config/nvim/lua/lsp/diagnostic.lua
ln -s $PWD/.config/nvim/lua/lsp/haskell.lua ~/.config/nvim/lua/lsp/haskell.lua
ln -s $PWD/.config/nvim/lua/lsp/json.lua ~/.config/nvim/lua/lsp/json.lua
ln -s $PWD/.config/nvim/lua/lsp/init.lua ~/.config/nvim/lua/lsp/init.lua
ln -s $PWD/.config/nvim/lua/lsp/typescript.lua ~/.config/nvim/lua/lsp/typescript.lua

ln -s $PWD/.config/nvim/lua/completion.lua ~/.config/nvim/lua/completion.lua
ln -s $PWD/.config/nvim/lua/fuzzy-finder.lua ~/.config/nvim/lua/fuzzy-finder.lua
ln -s $PWD/.config/nvim/lua/status-line.lua ~/.config/nvim/lua/status-line.lua
ln -s $PWD/.config/nvim/lua/toggle-tree.lua ~/.config/nvim/lua/toggle-tree.lua
ln -s $PWD/.config/nvim/lua/treesitter.lua ~/.config/nvim/lua/treesitter.lua

ln -s $PWD/.config/nvim/vim/basics.vim ~/.config/nvim/vim/basics.vim
ln -s $PWD/.config/nvim/vim/buffer-line.vim ~/.config/nvim/vim/buffer-line.vim
ln -s $PWD/.config/nvim/vim/coc.vim ~/.config/nvim/vim/coc.vim
ln -s $PWD/.config/nvim/vim/color-scheme.vim ~/.config/nvim/vim/color-scheme.vim
ln -s $PWD/.config/nvim/vim/fugitive.vim ~/.config/nvim/vim/fugitive.vim
ln -s $PWD/.config/nvim/vim/mappings.vim ~/.config/nvim/vim/mappings.vim
ln -s $PWD/.config/nvim/vim/nvim-tree.vim ~/.config/nvim/vim/nvim-tree.vim
ln -s $PWD/.config/nvim/vim/term.vim ~/.config/nvim/vim/term.vim

# Ranger file manager
mkdir -p ~/.config/ranger
ln -s $PWD/.config/ranger/commands.py ~/.config/ranger/commands.py
ln -s $PWD/.config/ranger/commands_full.py ~/.config/ranger/commands_full.py
ln -s $PWD/.config/ranger/rc.conf ~/.config/ranger/rc.conf
ln -s $PWD/.config/ranger/rifle.conf ~/.config/ranger/rifle.conf
ln -s $PWD/.config/ranger/scope.sh ~/.config/ranger/scope.sh

# Starship prompt
mkdir -p ~/.config/starship
ln -s $PWD/.config/starship/config.toml ~/.config/starship/config.toml
