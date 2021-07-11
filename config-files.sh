#!/bin/sh

# Remove all existing configs first
rm ~/.bash_profile
rm ~/.bashrc

rm ~/.fehbg

rm ~/.gitconfig

rm ~/.xinitrc
rm ~/.screenlayout/1-laptop.sh
rm ~/.screenlayout/2-monitor.sh
rm ~/.screenlayout/3-dual-monitor.sh

rm ~/.xmonad/xmonad.hs
rm ~/.xmonad/build
rm ~/.xmobarrc

rm ~/.config/mimeapps.list

rm ~/.config/alacritty/alacritty.yml

rm ~/.config/conky/conky.conf

rm ~/.config/dunst/dunstrc
rm ~/.config/dunst/notification-critical.png
rm ~/.config/dunst/notification-low.png
rm ~/.config/dunst/notification-normal.png

rm ~/.config/fish/config.fish
rm ~/.config/fish/fish_variables
rm ~/.config/fish/functions/fish_prompt.fish

rm ~/.config/nvim/init.vim
rm ~/.config/nvim/lua/completion.lua
rm ~/.config/nvim/lua/fuzzy-finder.lua
rm ~/.config/nvim/lua/lsp.lua
rm ~/.config/nvim/lua/status-line.lua
rm ~/.config/nvim/lua/treesitter.lua

rm ~/.config/ranger/commands.py
rm ~/.config/ranger/commands_full.py
rm ~/.config/ranger/rc.conf
rm ~/.config/ranger/rifle.conf
rm ~/.config/ranger/scope.sh

rm ~/.config/starship/config.toml

# Bash profile
ln -s $PWD/.bash_profile ~/.bash_profile
ln -s $PWD/.bashrc ~/.bashrc

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
mkdir -p ~/.xmonad
ln -s $PWD/.xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $PWD/.xmonad/build ~/.xmonad/build
ln -s $PWD/.xmobarrc ~/.xmobarrc


# Create config directory if it doesn't exist
mkdir -p ~/.config

# Default apps
ln -s $PWD/.config/mimeapps.list ~/.config/mimeapps.list

# Fish config
mkdir -p ~/.config/fish
mkdir -p ~/.config/fish/functions
ln -s $PWD/.config/fish/config.fish ~/.config/fish/config.fish
ln -s $PWD/.config/fish/fish_variables ~/.config/fish/fish_variables
ln -s $PWD/.config/fish/functions/fish_prompt.fish ~/.config/fish/functions/fish_prompt.fish

# Alacritty terminal
mkdir -p ~/.config/alacritty
ln -s $PWD/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# Conky system monitor
mkdir -p ~/.config/conky
ln -s $PWD/.config/conky/conky.conf ~/.config/conky/conky.conf

# Dunst notification-daemon
mkdir -p ~/.config/dunst
ln -s $PWD/.config/dunst/dunstrc ~/.config/dunst/dunstrc
ln -s $PWD/.config/dunst/notification-critical.png ~/.config/dunst/notification-critical.png
ln -s $PWD/.config/dunst/notification-low.png ~/.config/dunst/notification-low.png
ln -s $PWD/.config/dunst/notification-normal.png ~/.config/dunst/notification-normal.png

# Neovim
mkdir -p ~/.config/nvim
mkdir -p ~/.config/nvim/lua
ln -s $PWD/.config/nvim/init.vim ~/.config/nvim/init.vim
ln -s $PWD/.config/nvim/lua/completion.lua ~/.config/nvim/lua/completion.lua
ln -s $PWD/.config/nvim/lua/fuzzy-finder.lua ~/.config/nvim/lua/fuzzy-finder.lua
ln -s $PWD/.config/nvim/lua/lsp.lua ~/.config/nvim/lua/lsp.lua
ln -s $PWD/.config/nvim/lua/status-line.lua ~/.config/nvim/lua/status-line.lua
ln -s $PWD/.config/nvim/lua/treesitter.lua ~/.config/nvim/lua/treesitter.lua

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
