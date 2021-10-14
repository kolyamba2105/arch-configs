#!/bin/sh

# Remove all existing configs first
rm ~/.fehbg

rm ~/.gitconfig

rm ~/.xinitrc
rm ~/.screenlayout/1-laptop.sh
rm ~/.screenlayout/2-monitor.sh
rm ~/.screenlayout/3-dual-monitor.sh

rm -rf ~/.xmonad

rm ~/.stack/config.yaml

rm ~/.config/mimeapps.list

rm -rf ~/.config/alacritty

rm ~/.config/conky/conky.conf

rm ~/.config/dunst/dunstrc
rm ~/.config/dunst/notification-critical.png
rm ~/.config/dunst/notification-low.png
rm ~/.config/dunst/notification-normal.png

rm ~/.config/htop/htoprc

rm -rf ~/.config/nvim

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

ln -s $PWD/.stack/config.yaml ~/.stack/config.yaml

# Create config directory if it doesn't exist
mkdir -p ~/.config

# Default apps
ln -s $PWD/.config/mimeapps.list ~/.config/mimeapps.list

# Alacritty terminal
ln -s $PWD/.config/alacritty ~/.config/alacritty

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
ln -s $PWD/.config/nvim ~/.config/nvim

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
