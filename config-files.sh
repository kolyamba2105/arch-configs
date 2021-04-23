#!/bin/sh

# Remove all existing configs first
rm ~/.bash_profile
rm ~/.bashrc

rm ~/.fehbg

rm ~/.xinitrc
rm ~/.screenlayout/dual-monitor.sh
rm ~/.screenlayout/my-layout.sh
rm ~/.screenlayout/monitor-only.sh

rm ~/.xmonad/xmonad.hs
rm ~/.xmobarrc
rm ~/.hindent.yaml

rm ~/.config/mimeapps.list

rm ~/.config/alacritty/alacritty.yml
rm ~/.config/alacritty/nord-theme.yml

rm ~/.config/fish/config.fish

rm ~/.config/nvim/init.vim
rm ~/.config/nvim/coc-settings.json

rm ~/.config/picom/picom.conf

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

# X init
ln -s $PWD/.xinitrc ~/.xinitrc

# Screen layout (monitors settings)
mkdir -p ~/.screenlayout
ln -s $PWD/.screenlayout/dual-monitor.sh ~/.screenlayout/dual-monitor.sh
ln -s $PWD/.screenlayout/my-layout.sh ~/.screenlayout/my-layout.sh
ln -s $PWD/.screenlayout/monitor-only.sh ~/.screenlayout/monitor-only.sh

# XMonad WM
mkdir -p ~/.xmonad
ln -s $PWD/.xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $PWD/.xmobarrc ~/.xmobarrc
ln -s $PWD/.hindent.yaml ~/.hindent.yaml

# Create config directory if it doesn't exist
mkdir -p ~/.config

# Default apps
ln -s $PWD/.config/mimeapps.list ~/.config/mimeapps.list

# Fish config
mkdir -p ~/.config/fish
ln -s $PWD/.config/fish/config.fish ~/.config/fish/config.fish

# Alacritty terminal
mkdir -p ~/.config/alacritty
ln -s $PWD/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml
ln -s $PWD/.config/alacritty/nord-theme.yml ~/.config/alacritty/nord-theme.yml

# Neovim
mkdir -p ~/.config/nvim
ln -s $PWD/.config/nvim/init.vim ~/.config/nvim/init.vim
ln -s $PWD/.config/nvim/coc-settings.json ~/.config/nvim/coc-settings.json

# Picom compositor
mkdir -p ~/.config/picom
ln -s $PWD/.config/picom/picom.conf ~/.config/picom/picom.conf

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
