#!/bin/sh

mkdir -p ~/.config

rm ~/.zprofile
ln -s $PWD/.zprofile ~/.zprofile

rm ~/.zshrc
ln -s $PWD/.zshrc ~/.zshrc

rm ~/.fehbg
ln -s $PWD/.fehbg ~/.fehbg

rm ~/.gitconfig
ln -s $PWD/.gitconfig ~/.gitconfig

rm ~/.tmux.conf
ln -s $PWD/.tmux.conf ~/.tmux.conf

rm ~/.xinitrc
ln -s $PWD/.xinitrc ~/.xinitrc

rm -rf ~/.screenlayout
ln -s $PWD/.screenlayout ~/.screenlayout

rm -rf ~/.xmonad
ln -s $PWD/.xmonad ~/.xmonad

rm ~/.stack/config.yaml
ln -s $PWD/.stack/config.yaml ~/.stack/config.yaml

rm ~/.hindent.yaml
ln -s $PWD/.hindent.yaml ~/.hindent.yaml

rm ~/.config/mimeapps.list
ln -s $PWD/.config/mimeapps.list ~/.config/mimeapps.list

rm -rf ~/.config/alacritty
ln -s $PWD/.config/alacritty ~/.config/alacritty

rm -rf ~/.config/dunst
ln -s $PWD/.config/dunst ~/.config/dunst

rm -rf ~/.config/gtk-3.0/settings.ini
ln -s $PWD/.config/gtk-3.0/settings.ini ~/.config/gtk-3.0/settings.ini

rm -rf ~/.config/htop
ln -s $PWD/.config/htop ~/.config/htop

rm -rf ~/.config/nvim
ln -s $PWD/.config/nvim ~/.config/nvim
