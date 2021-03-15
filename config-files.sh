# Remove all existing configs first
rm ~/.bash_profile
rm ~/.bashrc

rm ~/.xinitrc
rm ~/.screenlayout/my-layout.sh

rm ~/.xmonad/xmonad.hs
rm ~/.xmobarrc
rm ~/.hindent.yaml

rm ~/.config/alacritty/alacritty.yml
rm ~/.config/kitty/kitty.conf

rm ~/.config/nvim/init.vim
rm ~/.config/nvim/coc-settings.json

rm ~/.config/picom/picom.conf
rm ~/.config/rofi/config.rasi
rm ~/.config/rofi/themes/custom.rasi

# In case if repo directory gets renamed
directory=$PWD

# Bash profile
ln -s $directory/.bashrc ~/.bashrc
ln -s $directory/.bash_profile ~/.bash_profile

# X init
ln -s $directory/.xinitrc ~/.xinitrc

# Screen layout (monitors settings)
mkdir -p ~/.screenlayout
ln -s $directory/.screenlayout/my-layout.sh ~/.screenlayout/my-layout.sh

# XMonad WM
mkdir -p ~/.xmonad
ln -s $directory/.xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $directory/.xmobarrc ~/.xmobarrc
ln -s $directory/.hindent.yaml ~/.hindent.yaml

# Create config directory if it doesn't exist
mkdir -p ~/.config

# Kitty terminal
mkdir -p ~/.config/kitty
ln -s $directory/.config/kitty/kitty.conf ~/.config/kitty/kitty.conf

# Alacritty terminal
mkdir -p ~/.config/alacritty
ln -s $directory/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# Neovim
mkdir -p ~/.config/nvim
ln -s $directory/.config/nvim/init.vim ~/.config/nvim/init.vim
ln -s $directory/.config/nvim/coc-settings.json ~/.config/nvim/coc-settings.json

# Picom compositor
mkdir -p ~/.config/picom
ln -s $directory/.config/picom/picom.conf ~/.config/picom/picom.conf

# Rofi application runner
mkdir -p ~/.config/rofi
mkdir -p ~/.config/rofi/themes
ln -s $directory/.config/rofi/config.rasi ~/.config/rofi/config.rasi
ln -s $directory/.config/rofi/themes/custom.rasi ~/.config/rofi/themes/custom.rasi
