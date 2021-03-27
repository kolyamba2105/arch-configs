# Remove all existing configs first
rm ~/.bash_profile
rm ~/.bashrc

rm ~/.xinitrc
rm ~/.screenlayout/my-layout.sh
rm ~/.screenlayout/monitor-only.sh

rm ~/.xmonad/xmonad.hs
rm ~/.xmobarrc
rm ~/.hindent.yaml

rm ~/.config/alacritty/alacritty.yml

rm ~/.config/fish/config.fish

rm ~/.config/nitrogen/bg-saved.cfg
rm ~/.config/nitrogen/nitrogen.cfg

rm ~/.config/nvim/init.vim
rm ~/.config/nvim/coc-settings.json

rm ~/.config/picom/picom.conf

rm ~/.config/rofi/config.rasi
rm ~/.config/rofi/themes/custom.rasi

rm ~/.config/starship/config.toml

rm /usr/local/bin/copy-screenshot
rm /usr/local/bin/toggle-bluetooth

# In case if repo directory gets renamed
directory=$PWD

# Bash profile
ln -s $directory/.bash_profile ~/.bash_profile
ln -s $directory/.bashrc ~/.bashrc

# X init
ln -s $directory/.xinitrc ~/.xinitrc

# Screen layout (monitors settings)
mkdir -p ~/.screenlayout
ln -s $directory/.screenlayout/my-layout.sh ~/.screenlayout/my-layout.sh
ln -s $directory/.screenlayout/monitor-only.sh ~/.screenlayout/monitor-only.sh

# XMonad WM
mkdir -p ~/.xmonad
ln -s $directory/.xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $directory/.xmobarrc ~/.xmobarrc
ln -s $directory/.hindent.yaml ~/.hindent.yaml

# Create config directory if it doesn't exist
mkdir -p ~/.config

# Fish config
mkdir -p ~/.config/fish
ln -s $directory/.config/fish/config.fish ~/.config/fish/config.fish

# Alacritty terminal
mkdir -p ~/.config/alacritty
ln -s $directory/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# Nitrogen wallpapers
mkdir -p ~/.config/nitrogen
ln -s $directory/.config/nitrogen/bg-saved.cfg ~/.config/nitrogen/bg-saved.cfg
ln -s $directory/.config/nitrogen/nitrogen.cfg ~/.config/nitrogen/nitrogen.cfg

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

# Starship prompt
mkdir -p ~/.config/starship
ln -s $directory/.config/starship/config.toml ~/.config/starship/config.toml

# Make copy-screenshot globally available
chmod +x $directory/scripts/copy-screenshot.sh
ln -s $directory/scripts/copy-screenshot.sh /usr/local/bin/copy-screenshot

# Make toggle-bluetooth globally available
chmod +x $directory/scripts/toggle-bluetooth.sh
ln -s $directory/scripts/toggle-bluetooth.sh /usr/local/bin/toggle-bluetooth
