# Remove all existing configs first
rm ~/.bash_profile
rm ~/.bashrc

rm ~/.xinitrc
rm ~/.screenlayout/my-layout.sh
rm ~/.screenlayout/monitor-only.sh

rm ~/.xmonad/xmonad.hs
rm ~/.xmobarrc
rm ~/.hindent.yaml

rm ~/.config/mimeapps.list

rm ~/.config/alacritty/alacritty.yml
rm ~/.config/alacritty/example-config.yml
rm ~/.config/alacritty/nord-theme.yml

rm ~/.config/fish/config.fish

rm ~/.config/nitrogen/bg-saved.cfg
rm ~/.config/nitrogen/nitrogen.cfg

rm ~/.config/nvim/init.vim
rm ~/.config/nvim/coc-settings.json

rm ~/.config/picom/picom.conf

rm ~/.config/ranger/rc.conf

rm ~/.config/rofi/config.rasi
rm ~/.config/rofi/themes/custom.rasi

rm ~/.config/starship/config.toml

# Bash profile
ln -s $PWD/.bash_profile ~/.bash_profile
ln -s $PWD/.bashrc ~/.bashrc

# X init
ln -s $PWD/.xinitrc ~/.xinitrc

# Screen layout (monitors settings)
mkdir -p ~/.screenlayout
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
ln -s $PWD/.config/alacritty/example-config.yml ~/.config/alacritty/example-config.yml
ln -s $PWD/.config/alacritty/nord-theme.yml ~/.config/alacritty/nord-theme.yml

# Nitrogen wallpapers
mkdir -p ~/.config/nitrogen
ln -s $PWD/.config/nitrogen/bg-saved.cfg ~/.config/nitrogen/bg-saved.cfg
ln -s $PWD/.config/nitrogen/nitrogen.cfg ~/.config/nitrogen/nitrogen.cfg

# Neovim
mkdir -p ~/.config/nvim
ln -s $PWD/.config/nvim/init.vim ~/.config/nvim/init.vim
ln -s $PWD/.config/nvim/coc-settings.json ~/.config/nvim/coc-settings.json

# Picom compositor
mkdir -p ~/.config/picom
ln -s $PWD/.config/picom/picom.conf ~/.config/picom/picom.conf

mkdir -p ~/.config/ranger
ln -s $PWD/.config/ranger/rc.conf ~/.config/ranger/rc.conf

# Rofi application runner
mkdir -p ~/.config/rofi
mkdir -p ~/.config/rofi/themes
ln -s $PWD/.config/rofi/config.rasi ~/.config/rofi/config.rasi
ln -s $PWD/.config/rofi/themes/custom.rasi ~/.config/rofi/themes/custom.rasi

# Starship prompt
mkdir -p ~/.config/starship
ln -s $PWD/.config/starship/config.toml ~/.config/starship/config.toml
