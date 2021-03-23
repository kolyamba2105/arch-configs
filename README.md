# Installation guide

## Basic installation

I was following [this video](https://www.youtube.com/watch?v=PQgyW10xD8s&list=PL5--8gKSku16Ncr9H_BAZSzWecjaSWlvY&index=5&ab_channel=DistroTube) to proceed with Arch basic installation, which basically follows the original [Arch installation guide](https://wiki.archlinux.org/index.php/installation_guide).
Most of the stuff will remain the same. The only thing that is not covered by the video is `Connect to Internet` part, but it's covered [here](https://wiki.archlinux.org/index.php/Network_configuration). I can also generate `RU`, `PL`, `UA` and `US` locales.

## Desktop installation

[This video](https://www.youtube.com/watch?v=pouX5VvX0_Q) will explain how to install Xorg along with the WM.

When it comes to GUI setup, first thing we have to take care of is video drivers installation.

- `lspci -v | grep -A1 -e VGA -e 3D` - shows what video-card is installed on your machine.

The output for my machine (Lenovo ThinkPad E580) was like this:

```
00:02.0 VGA compatible controller: Intel Corporation UHD Graphics 620 (rev 07) (prog-if 00 [VGA controller])
	Subsystem: Lenovo Device 5069
```

- `pacman -Ss xf86-video` - lists all the available video-drivers.

The output for my machine was like this:

```
extra/xf86-video-amdgpu 19.1.0-2 (xorg-drivers) [installed]
    X.org amdgpu video driver
extra/xf86-video-ati 1:19.1.0-2 (xorg-drivers) [installed]
    X.org ati video driver
extra/xf86-video-dummy 0.3.8-4 (xorg-drivers)
    X.org dummy video driver
extra/xf86-video-fbdev 0.5.0-2 (xorg-drivers)
    X.org framebuffer video driver
extra/xf86-video-intel 1:2.99.917+916+g31486f40-1 (xorg-drivers) [installed]
    X.org Intel i810/i830/i915/945G/G965+ video drivers
extra/xf86-video-nouveau 1.0.17-1 (xorg-drivers) [installed]
    Open Source 3D acceleration driver for nVidia cards
extra/xf86-video-openchrome 0.6.0-4 (xorg-drivers)
    X.Org Openchrome drivers
extra/xf86-video-sisusb 0.9.7-3
    X.org SiS USB video driver
extra/xf86-video-vesa 2.5.0-1 (xorg-drivers xorg)
    X.org vesa video driver
extra/xf86-video-vmware 13.3.0-2 (xorg-drivers)
    X.org vmware video driver
extra/xf86-video-voodoo 1.2.5-11 (xorg-drivers)
    X.org 3dfx Voodoo1/Voodoo2 2D video driver
community/xf86-video-qxl 0.1.5.r16.g52c421c-1 (xorg-drivers)
    Xorg X11 qxl video driver
community/xf86-video-qxl-debian 0.1.5+git20200331-1 (xorg-drivers)
    Xorg X11 qxl video driver (Debian binary)
```

Some of the drivers are already installed on my machine. They were installed automatically by `Manjaro` installer, so I'll need the same packages for fresh `Arch` installation.

When video-drivers are installed, we can move on to installing some basic packages for `GUI`.

### First steps

- `sudo pacman -Syuu` - updates pacman.
- `sudo pacman -S xorg xorg-xinit` - installs `X` window system.
- `sudo pacman -S xmonad xmonad-contrib xmobar` - installs window manager.

### Setup git

- `sudo pacman -S git open-ssh` - install git and ssh tool to be able to generate SSH keys.
- `git config --global user.name "John Doe"` - set name in git.
- `git config --global user.email johndoe@example.com` - set e-mail in git.

### IMPORTANT

At some point when you're booted into fresh Arch GUI, generate an `SSH` and add it to `GitHub` profile.

### Enable `AUR` in your system

- `git clone https://aur.archlinux.org/yay-git.git` - clone `yay` repo.
- `sudo pacman -S base-devel` - install `base-devel` (package required for lots of things in Arch).
- `cd yay-git && makepkg -si` - buildo `yay` from source.

### Install the most necessary programs

- `kitty` and `alacritty`.
- `vim` and `nvim`.
- `firefox`.
- `nitrogen`.
- `picom`.
- `rofi`.
- `fish` - interactive shell.

### Export `LANG` variable from `.bashrc`

- `export LANG=en_US.UTF8` - there was an issue with `xMobar`, it couldn't read the config file, because it couldn't recognize the locale, but then this problem disappeared (maybe won't be needed).

### Picom issue

[An issue](https://github.com/yshui/picom/wiki/Vsync-Situation) with terminal opacity.
This might not be needed if all video-drivers are installed correctly.

### Install `JetBrains Mono` font

[Article](https://wiki.archlinux.org/index.php/fonts) for reference.

- Download font from the [official site](https://www.jetbrains.com/ru-ru/lp/mono).
- `sudo pacman -S zip unzip` - install utilities that deal with archives.
- `unzip JetBrainsMono-2.225.zip` - extract the archive.
- Install font:
  1. Create a directory where all fonts are installed by default: `mkdir -p ~/.local/share/fonts/ttf`.
  2. Choose `ttf` fonts: `cd ~/Downloads/fonts`.
  3. Move fonts: `mv ttf ~/.local/share/fonts/ttf/JetBrainsMono`.

### Set typing repeat speed

In `.xinitrc` put this: `xset r rate 200 35` (aready added to config).

### Setup keyboard layouts

During basic installation proccess, generate both US and RU locales (with `locale-gen`).
Minor issue: Russian keyboard erases username in a terminal.

### Sound setup

[Article](https://linuxhint.com/guide_linux_audio/) for reference.

Install `alsa-utils` (sound doesn't work without it), `pulse-audio` along with `pulsemixer` as a front-end for `pulse-audio`.

### Bluetooth setup

Follow installation guide from [this article](https://wiki.archlinux.org/index.php/Bluetooth#Installation). Load `btusb` module, check if the device is not blocked by `rfkill` etc (everything is in the article).
If you're getting `No default controller available` error in `bluetoothctl` - check [this](https://stackoverflow.com/questions/48279646/bluetoothctl-no-default-controller-available).

In order to be able to use audio equipment like bluetooth headphones or speakers, you need to install the additional `pulseaudio-bluetooth` package. With a default PulseAudio installation you should immediately be able to stream audio from a bluetooth device to your speakers.
Check [Audio -> PulseAudio](https://wiki.archlinux.org/index.php/Bluetooth#PulseAudio) section to make bluetooth headphones work with `pulseaudio`.

Another [article](https://wiki.archlinux.org/index.php/bluetooth_headset) for reference.

### Power management

Articles about [power management](https://wiki.archlinux.org/index.php/Power_management) and [session lock](https://wiki.archlinux.org/index.php/Session_lock).

- `sudo pacman -S htop` - install Task Manager.
- `sudo nvim /etc/systemd/logind.conf` and change lid options. Brightness can be changed via terminal.
- `sudo pacman -S slock` - install simple session lock.

Follow the instructions from [this article](https://wiki.archlinux.org/index.php/Slock) to enable _Lock on suspend_.

Add the following lines to `/etc/X11/xorg.conf` (as mentioned in the article above):

```
Section "ServerFlags"
    Option "DontVTSwitch" "True"
    Option "DontZap" "True"
EndSection
```

Example settings:

```
[Login]
#NAutoVTs=6
#ReserveVT=6
#KillUserProcesses=no
#KillOnlyUsers=
#KillExcludeUsers=root
#InhibitDelayMaxSec=5
#UserStopDelaySec=10
#HandlePowerKey=poweroff
#HandleSuspendKey=suspend
#HandleHibernateKey=hibernate
HandleLidSwitch=hibernate
HandleLidSwitchExternalPower=hibernate
HandleLidSwitchDocked=ignore
#HandleRebootKey=reboot
#PowerKeyIgnoreInhibited=no
#SuspendKeyIgnoreInhibited=no
#HibernateKeyIgnoreInhibited=no
#LidSwitchIgnoreInhibited=yes
#RebootKeyIgnoreInhibited=no
#HoldoffTimeoutSec=30s
#IdleAction=ignore
#IdleActionSec=30min
#RuntimeDirectorySize=10%
#RuntimeDirectoryInodes=400k
#RemoveIPC=yes
#InhibitorsMax=8192
#SessionsMax=8192
```

### Monitors management

Install a front-end for `xrandr` (which comes with `xorg`) called `arandr`. That's a simple GUI that will let you manage your monitors setup.

### Network management

During basic installation procces install `networkmanager`, `dhcpcd`, `netctl`, `dialog` and `wifi-menu`. `wifi-menu` will let you connect to wi-fi during installation and while using the OS. All wifi configs will be stored in `/etc/netctl` directory.

Example:

- Save network as `wlp5s0-network-name` (via `wifi-menu`).
- `sudo netctl enable wlp5s0-network-name` - run this service on init each time.
- `sudo netctl disable wlp5s0-network-name` - stop running this service on init.
- `sudo netctl start wlp5s0-network-name` - run this each time when logging in.
- `sudo netctl stop wlp5s0-network-name`
- `sudo netctl stop-all`
- `sudo netctl switch-to wlp5s0-network-name-2`

Basically network manager was installed during basic installation proccess, check that first (`networkmanager`).

### Touchpad settings

[Link](https://unix.stackexchange.com/questions/337008/activate-tap-to-click-on-touchpad) for reference.

### Make screenshots

- `sudo pacman -S scrot` - install a simple CLI for taking screenshots.
- `sudo pacman -S xclip` - install a tool for copying stuff into clipboard.
- `cat <image-name>.png | xclip -selection clipboard -t image/png` - copy image into clipboard.

### Default applications settings

[Article](https://wiki.archlinux.org/index.php/Xdg-utils) for reference.
