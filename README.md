# Config files

A set of configuration files that help me to setup my OS the way I want.

## Arch installation guide

### Basic installation

Basically I was following [this video](https://www.youtube.com/watch?v=PQgyW10xD8s&list=PL5--8gKSku16Ncr9H_BAZSzWecjaSWlvY&index=5&ab_channel=DistroTube)...

### Desktop installation

When it comes to GUI setup, first thing we have to take care of is video drivers installation.

Run the command to determine what video card is in your machine: `lspci -v | grep -A1 -e VGA -e 3D`.
The output for my machine (Lenovo ThinkPad E580) was this:

```
00:02.0 VGA compatible controller: Intel Corporation UHD Graphics 620 (rev 07) (prog-if 00 [VGA controller])
	Subsystem: Lenovo Device 5069
```

Run the command that will list all the available video-drivers: `pacman -Ss xf86-video`.
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

As you may notice, some of the drivers are already installed on my machine. They were installed automatically
by Manjaro installer, so I'll probabably need the same packages for fresh Arch installation.
Another interesting thing is that accoring to [Arch Wiki](https://wiki.archlinux.org/index.php/Xorg#Driver_installation)
I have drivers for both `Radeon` and `Intel` video-cards. That's strange why the first command showed only one video-card.

When video-driver(s) is installed, we can move on to installing some basic packages for GUI.
Run the command: `sudo pacman -S xord xorg-xinit`.

Install the most Necessary programs:
- `nitrogen` for rendering wallpapers.
- `picom` as a window compositor.
- `kitty` and `alacritty` as terminal emulators.
- `rofi` for running (launching) applications.
- `firefox` and `google-chrome` for browsing.

Enable `AUR` packages in your system:
- Clone yay repo: `git clone https://aur.archlinux.org/yay-git.git`
- Make sure `base-devel` is installed, otherwise run `sudo pacman -S base-devel`
- `cd yay-git` and run `makepkg -si` to build and intall `yay` helper.

Install `open-ssh` to be able to generate ssh keys on your machine.

Export `LANG` variable from `.bashrc`: `export LANG=en_US.UTF8` (there was an issue with `xMobar`, it couldn't read the config
file, because it couldn't recognize the locale, but then this problem disappeared).

[This video](https://www.youtube.com/watch?v=pouX5VvX0_Q) will explain how to install Xorg along with the WM.

[Picom issue](https://github.com/yshui/picom/wiki/Vsync-Situation) - this might not be needed if all video-drivers are installed correctly.

### Install `JetBrains Mono` font

[Article](https://wiki.archlinux.org/index.php/fonts) for reference.

- Download font from the [official site](https://www.jetbrains.com/ru-ru/lp/mono).
- Install utilities that deal with archives: `sudo pacman -S zip unzip`
- Extract the archive: `unzip JetBrainsMono-2.225.zip`.
- Install (just move) font:
  1. Create a directory where all fonts are installed by default: `mkdir -p ~/.local/share/fonts/ttf`.
  2. Choose `ttf` fonts: `cd ~/Downloads/fonts`.
  3. Move fonts: `mv ttf ~/.local/share/fonts/ttf/JetBrainsMono`.

### Set typing repeat speed

In `.xinitrc` put this: `xset r rate 200 35` (aready done).

### Setup keyboard layouts

During basic installation proccess, generate both US and RU locales (with `locale-gen`).
Minor issue: Russian keyboard erases username in a terminal.

### Setup sound

[Article](https://linuxhint.com/guide_linux_audio/) for reference.

Install  `alsa-utils` (sound doesn't work without it), `pulse-audio` along with `pulsemixer` as a FrontEnd for PulseAudio.

### Power management

Go to `/etc/systemd/logind.conf` and change lid options. Brightness can be changed via terminal
Also install `htop` as a Task Manager.

### Network management

Install `netctl`, `dialog` and `wifi-menu` (when installing Arch)
`wifi-menu` will let you connect to wifi during installation and while using the OS.

All wifi configs will be stored in `/etc/netctl/` directory.

Example:

- Save network as `wlp5s0-network-name` (via `wifi-menu`).
- `sudo netctl enable wlp5s0-network-name` - will run this service on init each time
- `sudo netctl disable wlp5s0-network-name` - will stop running this service on init
- `sudo netctl start wlp5s0-network-name` - run this each time when logging in
- `sudo netctl stop wlp5s0-network-name`
- `sudo netctl stop-all`
- `sudo netctl switch-to wlp5s0-network-name-2`

Basically network manager was installed during basic installation proccess, check that first (`networkmanager`).
