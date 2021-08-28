# Arch Linux setup process

Mostly following [Arch installation guide](https://wiki.archlinux.org/title/installation_guide).

## Pre-installation

### Connect to the Internet

- Run `iwctl` (iNet wireless daemon interactive prompt)
- From interactive prompt:

  - device list
  - station _device_ scan
  - station _device_ get-networks
  - station _device_ connect _network_

### Update the system clock

- Use timedatectl to ensure the system clock is accurate

```sh
timedatectl set-ntp true
```

- Verify service status

```sh
timedatectl status
```

### Partition the disks

- List available devices

```sh
fdisk -l
```

- Start `fdisk` interactive prompt

```sh
fdisk <disk-name>
```

- From interactive prompt:

  - `g` - create a new empty GPT partition table
  - `n` - add a new partition
  - `t` - change partition type
  - `w` - write table to disk and exit

- Partition table:
  1. EFI partition (Size: +1G; Type: EFI System)
  2. Swap partition (Size: +2G; Type: Linux swap)
  3. Root partition (Size: remainder; Type: Linux filesystem)

### Format the partitions

1. EFI

   ```sh
   mkfs.fat -F32 /dev/<efi-partition-name>
   ```

2. Swap

   ```sh
   mkswap /dev/<swap-partition-name>
   ```

3. Root

   ```sh
   mkfs.ext4 /dev/<root-partition-name>
   ```

### Mount the file systems

1. Root

   ```sh
   mount /dev/<root-partition-name> /mnt
   ```

2. Swap

   ```sh
   swapon /dev/<swap-partition-name>
   ```

## Installation

### Install essential packages

```sh
pacstrap /mnt base base-devel linux linux-firmware git neovim
```

## Configure the system

### Generate file system table

```sh
genfstab -U /mnt >> /mnt/etc/fstab
```

### Chroot

```sh
arch-chroot /mnt
```

### Time zone

```sh
ln -sf /usr/share/zoneinfo/Europe/Kiev /etc/localtime
hwclock --systohc
```

### Localization

- Edit `/etc/locale.gen` and uncomment `en_US.UTF-8 UTF-8` and other needed locales:

  - `pl_PL.UTF-8 UTF-8`
  - `ru_RU.UTF-8 UTF-8`
  - `uk_UA.UTF-8 UTF-8`

- Generate the locales by running `locale-gen`

### Network configuration

- Create the `hostname` file

```sh
nvim /etc/hostname
```

Type the hostname (for ex. `Lenovo-ThinkPad`)

- Create `hosts` file

```sh
nvim /etc/hosts
```

Add the following content:

```sh
127.0.0.1     localhost
::1           localhost
127.0.1.1     Lenovo-ThinkPad.localdomain Lenovo-ThinkPad
```

### Users and passwords

- Set the root password

```sh
passwd
```

- Add non-root user

```sh
useradd -m <user-name>
```

- Set password for non-root user

```sh
passwd <user-name>
```

- Add non-root user to groups

```sh
usermod -aG wheel,audio,video,optical,storage <user-name>
```

- Edit `sudoers` file

```sh
EDITOR=nvim visudo
```

Uncomment the following line: `%wheel ALL=(ALL) ALL`

### Boot loader

- Install `grub` and other helper packages

```sh
pacman -S grub efibootmgr dosfstools os-prober mtools
```

- Create EFI directory

```sh
mkdir /boot/EFI
```

- Mount EFI partition

```sh
mount /dev/<efi-partition-name> /boot/EFI
```

- Install grub on EFI partition

```sh
grub-install --target=x86_64-efi --bootloader-id=grub_uefi --recheck
```

- Generate grub configuration file

```sh
grub-mkconfig -o /boot/grub/grub.cfg
```

### Before reboot

- Install network tools

```sh
pacman -S dhcpcd dialog netctl wpa_supplicant
```

### Reboot

- Exit `chroot` environment

```sh
exit
```

- Unmount root partition

```sh
umount -R /mnt
```

- Reboot and remove installation media

```sh
reboot
```

## Post-installation

### Clone this repo

```sh
git clone https://github.com/kolyamba2105/config-files.git
```

### Install `yay`

```sh
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
cd
rm -rf yay
```

### Clone `my-wallpapers` repo

```sh
mkdir ~/Pictures
cd ~/Pictures
mkdir Screenshots
git clone https://github.com/kolyamba2105/my-wallpapers.git
```

### Install packages

- Install official packages

```sh
cd ~/config-files/packages
pacman -S --needed - < pkgs.txt
```

- Install packages from AUR

```sh
cd ~/config-files/packages
yay -S --needed - < foreign-pkgs.txt
```

### Setup config files

```sh
cd ~/config-files
sh config-files.sh
```

### Setup ZSH

[Install ohmyzsh](https://github.com/ohmyzsh/ohmyzsh#basic-installation)

### Setup XMonad

[Setup guide](./.xmonad/README.md)

### Log into graphical environment

Just reboot and follow the final steps...

### Setup Neovim

[Setup guide](./.config/nvim/README.md)

### Setup SSH

- Generate `ssh` key

```sh
ssh-keygen
```

- Add content of `~/.ssh/id_rsa.pub` to Github

### Setup VPN

- Follow [this guide](https://michaelheap.com/arch-linux-netctl-and-vpnc-cisco-vpn)
- Set vpn profile in `/etc/vpnc/vpn-<username>.conf`

### Install global `yarn` packages

```sh
yarn global add @buildo/hophop @stoplight/prism-cli corsproxy-https-insecure eslint_d n prettier
```

### Laptop brightness setup

- Add the following line to `/etc/default/grub`

```sh
GRUB_CMDLINE_LINUX="intel_backlight.enable_dpcd_backlight=0"
```

- Update grub (generate grub config file again)

```sh
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

- Reboot

### Touchpad setup

Create `/etc/X11/xorg.conf.d/99-synaptics-overrides.conf` file and add the
following content:

```sh
Section "InputClass"
        Identifier "touchpad overrides"
        Driver "libinput"
        MatchIsTouchpad "on"
        Option "Tapping" "on"
EndSection
```

### Screen lock setup

- Create `/etc/systemd/system/slock@.service` file and add the following content:

```sh
[Unit]
Description=Lock X session using slock for user %i
Before=sleep.target

[Service]
User=%i
Environment=DISPLAY=:0
ExecStartPre=/usr/bin/xset dpms force suspend
ExecStart=/usr/bin/slock

[Install]
WantedBy=sleep.target
```

- Enable service

```sh
systemctl enable slock@<user-name>.service
```

### Pacman list-packages hook

- Create `/usr/share/libalpm/hooks/list-packages.hook` file and add the following content:

```sh
[Trigger]
Operation = Install
Operation = Remove
Type = Package
Target = *

[Action]
When = PostTransaction
Exec = /bin/sh -c '/usr/bin/pacman -Qqen > /etc/pkglist.txt'
```

### Connect HDD

- Use `fdisk` to partition the disk

```sh
# Create the only partion of type "Linux FileSystem"
fdisk /dev/sda
```

- Format partition

```sh
mkfs.ext4 /dev/sda1
```

- Add an entry in `/etc/fstab` file

```sh
# Retrieve disk UUID
lsa /dev/disk/by-uuid
```

- Create mount point

```sh
mkdir /hdd
```

- Re-mount

```sh
mount -a
```

- Change permissions

```sh
chgrp storage /hdd
chmod 775 /hdd

# This may be optional if user was already added to storage group
gpasswd -a <user-name> storage
```

## Useful Wiki pages

- [Bluetooth](https://wiki.archlinux.org/index.php/Bluetooth)
- [Default applications](https://wiki.archlinux.org/index.php/Xdg-utils)
- [Network configuration](https://wiki.archlinux.org/index.php/Network_configuration)
- [Pacman - Tips & Tricks](https://wiki.archlinux.org/title/Pacman/Tips_and_tricks)
- [Power management](https://wiki.archlinux.org/index.php/Power_management)
- [Xorg and video drivers](https://wiki.archlinux.org/index.php/xorg)
