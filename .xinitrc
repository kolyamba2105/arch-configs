#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
  xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
  xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
  xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
  xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# ----- MY CONFIG STARTS HERE -----

# Apply screen layout
# ~/.screenlayout/monitor-only.sh

if (xrandr --listmonitors | grep "Monitors: 2"); then
  # ~/.screenlayout/monitor-only.sh
  ~/.screenlayout/my-layout.sh
fi

# Set keyboard layouts
setxkbmap -layout us,pl,ru,ua -option grp:alt_shift_toggle

# Swap ESC and CAPS keys
setxkbmap -option caps:swapescape

# Set typing repeat speed
xset r rate 200 35

# Render wallpaper
nitrogen --restore &

# Start compositor
picom --config ~/.config/picom/picom.conf &

# Start window manager
xmonad --replace
