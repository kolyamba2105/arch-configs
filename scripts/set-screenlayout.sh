#!/usr/bin/env bash

if (xrandr --listmonitors | grep "Monitors: 2"); then
  # ~/.screenlayout/monitor-only.sh
  ~/.screenlayout/my-layout.sh
fi
