#!/usr/bin/env bash

controller="C0:B6:F9:AD:2B:E9"

if bluetoothctl show $controller | grep "Powered: yes"; then
  bluetoothctl power off
else
  bluetoothctl power on
fi
