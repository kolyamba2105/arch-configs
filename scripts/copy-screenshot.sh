#!/usr/bin/env bash

cat $1 | xclip -selection clipboard -t image/png

