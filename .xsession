#!/usr/bin/env zsh

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Launch autostart apps
dex -a -e $XDG_SESSION_DESKTOP

# Add sound control to system tray
pa-applet &

# Fallback cursor
xsetroot -cursor_name left_ptr

# Keyboard repeat rate
xset r rate 200 60

# Start Emacs
exec emacs -f my/configure-exwm
