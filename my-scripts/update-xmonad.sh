#!/bin/bash

# Actualizar submodulos si existen
git submodule update --init --recursive
git submodule update --remote

# Recompilar XMonad
cd ~/.config/xmonad
stack build
xmonad --recompile

# Reiniciar XMonad
xmonad --restart

echo "XMonad actualizado y recompilado correctamente"
