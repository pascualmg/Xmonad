#!/bin/bash

# Obtener el directorio del script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CONFIG_DIR="$HOME/.config"

# Función para crear enlaces simbólicos
create_symlink() {
    local src="$1"
    local dest="$2"

    # Crear directorio padre si no existe
    mkdir -p "$(dirname "$dest")"

    # Backup si existe y no es un symlink
    if [ -f "$dest" ] && [ ! -L "$dest" ]; then
        mv "$dest" "${dest}.backup"
        echo "Backup creado: ${dest}.backup"
    fi

    # Crear symlink
    ln -sf "$src" "$dest"
    echo "✓ Enlace creado: $dest -> $src"
}

echo "Creando enlaces simbólicos..."

# xmobar -> desde xmonad/xmobar/xmobarrc
create_symlink "$SCRIPT_DIR/xmonad/xmobar/xmobarrc" "$CONFIG_DIR/xmobar/xmobarrc"

# picom -> desde xmonad/picom/picom.conf
create_symlink "$SCRIPT_DIR/xmonad/picom/picom.conf" "$CONFIG_DIR/picom/picom.conf"

# xmonad.hs -> desde xmonad/xmonad.hs
create_symlink "$SCRIPT_DIR/xmonad/xmonad.hs" "$CONFIG_DIR/xmonad/xmonad.hs"

# Recompilar xmonad
if command -v xmonad >/dev/null 2>&1; then
    echo "Recompilando xmonad..."
    xmonad --recompile && xmonad --restart
fi

echo "¡Instalación completada!"
