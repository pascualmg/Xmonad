#!/bin/bash

# toggle-keyboard-layout.sh
# Este script toggle-keyboard-layout.sh cambia el layout del teclado entre español e inglés
# y muestra una notificación temporal en el centro de la pantalla

# Función para obtener las dimensiones de la pantalla
get_screen_dimensions() {
    xrandr | grep '*' | awk '{print $1}' | head -n1 | (IFS="x" read width height; echo $width $height)
}

# Función para mostrar notificación con dunst
show_temp_message_dunst() {
    dunstify -u low -t 2000 "Keyboard Layout" "$1"
}

# Función para mostrar notificación con dzen2
show_temp_message_dzen2() {
    read screen_width screen_height <<< $(get_screen_dimensions)
    notification_width=300
    notification_height=50
    x_position=$((($screen_width - $notification_width) / 2))
    y_position=$((($screen_height - $notification_height) / 2))

    (echo "$1" | dzen2 -p 2 -fn "DejaVu Sans-18:bold" -fg "#839496" -bg "#002b36" \
    -ta c -e 'onstart=uncollapse;button1=exit:0' \
    -h $notification_height -w $notification_width -x $x_position -y $y_position) &

    #use solarized colors for bg and fg
    # (echo "$1" | dzen2 -p 2 -fn "DejaVu Sans-18:bold" -fg "#839496" -bg "#002b36" \
}

# Función para mostrar notificación con xmessage
show_temp_message_xmessage() {
    timeout 2 xmessage -center -timeout 0 -buttons "" \
        -fg green -bg black \
        "$1" &
}

# Función principal para mostrar mensajes temporales
show_temp_message() {
    if command -v dzen2 &> /dev/null; then
        show_temp_message_dzen2 "$1"
    elif command -v dunstify &> /dev/null; then
        show_temp_message_dunst "$1"
    else
        show_temp_message_xmessage "$1"
        echo "Install dunstify or dzen2 for better notifications"
    fi
}

# Detectar el layout actual
current_layout=$(setxkbmap -query | grep layout | awk '{print $2}')

# Definir array con los layouts que queremos usar
layouts=("es" "us")

# Buscar el layout actual en el array y cambiar al siguiente
for i in "${!layouts[@]}"; do
    if [[ "${layouts[$i]}" = "$current_layout" ]]; then
        next_index=$(( (i + 1) % ${#layouts[@]} ))
        next_layout=${layouts[$next_index]}
        setxkbmap $next_layout
        show_temp_message "Keyboard layout: $next_layout"
        exit 0
    fi
done

# Si el layout actual no está en la lista, cambiar al primero
setxkbmap ${layouts[0]}
show_temp_message "Keyboard layout: ${layouts[0]}"
