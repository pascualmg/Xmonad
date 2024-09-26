-- Importaciones necesarias
import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing()
import XMonad.Actions.WithAll(sinkAll)

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xrandr --output DP-0 --mode 5120x1440" -- Ajusta según tu configuración
    spawnOnce "nitrogen --restore"  -- Restaura el fondo de pantalla
    spawnOnce "picom"  -- Compositor para efectos visuales
    --spawnOnce "eval $(ssh-agent)"
    --spawnOnce "ssh-add ~/.ssh/id_rsa"

-- Para añadir las aplicaciones de flatpak a dmenu
myDmenuCommand :: String
myDmenuCommand = "(flatpak list --app --columns=application | sed 's/^/flatpak run /' && dmenu_path) | sort -u | dmenu -i | ${SHELL:-\"/bin/sh\"} &"

main :: IO ()
main = xmonad $ docks $ def
    { modMask = mod4Mask  -- Usa la tecla Windows como modificador
    , terminal = "alacritty"
    , borderWidth = 1
    , normalBorderColor = "#002b36"  -- Fondo
    , focusedBorderColor = "#268bd2"  -- Borde de la ventana activa
    , startupHook = myStartupHook
    }
    `additionalKeysP`
    [ ("M-p", spawn myDmenuCommand)
    , ("M-S-l", spawn "slock")  -- Bloqueo de pantalla
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")  -- Bajar volumen
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")  -- Subir volumen
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")  -- Silenciar/Activar sonido
    , ("M-t", spawn "xsel -o | python3 ~/.config/xmonad/tts.py") -- Text to speech
    , ("M-S-t", sinkAll) -- Hunde todas las ventanas flotantes
    , ("M-e", spawn "/home/passh/.config/emacs/bin/doom run")

    , ("M-;", spawn "/usr/bin/xdotool type 'ñ'")  -- Añadido para la ñ minúscula
    , ("M-S-;", spawn "/usr/bin/xdotool type 'Ñ'")  -- Añadido para la Ñ mayúscula
    ]
