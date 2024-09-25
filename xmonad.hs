-- Importaciones necesarias
import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing()
import XMonad.Actions.WithAll(sinkAll)


myStartupHooks = do
        spawnOnce "xrandr --output DP-0 --mode 5120x1440" -- Ajusta según tu
        spawnOnce "nitrogen --restore"  -- Restaura el fondo de pantalla
        spawnOnce "picom"  -- Compositor para efectos visuales
        --spawnOnce "eval $(ssh-agent)"
        --spawnOnce "ssh-add ~/.ssh/id_rsa"

--Para a;adir las aplicaciones de flatpack a dmenu
myDmenuCommand = "(flatpak list --app --columns=application | sed 's/^/flatpak run /' && dmenu_path) | sort -u | dmenu -i | ${SHELL:-\"/bin/sh\"} &"


main :: IO ()
main = xmonad $ docks $ def
    { modMask = mod4Mask  -- Usa la tecla Windows como modificador
    , terminal = "alacritty"
    , borderWidth = 1
    , normalBorderColor = "#002b36"  -- Fondo
    , focusedBorderColor = "#268bd2"  -- Borde de la ventana activa
    , startupHook = myStartupHooks
    }
    `additionalKeys`
    [ ((mod4Mask, xK_p), spawn myDmenuCommand)
    , ((mod4Mask .|. shiftMask, xK_l), spawn "slock")  -- Bloqueo de pantalla
    , ((0, 0x1008FF11), spawn "amixer set Master 5%-")  -- Bajar volumen
    , ((0, 0x1008FF13), spawn "amixer set Master 5%+")  -- Subir volumen
    , ((0, 0x1008FF12), spawn "amixer set Master toggle")  -- Silenciar/Activar sonido
    , ((mod4Mask, xK_t), spawn "xsel -o | python3 ~/tts.py") -- Text to speech
    , ((mod4Mask .|. shiftMask, xK_t), sinkAll)
    , ((mod4Mask, xK_e), spawn "/home/passh/.config/emacs/bin/doom run")
    ]
