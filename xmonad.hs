-- Importaciones necesarias
import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing()
import XMonad.Actions.WithAll(sinkAll)
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xrandr --output DP-0 --mode 5120x1440" -- Ajusta según tu configuración
    spawnOnce "nitrogen --restore"  -- Restaura el fondo de pantalla
    spawnOnce "picom"  -- Compositor para efectos visuales
    spawnOnce "~/.local/share/JetBrains/Toolbox/bin/jetbrains-toolbox"
    --spawnOnce "eval $(ssh-agent)"
    --spawnOnce "ssh-add ~/.ssh/id_rsa"

-- Para añadir las aplicaciones de flatpak a dmenu
myDmenuCommand :: String
myDmenuCommand = "(flatpak list --app --columns=application | sed 's/^/flatpak run /' && dmenu_path) | sort -u | dmenu -i | ${SHELL:-\"/bin/sh\"} &"

myScratchPads = [
    NS "terminal" "alacritty --class scratchpad -e byobu" (className =? "scratchpad")
        (customFloating $ W.RationalRect 0.1 0.1 0.9 0.9),
    NS "doom" "doom run --class=scratchpad-notes" (className =? "scratchpad-notes")
        (customFloating $ W.RationalRect 0.15 0.1 0.7 0.75)
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"  -- Inicia xmobar
    xmonad $ docks $ def
        { modMask = mod4Mask  -- Usa la tecla Windows como modificador
        , terminal = "alacritty"
        , borderWidth = 1
        , normalBorderColor = "#002b36"  -- Fondo
        , focusedBorderColor = "#268bd2"  -- Borde de la ventana activa
        , startupHook = myStartupHook
        , manageHook = namedScratchpadManageHook myScratchPads <+> manageHook def
        , layoutHook = avoidStruts $ layoutHook def  -- Evita que las ventanas cubran xmobar
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]"
            , ppVisible = xmobarColor "#c3e88d" ""
            , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
            , ppHiddenNoWindows = xmobarColor "#F07178" ""
            , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60
            , ppSep =  "<fc=#666666> | </fc>"
            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
            }
        }
        `additionalKeysP`
        [ ("M-p", spawn myDmenuCommand)
        , ("M-S-l", spawn "slock")  -- Bloqueo de pantalla
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")  -- Bajar volumen
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")  -- Subir volumen
        , ("<XF86AudioMute>", spawn "amixer set Master toggle")  -- Silenciar/Activar sonido
        --tts
        , ("M-t", spawn "xsel -o | python3 ~/.config/xmonad/my-scripts/tts.py")
        -- Hunde todas las ventanas flotantes
        , ("M-s", sinkAll)
        --ides
        , ("M-S-e", spawn "/home/passh/.config/emacs/bin/doom run")
        , ("M-i", spawn "$(which idea)")
        --keyboard es-us layout toggling izi
        , ("M-ñ", spawn "~/.config/xmonad/my-scripts/toggle-keyboard-layout.sh")
        , ("M-;", spawn "~/.config/xmonad/my-scripts/toggle-keyboard-layout.sh")
        --scratchpads
        , ("M-a", namedScratchpadAction myScratchPads "terminal")
        , ("M-e", namedScratchpadAction myScratchPads "doom")
        -- Flameshot GUI
        , ("<Print>", spawn "flameshot gui")
        ]
