import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myStartupHook = do
     spawn "bash ~/bin/xmonad-startup.sh"

main = do
     xmproc <- spawnPipe "xmobar"
     xmonad $ defaultConfig
            { manageHook = manageDocks <+> manageHook defaultConfig
            , layoutHook = avoidStruts $ layoutHook defaultConfig
            , logHook = dynamicLogWithPP xmobarPP
                            { ppOutput= hPutStrLn xmproc
                            , ppTitle = xmobarColor "green" "" . shorten 50
                            }
            , modMask = mod4Mask
            , terminal = "urxvt"
            , startupHook = myStartupHook
            } `additionalKeys`
            [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
            , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
            , ((0, xK_Print), spawn "scrot")
            ]
