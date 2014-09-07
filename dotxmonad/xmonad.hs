import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
  
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
         { modMask = mod4Mask
         , manageHook = manageDocks <+> manageHook defaultConfig
         , layoutHook = avoidStruts  $  layoutHook defaultConfig
         , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
         } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/screenshots'")
        , ((0, xK_Print), spawn "scrot '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/screenshots'") ]
