

import XMonad
import XMonad.Util.EZConfig
import XMonad.StackSet
import XMonad.Actions.SpawnOn


main =
    xmonad $ defaultConfig
         { modMask = mod4Mask
         }
        `additionalKeys`
        (
        [ ((mod1Mask, xK_Tab), windows focusDown)
        , ((mod1Mask .|. shiftMask, xK_Tab), windows focusDown)
        , ((mod1Mask .|. shiftMask, xK_Return), spawn $ XMonad.terminal defaultConfig)
        ]
        ++
        [((mod1Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces defaultConfig) [xK_1 .. xK_9]
        , (f, m) <- [(greedyView, 0)]]
        )
        
