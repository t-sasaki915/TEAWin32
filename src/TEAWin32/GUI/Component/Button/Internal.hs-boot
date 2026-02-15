module TEAWin32.GUI.Component.Button.Internal (restoreButtonFromHWND) where

import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI.Component (GUIComponent)

restoreButtonFromHWND :: Win32.HWND -> IO GUIComponent
