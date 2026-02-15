module TEAWin32.GUI.Component.Window.Internal (restoreWindowFromHWND) where

import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI.Component (GUIComponent)

restoreWindowFromHWND :: Win32.HWND -> IO GUIComponent
