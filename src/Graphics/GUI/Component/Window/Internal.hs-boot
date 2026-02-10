module Graphics.GUI.Component.Window.Internal (restoreWindowFromHWND) where

import           Graphics.GUI.Component (GUIComponent)
import qualified Graphics.Win32         as Win32

restoreWindowFromHWND :: Win32.HWND -> IO GUIComponent
