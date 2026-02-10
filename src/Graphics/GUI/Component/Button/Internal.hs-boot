module Graphics.GUI.Component.Button.Internal (restoreButtonFromHWND) where

import           Graphics.GUI.Component (GUIComponent)
import qualified Graphics.Win32         as Win32

restoreButtonFromHWND :: Win32.HWND -> IO GUIComponent
