module TEAWin32.GUI.Component.Window.Internal (restoreWindowFromHWND) where

import           GHC.Stack              (HasCallStack)
import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI.Component (GUIComponent)

restoreWindowFromHWND :: HasCallStack => Win32.HWND -> IO GUIComponent
