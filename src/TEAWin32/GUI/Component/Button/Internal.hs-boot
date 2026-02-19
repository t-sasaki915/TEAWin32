module TEAWin32.GUI.Component.Button.Internal (restoreButtonFromHWND) where

import           GHC.Stack              (HasCallStack)
import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI.Component (GUIComponent)

restoreButtonFromHWND :: HasCallStack => Win32.HWND -> IO GUIComponent
