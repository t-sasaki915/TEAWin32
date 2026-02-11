module Graphics.GUI.Component.Window (destroyChildren) where

import qualified Graphics.Win32 as Win32

destroyChildren :: Win32.HWND -> IO ()
