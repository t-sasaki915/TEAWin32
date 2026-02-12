module Graphics.GUI.Component.Internal.Prop (isManagedByTEAWin32GUI) where

import qualified Graphics.Win32 as Win32

isManagedByTEAWin32GUI :: Win32.HWND -> IO Bool
