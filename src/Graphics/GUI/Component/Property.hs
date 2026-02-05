module Graphics.GUI.Component.Property (IsGUIComponentProperty (..)) where

import qualified Graphics.Win32 as Win32

class Eq a => IsGUIComponentProperty a where
    applyProperty :: a -> Win32.HWND -> IO ()

    unapplyProperty :: a -> Win32.HWND -> IO ()
