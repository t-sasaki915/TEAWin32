{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.Application.Internal (Msg (..), updateChildren) where

import           Data.Data              (Typeable)
import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI.Component (GUIComponent)

data Msg = forall a. (Typeable a, Eq a, Show a) => Msg a

updateChildren :: [GUIComponent] -> [GUIComponent] -> Win32.HWND -> IO ()
