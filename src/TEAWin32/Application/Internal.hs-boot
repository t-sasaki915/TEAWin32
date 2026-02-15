{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.Application.Internal (Msg (..), updateComponents) where

import           Data.Data              (Typeable)
import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI.Component (GUIComponent)

data Msg = forall a. (Typeable a, Eq a, Show a) => Msg a

updateComponents :: [GUIComponent] -> [GUIComponent] -> Maybe Win32.HWND -> IO ()
