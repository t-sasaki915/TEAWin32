{-# LANGUAGE ExistentialQuantification #-}

module Framework.TEA.Internal (Msg (..), updateChildren) where

import           Data.Data              (Typeable)
import           Graphics.GUI.Component (GUIComponent)
import qualified Graphics.Win32         as Win32

data Msg = forall a. (Typeable a, Eq a, Show a) => Msg a

updateChildren :: [GUIComponent] -> [GUIComponent] -> Win32.HWND -> IO ()
