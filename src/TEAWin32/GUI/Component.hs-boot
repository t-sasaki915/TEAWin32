{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.GUI.Component (GUIComponent (..) , IsGUIComponent (..)) where

import                          Data.Data                       (Typeable)
import                          GHC.Stack                       (HasCallStack)
import                qualified Graphics.Win32                  as Win32
import                          TEAWin32.GUI                    (UniqueId)
import {-# SOURCE #-}           TEAWin32.GUI.Component.Property (GUIComponentProperty)

class Eq a => IsGUIComponent a where
    render :: HasCallStack => a -> Maybe Win32.HWND -> IO Win32.HWND

    getProperties :: a -> [GUIComponentProperty]

    getUniqueId :: a -> UniqueId

    doesNeedToRedraw :: a -> a -> Bool

data GUIComponent = forall a. (Typeable a, Eq a, Show a, IsGUIComponent a) => GUIComponent a

instance Show GUIComponent
instance Eq GUIComponent
