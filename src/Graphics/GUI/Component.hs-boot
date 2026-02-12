{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component (GUIComponent (..) , IsGUIComponent (..)) where

import                          Data.Data                       (Typeable)
import                          Graphics.GUI                    (UniqueId)
import {-# SOURCE #-}           Graphics.GUI.Component.Property (GUIComponentProperty)
import                qualified Graphics.Win32                  as Win32

class Eq a => IsGUIComponent a where
    render :: a -> Maybe Win32.HWND -> IO Win32.HWND

    getProperties :: a -> [GUIComponentProperty]

    getUniqueId :: a -> UniqueId

    doesNeedToRedraw :: a -> a -> Bool

data GUIComponent = forall a. (Typeable a, Eq a, Show a, IsGUIComponent a) => GUIComponent a

instance Show GUIComponent
instance Eq GUIComponent
