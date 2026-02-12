{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component
    ( GUIComponents
    , IsGUIComponent (..)
    , GUIComponent (..)
    ) where

import                          Control.Monad.Writer            (Writer)
import                          Data.Data                       (Typeable, cast)
import                          Graphics.GUI                    (UniqueId)
import {-# SOURCE #-}           Graphics.GUI.Component.Property (GUIComponentProperty)
import                qualified Graphics.Win32                  as Win32

type GUIComponents = Writer [GUIComponent] ()

class Eq a => IsGUIComponent a where
    render :: a -> Maybe Win32.HWND -> IO Win32.HWND

    getProperties :: a -> [GUIComponentProperty]

    getUniqueId :: a -> UniqueId

    doesNeedToRedraw :: a -> a -> Bool

data GUIComponent = forall a. (Typeable a, Eq a, Show a, IsGUIComponent a) => GUIComponent a

instance Show GUIComponent where
    show (GUIComponent a) = show a

instance Eq GUIComponent where
    (GUIComponent a) == (GUIComponent b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance IsGUIComponent GUIComponent where
    render (GUIComponent a) = render a

    getProperties (GUIComponent a) = getProperties a

    getUniqueId (GUIComponent a) = getUniqueId a

    doesNeedToRedraw (GUIComponent a) (GUIComponent b) =
        case cast b of
            Just b' -> doesNeedToRedraw a b'
            Nothing -> True
