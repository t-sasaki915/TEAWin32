{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.GUI.Component
    ( GUIComponents
    , IsGUIComponent (..)
    , GUIComponent (..)
    ) where

import                          Control.Monad.Writer            (Writer)
import                          Data.Data                       (Typeable, cast)
import                qualified Graphics.Win32                  as Win32
import                          TEAWin32.GUI                    (UniqueId)
import {-# SOURCE #-}           TEAWin32.GUI.Component.Property (GUIComponentProperty)

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
