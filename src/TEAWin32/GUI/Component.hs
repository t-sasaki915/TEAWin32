{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.GUI.Component
    ( GUIComponents
    , IsGUIComponent (..)
    , GUIComponent (..)
    , ZIndex (..)
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

data ZIndex = SystemCalculatedZIndex Int
            | ZIndexWithUserSpecification Int Int
            deriving (Eq, Show)

instance Ord ZIndex where
    compare (ZIndexWithUserSpecification usrIndex1 sysIndex1) (ZIndexWithUserSpecification usrIndex2 sysIndex2) =
        compare usrIndex1 usrIndex2 <> compare sysIndex1 sysIndex2

    compare (ZIndexWithUserSpecification usrIndex1 _) (SystemCalculatedZIndex _)
        | usrIndex1 >= 0 = GT
        | otherwise      = LT

    compare (SystemCalculatedZIndex _) (ZIndexWithUserSpecification usrIndex2 _)
        | usrIndex2 >= 0 = LT
        | otherwise      = GT

    compare (SystemCalculatedZIndex sysIndex1) (SystemCalculatedZIndex sysIndex2) =
        compare sysIndex1 sysIndex2
