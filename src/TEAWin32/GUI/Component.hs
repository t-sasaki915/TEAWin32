{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.GUI.Component
    ( GUIComponents
    , IsGUIComponent (..)
    , GUIComponent (..)
    , ZIndex (..)
    ) where

import                          Control.Monad.State.Strict      (State)
import                          Control.Monad.Writer.Strict     (WriterT)
import                          Data.Data                       (Typeable, cast)
import                          GHC.Stack                       (HasCallStack)
import                qualified Graphics.Win32                  as Win32
import                          TEAWin32.GUI                    (UniqueId)
import {-# SOURCE #-}           TEAWin32.GUI.Component.Property (GUIComponentProperty)

type GUIComponents = WriterT [GUIComponent] (State Int) ()

class Eq a => IsGUIComponent a where
    render :: HasCallStack => a -> Maybe Win32.HWND -> IO Win32.HWND

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
            | NewlyCreatedZIndex Int
            | NewlyCreatedZIndexWithUserSpecification Int Int
            deriving (Eq, Show)

instance Ord ZIndex where
    compare (ZIndexWithUserSpecification u1 s1) (ZIndexWithUserSpecification u2 s2) =
        compare u1 u2 <> compare s1 s2

    compare (NewlyCreatedZIndexWithUserSpecification u1 l1) (NewlyCreatedZIndexWithUserSpecification u2 l2) =
        compare u1 u2 <> compare l1 l2

    compare (ZIndexWithUserSpecification u1 _) (NewlyCreatedZIndexWithUserSpecification u2 _) =
        compare u1 u2 <> LT

    compare (NewlyCreatedZIndexWithUserSpecification u1 _) (ZIndexWithUserSpecification u2 _) =
        compare u1 u2 <> GT

    compare (ZIndexWithUserSpecification u1 _) _
        | u1 >= 0   = GT
        | otherwise = LT

    compare _ (ZIndexWithUserSpecification u2 _)
        | u2 >= 0   = LT
        | otherwise = GT

    compare (NewlyCreatedZIndexWithUserSpecification u1 _) _
        | u1 >= 0   = GT
        | otherwise = LT

    compare _ (NewlyCreatedZIndexWithUserSpecification u2 _)
        | u2 >= 0   = LT
        | otherwise = GT

    compare (NewlyCreatedZIndex l1) (NewlyCreatedZIndex l2) =
        compare l1 l2

    compare (SystemCalculatedZIndex s1) (SystemCalculatedZIndex s2) =
        compare s1 s2

    compare (NewlyCreatedZIndex _) (SystemCalculatedZIndex _) =
        GT

    compare (SystemCalculatedZIndex _) (NewlyCreatedZIndex _) =
        LT
