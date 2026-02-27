{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.GUI.Component
    ( ComponentType (..)
    , IsGUIComponent (..)
    , GUIComponent (..)
    , ZIndex (..)
    , EventType (..)
    ) where

import           Data.Data    (Typeable, cast)
import           GHC.Stack    (HasCallStack)
import           TEAWin32.GUI (UniqueId)

data ComponentType = ComponentWindow
                   | ComponentButton
                   deriving (Eq, Show, Ord)

data EventType = ComponentClickEvent
               deriving (Eq, Show)

class Eq a => IsGUIComponent a where
    scheduleRendering :: HasCallStack => a -> Maybe UniqueId -> IO ()

data GUIComponent = forall a. (Typeable a, Eq a, Show a, IsGUIComponent a) => GUIComponent a

instance Show GUIComponent where
    show (GUIComponent a) = show a

instance Eq GUIComponent where
    (GUIComponent a) == (GUIComponent b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance IsGUIComponent GUIComponent where
    scheduleRendering (GUIComponent a) = scheduleRendering a

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
