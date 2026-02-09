{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component
    ( GUIComponents
    , IsGUIComponent (..)
    , GUIComponent (..)
    , compareGUIComponents
    ) where

import           Control.Monad.Writer            (Writer)
import           Data.Data                       (Typeable, cast)
import qualified Data.Map                        as Map
import           Graphics.GUI                    (UniqueId)
import           Graphics.GUI.Component.Property (GUIComponentProperty)
import qualified Graphics.Win32                  as Win32

type GUIComponents = Writer [GUIComponent] ()

class Eq a => IsGUIComponent a where
    render :: a -> Maybe Win32.HWND -> IO Win32.HWND

    getProperties :: a -> [GUIComponentProperty]

    getUniqueId :: a -> UniqueId

    doesNeedToRedraw :: a -> a -> Bool

    getChildren :: a -> [GUIComponent]

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

    getChildren (GUIComponent a) = getChildren a

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [(GUIComponent, GUIComponent)])
compareGUIComponents new old = (added, deleted, redraw, propertyChanged)
    where
        newMap = Map.fromList [ (getUniqueId x, x) | x <- new ]
        oldMap = Map.fromList [ (getUniqueId x, x) | x <- old ]

        added   = Map.elems $ Map.difference newMap oldMap
        deleted = Map.elems $ Map.difference oldMap newMap

        commonKeys = Map.keys $ Map.intersection newMap oldMap
        (redraw, propertyChanged) = foldr (checkChange newMap oldMap) ([], []) commonKeys

        checkChange nMap oMap k (redr, propc) =
            let newValue = nMap Map.! k
                oldValue = oMap Map.! k in
                    if newValue == oldValue
                        then (redr, propc)
                        else
                            if doesNeedToRedraw oldValue newValue
                                then (newValue : redr, propc)
                                else (redr, (newValue, oldValue) : propc)

