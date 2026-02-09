{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component.Property
    ( GUIComponentProperty (..)
    , IsGUIComponentProperty (..)
    , compareProperties
    ) where

import           Data.Data      (Typeable, cast)
import qualified Data.Map       as Map
import           Data.Text      (Text)
import qualified Graphics.Win32 as Win32

data GUIComponentProperty = forall a. (Typeable a, Show a, IsGUIComponentProperty a) => GUIComponentProperty a

instance Eq GUIComponentProperty where
    (GUIComponentProperty x) == (GUIComponentProperty y) =
        case cast y of
            Just y' -> x == y'
            Nothing -> False

instance Show GUIComponentProperty where
    show (GUIComponentProperty x) = show x

class Eq a => IsGUIComponentProperty a where
    applyProperty :: a -> Win32.HWND -> IO ()

    updateProperty :: a -> a -> Win32.HWND -> IO ()

    unapplyProperty :: a -> Win32.HWND -> IO ()

    getPropertyName :: a -> Text

instance IsGUIComponentProperty GUIComponentProperty where
    applyProperty (GUIComponentProperty x) = applyProperty x

    updateProperty (GUIComponentProperty new) (GUIComponentProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> error "Failed to cast GUIComponentProperty"

    unapplyProperty (GUIComponentProperty x) = unapplyProperty x

    getPropertyName (GUIComponentProperty x) = getPropertyName x

compareProperties :: [GUIComponentProperty] -> [GUIComponentProperty] -> ([GUIComponentProperty], [GUIComponentProperty], [(GUIComponentProperty, GUIComponentProperty)])
compareProperties new old = (added, deleted, changed)
    where
        newMap = Map.fromList [ (getPropertyName x, x) | x <- new ]
        oldMap = Map.fromList [ (getPropertyName x, x) | x <- old ]

        added   = Map.elems $ Map.difference newMap oldMap
        deleted = Map.elems $ Map.difference oldMap newMap

        commonKeys = Map.keys $ Map.intersection newMap oldMap
        changed = foldr (checkChange newMap oldMap) [] commonKeys

        checkChange nMap oMap k chgd
            | nMap Map.! k == oMap Map.! k = chgd
            | otherwise                    = (nMap Map.! k, oMap Map.! k) : chgd

