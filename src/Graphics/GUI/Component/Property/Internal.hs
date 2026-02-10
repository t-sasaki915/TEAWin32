module Graphics.GUI.Component.Property.Internal (compareProperties) where

import qualified Data.Map                        as Map
import Data.Map ((!))
import           Graphics.GUI.Component.Property (GUIComponentProperty,
                                                  IsGUIComponentProperty (..))

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
            | nMap ! k == oMap ! k = chgd
            | otherwise                    = (nMap ! k, oMap ! k) : chgd
