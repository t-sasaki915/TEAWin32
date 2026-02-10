module Graphics.GUI.Component.Internal (compareGUIComponents) where

import qualified Data.Map               as Map
import           Graphics.GUI.Component (GUIComponent, IsGUIComponent (..))

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
