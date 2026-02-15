module TEAWin32.GUI.Component.Property.Internal (PropertyUpdateAction (..), compareProperties) where

import qualified Data.Map                        as Map
import           TEAWin32.GUI.Component.Property (GUIComponentProperty,
                                                  HasPropertyName (..))

data PropertyUpdateAction = AddProperty GUIComponentProperty
                          | DeleteProperty GUIComponentProperty
                          | UpdateProperty GUIComponentProperty GUIComponentProperty
                          | NoPropertyChange

compareProperties :: [GUIComponentProperty] -> [GUIComponentProperty] -> [PropertyUpdateAction]
compareProperties newProperties oldProperties =
    let newPropertiesWithName = Map.fromList [ (getPropertyName x, x) | x <- newProperties ]
        oldPropertiesWithName = Map.fromList [ (getPropertyName x, x) | x <- oldProperties ]
        deletedProperties = [ DeleteProperty x | x <- Map.elems $ Map.difference oldPropertiesWithName newPropertiesWithName ]
        newPropertiesWithAction =
            flip map newProperties $ \newProperty ->
                case Map.lookup (getPropertyName newProperty) oldPropertiesWithName of
                    Just oldProperty | newProperty == oldProperty ->
                        NoPropertyChange

                    Just oldProperty ->
                        UpdateProperty newProperty oldProperty

                    Nothing ->
                        AddProperty newProperty

    in deletedProperties ++ newPropertiesWithAction
