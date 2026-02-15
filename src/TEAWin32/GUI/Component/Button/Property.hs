{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module TEAWin32.GUI.Component.Button.Property
    ( ButtonProperty (..)
    , IsButtonProperty
    ) where

import           Data.Data                       (Typeable, cast, typeOf)
import           TEAWin32.GUI.Component.Property

data ButtonProperty = forall a. (Typeable a, Show a, IsGUIComponentProperty a, IsButtonProperty a) => ButtonProperty a

instance Eq ButtonProperty where
    (ButtonProperty a) == (ButtonProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance Show ButtonProperty where
    show (ButtonProperty x) = show x

instance (Typeable a, Show a, IsGUIComponentProperty a, IsButtonProperty a) => IsPropertyWrapper ButtonProperty a where
    wrapComponentProperty = ButtonProperty

instance HasPropertyName ButtonProperty where
    getPropertyName (ButtonProperty a) = typeOf a

class Eq a => IsButtonProperty a

instance IsButtonProperty ButtonProperty

instance IsGUIComponentProperty ButtonProperty where
    applyProperty (ButtonProperty x) = applyProperty x

    updateProperty (ButtonProperty new) (ButtonProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> error "Failed to cast ButtonProperty"

    unapplyProperty (ButtonProperty x) = unapplyProperty x

instance IsButtonProperty ComponentTitle
instance IsButtonProperty ComponentSize
instance IsButtonProperty ComponentPosition
instance IsButtonProperty ComponentFont
instance IsButtonProperty ComponentOnClick
