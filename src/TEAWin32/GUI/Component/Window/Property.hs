{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module TEAWin32.GUI.Component.Window.Property
    ( WindowProperty (..)
    , IsWindowProperty
    , WindowIcon (..)
    , WindowCursor (..)
    , WindowBackgroundColour (..)
    ) where

import           Data.Data                       (Typeable, cast, typeOf)
import           TEAWin32.Drawing                (Colour)
import           TEAWin32.Exception              (TEAWin32Error (..),
                                                  errorTEAWin32)
import           TEAWin32.GUI
import           TEAWin32.GUI.Component.Property

data WindowProperty = forall a. (Typeable a, Show a, IsGUIComponentProperty a, IsWindowProperty a) => WindowProperty a

instance Eq WindowProperty where
    (WindowProperty a) == (WindowProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance Show WindowProperty where
    show (WindowProperty x) = show x

instance (Typeable a, Show a, IsGUIComponentProperty a, IsWindowProperty a) => IsPropertyWrapper WindowProperty a where
    wrapComponentProperty = WindowProperty

instance HasPropertyName WindowProperty where
    getPropertyName (WindowProperty a) = typeOf a

instance MayHaveZIndexProperty WindowProperty where
    getZIndexProperty (WindowProperty a) =
        case cast a of
            Just prop@(ComponentZIndex _) -> Just prop
            Nothing                       -> Nothing

class Eq a => IsWindowProperty a

instance IsWindowProperty WindowProperty

instance IsGUIComponentProperty WindowProperty where
    applyProperty (WindowProperty x) = applyProperty x

    updateProperty (WindowProperty new) (WindowProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> errorTEAWin32 (InternalTEAWin32Error "Failed to cast WindowProperty")

    unapplyProperty (WindowProperty x) = unapplyProperty x

    isPropertyChanged (WindowProperty x) = isPropertyChanged x

newtype WindowIcon             = WindowIcon             Icon           deriving (Show, Eq)
newtype WindowCursor           = WindowCursor           Cursor         deriving (Show, Eq)
newtype WindowBackgroundColour = WindowBackgroundColour Colour         deriving (Show, Eq)

instance IsWindowProperty WindowIcon
instance IsWindowProperty WindowCursor
instance IsWindowProperty WindowBackgroundColour

instance IsWindowProperty ComponentTitle
instance IsWindowProperty ComponentSize
instance IsWindowProperty ComponentPosition
instance IsWindowProperty ComponentFont
instance IsWindowProperty ComponentZIndex
instance IsWindowProperty ComponentChildren

instance IsGUIComponentProperty WindowIcon where

instance IsGUIComponentProperty WindowCursor where

instance IsGUIComponentProperty WindowBackgroundColour where
