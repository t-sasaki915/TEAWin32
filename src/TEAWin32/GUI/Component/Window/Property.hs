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

import           Data.Data                                 (Typeable, cast,
                                                            typeOf)
import           TEAWin32.Drawing                          (Colour)
import           TEAWin32.GUI
import qualified TEAWin32.GUI.Component.Internal           as ComponentInternal
import           TEAWin32.GUI.Component.Internal.Attribute
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
            Nothing   -> error "Failed to cast WindowProperty"

    unapplyProperty (WindowProperty x) = unapplyProperty x

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
    applyProperty (WindowIcon icon) windowHWND =
        ComponentInternal.setWindowIcon icon windowHWND >>
            addAttributeToHWND windowHWND (WindowIconAttr icon) >>
                addAttributeToHWND windowHWND (ComponentFlagAttr WindowIconSet)

    updateProperty (WindowIcon icon) _ windowHWND =
        ComponentInternal.setWindowIcon icon windowHWND >>
            updateAttributeOfHWND windowHWND (WindowIconAttr icon)

    unapplyProperty (WindowIcon icon) windowHWND =
        ComponentInternal.setWindowIcon Application windowHWND >>
            removeAttributeFromHWND windowHWND (WindowIconAttr icon) >>
                removeAttributeFromHWND windowHWND (ComponentFlagAttr WindowIconSet)

instance IsGUIComponentProperty WindowCursor where
    applyProperty (WindowCursor cursor) windowHWND =
        ComponentInternal.setWindowCursor cursor windowHWND >>
            addAttributeToHWND windowHWND (WindowCursorAttr cursor) >>
                addAttributeToHWND windowHWND (ComponentFlagAttr WindowCursorSet)

    updateProperty (WindowCursor cursor) _ windowHWND =
        ComponentInternal.setWindowCursor cursor windowHWND >>
            updateAttributeOfHWND windowHWND (WindowCursorAttr cursor)

    unapplyProperty (WindowCursor cursor) windowHWND =
        ComponentInternal.setWindowCursor Arrow windowHWND >>
            removeAttributeFromHWND windowHWND (WindowCursorAttr cursor) >>
                removeAttributeFromHWND windowHWND (ComponentFlagAttr WindowCursorSet)


instance IsGUIComponentProperty WindowBackgroundColour where
    applyProperty (WindowBackgroundColour colour) windowHWND =
        addAttributeToHWND windowHWND (ComponentBackgroundColourAttr colour) >>
            addAttributeToHWND windowHWND (ComponentFlagAttr ComponentBackgroundColourSet) >>
                ComponentInternal.requestRedraw windowHWND

    updateProperty (WindowBackgroundColour colour) _ windowHWND =
        updateAttributeOfHWND windowHWND (ComponentBackgroundColourAttr colour) >>
            ComponentInternal.requestRedraw windowHWND

    unapplyProperty (WindowBackgroundColour colour) windowHWND =
        removeAttributeFromHWND windowHWND (ComponentBackgroundColourAttr colour) >>
            removeAttributeFromHWND windowHWND (ComponentBackgroundColourAttr colour) >>
                ComponentInternal.requestRedraw windowHWND
