{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Graphics.GUI.Component.Window.Property
    ( WindowProperty (..)
    , IsWindowProperty
    , WindowIcon (..)
    , WindowCursor (..)
    , WindowBackgroundColour (..)
    ) where

import           Control.Monad                   (void)
import           Data.Data                       (Typeable, cast)
import           Graphics.Drawing                (Colour)
import           Graphics.GUI
import qualified Graphics.GUI.Component.Internal as ComponentInternal
import           Graphics.GUI.Component.Property
import qualified Graphics.GUI.Foreign            as Win32
import qualified Graphics.Win32                  as Win32

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

class Eq a => IsWindowProperty a

instance IsWindowProperty WindowProperty

instance IsGUIComponentProperty WindowProperty where
    applyProperty (WindowProperty x) = applyProperty x

    updateProperty (WindowProperty new) (WindowProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> error "Failed to cast WindowProperty"

    unapplyProperty (WindowProperty x) = unapplyProperty x

    getPropertyName (WindowProperty x) = getPropertyName x

newtype WindowIcon             = WindowIcon             Icon           deriving (Show, Eq)
newtype WindowCursor           = WindowCursor           Cursor         deriving (Show, Eq)
newtype WindowBackgroundColour = WindowBackgroundColour Colour         deriving (Show, Eq)

instance IsWindowProperty WindowIcon
instance IsWindowProperty WindowCursor
instance IsWindowProperty WindowBackgroundColour

instance IsWindowProperty ComponentTitle
instance IsWindowProperty ComponentSize
instance IsWindowProperty ComponentPosition
instance IsWindowProperty ComponentChildren

instance IsGUIComponentProperty WindowIcon where
    getPropertyName _ = "WindowIcon"

    applyProperty (WindowIcon icon) windowHWND =
        (toWin32Icon icon >>= \icon' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HICON icon') >>
                ComponentInternal.setFlag "WINDOWICON_SET" windowHWND

    updateProperty (WindowIcon icon) _ windowHWND =
        toWin32Icon icon >>= \icon' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HICON icon'

    unapplyProperty _ windowHWND =
        void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HICON Win32.nullPtr >>
            ComponentInternal.unsetFlag "WINDOWICON_SET" windowHWND

instance IsGUIComponentProperty WindowCursor where
    getPropertyName _ = "WindowCursor"

    applyProperty (WindowCursor cursor) windowHWND =
        (toWin32Cursor cursor >>= \cursor' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HCURSOR cursor') >>
                ComponentInternal.setFlag "WINDOWCURSOR_SET" windowHWND

    updateProperty (WindowCursor cursor) _ windowHWND =
        toWin32Cursor cursor >>= \cursor' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HCURSOR cursor'

    unapplyProperty _ windowHWND =
        void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HCURSOR Win32.nullPtr >>
            ComponentInternal.unsetFlag "WINDOWCURSOR_SET" windowHWND


instance IsGUIComponentProperty WindowBackgroundColour where
    getPropertyName _ = "WindowBackgroundColour"

    applyProperty (WindowBackgroundColour colour) windowHWND =
        ComponentInternal.setWindowBackgroundColour colour windowHWND >>
            ComponentInternal.setFlag "WINDOWBACKGROUNDCOLOUR_SET" windowHWND >>
                Win32.invalidateRect (Just windowHWND) Nothing True

    updateProperty (WindowBackgroundColour colour) _ windowHWND =
        ComponentInternal.setWindowBackgroundColour colour windowHWND >>
            Win32.invalidateRect (Just windowHWND) Nothing True

    unapplyProperty _ windowHWND =
        ComponentInternal.removeWindowBackgroundColour windowHWND >>
            ComponentInternal.unsetFlag "WINDOWBACKGROUNDCOLOUR_SET" windowHWND >>
                Win32.invalidateRect (Just windowHWND) Nothing True
