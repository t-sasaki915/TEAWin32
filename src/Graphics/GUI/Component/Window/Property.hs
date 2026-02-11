{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component.Window.Property
    ( WindowProperty (..)
    , IsWindowProperty
    , WindowTitle (..)
    , WindowIcon (..)
    , WindowCursor (..)
    , WindowSize (..)
    , WindowPosition (..)
    , WindowBackgroundColour (..)
    , WindowChildren (..)
    ) where

import                          Control.Monad                   (forM_, void)
import                          Data.Bits                       ((.|.))
import                          Data.Data                       (Typeable, cast)
import                          Data.Text                       (Text)
import                qualified Data.Text                       as Text
import                qualified Framework.TEA.Internal          as TEAInternal
import                          Graphics.Drawing                (Colour)
import                          Graphics.GUI
import                          Graphics.GUI.Component          (GUIComponent (..),
                                                                 IsGUIComponent (render))
import                qualified Graphics.GUI.Component.Internal as ComponentInternal
import                          Graphics.GUI.Component.Property (IsGUIComponentProperty (..))
import {-# SOURCE #-}           Graphics.GUI.Component.Window   (destroyChildren)
import                qualified Graphics.GUI.Foreign            as Win32
import                qualified Graphics.Win32                  as Win32

data WindowProperty = forall a. (Typeable a, Show a, IsGUIComponentProperty a, IsWindowProperty a) => WindowProperty a

instance Eq WindowProperty where
    (WindowProperty a) == (WindowProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance Show WindowProperty where
    show (WindowProperty x) = show x

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

newtype WindowTitle            = WindowTitle            Text           deriving (Show, Eq)
newtype WindowIcon             = WindowIcon             Icon           deriving (Show, Eq)
newtype WindowCursor           = WindowCursor           Cursor         deriving (Show, Eq)
newtype WindowSize             = WindowSize             (Int, Int)     deriving (Show, Eq)
newtype WindowPosition         = WindowPosition         (Int, Int)     deriving (Show, Eq)
newtype WindowBackgroundColour = WindowBackgroundColour Colour         deriving (Show, Eq)
newtype WindowChildren         = WindowChildren         [GUIComponent] deriving (Show, Eq)

instance IsWindowProperty WindowTitle
instance IsWindowProperty WindowIcon
instance IsWindowProperty WindowCursor
instance IsWindowProperty WindowSize
instance IsWindowProperty WindowPosition
instance IsWindowProperty WindowBackgroundColour
instance IsWindowProperty WindowChildren

instance IsGUIComponentProperty WindowTitle where
    getPropertyName _ = "WindowTitle"

    applyProperty (WindowTitle title) windowHWND =
        Win32.setWindowText windowHWND (Text.unpack title) >>
            ComponentInternal.setFlag "WINDOWTITLE_SET" windowHWND

    updateProperty (WindowTitle title) _ windowHWND =
        Win32.setWindowText windowHWND (Text.unpack title)

    unapplyProperty _ windowHWND =
        Win32.setWindowText windowHWND "" >>
            ComponentInternal.unsetFlag "WINDOWTITLE_SET" windowHWND

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

instance IsGUIComponentProperty WindowSize where
    getPropertyName _ = "WindowSize"

    applyProperty (WindowSize (width, height)) windowHWND =
        Win32.c_SetWindowPos windowHWND
            Win32.nullPtr
            0
            0
            (fromIntegral width)
            (fromIntegral height)
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.setFlag "WINDOWSIZE_SET" windowHWND

    updateProperty (WindowSize (width, height)) _ windowHWND =
        void $ Win32.c_SetWindowPos windowHWND
            Win32.nullPtr
            0
            0
            (fromIntegral width)
            (fromIntegral height)
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    unapplyProperty _ windowHWND =
        Win32.c_SetWindowPos windowHWND
            Win32.nullPtr
            0
            0
            0
            0
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.unsetFlag "WINDOWSIZE_SET" windowHWND

instance IsGUIComponentProperty WindowPosition where
    getPropertyName _ = "WindowPosition"

    applyProperty (WindowPosition (x, y)) windowHWND =
        Win32.c_SetWindowPos
            windowHWND
            Win32.nullPtr
            (fromIntegral x)
            (fromIntegral y)
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.setFlag "WINDOWPOSITION_SET" windowHWND

    updateProperty (WindowPosition (x, y)) _ windowHWND =
        void $ Win32.c_SetWindowPos
            windowHWND
            Win32.nullPtr
            (fromIntegral x)
            (fromIntegral y)
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    unapplyProperty _ windowHWND =
        Win32.c_SetWindowPos
            windowHWND
            Win32.nullPtr
            0
            0
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.unsetFlag "WINDOWPOSITION_SET" windowHWND

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

instance IsGUIComponentProperty WindowChildren where
    getPropertyName _ = "WindowChildren"

    applyProperty (WindowChildren children) windowHWND =
        forM_ children (\(GUIComponent child) ->
            render child (Just windowHWND)) >>
                ComponentInternal.setFlag "WINDOWCHILDREN_SET" windowHWND

    updateProperty (WindowChildren newChildren) (WindowChildren oldChildren) =
        TEAInternal.updateChildren newChildren oldChildren

    unapplyProperty _ windowHWND =
        destroyChildren windowHWND >>
            ComponentInternal.unsetFlag "WINDOWCHILDREN_SET" windowHWND
