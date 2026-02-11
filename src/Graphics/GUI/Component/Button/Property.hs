{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component.Button.Property
    ( ButtonProperty (..)
    , IsButtonProperty
    , ButtonLabel (..)
    , ButtonSize (..)
    , ButtonPosition (..)
    , ButtonClicked (..)
    ) where

import           Control.Monad                   (void)
import           Data.Bits                       ((.|.))
import           Data.Data                       (Typeable, cast)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Framework.TEA.Internal          as TEAInternal
import qualified Graphics.GUI.Component.Internal as ComponentInternal
import           Graphics.GUI.Component.Property (IsGUIComponentProperty (..))
import qualified Graphics.GUI.Foreign            as Win32
import qualified Graphics.Win32                  as Win32

data ButtonProperty = forall a. (Typeable a, Show a, IsGUIComponentProperty a, IsButtonProperty a) => ButtonProperty a

instance Eq ButtonProperty where
    (ButtonProperty a) == (ButtonProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance Show ButtonProperty where
    show (ButtonProperty x) = show x

class Eq a => IsButtonProperty a

instance IsButtonProperty ButtonProperty

instance IsGUIComponentProperty ButtonProperty where
    applyProperty (ButtonProperty x) = applyProperty x

    updateProperty (ButtonProperty new) (ButtonProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> error "Failed to cast ButtonProperty"

    unapplyProperty (ButtonProperty x) = unapplyProperty x

    getPropertyName (ButtonProperty x) = getPropertyName x

newtype ButtonLabel    = ButtonLabel    Text       deriving (Show, Eq)
newtype ButtonSize     = ButtonSize     (Int, Int) deriving (Show, Eq)
newtype ButtonPosition = ButtonPosition (Int, Int) deriving (Show, Eq)
data    ButtonClicked  = forall a. (Typeable a, Show a, Eq a) => ButtonClicked a

instance Eq ButtonClicked where
    (ButtonClicked a) == (ButtonClicked b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance Show ButtonClicked where
    show (ButtonClicked x) = "ButtonClicked " <> show x

instance IsButtonProperty ButtonLabel
instance IsButtonProperty ButtonSize
instance IsButtonProperty ButtonPosition
instance IsButtonProperty ButtonClicked

instance IsGUIComponentProperty ButtonLabel where
    getPropertyName _ = "ButtonLabel"

    applyProperty (ButtonLabel label) buttonHWND =
        Win32.setWindowText buttonHWND (Text.unpack label) >>
            ComponentInternal.setFlag "BUTTONLABEL_SET" buttonHWND

    updateProperty (ButtonLabel newLabel) _ buttonHWND =
        Win32.setWindowText buttonHWND (Text.unpack newLabel)

    unapplyProperty _ buttonHWND =
        Win32.setWindowText buttonHWND "" >>
            ComponentInternal.unsetFlag "BUTTONLABEL_SET" buttonHWND

instance IsGUIComponentProperty ButtonSize where
    getPropertyName _ = "ButtonSize"

    applyProperty (ButtonSize (width, height)) buttonHWND =
        Win32.c_SetWindowPos buttonHWND
            Win32.nullPtr
            0
            0
            (fromIntegral width)
            (fromIntegral height)
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.setFlag "BUTTONSIZE_SET" buttonHWND

    updateProperty (ButtonSize (width, height)) _ buttonHWND =
        void $ Win32.c_SetWindowPos buttonHWND
            Win32.nullPtr
            0
            0
            (fromIntegral width)
            (fromIntegral height)
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    unapplyProperty _ buttonHWND =
        Win32.c_SetWindowPos buttonHWND
            Win32.nullPtr
            0
            0
            0
            0
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.unsetFlag "BUTTONSIZE_SET" buttonHWND

instance IsGUIComponentProperty ButtonPosition where
    getPropertyName _ = "ButtonPosition"

    applyProperty (ButtonPosition (x, y)) buttonHWND =
        Win32.c_SetWindowPos
            buttonHWND
            Win32.nullPtr
            (fromIntegral x)
            (fromIntegral y)
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.setFlag "BUTTONPOSITION_SET" buttonHWND

    updateProperty (ButtonPosition (x, y)) _ buttonHWND =
        void $ Win32.c_SetWindowPos
            buttonHWND
            Win32.nullPtr
            (fromIntegral x)
            (fromIntegral y)
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    unapplyProperty _ buttonHWND =
        Win32.c_SetWindowPos
            buttonHWND
            Win32.nullPtr
            0
            0
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.unsetFlag "BUTTONPOSITION_SET" buttonHWND

instance IsGUIComponentProperty ButtonClicked where
    getPropertyName _ = "ButtonClicked"

    applyProperty (ButtonClicked msg) buttonHWND =
        ComponentInternal.setEventHandler "BUTTONCLICKED" (TEAInternal.Msg msg) buttonHWND >>
            ComponentInternal.setFlag "BUTTONCLICKED_SET" buttonHWND

    updateProperty (ButtonClicked msg) _ buttonHWND =
        ComponentInternal.unregisterEventHandler "BUTTONCLICKED" buttonHWND >>
            ComponentInternal.setEventHandler "BUTTONCLICKED" (TEAInternal.Msg msg) buttonHWND

    unapplyProperty _ buttonHWND =
        ComponentInternal.unregisterEventHandler "BUTTONCLICKED" buttonHWND >>
            ComponentInternal.unsetFlag "BUTTONCLICKED_SET" buttonHWND
