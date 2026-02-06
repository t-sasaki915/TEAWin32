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
import           Data.IORef                      (atomicModifyIORef')
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Framework.TEA                   (IsMsg)
import qualified Framework.TEA.Internal          as TEAInternal
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

    updateProperty (ButtonProperty x) = updateProperty x

    unapplyProperty (ButtonProperty x) = unapplyProperty x

    getPropertyName (ButtonProperty x) = getPropertyName x

newtype ButtonLabel    = ButtonLabel    Text       deriving (Show, Eq)
newtype ButtonSize     = ButtonSize     (Int, Int) deriving (Show, Eq)
newtype ButtonPosition = ButtonPosition (Int, Int) deriving (Show, Eq)
data    ButtonClicked  = forall a. (Typeable a, Show a, IsMsg a) => ButtonClicked a

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
        Win32.setWindowText buttonHWND (Text.unpack label)

    updateProperty = applyProperty

    unapplyProperty _ = applyProperty (ButtonLabel "")

instance IsGUIComponentProperty ButtonSize where
    getPropertyName _ = "ButtonSize"

    applyProperty (ButtonSize (width, height)) buttonHWND =
        void $
            Win32.c_SetWindowPos buttonHWND
                Win32.nullPtr
                0
                0
                (fromIntegral width)
                (fromIntegral height)
                (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    updateProperty = applyProperty

    unapplyProperty _ = applyProperty (ButtonSize (0, 0))

instance IsGUIComponentProperty ButtonPosition where
    getPropertyName _ = "ButtonPosition"

    applyProperty (ButtonPosition (x, y)) buttonHWND =
        void $
            Win32.c_SetWindowPos
                buttonHWND
                Win32.nullPtr
                (fromIntegral x)
                (fromIntegral y)
                0
                0
                (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    updateProperty = applyProperty

    unapplyProperty _ = applyProperty (ButtonPosition (0, 0))

instance IsGUIComponentProperty ButtonClicked where
    getPropertyName _ = "ButtonClicked"

    applyProperty (ButtonClicked msg) buttonHWND =
        void $ atomicModifyIORef' TEAInternal.buttonClickEventHandlersRef $ \a ->
            let updatedMap = Map.insert buttonHWND (TEAInternal.Msg msg) a in
                (updatedMap, updatedMap)

    updateProperty = applyProperty

    unapplyProperty _ buttonHWND =
        void $ atomicModifyIORef' TEAInternal.buttonClickEventHandlersRef $ \a ->
            let updatedMap = Map.delete buttonHWND a in
                (updatedMap, updatedMap)

