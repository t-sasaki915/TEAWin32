{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Graphics.GUI.Component.Property
    ( GUIComponentProperty (..)
    , IsGUIComponentProperty (..)
    , IsPropertyWrapper (..)
    , ComponentTitle (..)
    , ComponentSize (..)
    , ComponentPosition (..)
    , ComponentFont (..)
    , ComponentChildren (..)
    , ComponentOnClick (..)
    ) where

import                          Control.Monad                   (forM_, void)
import                          Data.Bits                       ((.|.))
import                          Data.Data                       (Typeable, cast)
import                          Data.Text                       (Text)
import                qualified Data.Text                       as Text
import {-# SOURCE #-} qualified Framework.TEA.Internal          as TEAInternal
import                          Graphics.GUI                    (Font)
import {-# SOURCE #-}           Graphics.GUI.Component          (GUIComponent (..),
                                                                 IsGUIComponent (..))
import {-# SOURCE #-} qualified Graphics.GUI.Component.Internal as ComponentInternal
import {-# SOURCE #-}           Graphics.GUI.Component.Window   (destroyChildren)
import                qualified Graphics.GUI.Foreign            as Win32
import                qualified Graphics.Win32                  as Win32

data GUIComponentProperty = forall a. (Typeable a, Show a, IsGUIComponentProperty a) => GUIComponentProperty a

instance Eq GUIComponentProperty where
    (GUIComponentProperty x) == (GUIComponentProperty y) =
        case cast y of
            Just y' -> x == y'
            Nothing -> False

instance Show GUIComponentProperty where
    show (GUIComponentProperty x) = show x

class Eq a => IsGUIComponentProperty a where
    applyProperty :: a -> Win32.HWND -> IO ()

    updateProperty :: a -> a -> Win32.HWND -> IO ()

    unapplyProperty :: a -> Win32.HWND -> IO ()

    getPropertyName :: a -> Text

instance IsGUIComponentProperty GUIComponentProperty where
    applyProperty (GUIComponentProperty x) = applyProperty x

    updateProperty (GUIComponentProperty new) (GUIComponentProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> error "Failed to cast GUIComponentProperty"

    unapplyProperty (GUIComponentProperty x) = unapplyProperty x

    getPropertyName (GUIComponentProperty x) = getPropertyName x

class IsPropertyWrapper a b where
    wrapComponentProperty :: b -> a

newtype ComponentTitle    = ComponentTitle    Text           deriving (Show, Eq)
newtype ComponentSize     = ComponentSize     (Int, Int)     deriving (Show, Eq)
newtype ComponentPosition = ComponentPosition (Int, Int)     deriving (Show, Eq)
newtype ComponentFont     = ComponentFont     Font           deriving (Show, Eq)
newtype ComponentChildren = ComponentChildren [GUIComponent] deriving (Show, Eq)
data    ComponentOnClick  = forall a. (Typeable a, Show a, Eq a) => ComponentOnClick a

instance Eq ComponentOnClick where
    (ComponentOnClick a) == (ComponentOnClick b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance Show ComponentOnClick where
    show (ComponentOnClick x) = "ComponentOnClick " <> show x

instance IsGUIComponentProperty ComponentTitle where
    getPropertyName _ = "ComponentTitle"

    applyProperty (ComponentTitle title) componentHWND =
        Win32.setWindowText componentHWND (Text.unpack title) >>
            ComponentInternal.setFlag "COMPONENTTITLE_SET" componentHWND

    updateProperty (ComponentTitle title) _ componentHWND =
        Win32.setWindowText componentHWND (Text.unpack title)

    unapplyProperty _ componentHWND =
        Win32.setWindowText componentHWND "" >>
            ComponentInternal.unsetFlag "COMPONENTTITLE_SET" componentHWND

instance IsGUIComponentProperty ComponentSize where
    getPropertyName _ = "ComponentSize"

    applyProperty (ComponentSize (width, height)) componentHWND =
        Win32.c_SetWindowPos componentHWND
            Win32.nullPtr
            0
            0
            (fromIntegral width)
            (fromIntegral height)
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.setFlag "COMPONENTSIZE_SET" componentHWND

    updateProperty (ComponentSize (width, height)) _ componentHWND =
        void $ Win32.c_SetWindowPos componentHWND
            Win32.nullPtr
            0
            0
            (fromIntegral width)
            (fromIntegral height)
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    unapplyProperty _ componentHWND =
        Win32.c_SetWindowPos componentHWND
            Win32.nullPtr
            0
            0
            0
            0
            (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.unsetFlag "COMPONENTSIZE_SET" componentHWND

instance IsGUIComponentProperty ComponentPosition where
    getPropertyName _ = "ComponentPosition"

    applyProperty (ComponentPosition (x, y)) componentHWND =
        Win32.c_SetWindowPos
            componentHWND
            Win32.nullPtr
            (fromIntegral x)
            (fromIntegral y)
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.setFlag "COMPONENTPOSITION_SET" componentHWND

    updateProperty (ComponentPosition (x, y)) _ componentHWND =
        void $ Win32.c_SetWindowPos
            componentHWND
            Win32.nullPtr
            (fromIntegral x)
            (fromIntegral y)
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    unapplyProperty _ componentHWND =
        Win32.c_SetWindowPos
            componentHWND
            Win32.nullPtr
            0
            0
            0
            0
            (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE) >>
                ComponentInternal.unsetFlag "COMPONENTPOSITION_SET" componentHWND

instance IsGUIComponentProperty ComponentFont where
    getPropertyName _ = "ComponentFont"

    applyProperty (ComponentFont font) componentHWND =
        ComponentInternal.setHWNDFont font componentHWND >>
            ComponentInternal.setFlag "COMPONENTFONT_SET" componentHWND

    updateProperty (ComponentFont font) _ =
        ComponentInternal.setHWNDFont font

    unapplyProperty _ componentHWND =
        ComponentInternal.useDefaultFont componentHWND >>
            ComponentInternal.unsetFlag "COMPONENTFONT_SET" componentHWND

instance IsGUIComponentProperty ComponentChildren where
    getPropertyName _ = "ComponentChildren"

    applyProperty (ComponentChildren children) componentHWND =
        forM_ children (\(GUIComponent child) ->
            render child (Just componentHWND)) >>
                ComponentInternal.setFlag "COMPONENTCHILDREN_SET" componentHWND

    updateProperty (ComponentChildren newChildren) (ComponentChildren oldChildren) =
        TEAInternal.updateChildren newChildren oldChildren

    unapplyProperty _ componentHWND =
        destroyChildren componentHWND >>
            ComponentInternal.unsetFlag "COMPONENTCHILDREN_SET" componentHWND

instance IsGUIComponentProperty ComponentOnClick where
    getPropertyName _ = "ComponentOnClick"

    applyProperty (ComponentOnClick msg) componentHWND =
        ComponentInternal.setEventHandler "COMPONENTONCLICK" (TEAInternal.Msg msg) componentHWND >>
            ComponentInternal.setFlag "COMPONENTONCLICK_SET" componentHWND

    updateProperty (ComponentOnClick msg) _ =
        ComponentInternal.setEventHandler "COMPONENTONCLICK" (TEAInternal.Msg msg)

    unapplyProperty _ componentHWND =
        ComponentInternal.unregisterEventHandler "COMPONENTONCLICK" componentHWND >>
            ComponentInternal.unsetFlag "COMPONENTONCLICK_SET" componentHWND
