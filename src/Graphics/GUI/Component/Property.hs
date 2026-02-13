{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Graphics.GUI.Component.Property
    ( GUIComponentProperty (..)
    , IsGUIComponentProperty (..)
    , IsPropertyWrapper (..)
    , HasPropertyName (..)
    , ComponentTitle (..)
    , ComponentSize (..)
    , ComponentPosition (..)
    , ComponentFont (..)
    , ComponentChildren (..)
    , ComponentOnClick (..)
    ) where

import                          Control.Monad                             (forM_)
import                          Data.Data                                 (TypeRep,
                                                                           Typeable,
                                                                           cast)
import                          Data.Text                                 (Text)
import {-# SOURCE #-} qualified Framework.TEA.Internal                    as TEAInternal
import                          Graphics.GUI                              (Font)
import {-# SOURCE #-}           Graphics.GUI.Component                    (GUIComponent (..),
                                                                           IsGUIComponent (..))
import {-# SOURCE #-} qualified Graphics.GUI.Component.Internal           as ComponentInternal
import {-# SOURCE #-}           Graphics.GUI.Component.Internal.Attribute
import {-# SOURCE #-}           Graphics.GUI.Component.Window             (destroyChildren)
import                qualified Graphics.Win32                            as Win32

data GUIComponentProperty = forall a. (Typeable a, Show a, HasPropertyName a, IsGUIComponentProperty a) => GUIComponentProperty a

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

instance IsGUIComponentProperty GUIComponentProperty where
    applyProperty (GUIComponentProperty x) = applyProperty x

    updateProperty (GUIComponentProperty new) (GUIComponentProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> error "Failed to cast GUIComponentProperty"

    unapplyProperty (GUIComponentProperty x) = unapplyProperty x


class IsPropertyWrapper a b where
    wrapComponentProperty :: b -> a

class HasPropertyName a where
    getPropertyName :: a -> TypeRep

instance HasPropertyName GUIComponentProperty where
    getPropertyName (GUIComponentProperty a) = getPropertyName a

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
    applyProperty (ComponentTitle title) componentHWND =
        ComponentInternal.setComponentTitle title componentHWND >>
            addAttributeToHWND componentHWND (ComponentTitleAttr title) >>
                addAttributeToHWND componentHWND (ComponentFlagAttr ComponentTitleSet)

    updateProperty (ComponentTitle title) _ componentHWND =
        ComponentInternal.setComponentTitle title componentHWND >>
            updateAttributeOfHWND componentHWND (ComponentTitleAttr title)

    unapplyProperty (ComponentTitle title) componentHWND =
        ComponentInternal.setComponentTitle "" componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentTitleAttr title) >>
                removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentTitleSet)

instance IsGUIComponentProperty ComponentSize where
    applyProperty (ComponentSize (width, height)) componentHWND =
        ComponentInternal.setComponentSize width height componentHWND >>
            addAttributeToHWND componentHWND (ComponentFlagAttr ComponentSizeSet)

    updateProperty (ComponentSize (width, height)) _ =
        ComponentInternal.setComponentSize width height

    unapplyProperty _ componentHWND =
        ComponentInternal.setComponentSize 0 0 componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentSizeSet)

instance IsGUIComponentProperty ComponentPosition where
    applyProperty (ComponentPosition (x, y)) componentHWND =
        ComponentInternal.setComponentPosition x y componentHWND >>
            addAttributeToHWND componentHWND (ComponentFlagAttr ComponentPositionSet)

    updateProperty (ComponentPosition (x, y)) _ =
        ComponentInternal.setComponentPosition x y

    unapplyProperty _ componentHWND =
        ComponentInternal.setComponentPosition 0 0 componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentPositionSet)

instance IsGUIComponentProperty ComponentFont where
    applyProperty (ComponentFont font) componentHWND =
        ComponentInternal.setComponentFont font componentHWND >>
            addAttributeToHWND componentHWND (ComponentFontAttr font) >>
                addAttributeToHWND componentHWND (ComponentFlagAttr ComponentFontSet)

    updateProperty (ComponentFont font) _ componentHWND =
        ComponentInternal.setComponentFont font componentHWND >>
            updateAttributeOfHWND componentHWND (ComponentFontAttr font)

    unapplyProperty (ComponentFont font) componentHWND =
        ComponentInternal.useDefaultFont componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentFontAttr font) >>
                removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentFontSet)

instance IsGUIComponentProperty ComponentChildren where
    applyProperty (ComponentChildren children) componentHWND =
        forM_ children (\(GUIComponent child) ->
            render child (Just componentHWND)) >>
                addAttributeToHWND componentHWND (ComponentFlagAttr ComponentChildrenSet)

    updateProperty (ComponentChildren newChildren) (ComponentChildren oldChildren) =
        TEAInternal.updateChildren newChildren oldChildren

    unapplyProperty _ componentHWND =
        destroyChildren componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentChildrenSet)

instance IsGUIComponentProperty ComponentOnClick where
    applyProperty (ComponentOnClick msg) componentHWND =
        addAttributeToHWND componentHWND (ComponentEventHandlerAttr ComponentClickEvent (TEAInternal.Msg msg)) >>
            addAttributeToHWND componentHWND (ComponentFlagAttr ComponentOnClickSet)

    updateProperty (ComponentOnClick msg) _ componentHWND =
        updateAttributeOfHWND componentHWND (ComponentEventHandlerAttr ComponentClickEvent (TEAInternal.Msg msg))

    unapplyProperty (ComponentOnClick msg) componentHWND =
        removeAttributeFromHWND componentHWND (ComponentEventHandlerAttr ComponentClickEvent (TEAInternal.Msg msg)) >>
            removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentOnClickSet)
