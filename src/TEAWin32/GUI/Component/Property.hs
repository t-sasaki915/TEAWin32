{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module TEAWin32.GUI.Component.Property
    ( GUIComponentProperty (..)
    , IsGUIComponentProperty (..)
    , IsPropertyWrapper (..)
    , HasPropertyName (..)
    , MayHaveZIndexProperty (..)
    , ComponentTitle (..)
    , ComponentSize (..)
    , ComponentPosition (..)
    , ComponentFont (..)
    , ComponentChildren (..)
    , ComponentZIndex (..)
    , ComponentOnClick (..)
    ) where

import                          Control.Monad                             (forM_)
import                          Data.Data                                 (TypeRep,
                                                                           Typeable,
                                                                           cast)
import                          Data.Text                                 (Text)
import                          GHC.Stack                                 (HasCallStack)
import                qualified Graphics.Win32                            as Win32
import {-# SOURCE #-} qualified TEAWin32.Application.Internal             as ApplicationInternal
import                          TEAWin32.GUI                              (Font,
                                                                           ScalableValue)
import {-# SOURCE #-}           TEAWin32.GUI.Component                    (GUIComponent (..),
                                                                           IsGUIComponent (..))
import {-# SOURCE #-} qualified TEAWin32.GUI.Component.Internal           as ComponentInternal
import {-# SOURCE #-}           TEAWin32.GUI.Component.Internal.Attribute
import                          TEAWin32.Internal                         (throwTEAWin32InternalError)

data GUIComponentProperty =  forall a. (Typeable a, Show a, HasPropertyName a, IsGUIComponentProperty a, MayHaveZIndexProperty a)
                          => GUIComponentProperty a

instance Eq GUIComponentProperty where
    (GUIComponentProperty x) == (GUIComponentProperty y) =
        case cast y of
            Just y' -> x == y'
            Nothing -> False

instance Show GUIComponentProperty where
    show (GUIComponentProperty x) = show x

class Eq a => IsGUIComponentProperty a where
    applyProperty :: HasCallStack => a -> Win32.HWND -> IO ()

    updateProperty :: HasCallStack => a -> a -> Win32.HWND -> IO ()

    unapplyProperty :: HasCallStack => a -> Win32.HWND -> IO ()

instance IsGUIComponentProperty GUIComponentProperty where
    applyProperty (GUIComponentProperty x) = applyProperty x

    updateProperty (GUIComponentProperty new) (GUIComponentProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> throwTEAWin32InternalError "Failed to cast GUIComponentProperty"

    unapplyProperty (GUIComponentProperty x) = unapplyProperty x


class IsPropertyWrapper a b where
    wrapComponentProperty :: b -> a

class HasPropertyName a where
    getPropertyName :: a -> TypeRep

class MayHaveZIndexProperty a where
    getZIndexProperty :: a -> Maybe ComponentZIndex

instance HasPropertyName GUIComponentProperty where
    getPropertyName (GUIComponentProperty a) = getPropertyName a

instance MayHaveZIndexProperty GUIComponentProperty where
    getZIndexProperty (GUIComponentProperty a) = getZIndexProperty a

newtype ComponentTitle    = ComponentTitle    Text                           deriving (Show, Eq)
newtype ComponentSize     = ComponentSize     (ScalableValue, ScalableValue) deriving (Show, Eq)
newtype ComponentPosition = ComponentPosition (ScalableValue, ScalableValue) deriving (Show, Eq)
newtype ComponentFont     = ComponentFont     Font                           deriving (Show, Eq)
newtype ComponentChildren = ComponentChildren [GUIComponent]                 deriving (Show, Eq)
newtype ComponentZIndex   = ComponentZIndex   Int                            deriving (Show, Eq)
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
        ComponentInternal.resolveScalableValueForHWND componentHWND width >>= \width' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND height >>= \height' ->
                ComponentInternal.setComponentSize width' height' componentHWND >>
                    addAttributeToHWND componentHWND (ComponentSizeAttr (width, height)) >>
                        addAttributeToHWND componentHWND (ComponentFlagAttr ComponentSizeSet)

    updateProperty (ComponentSize (width, height)) _ componentHWND =
        ComponentInternal.resolveScalableValueForHWND componentHWND width >>= \width' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND height >>= \height' ->
                ComponentInternal.setComponentSize width' height' componentHWND >>
                    updateAttributeOfHWND componentHWND (ComponentSizeAttr (width, height))

    unapplyProperty (ComponentSize (width, height)) componentHWND =
        ComponentInternal.setComponentSize 0 0 componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentSizeAttr (width, height)) >>
                removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentSizeSet)

instance IsGUIComponentProperty ComponentPosition where
    applyProperty (ComponentPosition (x, y)) componentHWND =
        ComponentInternal.resolveScalableValueForHWND componentHWND x >>= \x' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND y >>= \y' ->
                ComponentInternal.setComponentPosition x' y' componentHWND >>
                    addAttributeToHWND componentHWND (ComponentPositionAttr (x, y)) >>
                        addAttributeToHWND componentHWND (ComponentFlagAttr ComponentPositionSet)

    updateProperty (ComponentPosition (x, y)) _ componentHWND =
        ComponentInternal.resolveScalableValueForHWND componentHWND x >>= \x' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND y >>= \y' ->
                ComponentInternal.setComponentPosition x' y' componentHWND >>
                    updateAttributeOfHWND componentHWND (ComponentPositionAttr (x, y))

    unapplyProperty (ComponentPosition (x, y)) componentHWND =
        ComponentInternal.setComponentPosition 0 0 componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentPositionAttr (x, y)) >>
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
        ComponentInternal.sortComponentsWithZIndex children (Just componentHWND) >>= \sortedChildren ->
            forM_ sortedChildren (\(GUIComponent child) ->
                render child (Just componentHWND)) >>
                    addAttributeToHWND componentHWND (ComponentFlagAttr ComponentChildrenSet)

    updateProperty (ComponentChildren newChildren) (ComponentChildren oldChildren) componentHWND =
        ApplicationInternal.updateComponents newChildren oldChildren (Just componentHWND)

    unapplyProperty _ componentHWND =
        ComponentInternal.destroyChildren componentHWND >>
            removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentChildrenSet)

instance IsGUIComponentProperty ComponentZIndex where
    applyProperty (ComponentZIndex zIndex) componentHWND =
        addAttributeToHWND componentHWND (ComponentZIndexAttr zIndex) >>
            addAttributeToHWND componentHWND (ComponentFlagAttr ComponentZIndexSet)

    updateProperty (ComponentZIndex zIndex) _ componentHWND =
        updateAttributeOfHWND componentHWND (ComponentZIndexAttr zIndex)

    unapplyProperty (ComponentZIndex zIndex) componentHWND =
        removeAttributeFromHWND componentHWND (ComponentZIndexAttr zIndex) >>
            removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentZIndexSet)

instance IsGUIComponentProperty ComponentOnClick where
    applyProperty (ComponentOnClick msg) componentHWND =
        addAttributeToHWND componentHWND (ComponentEventHandlerAttr ComponentClickEvent (ApplicationInternal.Msg msg)) >>
            addAttributeToHWND componentHWND (ComponentFlagAttr ComponentOnClickSet)

    updateProperty (ComponentOnClick msg) _ componentHWND =
        updateAttributeOfHWND componentHWND (ComponentEventHandlerAttr ComponentClickEvent (ApplicationInternal.Msg msg))

    unapplyProperty (ComponentOnClick msg) componentHWND =
        removeAttributeFromHWND componentHWND (ComponentEventHandlerAttr ComponentClickEvent (ApplicationInternal.Msg msg)) >>
            removeAttributeFromHWND componentHWND (ComponentFlagAttr ComponentOnClickSet)
