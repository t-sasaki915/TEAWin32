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

import                          Control.Monad                            (forM_)
import                          Data.Data                                (TypeRep,
                                                                          Typeable,
                                                                          cast)
import                          Data.Functor                             ((<&>))
import                          Data.Text                                (Text)
import                          GHC.Stack                                (HasCallStack)
import                qualified Graphics.Win32                           as Win32
import {-# SOURCE #-} qualified TEAWin32.Application.Internal            as ApplicationInternal
import                          TEAWin32.Exception                       (TEAWin32Error (..),
                                                                          errorTEAWin32)
import                          TEAWin32.GUI                             (Font,
                                                                          ScalableValue)
import {-# SOURCE #-}           TEAWin32.GUI.Component                   (GUIComponent (..),
                                                                          IsGUIComponent (..))
import                          TEAWin32.GUI.Component.ComponentRegistry
import {-# SOURCE #-} qualified TEAWin32.GUI.Component.Internal          as ComponentInternal

data GUIComponentProperty = forall a.
                          (Typeable a, Show a, HasPropertyName a, IsGUIComponentProperty a, MayHaveZIndexProperty a)
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

    isPropertyChanged :: HasCallStack => a -> Win32.HWND -> IO (Maybe Bool)

instance IsGUIComponentProperty GUIComponentProperty where
    applyProperty (GUIComponentProperty x) = applyProperty x

    updateProperty (GUIComponentProperty new) (GUIComponentProperty old) =
        case cast old of
            Just old' -> updateProperty new old'
            Nothing   -> errorTEAWin32 (InternalTEAWin32Error "Failed to cast GUIComponentProperty")

    unapplyProperty (GUIComponentProperty x) = unapplyProperty x

    isPropertyChanged (GUIComponentProperty x) = isPropertyChanged x


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
newtype ComponentOnClick  = ComponentOnClick  ApplicationInternal.Msg        deriving (Show, Eq)

instance IsGUIComponentProperty ComponentTitle where
    isPropertyChanged (ComponentTitle title) = isRegistryValueChangedMaybe ComponentTitleRegKey title

    applyProperty (ComponentTitle title) componentHWND =
        ComponentInternal.setComponentTitle title componentHWND >>
            addComponentRegistryEntry ComponentTitleRegKey (ComponentTitleReg title) componentHWND

    updateProperty (ComponentTitle title) _ componentHWND =
        ComponentInternal.setComponentTitle title componentHWND >>
            updateComponentRegistryEntry ComponentTitleRegKey (ComponentTitleReg title) componentHWND

    unapplyProperty _ componentHWND =
        ComponentInternal.setComponentTitle "" componentHWND >>
            removeComponentRegistryEntry ComponentTitleRegKey componentHWND

instance IsGUIComponentProperty ComponentSize where
    isPropertyChanged (ComponentSize size) = isRegistryValueChangedMaybe ComponentSizeRegKey size

    applyProperty (ComponentSize (width, height)) componentHWND =
        ComponentInternal.resolveScalableValueForHWND componentHWND width >>= \width' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND height >>= \height' ->
                ComponentInternal.setComponentSize width' height' componentHWND >>
                    addComponentRegistryEntry ComponentSizeRegKey (ComponentSizeReg (width, height)) componentHWND

    updateProperty (ComponentSize (width, height)) _ componentHWND =
        ComponentInternal.resolveScalableValueForHWND componentHWND width >>= \width' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND height >>= \height' ->
                ComponentInternal.setComponentSize width' height' componentHWND >>
                    updateComponentRegistryEntry ComponentSizeRegKey (ComponentSizeReg (width, height)) componentHWND

    unapplyProperty _ componentHWND =
        ComponentInternal.setComponentSize 0 0 componentHWND >>
            removeComponentRegistryEntry ComponentSizeRegKey componentHWND

instance IsGUIComponentProperty ComponentPosition where
    isPropertyChanged (ComponentPosition pos) = isRegistryValueChangedMaybe ComponentPositionRegKey pos

    applyProperty (ComponentPosition (x, y)) componentHWND =
        ComponentInternal.resolveScalableValueForHWND componentHWND x >>= \x' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND y >>= \y' ->
                ComponentInternal.setComponentPosition x' y' componentHWND >>
                    addComponentRegistryEntry ComponentPositionRegKey (ComponentPositionReg (x, y)) componentHWND

    updateProperty (ComponentPosition (x, y)) _ componentHWND =
        ComponentInternal.resolveScalableValueForHWND componentHWND x >>= \x' ->
            ComponentInternal.resolveScalableValueForHWND componentHWND y >>= \y' ->
                ComponentInternal.setComponentPosition x' y' componentHWND >>
                    updateComponentRegistryEntry ComponentPositionRegKey (ComponentPositionReg (x, y)) componentHWND

    unapplyProperty _ componentHWND =
        ComponentInternal.setComponentPosition 0 0 componentHWND >>
            removeComponentRegistryEntry ComponentPositionRegKey componentHWND

instance IsGUIComponentProperty ComponentFont where
    isPropertyChanged (ComponentFont font) = isRegistryValueChangedMaybe ComponentFontRegKey font

    applyProperty (ComponentFont font) componentHWND =
        ComponentInternal.setComponentFont font componentHWND >>
            addComponentRegistryEntry ComponentFontRegKey (ComponentFontReg font) componentHWND

    updateProperty (ComponentFont font) _ componentHWND =
        ComponentInternal.setComponentFont font componentHWND >>
            updateComponentRegistryEntry ComponentFontRegKey (ComponentFontReg font) componentHWND

    unapplyProperty _ componentHWND =
        ComponentInternal.useDefaultFont componentHWND >>
            removeComponentRegistryEntry ComponentFontRegKey componentHWND

instance IsGUIComponentProperty ComponentChildren where
    applyProperty (ComponentChildren children) componentHWND =
        ComponentInternal.sortComponentsWithZIndex children (Just componentHWND) >>= \sortedChildren ->
            forM_ sortedChildren $ \(GUIComponent child) ->
                render child (Just componentHWND)

    updateProperty (ComponentChildren newChildren) _ componentHWND =
        ApplicationInternal.updateComponents newChildren (Just componentHWND)

    unapplyProperty _ =
        ComponentInternal.destroyChildren

instance IsGUIComponentProperty ComponentZIndex where
    isPropertyChanged _ _ = pure (Just False)

    applyProperty (ComponentZIndex zIndex) =
        addComponentRegistryEntry ComponentZIndexRegKey (ComponentZIndexReg zIndex)

    updateProperty (ComponentZIndex zIndex) _ =
        updateComponentRegistryEntry ComponentZIndexRegKey (ComponentZIndexReg zIndex)

    unapplyProperty _ =
        removeComponentRegistryEntry ComponentZIndexRegKey

instance IsGUIComponentProperty ComponentOnClick where
    isPropertyChanged (ComponentOnClick msg) = isRegistryValueChangedMaybe ComponentClickEventHandlerRegKey msg

    applyProperty (ComponentOnClick msg) =
        addComponentRegistryEntry ComponentClickEventHandlerRegKey (ComponentClickEventHandlerReg msg)

    updateProperty (ComponentOnClick msg) _ =
        updateComponentRegistryEntry ComponentClickEventHandlerRegKey (ComponentClickEventHandlerReg msg)

    unapplyProperty _ =
        removeComponentRegistryEntry ComponentClickEventHandlerRegKey
