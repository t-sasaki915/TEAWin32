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

instance IsGUIComponentProperty ComponentSize where

instance IsGUIComponentProperty ComponentPosition where

instance IsGUIComponentProperty ComponentFont where

instance IsGUIComponentProperty ComponentChildren where

instance IsGUIComponentProperty ComponentZIndex where

instance IsGUIComponentProperty ComponentOnClick where
