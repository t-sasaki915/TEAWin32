{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component.Property
    ( GUIComponentProperty (..)
    , IsGUIComponentProperty (..)
    ) where

import           Data.Data      (Typeable, cast)
import           Data.Text      (Text)
import qualified Graphics.Win32 as Win32

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

    updateProperty :: a -> Win32.HWND -> IO ()

    unapplyProperty :: a -> Win32.HWND -> IO ()

    getPropertyName :: a -> Text

instance IsGUIComponentProperty GUIComponentProperty where
    applyProperty (GUIComponentProperty x) = applyProperty x

    updateProperty (GUIComponentProperty x) = updateProperty x

    unapplyProperty (GUIComponentProperty x) = unapplyProperty x

    getPropertyName (GUIComponentProperty x) = getPropertyName x
