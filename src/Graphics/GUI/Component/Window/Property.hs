{-# LANGUAGE ExistentialQuantification #-}

module Graphics.GUI.Component.Window.Property
    ( WindowProperty (..)
    , IsWindowProperty
    , WindowTitle (..)
    , WindowIcon (..)
    , WindowCursor (..)
    , WindowSize (..)
    , WindowPosition (..)
    , WindowBrush (..)
    , WindowChildren (..)
    ) where

import           Control.Monad                   (forM_, void)
import           Data.Bits                       ((.|.))
import           Data.Data                       (Typeable, cast)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Foreign                         (intPtrToPtr)
import           Graphics.GUI
import           Graphics.GUI.Component          (GUIComponent (..),
                                                  IsGUIComponent (render))
import           Graphics.GUI.Component.Property (IsGUIComponentProperty (..))
import qualified Graphics.GUI.Foreign            as Win32
import qualified Graphics.GUI.Internal           as Internal
import qualified Graphics.Win32                  as Win32

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

    updateProperty (WindowProperty x) = updateProperty x

    unapplyProperty (WindowProperty x) = unapplyProperty x

    getPropertyName (WindowProperty x) = getPropertyName x

newtype WindowTitle    = WindowTitle    Text           deriving (Show, Eq)
newtype WindowIcon     = WindowIcon     Icon           deriving (Show, Eq)
newtype WindowCursor   = WindowCursor   Cursor         deriving (Show, Eq)
newtype WindowSize     = WindowSize     (Int, Int)     deriving (Show, Eq)
newtype WindowPosition = WindowPosition (Int, Int)     deriving (Show, Eq)
newtype WindowBrush    = WindowBrush    Brush          deriving (Show, Eq)
newtype WindowChildren = WindowChildren [GUIComponent] deriving (Show, Eq)

instance IsWindowProperty WindowTitle
instance IsWindowProperty WindowIcon
instance IsWindowProperty WindowCursor
instance IsWindowProperty WindowSize
instance IsWindowProperty WindowPosition
instance IsWindowProperty WindowBrush
instance IsWindowProperty WindowChildren

instance IsGUIComponentProperty WindowTitle where
    getPropertyName _ = "WindowTitle"

    applyProperty (WindowTitle title) windowHWND =
        Win32.setWindowText windowHWND (Text.unpack title)

    updateProperty = applyProperty

    unapplyProperty _ = applyProperty (WindowTitle "")

instance IsGUIComponentProperty WindowIcon where
    getPropertyName _ = "WindowIcon"

    applyProperty (WindowIcon icon) windowHWND =
        toWin32Icon icon >>= \icon' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HICON icon'

    updateProperty = applyProperty

    unapplyProperty _ windowHWND =
        void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HICON Win32.nullPtr

instance IsGUIComponentProperty WindowCursor where
    getPropertyName _ = "WindowCursor"

    applyProperty (WindowCursor cursor) windowHWND =
        Win32.loadCursor Nothing (toWin32Cursor cursor) >>= \cursor' ->
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HCURSOR cursor'

    updateProperty = applyProperty

    unapplyProperty _ windowHWND =
        void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HCURSOR Win32.nullPtr

instance IsGUIComponentProperty WindowSize where
    getPropertyName _ = "WindowSize"

    applyProperty (WindowSize (width, height)) windowHWND =
        void $
            Win32.c_SetWindowPos windowHWND
                Win32.nullPtr
                0
                0
                (fromIntegral width)
                (fromIntegral height)
                (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    updateProperty = applyProperty

    unapplyProperty _ = applyProperty (WindowSize (0, 0))

instance IsGUIComponentProperty WindowPosition where
    getPropertyName _ = "WindowPosition"

    applyProperty (WindowPosition (x, y)) windowHWND =
        void $
            Win32.c_SetWindowPos
                windowHWND
                Win32.nullPtr
                (fromIntegral x)
                (fromIntegral y)
                0
                0
                (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

    updateProperty = applyProperty

    unapplyProperty _ = applyProperty (WindowPosition (0, 0))

instance IsGUIComponentProperty WindowBrush where
    -- TODO
    getPropertyName _ = "WindowBrush"
    applyProperty (WindowBrush brush) windowHWND = do
        unapplyProperty (WindowBrush brush) windowHWND

        toWin32Brush brush >>= \brush' -> do
            void $ Win32.c_SetClassLongPtr windowHWND Win32.gCLP_HBRBACKGROUND brush'

            Win32.withTString "WINDOW_BRUSH" $ \pName ->
                void $ Win32.c_SetProp windowHWND pName brush'

    updateProperty = applyProperty

    unapplyProperty _ windowHWND =
        Win32.c_GetClassLongPtr windowHWND Win32.gCLP_HBRBACKGROUND >>= \oldBrush ->
            void $ Win32.c_DeleteObject (intPtrToPtr $ fromIntegral oldBrush)

instance IsGUIComponentProperty WindowChildren where
    getPropertyName _ = "WindowChildren"

    applyProperty (WindowChildren children) windowHWND =
        forM_ children $ \(GUIComponent child) ->
            render child (Just windowHWND)

    updateProperty _ _ = putStrLn "WindowChildren Update!!!!"

    unapplyProperty _ windowHWND =
        Internal.withChildWindows windowHWND $ \children ->
            mapM_ Win32.destroyWindow children
