module TEAWin32.GUI.Component.Window (Window (..)) where

import           Data.IORef                               (atomicModifyIORef')
import           Data.Maybe                               (fromMaybe)
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           Foreign.C                                (withCWString)
import qualified Graphics.Win32                           as Win32
import           TEAWin32.GUI                             (UniqueId,
                                                           WindowStyle,
                                                           toWin32WindowStyle)
import           TEAWin32.GUI.Component                   (ComponentType (ComponentWindow),
                                                           IsGUIComponent (..))
import           TEAWin32.GUI.Component.ComponentRegistry
import           TEAWin32.GUI.Component.Property          (GUIComponentProperty (..),
                                                           IsGUIComponentProperty (applyProperty))
import           TEAWin32.GUI.Component.Window.Property   (WindowProperty (..))
import           TEAWin32.GUI.VirtualDOM                  (CCallRequest (..),
                                                           CreateWindowReq (..),
                                                           scheduleCCall)

data Window = Window UniqueId Text WindowStyle [WindowProperty] deriving (Show, Eq)

instance IsGUIComponent Window where
    scheduleRendering (Window windowUniqueId windowClassName windowStyle windowProperties) maybeParent = do
        let createRequest = CreateWindowRequest $
                CreateWindowReq
                    { newWindowUniqueId       = windowUniqueId
                    , newWindowClassName      = windowClassName
                    , newWindowExStyles       = 0
                    , newWindowStyles         = toWin32WindowStyle windowStyle
                    , newWindowParentUniqueId = maybeParent
                    }

        scheduleCCall createRequest
