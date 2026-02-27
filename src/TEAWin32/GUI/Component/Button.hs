module TEAWin32.GUI.Component.Button (Button (..)) where

import           Data.Bits                                ((.|.))
import           Data.Maybe                               (fromJust)
import           Foreign                                  (intPtrToPtr)
import qualified Graphics.Win32                           as Win32
import           TEAWin32.GUI                             (UniqueId)
import           TEAWin32.GUI.Component                   (ComponentType (ComponentButton),
                                                           IsGUIComponent (..))
import           TEAWin32.GUI.Component.Button.Property   (ButtonProperty)
import           TEAWin32.GUI.Component.ComponentRegistry
import           TEAWin32.GUI.Component.Property          (GUIComponentProperty (..),
                                                           IsGUIComponentProperty (applyProperty))
import           TEAWin32.GUI.VirtualDOM                  (CCallRequest (..),
                                                           CreateButtonReq (..),
                                                           scheduleCCall)

data Button = Button UniqueId [ButtonProperty] deriving (Show, Eq)

instance IsGUIComponent Button where
    scheduleRendering (Button buttonUniqueId buttonProperties) parentHWND = do
        let createRequest = CreateButtonRequest $
                CreateButtonReq
                    { newButtonUniqueId       = buttonUniqueId
                    , newButtonParentUniqueId = fromJust parentHWND -- TODO
                    }

        scheduleCCall createRequest
