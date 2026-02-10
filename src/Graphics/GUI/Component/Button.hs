module Graphics.GUI.Component.Button (Button (..)) where

import           Data.Bits                              ((.|.))
import           Data.Maybe                             (fromJust)
import           Foreign                                (intPtrToPtr)
import qualified Framework.TEA.Internal                 as TEAInternal
import           Graphics.GUI                           (UniqueId)
import           Graphics.GUI.Component                 (IsGUIComponent (..))
import           Graphics.GUI.Component.Button.Property (ButtonProperty)
import qualified Graphics.GUI.Component.Internal        as ComponentInternal
import           Graphics.GUI.Component.Property        (GUIComponentProperty (..),
                                                         IsGUIComponentProperty (applyProperty))
import qualified Graphics.Win32                         as Win32

data Button = Button UniqueId [ButtonProperty] deriving (Show, Eq)

instance IsGUIComponent Button where
    getProperties (Button _ properties) = GUIComponentProperty <$> properties

    getUniqueId (Button uniqueId _) = uniqueId

    doesNeedToRedraw (Button uniqueId1 _) (Button uniqueId2 _) = uniqueId1 /= uniqueId2

    render (Button buttonUniqueId buttonProperties) parentHWND = do
        parentInstance <- Win32.c_GetWindowLongPtr (fromJust parentHWND) (-6)

        button <- Win32.createWindow
            (Win32.mkClassName "BUTTON")
            ""
            (Win32.wS_TABSTOP .|. Win32.wS_VISIBLE .|. Win32.wS_CHILD .|. Win32.bS_DEFPUSHBUTTON)
            Nothing
            Nothing
            Nothing
            Nothing
            parentHWND
            Nothing
            (intPtrToPtr $ fromIntegral parentInstance)
            (const $ const $ const $ const $ pure 0)

        mapM_ (`applyProperty` button) buttonProperties

        TEAInternal.registerHWND buttonUniqueId button

        ComponentInternal.setComponentType "BUTTON" button
        ComponentInternal.setUniqueIdToHWND buttonUniqueId button

        pure button
