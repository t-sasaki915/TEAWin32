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
import qualified TEAWin32.GUI.Component.Internal          as ComponentInternal
import           TEAWin32.GUI.Component.Property          (GUIComponentProperty (..),
                                                           IsGUIComponentProperty (applyProperty))
import qualified TEAWin32.GUI.Internal                    as GUIInternal

data Button = Button UniqueId [ButtonProperty] deriving (Show, Eq)

instance IsGUIComponent Button where
    getProperties (Button _ properties) = GUIComponentProperty <$> properties

    getUniqueId (Button uniqueId _) = uniqueId

    doesNeedToRedraw (Button uniqueId1 _) (Button uniqueId2 _) = uniqueId1 /= uniqueId2

    getComponentType _ = ComponentButton

    render (Button buttonUniqueId buttonProperties) parentHWND = do
        parentInstance <- Win32.c_GetWindowLongPtr (fromJust parentHWND) (-6)

        button <- Win32.createWindow
            (Win32.mkClassName "BUTTON")
            ""
            (Win32.wS_TABSTOP .|. Win32.wS_VISIBLE .|. Win32.wS_CHILD .|. Win32.bS_DEFPUSHBUTTON .|. Win32.wS_CLIPSIBLINGS)
            Nothing
            Nothing
            Nothing
            Nothing
            parentHWND
            Nothing
            (intPtrToPtr $ fromIntegral parentInstance)
            (const $ const $ const $ const $ pure 0)

        currentDPI <- GUIInternal.getDPIFromHWND button

        registerComponentToRegistry buttonUniqueId button
        addComponentRegistryEntry ComponentUniqueIdRegKey   (ComponentUniqueIdReg buttonUniqueId) button
        addComponentRegistryEntry ComponentTypeRegKey       (ComponentTypeReg ComponentButton)    button
        addComponentRegistryEntry ComponentCurrentDPIRegKey (ComponentCurrentDPIReg currentDPI)   button

        ComponentInternal.useDefaultFont button
        ComponentInternal.bringComponentToTop button

        mapM_ (`applyProperty` button) buttonProperties

        pure button
