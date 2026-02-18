module TEAWin32.GUI.Component.Button (Button (..)) where

import           Data.Bits                                 ((.|.))
import           Data.Maybe                                (fromJust)
import           Foreign                                   (intPtrToPtr)
import qualified Graphics.Win32                            as Win32
import qualified TEAWin32.Application.Internal             as ApplicationInternal
import           TEAWin32.GUI                              (UniqueId)
import           TEAWin32.GUI.Component                    (IsGUIComponent (..))
import           TEAWin32.GUI.Component.Button.Property    (ButtonProperty)
import qualified TEAWin32.GUI.Component.Internal           as ComponentInternal
import           TEAWin32.GUI.Component.Internal.Attribute
import           TEAWin32.GUI.Component.Property           (GUIComponentProperty (..),
                                                            IsGUIComponentProperty (applyProperty))

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
            (Win32.wS_TABSTOP .|. Win32.wS_VISIBLE .|. Win32.wS_CHILD .|. Win32.bS_DEFPUSHBUTTON .|. Win32.wS_CLIPSIBLINGS)
            Nothing
            Nothing
            Nothing
            Nothing
            parentHWND
            Nothing
            (intPtrToPtr $ fromIntegral parentInstance)
            (const $ const $ const $ const $ pure 0)

        ApplicationInternal.registerHWND buttonUniqueId button

        registerHWNDToAttributeMap button
        addAttributeToHWND button (ComponentUniqueIdAttr buttonUniqueId)
        addAttributeToHWND button (ComponentTypeAttr ComponentButton)

        ComponentInternal.useDefaultFont button
        ComponentInternal.bringComponentToTop button

        mapM_ (`applyProperty` button) buttonProperties

        pure button
