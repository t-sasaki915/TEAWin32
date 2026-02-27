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
import qualified TEAWin32.GUI.Internal                    as GUIInternal
import qualified TEAWin32.Internal.Native                 as Native

data Window = Window UniqueId Text WindowStyle [WindowProperty] deriving (Show, Eq)

instance IsGUIComponent Window where
    getProperties (Window _ _ _ properties) = GUIComponentProperty <$> properties

    getUniqueId (Window uniqueId _ _ _) = uniqueId

    doesNeedToRedraw (Window uniqueId1 className1 style1 _) (Window uniqueId2 className2 style2 _) =
        uniqueId1 /= uniqueId2 || className1 /= className2 || style1 /= style2

    getComponentType _ = ComponentWindow

    render (Window windowUniqueId windowClassName windowStyle windowProperties) parentHWND = do
        window <- withCWString (Text.unpack windowClassName) $ \windowClassName' -> do
            let args = Native.CreateManagedWindowArgs
                    { Native.managedWindowClassName   = windowClassName'
                    , Native.managedWindowClassStyles = Win32.cS_VREDRAW + Win32.cS_HREDRAW
                    , Native.managedWindowExStyles    = 0
                    , Native.managedWindowStyles      = toWin32WindowStyle windowStyle
                    , Native.managedWindowParentHWND  = fromMaybe Win32.nullPtr parentHWND
                    }

            Native.createManagedWindow args

        scaleFactor <- Native.getScaleFactorForHWND window

        registerComponentToRegistry windowUniqueId window
        addComponentRegistryEntry ComponentUniqueIdRegKey    (ComponentUniqueIdReg windowUniqueId) window
        addComponentRegistryEntry ComponentTypeRegKey        (ComponentTypeReg ComponentWindow)    window
        addComponentRegistryEntry ComponentScaleFactorRegKey (ComponentScaleFactorReg scaleFactor) window
        addComponentRegistryEntry WindowClassNameRegKey      (WindowClassNameReg windowClassName)  window
        addComponentRegistryEntry WindowStyleRegKey          (WindowStyleReg windowStyle)          window

        --ComponentInternal.useDefaultFont window

        mapM_ (`applyProperty` window) windowProperties

        _ <- Win32.showWindow window Win32.sW_SHOW
        Win32.updateWindow window
        --ComponentInternal.bringComponentToTop window

        atomicModifyIORef' GUIInternal.activeWindowCountRef $ \n -> (n + 1, ())

        pure window
