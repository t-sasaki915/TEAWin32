module Graphics.GUI.Component.Window (Window (..), destroyChildren) where

import           Control.Exception                      (bracket)
import           Control.Monad                          (unless, when)
import           Data.IORef                             (atomicModifyIORef')
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Foreign                                (intPtrToPtr)
import qualified Framework.TEA.Internal                 as TEAInternal
import           Graphics.Drawing                       (toWin32Colour)
import           Graphics.GUI                           (UniqueId, WindowStyle,
                                                         toWin32WindowStyle)
import           Graphics.GUI.Component                 (IsGUIComponent (..))
import qualified Graphics.GUI.Component.Internal        as ComponentInternal
import           Graphics.GUI.Component.Internal.Prop
import           Graphics.GUI.Component.Property        (GUIComponentProperty (..),
                                                         IsGUIComponentProperty (applyProperty))
import           Graphics.GUI.Component.Window.Property (WindowProperty (..))
import qualified Graphics.GUI.Foreign                   as Win32
import qualified Graphics.GUI.Internal                  as Internal
import qualified Graphics.Win32                         as Win32
import qualified System.Win32                           as Win32

data Window = Window UniqueId Text WindowStyle [WindowProperty] deriving (Show, Eq)

instance IsGUIComponent Window where
    getProperties (Window _ _ _ properties) = GUIComponentProperty <$> properties

    getUniqueId (Window uniqueId _ _ _) = uniqueId

    doesNeedToRedraw (Window uniqueId1 className1 style1 _) (Window uniqueId2 className2 style2 _) =
        uniqueId1 /= uniqueId2 || className1 /= className2 || style1 /= style2

    render (Window windowUniqueId windowClassName windowStyle windowProperties) parentHWND = do
        mainInstance <- Win32.getModuleHandle Nothing

        let windowClass = Win32.mkClassName (ComponentInternal.windowClassPrefix <> Text.unpack windowClassName)

        _ <- Win32.registerClass
                ( Win32.cS_VREDRAW + Win32.cS_HREDRAW
                , mainInstance
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , windowClass
                )

        window <- Win32.createWindow
                    windowClass
                    ""
                    (toWin32WindowStyle windowStyle)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    parentHWND
                    Nothing
                    mainInstance
                    defaultWindowProc

        registerHWNDToPropMap window

        mapM_ (`applyProperty` window) windowProperties

        _ <- Win32.showWindow window Win32.sW_SHOWNORMAL
        Win32.updateWindow window

        atomicModifyIORef' Internal.activeWindowCountRef $ \n -> (n + 1, ())

        TEAInternal.registerHWND windowUniqueId window

        ComponentInternal.setComponentType "WINDOW" window
        ComponentInternal.setUniqueIdToHWND windowUniqueId window

        pure window

defaultWindowProc :: Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
defaultWindowProc hwnd wMsg wParam lParam
    | wMsg == Win32.wM_DESTROY = do
        destroyChildren hwnd
        finaliseHWND hwnd

        remainingWindow <- atomicModifyIORef' Internal.activeWindowCountRef $ \n -> (n - 1, n - 1)
        when (remainingWindow == 0) $
            Win32.postQuitMessage 0

        pure 0

    | wMsg == Win32.wM_COMMAND = do
        let notification = Win32.hIWORD (fromIntegral wParam)
            targetHWND   = intPtrToPtr (fromIntegral lParam)

        case notification of
            0 -> do -- BN_CLICKED
                ComponentInternal.getEventHandlerMaybe "COMPONENTONCLICK" targetHWND >>= \case
                    Just msg -> TEAInternal.issueMsg msg >> pure 0
                    Nothing  -> Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

            _ ->
                Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

    | wMsg == Win32.wM_ERASEBKGND =
        ComponentInternal.getWindowBackgroundColourMaybe hwnd >>= \case
            Just backgroundColour -> do
                let hdc = intPtrToPtr $ fromIntegral wParam

                rect <- Win32.getClientRect hwnd
                bracket (Win32.createSolidBrush (toWin32Colour backgroundColour)) Win32.c_DeleteObject $
                    Win32.fillRect hdc rect

                pure 1

            Nothing ->
                pure 0

    | otherwise =
        Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

destroyChildren :: Win32.HWND -> IO ()
destroyChildren hwnd =
    Internal.withImmediateChildWindows hwnd $ mapM_ $ \child -> do
        isWindow <- ComponentInternal.isManagedWindow child

        unless isWindow $
            finaliseHWND child

        Win32.destroyWindow child

finaliseHWND :: Win32.HWND -> IO ()
finaliseHWND hwnd = do
    finaliseAndUnregisterHWNDFromPropMap hwnd

    TEAInternal.unregisterHWND hwnd
