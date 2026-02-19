module TEAWin32.GUI.Component.Window (Window (..), destroyChildren) where

import           Control.Exception                         (bracket)
import           Control.Monad                             (unless, when)
import           Data.IORef                                (atomicModifyIORef')
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Foreign                                   (Storable (peek),
                                                            castPtr,
                                                            intPtrToPtr,
                                                            wordPtrToPtr)
import           GHC.Ptr                                   (Ptr (Ptr))
import qualified Graphics.Win32                            as Win32
import qualified System.Win32                              as Win32
import qualified TEAWin32.Application.Internal             as ApplicationInternal
import           TEAWin32.Drawing                          (toWin32Colour)
import           TEAWin32.GUI                              (UniqueId,
                                                            WindowStyle,
                                                            toWin32WindowStyle)
import           TEAWin32.GUI.Component                    (IsGUIComponent (..))
import qualified TEAWin32.GUI.Component.Internal           as ComponentInternal
import           TEAWin32.GUI.Component.Internal.Attribute
import           TEAWin32.GUI.Component.Property           (GUIComponentProperty (..),
                                                            IsGUIComponentProperty (applyProperty))
import           TEAWin32.GUI.Component.Window.Property    (WindowProperty (..))
import qualified TEAWin32.GUI.Internal                     as GUIInternal
import qualified TEAWin32.Internal.Foreign                 as Win32

data Window = Window UniqueId Text WindowStyle [WindowProperty] deriving (Show, Eq)

instance IsGUIComponent Window where
    getProperties (Window _ _ _ properties) = GUIComponentProperty <$> properties

    getUniqueId (Window uniqueId _ _ _) = uniqueId

    doesNeedToRedraw (Window uniqueId1 className1 style1 _) (Window uniqueId2 className2 style2 _) =
        uniqueId1 /= uniqueId2 || className1 /= className2 || style1 /= style2

    render (Window windowUniqueId windowClassName windowStyle windowProperties) parentHWND = do
        mainInstance <- Win32.getModuleHandle Nothing

        let windowClass = Win32.mkClassName (Text.unpack windowClassName)

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

        currentDPI <- GUIInternal.getDPIFromHWND window

        ApplicationInternal.registerHWND windowUniqueId window

        registerHWNDToAttributeMap window
        addAttributeToHWND window (ComponentUniqueIdAttr windowUniqueId)
        addAttributeToHWND window (ComponentTypeAttr ComponentWindow)
        addAttributeToHWND window (ComponentCurrentDPIAttr currentDPI)
        addAttributeToHWND window (WindowClassNameAttr windowClassName)
        addAttributeToHWND window (WindowStyleAttr windowStyle)

        ComponentInternal.bringComponentToTop window
        ComponentInternal.useDefaultFont window

        mapM_ (`applyProperty` window) windowProperties

        _ <- Win32.showWindow window Win32.sW_SHOWNORMAL
        Win32.updateWindow window

        atomicModifyIORef' GUIInternal.activeWindowCountRef $ \n -> (n + 1, ())

        pure window

defaultWindowProc :: Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
defaultWindowProc hwnd wMsg wParam lParam
    | wMsg == Win32.wM_DESTROY = do
        destroyChildren hwnd
        finaliseHWND hwnd

        remainingWindow <- atomicModifyIORef' GUIInternal.activeWindowCountRef $ \n -> (n - 1, n - 1)
        when (remainingWindow == 0) $
            Win32.postQuitMessage 0

        pure 0

    | wMsg == Win32.wM_COMMAND = do
        let notification = Win32.hIWORD (fromIntegral wParam)
            targetHWND   = intPtrToPtr (fromIntegral lParam)

        case notification of
            0 -> do -- BN_CLICKED
                getEventHandlerFromHWNDMaybe ComponentClickEvent targetHWND >>= \case
                    Just msg -> ApplicationInternal.issueMsg msg >> pure 0
                    Nothing  -> Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

            _ ->
                Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

    | wMsg == Win32.wM_ERASEBKGND = do
        let hdc = intPtrToPtr $ fromIntegral wParam
        rect <- Win32.getClientRect hwnd

        getComponentBackgroundColourFromHWNDMaybe hwnd >>= \case
            Just backgroundColour -> do
                bracket (Win32.createSolidBrush (toWin32Colour backgroundColour)) Win32.c_DeleteObject $
                    Win32.fillRect hdc rect

                pure 1

            Nothing -> do
                Win32.c_GetSysColorBrush Win32.cOLOR_WINDOW >>= \brush ->
                    Win32.fillRect hdc rect brush

                pure 1

    | wMsg == Win32.wM_DPICHANGED = do
        let newDPI = Win32.lOWORD (fromIntegral wParam)
        ComponentInternal.updateComponentDPIProperty hwnd (fromIntegral newDPI)

        let rectPtr = castPtr $ wordPtrToPtr $ fromIntegral lParam :: Ptr Win32.RECT
        (w, h, x, y) <- peek rectPtr

        ComponentInternal.setComponentSize (fromIntegral w) (fromIntegral h) hwnd
        ComponentInternal.setComponentPosition (fromIntegral x) (fromIntegral y) hwnd

        pure 0

    | otherwise =
        Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

destroyChildren :: Win32.HWND -> IO ()
destroyChildren hwnd =
    GUIInternal.withImmediateChildWindows hwnd $ mapM_ $ \child -> do
        isWindow <- isManagedByTEAWin32 child

        unless isWindow $
            finaliseHWND child

        Win32.destroyWindow child

finaliseHWND :: Win32.HWND -> IO ()
finaliseHWND hwnd = do
    unregisterHWNDFromAttributeMap hwnd
    ApplicationInternal.unregisterHWND hwnd
