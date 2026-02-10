module Graphics.GUI.Component.Window (Window (..)) where

import           Control.Monad                          (forM_, void, when)
import           Data.IORef                             (atomicModifyIORef',
                                                         readIORef)
import qualified Data.Map                               as Map
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           Foreign                                (freeHaskellFunPtr,
                                                         intPtrToPtr)
import qualified Framework.TEA.Internal                 as TEAInternal
import           Graphics.GUI                           (UniqueId, WindowStyle,
                                                         toWin32WindowStyle)
import           Graphics.GUI.Component                 (IsGUIComponent (..))
import qualified Graphics.GUI.Component.Internal        as ComponentInternal
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

        mapM_ (`applyProperty` window) windowProperties

        _ <- Win32.showWindow window Win32.sW_SHOWNORMAL
        Win32.updateWindow window

        void $ atomicModifyIORef' Internal.activeWindowCountRef $ \n -> (n + 1, n + 1)

        TEAInternal.registerHWND windowUniqueId window

        ComponentInternal.setComponentType "WINDOW" window
        ComponentInternal.setUniqueIdToHWND windowUniqueId window

        pure window

defaultWindowProc :: Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
defaultWindowProc hwnd wMsg wParam lParam
    | wMsg == Win32.wM_DESTROY = do
        remainingWindow <- atomicModifyIORef' Internal.activeWindowCountRef $ \n -> (n - 1, n - 1)

        cleanupEventHandlers hwnd
        cleanupProps hwnd
        cleanupGDIs hwnd
        unregisterHWND hwnd

        when (remainingWindow <= 0) $
            Win32.postQuitMessage 0

        pure 0

    | wMsg == Win32.wM_COMMAND = do
        let notification = Win32.hIWORD (fromIntegral wParam)
            targetHWND = intPtrToPtr (fromIntegral lParam)

        case notification of
            0 -> do -- BN_CLICKED
                buttonClickEventHandlers <- readIORef TEAInternal.buttonClickEventHandlersRef

                case Map.lookup targetHWND buttonClickEventHandlers of
                    Just action -> TEAInternal.issueMsg action >> pure 0
                    Nothing     -> Win32.defWindowProc (Just hwnd) wMsg wParam lParam

            _ ->
                Win32.defWindowProc (Just hwnd) wMsg wParam lParam

    | otherwise =
        Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

cleanupEventHandlers :: Win32.HWND -> IO ()
cleanupEventHandlers hwnd =
    Internal.withChildWindows hwnd $ \children ->
        void $ atomicModifyIORef' TEAInternal.buttonClickEventHandlersRef $ \handlers ->
            let newHandlers = Map.filterWithKey (\k -> const $ k `notElem` children && k /= hwnd) handlers in
                (newHandlers, newHandlers)

cleanupProps :: Win32.HWND -> IO ()
cleanupProps hwnd = do
    ComponentInternal.unregisterComponentType hwnd
    ComponentInternal.unregisterUniqueIdFromHWND hwnd

    Internal.withChildWindows hwnd $ \children ->
        forM_ children $ \child -> do
            ComponentInternal.unregisterComponentType child
            ComponentInternal.unregisterUniqueIdFromHWND child

unregisterHWND :: Win32.HWND -> IO ()
unregisterHWND targetHWND =
    unregister targetHWND >>
        Internal.withChildWindows targetHWND (mapM_ unregister)
    where
        unregister hwnd =
            void $ atomicModifyIORef' TEAInternal.uniqueIdAndHWNDMapRef $ \hwndMap ->
                let newHWNDMap = Map.filter (/= hwnd) hwndMap in
                    (newHWNDMap, newHWNDMap)

cleanupGDIs :: Win32.HWND -> IO ()
cleanupGDIs hwnd = do
    let callback _ _ hData _ =
            void (Win32.c_DeleteObject hData) >>
                pure True

    callbackPtr <- Win32.makePropEnumProcEx callback

    void $ Win32.c_EnumPropsEx hwnd callbackPtr 0

    freeHaskellFunPtr callbackPtr
