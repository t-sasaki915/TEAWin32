module TEAWin32.Application.WndProc (windowProc) where

import           Control.Exception                        (bracket)
import           Control.Monad                            (when)
import           Data.IORef                               (atomicModifyIORef')
import qualified Data.Text                                as Text
import           Foreign                                  (intPtrToPtr)
import           GHC.Stack                                (HasCallStack)
import qualified Graphics.Win32                           as Win32
import qualified System.Win32                             as Win32
import qualified TEAWin32.Application.Internal            as ApplicationInternal
import           TEAWin32.Drawing                         (toWin32Colour)
import           TEAWin32.Exception                       (TEAWin32Error (InternalTEAWin32Error),
                                                           errorTEAWin32)
import           TEAWin32.GUI.Component.ComponentRegistry
import qualified TEAWin32.GUI.Component.Internal          as ComponentInternal
import qualified TEAWin32.GUI.Internal                    as GUIInternal
import qualified TEAWin32.Internal.Foreign                as Win32

onWindowDestroy :: Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
onWindowDestroy hwnd _ _ _ = do
    ComponentInternal.destroyChildren hwnd

    unregisterComponentFromRegistry hwnd

    remainingWindow <- atomicModifyIORef' GUIInternal.activeWindowCountRef $ \n -> (n - 1, n - 1)

    isUpdateProgressing' <- ApplicationInternal.isUpdateProgressing
    when (remainingWindow == 0 && not isUpdateProgressing') $
        Win32.postQuitMessage 0

    pure 0

onWindowCommand :: HasCallStack => Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
onWindowCommand hwnd wMsg wParam lParam = do
    let notification = Win32.hIWORD (fromIntegral wParam)
        targetHWND   = intPtrToPtr (fromIntegral lParam)

    case notification of
        0 -> -- BN_CLICKED
            getComponentRegistryEntryValueMaybe ComponentClickEventHandlerRegKey targetHWND >>= \case
                Just msg -> ApplicationInternal.issueMsg msg >> pure 0
                Nothing  -> Win32.defWindowProc (Just hwnd) wMsg wParam lParam

        _ ->
            Win32.defWindowProc (Just hwnd) wMsg wParam lParam

onWindowEraseBkgnd :: HasCallStack => Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
onWindowEraseBkgnd hwnd _ wParam _ = do
    let hdc = intPtrToPtr $ fromIntegral wParam
    rect <- Win32.getClientRect hwnd

    getComponentRegistryEntryValueMaybe WindowBackgroundColourRegKey hwnd >>= \case
        Just backgroundColour -> do
            bracket (Win32.createSolidBrush (toWin32Colour backgroundColour)) Win32.c_DeleteObject $
                Win32.fillRect hdc rect

            pure 1

        Nothing -> do
            Win32.c_GetSysColorBrush Win32.cOLOR_WINDOW >>= \brush ->
                Win32.fillRect hdc rect brush

            pure 1

onWindowDPIChanged :: Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
onWindowDPIChanged _ _ _ _ = {-do
    putStrLn "!?"
    let rectPtr = castPtr $ wordPtrToPtr $ fromIntegral lParam
    (l, t, r, b) <- peek rectPtr :: IO Win32.RECT

    Win32.c_SetWindowPos hwnd Win32.nullPtr l t (r - l) (b - t) (Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE .|. Win32.sWP_FRAMECHANGED) >>= print

    let newDPI = Win32.lOWORD (fromIntegral wParam)
    ComponentInternal.updateComponentDPI hwnd (fromIntegral newDPI)-}

    pure 0

windowProc :: HasCallStack => Win32.HWND -> Win32.WindowMessage -> Win32.WPARAM -> Win32.LPARAM -> IO Win32.LRESULT
windowProc hwnd wMsg wParam lParam
    | wMsg == Win32.wM_DESTROY    = onWindowDestroy hwnd wMsg wParam lParam
    | wMsg == Win32.wM_COMMAND    = onWindowCommand hwnd wMsg wParam lParam
    | wMsg == Win32.wM_ERASEBKGND = onWindowEraseBkgnd hwnd wMsg wParam lParam
    | wMsg == Win32.wM_DPICHANGED = onWindowDPIChanged hwnd wMsg wParam lParam
    | otherwise = errorTEAWin32 $ InternalTEAWin32Error $
        "Received an WindowMessage for which Haskell is not responsible: " <> Text.show wMsg
            <> ", HWND: " <> Text.show hwnd
            <> ", WPARAM: " <> Text.show wParam
            <> ", LPARAM: " <> Text.show lParam
            <> "."
