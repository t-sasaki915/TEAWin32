module TEAWin32.Internal.Native
    ( c_InitialiseDPIAwareFunctions
    , c_GetScaleFactorForHWND
    , c_EnableVisualStyles
    , c_GetImmediateChildWindows
    , c_GetTopLevelWindows
    , c_IsWindowTopLevel
    , c_GetHighDPIIcon
    , c_EnableDPIAware
    , c_ShowErrorReporter
    , c_CreateFontSimple
    ) where

import           Foreign                   (Ptr)
import           Foreign.C                 (CWString)
import qualified Graphics.Win32            as Win32
import qualified TEAWin32.Internal.Foreign as Win32

foreign import ccall unsafe "InitialiseDPIAwareFunctions"
    c_InitialiseDPIAwareFunctions :: IO ()

foreign import ccall unsafe "GetScaleFactorForHWND"
    c_GetScaleFactorForHWND :: Win32.HWND -> IO Double

foreign import ccall unsafe "EnableVisualStyles"
    c_EnableVisualStyles :: IO Win32.HANDLE

foreign import ccall unsafe "GetImmediateChildWindows"
    c_GetImmediateChildWindows :: Win32.HWND -> Ptr Win32.HWND -> Int -> IO Int

foreign import ccall unsafe "GetTopLevelWindows"
    c_GetTopLevelWindows :: Ptr Win32.HWND -> Int -> IO Int

foreign import ccall unsafe "IsWindowTopLevel"
    c_IsWindowTopLevel :: Win32.HWND -> IO Bool

foreign import ccall unsafe "GetHighDPIIcon"
    c_GetHighDPIIcon :: Win32.SHSTOCKICONID -> IO Win32.HICON

foreign import ccall unsafe "EnableDPIAware"
    c_EnableDPIAware :: IO ()

foreign import ccall "ShowErrorReporter"
    c_ShowErrorReporter :: CWString -> CWString -> CWString -> CWString -> IO ()

foreign import ccall "CreateFontSimple"
    c_CreateFontSimple :: Int -> CWString -> IO Win32.HFONT
