module TEAWin32.Internal.Foreign
    ( ACTCTX (..)
    , GetDpiForWindow
    , c_SetClassLongPtr
    , c_SetWindowPos
    , c_DeleteObject
    , c_EnumChildWindows
    , c_EnumThreadWindows
    , makeEnumWindowProc
    , c_CreateActCtx
    , c_ActivateActCtx
    , c_ReleaseActCtx
    , c_GetSysColorBrush
    , c_SetProcessDPIAware
    , c_GetDeviceCaps
    , makeGetDpiForWindow
    , gCLP_HICON
    , gCLP_HCURSOR
    , dEFAULT_GUI_FONT
    , wM_DPICHANGED
    , makeIntResource
    ) where

import           Data.Int       (Int32)
import           Foreign        (FunPtr, Ptr, Storable (..), Word16, Word32,
                                 fillBytes, intPtrToPtr)
import           Foreign.C      (CIntPtr (..))
import qualified Graphics.Win32 as Win32

data ACTCTX = ACTCTX
    { cbSize        :: Word32
    , actctxDWFlags :: Word32
    , lpSource      :: Win32.LPCWSTR
    , lpResName     :: Win32.LPCWSTR
    }

instance Storable ACTCTX where
    sizeOf _ = 56

    alignment _ = 8

    poke ptr act = do
        fillBytes ptr 0 (sizeOf act)

        (`pokeByteOff` 0)  ptr (cbSize act)
        (`pokeByteOff` 4)  ptr (actctxDWFlags act)
        (`pokeByteOff` 8)  ptr (lpSource act)
        (`pokeByteOff` 32) ptr (lpResName act)

    peek ptr = do
        cbSize'        <- peekByteOff ptr 0
        actctxDWFlags' <- peekByteOff ptr 4
        lpSource'      <- peekByteOff ptr 8
        lpResName'     <- peekByteOff ptr 32

        pure $ ACTCTX
            { cbSize        = cbSize'
            , actctxDWFlags = actctxDWFlags'
            , lpSource      =  lpSource'
            , lpResName     = lpResName'
            }

foreign import ccall "SetClassLongPtrW"
    c_SetClassLongPtr :: Win32.HWND -> Int32 -> Ptr () -> IO (Ptr ())

foreign import ccall "SetWindowPos"
    c_SetWindowPos :: Win32.HWND -> Win32.HWND -> Int32 -> Int32 -> Int32 -> Int32 -> Win32.UINT -> IO Win32.BOOL

foreign import ccall "DeleteObject"
    c_DeleteObject :: Ptr () -> IO Win32.BOOL

type WindowEnumProc = Win32.HWND -> Win32.LPARAM -> IO Win32.BOOL

foreign import ccall "wrapper"
    makeEnumWindowProc :: WindowEnumProc -> IO (FunPtr WindowEnumProc)

foreign import ccall "EnumChildWindows"
    c_EnumChildWindows :: Win32.HWND -> FunPtr WindowEnumProc -> Win32.LPARAM -> IO Win32.BOOL

foreign import ccall "EnumThreadWindows"
    c_EnumThreadWindows :: Win32.DWORD -> FunPtr WindowEnumProc -> Win32.LPARAM -> IO Win32.BOOL

foreign import ccall "CreateActCtxW"
    c_CreateActCtx :: Ptr ACTCTX -> IO Win32.HANDLE

foreign import ccall "ActivateActCtx"
    c_ActivateActCtx :: Win32.HANDLE -> Ptr Win32.ULONG_PTR -> IO Bool

foreign import ccall "ReleaseActCtx"
    c_ReleaseActCtx :: Win32.HANDLE -> IO ()

foreign import ccall "GetSysColorBrush"
    c_GetSysColorBrush :: Word32 -> IO Win32.HBRUSH

foreign import ccall "SetProcessDPIAware"
    c_SetProcessDPIAware :: IO Bool

foreign import ccall "GetDeviceCaps"
    c_GetDeviceCaps :: Win32.HDC -> Int -> IO Int

type GetDpiForWindow = Win32.HWND -> IO Word32

foreign import ccall "dynamic"
    makeGetDpiForWindow :: FunPtr GetDpiForWindow -> GetDpiForWindow

gCLP_HICON :: Int32
gCLP_HICON = -14

gCLP_HCURSOR :: Int32
gCLP_HCURSOR = -12

dEFAULT_GUI_FONT :: Word16
dEFAULT_GUI_FONT = 17

wM_DPICHANGED :: Win32.WindowMessage
wM_DPICHANGED = 0x02E0

makeIntResource :: Int -> Win32.LPCTSTR
makeIntResource = intPtrToPtr . fromIntegral
