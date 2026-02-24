{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TEAWin32.Internal.Foreign
    ( c_SetClassLongPtr
    , c_SetWindowPos
    , c_DeleteObject
    , c_ReleaseActCtx
    , c_GetSysColorBrush
    , c_SelectObject
    , c_DrawIconEx
    , c_GetScaleFactorForHWND
    , c_InitialiseDPIAwareFunctions
    , c_EnableVisualStyles
    , c_GetImmediateChildWindows
    , c_GetTopLevelWindows
    , c_IsWindowTopLevel
    , c_GetHighDPIIcon
    , c_EnableDPIAware
    , c_ShowErrorReporter
    , gCLP_HICON
    , gCLP_HCURSOR
    , dEFAULT_GUI_FONT
    , wM_DPICHANGED
    , mB_CANCELTRYIGNORE
    , mB_ICONWARNING
    , mB_ICONERROR
    , mB_RIGHT
    , mB_TOPMOST
    , iDCONTINUE
    , iDTRYAGAIN
    , makeIntResource
    ) where

import           Data.Int       (Int32)
import           Foreign        (Ptr, Storable (..), Word16, Word32, fillBytes,
                                 intPtrToPtr)
import           Foreign.C      (CWString)
import qualified Graphics.Win32 as Win32

foreign import ccall "SetClassLongPtrW"
    c_SetClassLongPtr :: Win32.HWND -> Int32 -> Ptr () -> IO (Ptr ())

foreign import ccall "SetWindowPos"
    c_SetWindowPos :: Win32.HWND -> Win32.HWND -> Int32 -> Int32 -> Int32 -> Int32 -> Win32.UINT -> IO Win32.BOOL

foreign import ccall "DeleteObject"
    c_DeleteObject :: Ptr () -> IO Win32.BOOL

foreign import ccall "ReleaseActCtx"
    c_ReleaseActCtx :: Win32.HANDLE -> IO ()

foreign import ccall "GetSysColorBrush"
    c_GetSysColorBrush :: Word32 -> IO Win32.HBRUSH

foreign import ccall "SelectObject"
    c_SelectObject :: Win32.HDC -> Win32.HANDLE -> IO Win32.HANDLE

foreign import ccall "DrawIconEx"
    c_DrawIconEx :: Win32.HDC -> Int -> Int -> Win32.HICON -> Int -> Int -> Win32.UINT -> Win32.HBRUSH -> Win32.UINT -> IO Win32.BOOL

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
    c_GetHighDPIIcon :: Int -> IO Win32.HICON

foreign import ccall unsafe "EnableDPIAware"
    c_EnableDPIAware :: IO ()

foreign import ccall "ShowErrorReporter"
    c_ShowErrorReporter :: CWString -> CWString -> CWString -> CWString -> IO ()

gCLP_HICON :: Int32
gCLP_HICON = -14

gCLP_HCURSOR :: Int32
gCLP_HCURSOR = -12

dEFAULT_GUI_FONT :: Word16
dEFAULT_GUI_FONT = 17

wM_DPICHANGED :: Win32.WindowMessage
wM_DPICHANGED = 0x02E0

mB_CANCELTRYIGNORE :: Win32.MBStyle
mB_CANCELTRYIGNORE = 0x00000006

mB_ICONWARNING :: Win32.MBStyle
mB_ICONWARNING = 0x00000030

mB_ICONERROR :: Win32.MBStyle
mB_ICONERROR = 0x00000010

mB_RIGHT :: Win32.MBStyle
mB_RIGHT = 0x00080000

mB_TOPMOST :: Win32.MBStyle
mB_TOPMOST = 0x00040000

iDCONTINUE :: Win32.MBStatus
iDCONTINUE = 11

iDTRYAGAIN :: Win32.MBStatus
iDTRYAGAIN = 10

makeIntResource :: Int -> Win32.LPCTSTR
makeIntResource = intPtrToPtr . fromIntegral

instance Storable Win32.RECT where
    sizeOf _ = sizeOf (0 :: Win32.LONG) * 4

    alignment _ = alignment (0 :: Win32.LONG)

    peek ptr = do
        left   <- peekByteOff ptr 0
        top    <- peekByteOff ptr (sizeOf (0 :: Win32.LONG))
        right  <- peekByteOff ptr (sizeOf (0 :: Win32.LONG) * 2)
        bottom <- peekByteOff ptr (sizeOf (0 :: Win32.LONG) * 3)

        pure (left, top, right, bottom)

    poke ptr (left, top, right, bottom) = do
        fillBytes ptr 0 (sizeOf (0 :: Win32.LONG) * 4)

        pokeByteOff ptr 0 left
        pokeByteOff ptr (sizeOf (0 :: Win32.LONG)) top
        pokeByteOff ptr (sizeOf (0 :: Win32.LONG) * 2) right
        pokeByteOff ptr (sizeOf (0 :: Win32.LONG) * 3) bottom
