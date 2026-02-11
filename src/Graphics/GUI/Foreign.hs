{-# OPTIONS_GHC -Wno-unsupported-calling-conventions #-}

module Graphics.GUI.Foreign
    ( WindowEnumProc
    , c_SetClassLongPtr
    , c_GetClassLongPtr
    , c_SetWindowPos
    , c_DeleteObject
    , c_EnumChildWindows
    , c_GetClassName
    , c_EnumThreadWindows
    , makeEnumWindowProc
    , gWL_STYLE
    , gCLP_HICON
    , gCLP_HCURSOR
    , gCLP_HBRBACKGROUND
    , makeIntResource
    ) where

import           Data.Int       (Int32)
import           Foreign        (FunPtr, Ptr, intPtrToPtr)
import           Foreign.C      (CIntPtr (..))
import qualified Graphics.Win32 as Win32

foreign import ccall "SetClassLongPtrW"
    c_SetClassLongPtr :: Win32.HWND -> Int32 -> Ptr () -> IO (Ptr ())

foreign import ccall "SetWindowPos"
    c_SetWindowPos :: Win32.HWND -> Win32.HWND -> Int32 -> Int32 -> Int32 -> Int32 -> Win32.UINT -> IO Win32.BOOL

foreign import ccall "GetClassLongPtrW"
    c_GetClassLongPtr :: Win32.HWND -> Int32 -> IO CIntPtr

foreign import ccall "GetClassNameW"
    c_GetClassName :: Win32.HWND -> Win32.LPWSTR -> Int -> IO Int

foreign import ccall "DeleteObject"
    c_DeleteObject :: Ptr () -> IO Win32.BOOL

type WindowEnumProc = Win32.HWND -> Win32.LPARAM -> IO Win32.BOOL

foreign import ccall "wrapper"
    makeEnumWindowProc :: WindowEnumProc -> IO (FunPtr WindowEnumProc)

foreign import ccall "EnumChildWindows"
    c_EnumChildWindows :: Win32.HWND -> FunPtr WindowEnumProc -> Win32.LPARAM -> IO Win32.BOOL

foreign import ccall "EnumThreadWindows"
    c_EnumThreadWindows :: Win32.DWORD -> FunPtr WindowEnumProc -> Win32.LPARAM -> IO Win32.BOOL

gWL_STYLE :: Int32
gWL_STYLE = -16

gCLP_HICON :: Int32
gCLP_HICON = -14

gCLP_HCURSOR :: Int32
gCLP_HCURSOR = -12

gCLP_HBRBACKGROUND :: Int32
gCLP_HBRBACKGROUND = -10

makeIntResource :: Int -> Win32.LPCTSTR
makeIntResource = intPtrToPtr . fromIntegral
