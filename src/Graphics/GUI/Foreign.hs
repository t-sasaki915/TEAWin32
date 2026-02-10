{-# OPTIONS_GHC -Wno-unsupported-calling-conventions #-}

module Graphics.GUI.Foreign
    ( WindowEnumProc
    , c_SetClassLongPtr
    , c_GetClassLongPtr
    , c_SetWindowPos
    , c_SetProp
    , c_GetProp
    , c_RemoveProp
    , c_EnumPropsEx
    , c_DeleteObject
    , c_EnumChildWindows
    , c_GetClassName
    , makePropEnumProcEx
    , makeEnumWindowProc
    , gCLP_HICON
    , gCLP_HCURSOR
    , gCLP_HBRBACKGROUND
    , makeIntResource
    ) where

import           Data.Int       (Int32)
import           Foreign        (FunPtr, Ptr, intPtrToPtr)
import           Foreign.C      (CIntPtr (..))
import           Graphics.Win32 (BOOL, HANDLE, HWND, INT, LPARAM, LPCTSTR,
                                 LPCWSTR, LPWSTR, UINT)

foreign import stdcall "windows.h SetClassLongPtrW"
    c_SetClassLongPtr :: HWND -> Int32 -> Ptr () -> IO (Ptr ())

foreign import stdcall "windows.h SetWindowPos"
    c_SetWindowPos :: HWND -> HWND -> Int32 -> Int32 -> Int32 -> Int32 -> UINT -> IO BOOL

foreign import ccall "GetClassLongPtrW"
    c_GetClassLongPtr :: HWND -> Int32 -> IO CIntPtr

foreign import ccall "SetPropW"
    c_SetProp :: HWND -> LPCTSTR -> HANDLE -> IO Bool

foreign import ccall "GetPropW"
    c_GetProp :: HWND -> LPCWSTR -> IO HANDLE

foreign import ccall "RemovePropW"
    c_RemoveProp :: HWND -> LPCWSTR -> IO HANDLE

foreign import ccall "GetClassNameW"
    c_GetClassName :: HWND -> LPWSTR -> Int -> IO Int

type PropEnumProcEx = HWND -> LPCTSTR -> HANDLE -> CIntPtr -> IO BOOL

foreign import ccall "wrapper"
    makePropEnumProcEx :: PropEnumProcEx -> IO (FunPtr PropEnumProcEx)

foreign import ccall "EnumPropsExW"
    c_EnumPropsEx :: HWND -> FunPtr PropEnumProcEx -> CIntPtr -> IO INT

foreign import ccall "DeleteObject"
    c_DeleteObject :: Ptr () -> IO BOOL

type WindowEnumProc = HWND -> LPARAM -> IO BOOL

foreign import stdcall "wrapper"
    makeEnumWindowProc :: WindowEnumProc -> IO (FunPtr WindowEnumProc)

foreign import stdcall "windows.h EnumChildWindows"
    c_EnumChildWindows :: HWND -> FunPtr WindowEnumProc -> LPARAM -> IO BOOL

gCLP_HICON :: Int32
gCLP_HICON = -14

gCLP_HCURSOR :: Int32
gCLP_HCURSOR = -12

gCLP_HBRBACKGROUND :: Int32
gCLP_HBRBACKGROUND = -10

makeIntResource :: Int -> LPCTSTR
makeIntResource = intPtrToPtr . fromIntegral
