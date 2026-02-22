module TEAWin32.Util (hwndToInt, intToHWND) where

import           Foreign        (intPtrToPtr, ptrToIntPtr)
import qualified Graphics.Win32 as Win32

hwndToInt :: Win32.HWND -> Int
hwndToInt hwnd = fromIntegral (ptrToIntPtr hwnd)

intToHWND :: Int -> Win32.HWND
intToHWND hwnd = intPtrToPtr (fromIntegral hwnd)
