module TEAWin32.Util (try_, hwndToInt) where

import           Control.Exception (SomeException, try)
import           Foreign           (ptrToIntPtr)
import qualified Graphics.Win32    as Win32

try_ :: IO a -> IO (Either SomeException a)
try_ = try

hwndToInt :: Win32.HWND -> Int
hwndToInt hwnd = fromIntegral (ptrToIntPtr hwnd)
