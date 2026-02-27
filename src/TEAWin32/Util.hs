module TEAWin32.Util (try_, hwndToInt, applyScaleFactor) where

import           Control.Exception (SomeException, try)
import           Foreign           (ptrToIntPtr)
import qualified Graphics.Win32    as Win32
import           TEAWin32.GUI      (ScalableValue (..))

try_ :: IO a -> IO (Either SomeException a)
try_ = try

hwndToInt :: Win32.HWND -> Int
hwndToInt hwnd = fromIntegral (ptrToIntPtr hwnd)

applyScaleFactor :: ScalableValue -> Double -> Int
applyScaleFactor (RawValue x) _ = truncate (x + 0.5)
applyScaleFactor (ScalableValue x) scaleFactor =
    truncate ((x * scaleFactor) + 0.5)
