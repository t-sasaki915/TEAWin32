module Graphics.GUI.Internal
    ( activeWindowCountRef
    , withChildWindows
    ) where

import           Data.IORef           (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import           Foreign              (freeHaskellFunPtr)
import qualified Graphics.GUI.Foreign as Win32
import qualified Graphics.Win32       as Win32
import           System.IO.Unsafe     (unsafePerformIO)

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}

withChildWindows :: Win32.HWND -> ([Win32.HWND] -> IO ()) -> IO ()
withChildWindows targetHWND func = do
    childrenRef <- newIORef []

    let callback hwnd _ =
            atomicModifyIORef' childrenRef (\x -> (hwnd : x, hwnd : x)) >>
                pure True

    enumPtr <- Win32.makeEnumWindowProc callback
    _ <- Win32.c_EnumChildWindows targetHWND enumPtr 0

    children <- readIORef childrenRef

    func children

    freeHaskellFunPtr enumPtr
