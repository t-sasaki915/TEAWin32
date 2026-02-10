module Graphics.GUI.Internal
    ( activeWindowCountRef
    , withChildWindows
    , isParentWindow
    , withParentWindows
    ) where

import           Control.Exception    (SomeException, try)
import           Control.Monad        (filterM)
import           Data.IORef           (IORef, atomicModifyIORef', modifyIORef,
                                       newIORef, readIORef)
import           Foreign              (freeHaskellFunPtr)
import qualified Graphics.GUI.Foreign as Win32
import qualified Graphics.Win32       as Win32
import           System.IO.Unsafe     (unsafePerformIO)
import qualified System.Win32         as Win32

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}

withChildWindows :: Win32.HWND -> ([Win32.HWND] -> IO a) -> IO a
withChildWindows targetHWND func = do
    childrenRef <- newIORef []

    let callback hwnd _ =
            atomicModifyIORef' childrenRef (\x -> (hwnd : x, hwnd : x)) >>
                pure True

    enumPtr <- Win32.makeEnumWindowProc callback
    _       <- Win32.c_EnumChildWindows targetHWND enumPtr 0

    children <- readIORef childrenRef
    x        <- func children

    freeHaskellFunPtr enumPtr

    pure x

isParentWindow :: Win32.HWND -> IO Bool
isParentWindow hwnd =
    (try (Win32.getParent hwnd) :: IO (Either SomeException Win32.HWND)) >>= \case
        Left _       -> pure True
        Right parent -> pure (parent == Win32.nullPtr)

withParentWindows :: ([Win32.HWND] -> IO a) -> IO a
withParentWindows func = do
    threadId <- Win32.getCurrentThreadId
    hwndsRef <- newIORef []

    let callback hwnd _ =
            modifyIORef hwndsRef (hwnd :) >>
                pure True

    enumPtr <- Win32.makeEnumWindowProc callback
    _       <- Win32.c_EnumThreadWindows threadId enumPtr 0

    windows       <- readIORef hwndsRef
    parentWindows <- filterM isParentWindow windows
    x             <- func parentWindows

    freeHaskellFunPtr enumPtr

    pure x
