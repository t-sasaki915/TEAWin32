module TEAWin32.GUI.Internal
    ( activeWindowCountRef
    , withVisualStyles
    ) where

import           Control.Exception         (bracket)
import           Data.IORef                (IORef, newIORef)
import           System.IO.Unsafe          (unsafePerformIO)
import qualified TEAWin32.Internal.Foreign as Win32
import qualified TEAWin32.Internal.Native  as Native

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}

withVisualStyles :: IO a -> IO a
withVisualStyles action =
    bracket Native.enableVisualStyles
            Win32.c_ReleaseActCtx
            (const action)
