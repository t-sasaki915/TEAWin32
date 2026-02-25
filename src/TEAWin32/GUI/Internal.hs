module TEAWin32.GUI.Internal
    ( activeWindowCountRef
    , getImmediateChildWindows
    , getTopLevelWindows
    , cursorCacheRef
    , resourceIconCacheRef
    , stockIconCacheRef
    , fontCacheRef
    , initialiseCursorCache
    , finaliseStockIconCache
    , finaliseFontCache
    , withVisualStyles
    ) where

import                          Control.Concurrent        (MVar, modifyMVar_,
                                                           newMVar, takeMVar)
import                          Control.Exception         (bracket)
import                          Data.IORef                (IORef, newIORef)
import                          Data.Map                  (Map)
import                qualified Data.Map                  as Map
import                          Foreign                   (allocaArray,
                                                           peekArray)
import                qualified Graphics.Win32            as Win32
import                          System.IO.Unsafe          (unsafePerformIO)
import {-# SOURCE #-}           TEAWin32.GUI              (Cursor (..), Font,
                                                           Icon (..))
import                qualified TEAWin32.Internal.Foreign as Win32
import                qualified TEAWin32.Internal.Native  as Native

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}

cursorCacheRef :: MVar (Map Cursor Win32.HCURSOR)
cursorCacheRef = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE cursorCacheRef #-}

resourceIconCacheRef :: MVar (Map Icon Win32.HICON)
resourceIconCacheRef = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE resourceIconCacheRef #-}

stockIconCacheRef :: MVar (Map Icon Win32.HICON)
stockIconCacheRef = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE stockIconCacheRef #-}

fontCacheRef :: MVar (Map Font Win32.HFONT)
fontCacheRef = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE fontCacheRef #-}

initialiseCursorCache :: IO ()
initialiseCursorCache = do
    buildInCursorCache <- Map.fromList <$>
        mapM (\(a, b) -> Win32.loadCursor Nothing b >>= \b' -> pure (a, b'))
            [ (CursorArrow   , Win32.iDC_ARROW   )
            , (CursorIBeam   , Win32.iDC_IBEAM   )
            , (CursorWait    , Win32.iDC_WAIT    )
            , (CursorCross   , Win32.iDC_CROSS   )
            , (CursorUparrow , Win32.iDC_UPARROW )
            , (CursorSizeNWSE, Win32.iDC_SIZENWSE)
            , (CursorSizeNESW, Win32.iDC_SIZENESW)
            , (CursorSizeWE  , Win32.iDC_SIZEWE  )
            , (CursorSizeNS  , Win32.iDC_SIZENS  )
            ]

    modifyMVar_ cursorCacheRef (const $ pure buildInCursorCache)

finaliseStockIconCache :: IO ()
finaliseStockIconCache =
    takeMVar stockIconCacheRef >>=
        mapM_ Win32.c_DestroyIcon . Map.elems

finaliseFontCache :: IO ()
finaliseFontCache =
    takeMVar fontCacheRef >>=
        mapM_ Win32.c_DeleteObject . Map.elems

getImmediateChildWindows :: Win32.HWND -> IO [Win32.HWND]
getImmediateChildWindows parent =
    let maxWindows = 1024 in
        allocaArray maxWindows $ \arrayPtr ->
            Native.c_GetImmediateChildWindows parent arrayPtr maxWindows >>= \hwndCount ->
                peekArray hwndCount arrayPtr

getTopLevelWindows :: IO [Win32.HWND]
getTopLevelWindows =
    let maxWindows = 1024 in
        allocaArray maxWindows $ \arrayPtr ->
            Native.c_GetTopLevelWindows arrayPtr maxWindows >>= \hwndCount ->
                peekArray hwndCount arrayPtr

withVisualStyles :: IO a -> IO a
withVisualStyles action =
    bracket Native.c_EnableVisualStyles
            Win32.c_ReleaseActCtx
            (const action)
