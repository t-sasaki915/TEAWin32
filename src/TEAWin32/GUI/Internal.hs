module TEAWin32.GUI.Internal
    ( activeWindowCountRef
    , getImmediateChildWindows
    , getTopLevelWindows
    , cursorCacheRef
    , iconCacheRef
    , fontCacheRef
    , initialiseCursorCache
    , initialiseIconCache
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

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}

cursorCacheRef :: MVar (Map Cursor Win32.HANDLE)
cursorCacheRef = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE cursorCacheRef #-}

iconCacheRef :: MVar (Map Icon Win32.HANDLE)
iconCacheRef = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE iconCacheRef #-}

fontCacheRef :: MVar (Map Font Win32.HANDLE)
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

initialiseIconCache :: IO ()
initialiseIconCache = do
    builtInIconCache <- Map.fromList <$>
        mapM (\(a, b) -> Win32.loadIcon Nothing b >>= \b' -> pure (a, b'))
            [ (IconApplication, Win32.iDI_APPLICATION)
            , (IconHand,        Win32.iDI_HAND       )
            , (IconQuestion,    Win32.iDI_QUESTION   )
            , (IconExclamation, Win32.iDI_EXCLAMATION)
            , (IconAsterisk,    Win32.iDI_ASTERISK   )
            ]

    modifyMVar_ iconCacheRef (const $ pure builtInIconCache)

finaliseFontCache :: IO ()
finaliseFontCache =
    takeMVar fontCacheRef >>=
        mapM_ Win32.c_DeleteObject . Map.elems

getImmediateChildWindows :: Win32.HWND -> IO [Win32.HWND]
getImmediateChildWindows parent =
    let maxWindows = 1024 in
        allocaArray maxWindows $ \arrayPtr ->
            Win32.c_GetImmediateChildWindows parent arrayPtr maxWindows >>= \hwndCount ->
                peekArray hwndCount arrayPtr

getTopLevelWindows :: IO [Win32.HWND]
getTopLevelWindows =
    let maxWindows = 1024 in
        allocaArray maxWindows $ \arrayPtr ->
            Win32.c_GetTopLevelWindows arrayPtr maxWindows >>= \hwndCount ->
                peekArray hwndCount arrayPtr

withVisualStyles :: IO a -> IO a
withVisualStyles action =
    bracket Win32.c_EnableVisualStyles
            Win32.c_ReleaseActCtx
            (const action)
