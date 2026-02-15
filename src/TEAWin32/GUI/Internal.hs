module TEAWin32.GUI.Internal
    ( activeWindowCountRef
    , withChildWindows
    , whoseParentIs
    , isTopLevelWindow
    , withTopLevelWindows
    , withImmediateChildWindows
    , cursorCacheRef
    , iconCacheRef
    , fontCacheRef
    , initialiseCursorCache
    , initialiseIconCache
    , finaliseFontCache
    ) where

import                          Control.Concurrent (MVar, modifyMVar_, newMVar,
                                                    takeMVar)
import                          Control.Exception  (SomeException, try)
import                          Control.Monad      (filterM)
import                          Data.IORef         (IORef, atomicModifyIORef',
                                                    modifyIORef, newIORef,
                                                    readIORef)
import                          Data.Map           (Map)
import                qualified Data.Map           as Map
import                          Foreign            (freeHaskellFunPtr)
import                qualified Graphics.Win32     as Win32
import                          System.IO.Unsafe   (unsafePerformIO)
import                qualified System.Win32       as Win32
import                qualified TEAWin32.Foreign   as Win32
import {-# SOURCE #-}           TEAWin32.GUI       (Cursor (..), Font,
                                                    Icon (..))

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
            [ (Arrow   , Win32.iDC_ARROW   )
            , (IBeam   , Win32.iDC_IBEAM   )
            , (Wait    , Win32.iDC_WAIT    )
            , (Cross   , Win32.iDC_CROSS   )
            , (Uparrow , Win32.iDC_UPARROW )
            , (SizeNWSE, Win32.iDC_SIZENWSE)
            , (SizeNESW, Win32.iDC_SIZENESW)
            , (SizeWE  , Win32.iDC_SIZEWE  )
            , (SizeNS  , Win32.iDC_SIZENS  )
            ]

    modifyMVar_ cursorCacheRef (const $ pure buildInCursorCache)

initialiseIconCache :: IO ()
initialiseIconCache = do
    builtInIconCache <- Map.fromList <$>
        mapM (\(a, b) -> Win32.loadIcon Nothing b >>= \b' -> pure (a, b'))
            [ (Application, Win32.iDI_APPLICATION)
            , (Hand,        Win32.iDI_HAND       )
            , (Question,    Win32.iDI_QUESTION   )
            , (Exclamation, Win32.iDI_EXCLAMATION)
            , (Asterisk,    Win32.iDI_ASTERISK   )
            ]

    modifyMVar_ iconCacheRef (const $ pure builtInIconCache)

finaliseFontCache :: IO ()
finaliseFontCache =
    takeMVar fontCacheRef >>=
        mapM_ Win32.c_DeleteObject . Map.elems

withChildWindows :: Win32.HWND -> ([Win32.HWND] -> IO a) -> IO a
withChildWindows targetHWND func = do
    childrenRef <- newIORef []

    let callback hwnd _ =
            atomicModifyIORef' childrenRef (\x -> (hwnd : x, ())) >>
                pure True

    enumPtr <- Win32.makeEnumWindowProc callback
    _       <- Win32.c_EnumChildWindows targetHWND enumPtr 0

    children <- readIORef childrenRef
    x        <- func children

    freeHaskellFunPtr enumPtr

    pure x

withImmediateChildWindows :: Win32.HWND -> ([Win32.HWND] -> IO a) -> IO a
withImmediateChildWindows targetHWND func =
    withChildWindows targetHWND $ \children ->
        filterM (`whoseParentIs` targetHWND) children >>=
            func

isTopLevelWindow :: Win32.HWND -> IO Bool
isTopLevelWindow = (`whoseParentIs` Win32.nullPtr)

whoseParentIs :: Win32.HWND -> Win32.HWND -> IO Bool
whoseParentIs hwnd parent =
    (try (Win32.getParent hwnd) :: IO (Either SomeException Win32.HWND)) >>= \case
        Left _        -> pure (parent == Win32.nullPtr)
        Right parent' -> pure (parent == parent')

withTopLevelWindows :: ([Win32.HWND] -> IO a) -> IO a
withTopLevelWindows func = do
    threadId <- Win32.getCurrentThreadId
    hwndsRef <- newIORef []

    let callback hwnd _ =
            modifyIORef hwndsRef (hwnd :) >>
                pure True

    enumPtr <- Win32.makeEnumWindowProc callback
    _       <- Win32.c_EnumThreadWindows threadId enumPtr 0

    windows       <- readIORef hwndsRef
    parentWindows <- filterM isTopLevelWindow windows
    x             <- func parentWindows

    freeHaskellFunPtr enumPtr

    pure x
