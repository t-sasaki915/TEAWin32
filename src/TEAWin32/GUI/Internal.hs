module TEAWin32.GUI.Internal
    ( DPIStrategy (..)
    , activeWindowCountRef
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
    , setProcessDPIAware
    , finaliseFontCache
    , initialiseDPIStrategy
    , getDPIFromHWND
    ) where

import                          Control.Concurrent        (MVar, modifyMVar_,
                                                           newMVar, takeMVar)
import                          Control.Monad             (filterM, void)
import                          Data.Either               (fromRight)
import                          Data.IORef                (IORef,
                                                           atomicModifyIORef',
                                                           modifyIORef,
                                                           newIORef, readIORef)
import                          Data.Map                  (Map)
import                qualified Data.Map                  as Map
import                          Foreign                   (castPtrToFunPtr,
                                                           freeHaskellFunPtr)
import                qualified Graphics.Win32            as Win32
import                          System.IO.Unsafe          (unsafePerformIO)
import                qualified System.Win32              as Win32
import                          TEAWin32.Exception        (TEAWin32Error (..),
                                                           errorTEAWin32, try_)
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

dpiStrategyRef :: IORef DPIStrategy
dpiStrategyRef = unsafePerformIO (newIORef (errorTEAWin32 (InternalTEAWin32Error "DPIStrategy is not initialised.")))
{-# NOINLINE dpiStrategyRef #-}

data DPIStrategy = ModernDPIStrategy Win32.GetDpiForWindow
                 | LegacyDPIStrategy

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

setProcessDPIAware :: IO ()
setProcessDPIAware = do
    shcore                    <- fromRight Win32.nullPtr <$> try_ (Win32.getModuleHandle (Just "shcore.dll"))
    setProcessDpiAwarenessPtr <- fromRight Win32.nullPtr <$> try_ (Win32.getProcAddress shcore "SetProcessDpiAwareness")

    if setProcessDpiAwarenessPtr == Win32.nullPtr
        then void Win32.c_SetProcessDPIAware
        else
            let setProcessDpiAwareness = Win32.makeSetProcessDpiAwareness (castPtrToFunPtr setProcessDpiAwarenessPtr) in
                void (setProcessDpiAwareness 2)

initialiseDPIStrategy :: IO ()
initialiseDPIStrategy = do
    user32             <- Win32.getModuleHandle (Just "user32.dll")
    getDpiForWindowPtr <- fromRight Win32.nullPtr <$> try_ (Win32.getProcAddress user32 "GetDpiForWindow")

    let dpiStrategy
            | getDpiForWindowPtr /= Win32.nullPtr = ModernDPIStrategy (Win32.makeGetDpiForWindow (castPtrToFunPtr getDpiForWindowPtr))
            | otherwise                           = LegacyDPIStrategy

    atomicModifyIORef' dpiStrategyRef (const (dpiStrategy, ()))

getDPIFromHWND :: Win32.HWND -> IO Int
getDPIFromHWND hwnd =
    readIORef dpiStrategyRef >>= \case
        (ModernDPIStrategy getDpiForWindow) ->
            fromIntegral <$> getDpiForWindow hwnd

        LegacyDPIStrategy -> do
            hdc <- Win32.getDC (Just hwnd)
            dpi <- Win32.c_GetDeviceCaps hdc 88
            Win32.releaseDC (Just hwnd) hdc
            pure dpi

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
    try_ (Win32.getParent hwnd) >>= \case
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
