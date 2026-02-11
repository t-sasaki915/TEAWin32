module Graphics.GUI.Internal
    ( activeWindowCountRef
    , withChildWindows
    , whoseParentIs
    , isTopLevelWindow
    , withTopLevelWindows
    , withImmediateChildWindows
    , cursorCacheRef
    , initialiseCursorCache
    ) where

import                          Control.Exception    (SomeException, try)
import                          Control.Monad        (filterM)
import                          Data.Bimap           (Bimap)
import                qualified Data.Bimap           as Bimap
import                          Data.IORef           (IORef, atomicModifyIORef',
                                                      modifyIORef, newIORef,
                                                      readIORef)
import                          Foreign              (freeHaskellFunPtr)
import {-# SOURCE #-}           Graphics.GUI         (Cursor (..))
import                qualified Graphics.GUI.Foreign as Win32
import                qualified Graphics.Win32       as Win32
import                          System.IO.Unsafe     (unsafePerformIO)
import                qualified System.Win32         as Win32

activeWindowCountRef :: IORef Int
activeWindowCountRef = unsafePerformIO (newIORef 0)
{-# NOINLINE activeWindowCountRef #-}

cursorCacheRef :: IORef (Bimap Cursor Win32.HANDLE)
cursorCacheRef = unsafePerformIO (newIORef $ Bimap.fromList [])
{-# NOINLINE cursorCacheRef #-}

initialiseCursorCache :: IO ()
initialiseCursorCache = do
    buildInCursorCache <- Bimap.fromList <$>
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

    _ <- atomicModifyIORef' cursorCacheRef (const (buildInCursorCache, buildInCursorCache))

    pure ()

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
