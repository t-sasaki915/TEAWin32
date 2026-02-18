module TEAWin32.GUI
    ( UniqueId (..)
    , WindowStyle (..)
    , Icon (..)
    , Cursor (..)
    , Font (..)
    , toWin32WindowStyle
    , toWin32Icon
    , toWin32Cursor
    , toWin32Font
    , withVisualStyles
    ) where

import                          Control.Concurrent        (modifyMVar, readMVar)
import                          Control.Monad             (unless, void)
import                          Data.Bits                 ((.|.))
import                          Data.Map                  ((!))
import                qualified Data.Map                  as Map
import                          Data.Text                 (Text)
import                qualified Data.Text                 as Text
import                          Foreign                   (Storable (poke, sizeOf),
                                                           alloca)
import                          Foreign.C                 (withCWString)
import                qualified Graphics.Win32            as Win32
import                qualified System.Win32              as Win32
import {-# SOURCE #-}           TEAWin32.GUI.Internal     (cursorCacheRef,
                                                           fontCacheRef,
                                                           iconCacheRef)
import                qualified TEAWin32.Internal.Foreign as Win32

newtype UniqueId = UniqueId Text deriving (Show, Eq, Ord)

data WindowStyle = Borderless
                 | Normal
                 | BorderlessChild
                 | NormalChild
                 deriving (Show, Eq, Ord)

toWin32WindowStyle :: WindowStyle -> Win32.WindowStyle
toWin32WindowStyle Borderless      = Win32.wS_POPUP
toWin32WindowStyle Normal          = Win32.wS_OVERLAPPEDWINDOW
toWin32WindowStyle BorderlessChild = Win32.wS_CHILD
toWin32WindowStyle NormalChild     = Win32.wS_OVERLAPPEDWINDOW .|. Win32.wS_CHILD .|. Win32.wS_TABSTOP

data Icon = Application
          | Hand
          | Question
          | Exclamation
          | Asterisk
          | FromResource Int
          deriving (Show, Eq, Ord)

toWin32Icon :: Icon -> IO Win32.HANDLE
toWin32Icon icon@(FromResource resourceId) =
    modifyMVar iconCacheRef $ \iconCache ->
        case Map.lookup icon iconCache of
            Just hndl -> pure (iconCache, hndl)
            Nothing ->
                Win32.getModuleHandle Nothing >>= \hInstance ->
                    Win32.loadIcon (Just hInstance) (Win32.makeIntResource resourceId) >>= \iconHandle ->
                        pure (Map.insert icon iconHandle iconCache, iconHandle)

toWin32Icon icon =
    readMVar iconCacheRef >>= \iconCache ->
        pure (iconCache ! icon)

data Cursor = Arrow
            | IBeam
            | Wait
            | Cross
            | Uparrow
            | SizeNWSE
            | SizeNESW
            | SizeWE
            | SizeNS
            deriving (Show, Eq, Ord)

toWin32Cursor :: Cursor -> IO Win32.HANDLE
toWin32Cursor cursor =
    readMVar cursorCacheRef >>= \cursorCache ->
        pure (cursorCache ! cursor)

data Font = DefaultGUIFont
          | SystemFont
          | Font Text Int
          deriving (Show, Eq, Ord)

toWin32Font :: Font -> IO Win32.HFONT
toWin32Font DefaultGUIFont = Win32.getStockFont Win32.dEFAULT_GUI_FONT
toWin32Font SystemFont     = Win32.getStockFont Win32.sYSTEM_FONT
toWin32Font font@(Font fontName fontSize) =
    modifyMVar fontCacheRef $ \fontCache ->
        case Map.lookup font fontCache of
            Just hndl -> pure (fontCache, hndl)
            Nothing ->
                Win32.createFont (fromIntegral fontSize) 0 0 0 Win32.fW_NORMAL False False False Win32.dEFAULT_CHARSET
                    Win32.oUT_DEFAULT_PRECIS Win32.cLIP_DEFAULT_PRECIS Win32.dEFAULT_QUALITY
                        (Win32.fIXED_PITCH .|. Win32.fF_DONTCARE) (Text.unpack fontName) >>= \fontHandle ->
                            pure (Map.insert font fontHandle fontCache, fontHandle)

withVisualStyles :: IO a -> IO a
withVisualStyles action =
    alloca $ \ul ->
        alloca $ \actctxPtr -> do
            hInstance <- Win32.loadLibrary "SHLWAPI.DLL"
            szPath    <- Win32.getModuleFileName hInstance

            withCWString szPath $ \szPath' -> do
                let actctx = Win32.ACTCTX
                        { Win32.cbSize        = fromIntegral $ sizeOf (undefined :: Win32.ACTCTX)
                        , Win32.actctxDWFlags = 0x008
                        , Win32.lpResName     = Win32.makeIntResource 123
                        , Win32.lpSource      = szPath'
                        }

                poke actctxPtr actctx

                hActCtx <- Win32.c_CreateActCtx actctxPtr

                unless (hActCtx == Win32.iNVALID_HANDLE_VALUE) $
                    void $ Win32.c_ActivateActCtx hActCtx ul

                x <- action

                _ <- Win32.c_ReleaseActCtx hActCtx

                pure x
