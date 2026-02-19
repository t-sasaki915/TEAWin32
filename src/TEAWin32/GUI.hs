{-# LANGUAGE InstanceSigs #-}
module TEAWin32.GUI
    ( UniqueId (..)
    , WindowStyle (..)
    , Icon (..)
    , Cursor (..)
    , Font (..)
    , ScalableValue (..)
    , CanBeRawValue (..)
    , fontCacheRef
    , toWin32WindowStyle
    , toWin32Icon
    , toWin32Cursor
    , withVisualStyles
    ) where

import                          Control.Concurrent        (modifyMVar, readMVar)
import                          Control.Monad             (unless, void)
import                          Data.Bits                 ((.|.))
import                          Data.Map                  ((!))
import                qualified Data.Map                  as Map
import                          Data.Text                 (Text)
import                          Foreign                   (Storable (poke, sizeOf),
                                                           alloca)
import                          Foreign.C                 (withCWString)
import                qualified Graphics.Win32            as Win32
import                qualified System.Win32              as Win32
import {-# SOURCE #-}           TEAWin32.GUI.Internal     (cursorCacheRef,
                                                           fontCacheRef,
                                                           iconCacheRef)
import                qualified TEAWin32.Internal.Foreign as Win32

data UniqueId = UserUniqueId  Text
              | SystemUniqueId Int
              deriving (Show, Eq, Ord)

data WindowStyle = WindowStyleBorderless
                 | WindowStyleNormal
                 | WindowStyleBorderlessChild
                 | WindowStyleNormalChild
                 deriving (Show, Eq, Ord)

toWin32WindowStyle :: WindowStyle -> Win32.WindowStyle
toWin32WindowStyle WindowStyleBorderless =
    Win32.wS_POPUP .|. Win32.wS_CLIPCHILDREN

toWin32WindowStyle WindowStyleNormal =
    Win32.wS_OVERLAPPEDWINDOW .|. Win32.wS_CLIPCHILDREN

toWin32WindowStyle WindowStyleBorderlessChild =
    Win32.wS_CHILD .|. Win32.wS_CLIPSIBLINGS .|. Win32.wS_CLIPCHILDREN

toWin32WindowStyle WindowStyleNormalChild =
    Win32.wS_OVERLAPPEDWINDOW .|. Win32.wS_CHILD .|. Win32.wS_TABSTOP .|. Win32.wS_CLIPSIBLINGS .|. Win32.wS_CLIPCHILDREN

data Icon = IconApplication
          | IconHand
          | IconQuestion
          | IconExclamation
          | IconAsterisk
          | IconFromResource Int
          deriving (Show, Eq, Ord)

toWin32Icon :: Icon -> IO Win32.HANDLE
toWin32Icon icon@(IconFromResource resourceId) =
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

data Cursor = CursorArrow
            | CursorIBeam
            | CursorWait
            | CursorCross
            | CursorUparrow
            | CursorSizeNWSE
            | CursorSizeNESW
            | CursorSizeWE
            | CursorSizeNS
            deriving (Show, Eq, Ord)

toWin32Cursor :: Cursor -> IO Win32.HANDLE
toWin32Cursor cursor =
    readMVar cursorCacheRef >>= \cursorCache ->
        pure (cursorCache ! cursor)

data Font = DefaultGUIFont
          | SystemFont
          | Font Text ScalableValue
          deriving (Show, Eq, Ord)

data ScalableValue = RawValue      Double
                   | ScalableValue Double
                   deriving (Show, Eq, Ord)

class CanBeRawValue a where
    raw :: a -> ScalableValue

instance CanBeRawValue Integer where
    raw = RawValue . fromIntegral

instance CanBeRawValue Int where
    raw = RawValue . fromIntegral

instance CanBeRawValue Double where
    raw = RawValue

instance Num ScalableValue where
    fromInteger :: Integer -> ScalableValue
    fromInteger a = ScalableValue (fromInteger a)

    (ScalableValue a) + (ScalableValue b) = ScalableValue (a + b)
    (RawValue a)      + (RawValue b)      = RawValue      (a + b)
    (ScalableValue a) + (RawValue b)      = ScalableValue (a + b)
    (RawValue a)      + (ScalableValue b) = RawValue      (a + b)

    (ScalableValue a) * (ScalableValue b) = ScalableValue (a * b)
    (RawValue a)      * (RawValue b)      = RawValue      (a * b)
    (ScalableValue a) * (RawValue b)      = ScalableValue (a * b)
    (RawValue a)      * (ScalableValue b) = RawValue      (a * b)

    abs (ScalableValue a) = ScalableValue (abs a)
    abs (RawValue a)      = RawValue      (abs a)

    signum (ScalableValue a) = ScalableValue (signum a)
    signum (RawValue a)      = RawValue      (signum a)

    negate (ScalableValue a) = ScalableValue (negate a)
    negate (RawValue a)      = RawValue      (negate a)

instance Fractional ScalableValue where
    fromRational a = ScalableValue (fromRational a)

    (ScalableValue a) / (ScalableValue b) = ScalableValue (a / b)
    (RawValue a)      / (RawValue b)      = RawValue      (a / b)
    (ScalableValue a) / (RawValue b)      = ScalableValue (a / b)
    (RawValue a)      / (ScalableValue b) = RawValue      (a / b)

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
