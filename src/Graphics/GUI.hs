module Graphics.GUI
    ( UniqueId (..)
    , WindowStyle (..)
    , Icon (..)
    , Cursor (..)
    , Brush (..)
    , toWin32WindowStyle
    , fromWin32WindowStyle
    , toWin32Icon
    , toWin32Cursor
    , fromWin32Cursor
    , toWin32Brush
    ) where

import                          Data.Bimap            ((!), (!>))
import                          Data.Bits             ((.&.), (.|.))
import                          Data.IORef            (readIORef)
import                          Data.Text             (Text)
import                qualified Graphics.GUI.Foreign  as Win32
import {-# SOURCE #-}           Graphics.GUI.Internal (cursorCacheRef)
import                qualified Graphics.Win32        as Win32
import                qualified System.Win32          as Win32

newtype UniqueId = UniqueId Text deriving (Show, Eq)

instance Ord UniqueId where
    compare (UniqueId x) (UniqueId y) = compare x y

data WindowStyle = Borderless
                 | Normal
                 | BorderlessChild
                 | NormalChild
                 deriving (Show, Eq)

toWin32WindowStyle :: WindowStyle -> Win32.WindowStyle
toWin32WindowStyle Borderless      = Win32.wS_POPUP
toWin32WindowStyle Normal          = Win32.wS_OVERLAPPEDWINDOW
toWin32WindowStyle BorderlessChild = Win32.wS_CHILD
toWin32WindowStyle NormalChild     = Win32.wS_OVERLAPPEDWINDOW .|. Win32.wS_CHILD

fromWin32WindowStyle :: Win32.WindowStyle -> WindowStyle
fromWin32WindowStyle windowStyle
    | windowStyle `hasAll` (Win32.wS_OVERLAPPEDWINDOW .|. Win32.wS_CHILD) = NormalChild
    | windowStyle `hasAll` Win32.wS_POPUP                                 = Borderless
    | windowStyle `hasAll` Win32.wS_OVERLAPPEDWINDOW                      = Normal
    | windowStyle `hasAll` Win32.wS_CHILD                                 = BorderlessChild
    | otherwise                                                           = error "Unknown WindowStyle"
    where hasAll a b = (a .&. b) == b

data Icon = Application
          | Hand
          | Question
          | Exclamation
          | Asterisk
          | FromResource Int
          deriving (Show, Eq)

toWin32Icon :: Icon -> IO Win32.HICON
toWin32Icon Application      = Win32.loadIcon Nothing Win32.iDI_APPLICATION
toWin32Icon Hand             = Win32.loadIcon Nothing Win32.iDI_HAND
toWin32Icon Question         = Win32.loadIcon Nothing Win32.iDI_QUESTION
toWin32Icon Exclamation      = Win32.loadIcon Nothing Win32.iDI_EXCLAMATION
toWin32Icon Asterisk         = Win32.loadIcon Nothing Win32.iDI_ASTERISK
toWin32Icon (FromResource x) =
    Win32.getModuleHandle Nothing >>= \hInstance ->
        Win32.loadIcon (Just hInstance) (Win32.makeIntResource x)

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
    readIORef cursorCacheRef >>= \cursorCache ->
        pure (cursorCache ! cursor)

fromWin32Cursor :: Win32.HANDLE -> IO Cursor
fromWin32Cursor cursor =
    readIORef cursorCacheRef >>= \cursorCache ->
        pure (cursorCache !> cursor)

data Brush = SolidBrush Int Int Int deriving (Show, Eq)

toWin32Brush :: Brush -> IO Win32.HBRUSH
toWin32Brush (SolidBrush r g b) = Win32.createSolidBrush (Win32.rgb (fromIntegral r) (fromIntegral g) (fromIntegral b))
