module TEAWin32.Drawing
    ( Colour (..)
    , toWin32Colour
    ) where

import qualified Graphics.Win32 as Win32

data Colour = RGB Int Int Int deriving (Show, Eq)

toWin32Colour :: Colour -> Win32.COLORREF
toWin32Colour (RGB r g b) = Win32.rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)
