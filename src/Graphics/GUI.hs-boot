module Graphics.GUI (Cursor (..)) where

data Cursor = Arrow
            | IBeam
            | Wait
            | Cross
            | Uparrow
            | SizeNWSE
            | SizeNESW
            | SizeWE
            | SizeNS

instance Ord Cursor
