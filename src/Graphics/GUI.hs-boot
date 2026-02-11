module Graphics.GUI (Icon (..), Cursor (..)) where

data Icon = Application
          | Hand
          | Question
          | Exclamation
          | Asterisk
          | FromResource Int

instance Ord Icon

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
