module TEAWin32.GUI (Icon (..), Cursor (..), Font (..)) where

import           Data.Text (Text)

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

data Font = DefaultGUIFont
          | SystemFont
          | Font Text Int

instance Ord Font
