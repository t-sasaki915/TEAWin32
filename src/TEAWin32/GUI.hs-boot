module TEAWin32.GUI (Icon (..), Cursor (..), Font (..)) where

import           Data.Text (Text)

data Icon = IconApplication
          | IconHand
          | IconQuestion
          | IconExclamation
          | IconAsterisk
          | IconFromResource Int

instance Ord Icon

data Cursor = CursorArrow
            | CursorIBeam
            | CursorWait
            | CursorCross
            | CursorUparrow
            | CursorSizeNWSE
            | CursorSizeNESW
            | CursorSizeWE
            | CursorSizeNS

instance Ord Cursor

data Font = DefaultGUIFont
          | SystemFont
          | Font Text ScalableValue

instance Ord Font

data ScalableValue = RawValue      Double
                   | ScalableValue Double
