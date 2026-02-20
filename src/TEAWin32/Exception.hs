module TEAWin32.Exception (InternalTEAWin32Exception (..)) where

import           Control.Exception (Exception)
import           Data.Text         (Text)

newtype InternalTEAWin32Exception = InternalTEAWin32Exception Text deriving Show

instance Exception InternalTEAWin32Exception
