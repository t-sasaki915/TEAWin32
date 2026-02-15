module TEAWin32.Effect (showMessageBox) where

import           Data.Bits                  ((.|.))
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as Text
import qualified Graphics.Win32             as Win32
import           TEAWin32.Effect.MessageBox

showMessageBox :: MessageBoxSettings -> IO MessageBoxResult
showMessageBox (MessageBoxSettings msgBoxTitle msgBoxContent msgBoxBtns msgBoxIcon msgBoxDefBtn) =
    let msgBoxBtns' = toWin32MessageBoxButtons msgBoxBtns
        msgBoxIcon' = toWin32MessageBoxIcon msgBoxIcon
        msgBoxDefBtn' = toWin32MessageBoxDefaultButton (fromMaybe MessageBoxButton1 msgBoxDefBtn)
        msgBoxStyle = msgBoxBtns' .|. msgBoxIcon' .|. msgBoxDefBtn' in
            fromWin32MessageBoxResult <$>
                Win32.messageBox Nothing (Text.unpack msgBoxContent) (Text.unpack msgBoxTitle) msgBoxStyle
