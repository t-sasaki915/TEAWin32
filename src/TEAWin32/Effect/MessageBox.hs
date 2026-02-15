module TEAWin32.Effect.MessageBox
    ( MessageBoxSettings (..)
    , MessageBoxButtons (..)
    , MessageBoxIcon (..)
    , MessageBoxDefaultButton (..)
    , MessageBoxResult (..)
    , showMessageBox
    , defaultMessageBoxSettings
    , toWin32MessageBoxButtons
    , toWin32MessageBoxIcon
    , toWin32MessageBoxDefaultButton
    , fromWin32MessageBoxResult
    ) where

import           Data.Bits      ((.|.))
import           Data.Maybe     (fromMaybe)
import           Data.Text      (Text)
import qualified Data.Text      as Text
import qualified Graphics.Win32 as Win32

showMessageBox :: MessageBoxSettings -> IO MessageBoxResult
showMessageBox (MessageBoxSettings msgBoxTitle msgBoxContent msgBoxBtns msgBoxIcon msgBoxDefBtn) =
    let msgBoxBtns' = toWin32MessageBoxButtons msgBoxBtns
        msgBoxIcon' = toWin32MessageBoxIcon msgBoxIcon
        msgBoxDefBtn' = toWin32MessageBoxDefaultButton (fromMaybe MessageBoxButton1 msgBoxDefBtn)
        msgBoxStyle = msgBoxBtns' .|. msgBoxIcon' .|. msgBoxDefBtn' in
            fromWin32MessageBoxResult <$>
                Win32.messageBox Nothing (Text.unpack msgBoxContent) (Text.unpack msgBoxTitle) msgBoxStyle

data MessageBoxSettings = MessageBoxSettings
    { messageBoxTitle         :: Text
    , messageBoxContent       :: Text
    , messageBoxButtons       :: MessageBoxButtons
    , messageBoxIcon          :: MessageBoxIcon
    , messageBoxDefaultButton :: Maybe MessageBoxDefaultButton
    } deriving (Show, Eq)

defaultMessageBoxSettings :: MessageBoxSettings
defaultMessageBoxSettings = MessageBoxSettings
    { messageBoxTitle         = ""
    , messageBoxContent       = ""
    , messageBoxButtons       = MessageBoxButtonsOK
    , messageBoxIcon          = MessageBoxIconInformation
    , messageBoxDefaultButton = Nothing
    }

data MessageBoxButtons = MessageBoxButtonsAbortRetryIgnore
                       | MessageBoxButtonsCancelTryAgainContinue
                       | MessageBoxButtonsHelp
                       | MessageBoxButtonsOK
                       | MessageBoxButtonsOKCancel
                       | MessageBoxButtonsRetryCancel
                       | MessageBoxButtonsYesNo
                       | MessageBoxButtonsYesNoCancel
                       deriving (Show, Eq)

toWin32MessageBoxButtons :: MessageBoxButtons -> Win32.MBStyle
toWin32MessageBoxButtons MessageBoxButtonsAbortRetryIgnore       = 0x00000002
toWin32MessageBoxButtons MessageBoxButtonsCancelTryAgainContinue = 0x00000006
toWin32MessageBoxButtons MessageBoxButtonsHelp                   = 0x00004000
toWin32MessageBoxButtons MessageBoxButtonsOK                     = 0x00000000
toWin32MessageBoxButtons MessageBoxButtonsOKCancel               = 0x00000001
toWin32MessageBoxButtons MessageBoxButtonsRetryCancel            = 0x00000005
toWin32MessageBoxButtons MessageBoxButtonsYesNo                  = 0x00000004
toWin32MessageBoxButtons MessageBoxButtonsYesNoCancel            = 0x00000003

data MessageBoxIcon = MessageBoxIconExclamation
                    | MessageBoxIconWarning
                    | MessageBoxIconInformation
                    | MessageBoxIconAsterisk
                    | MessageBoxIconQuestion
                    | MessageBoxIconStop
                    | MessageBoxIconError
                    | MessageBoxIconHand
                    deriving (Show, Eq)

toWin32MessageBoxIcon :: MessageBoxIcon -> Win32.MBStyle
toWin32MessageBoxIcon MessageBoxIconExclamation = 0x00000030
toWin32MessageBoxIcon MessageBoxIconWarning     = 0x00000030
toWin32MessageBoxIcon MessageBoxIconInformation = 0x00000040
toWin32MessageBoxIcon MessageBoxIconAsterisk    = 0x00000040
toWin32MessageBoxIcon MessageBoxIconQuestion    = 0x00000020
toWin32MessageBoxIcon MessageBoxIconStop        = 0x00000010
toWin32MessageBoxIcon MessageBoxIconError       = 0x00000010
toWin32MessageBoxIcon MessageBoxIconHand        = 0x00000010

data MessageBoxDefaultButton = MessageBoxButton1
                             | MessageBoxButton2
                             | MessageBoxButton3
                             | MessageBoxButton4
                             deriving (Show, Eq)

toWin32MessageBoxDefaultButton :: MessageBoxDefaultButton -> Win32.MBStyle
toWin32MessageBoxDefaultButton MessageBoxButton1 = 0x00000000
toWin32MessageBoxDefaultButton MessageBoxButton2 = 0x00000100
toWin32MessageBoxDefaultButton MessageBoxButton3 = 0x00000200
toWin32MessageBoxDefaultButton MessageBoxButton4 = 0x00000300

data MessageBoxResult = MessageBoxResultAbort
                      | MessageBoxResultCancel
                      | MessageBoxResultContinue
                      | MessageBoxResultIgnore
                      | MessageBoxResultNo
                      | MessageBoxResultOK
                      | MessageBoxResultRetry
                      | MessageBoxResultTryAgain
                      | MessageBoxResultYes
                      deriving (Show, Eq)

fromWin32MessageBoxResult :: Win32.MBStatus -> MessageBoxResult
fromWin32MessageBoxResult 3  = MessageBoxResultAbort
fromWin32MessageBoxResult 2  = MessageBoxResultCancel
fromWin32MessageBoxResult 11 = MessageBoxResultContinue
fromWin32MessageBoxResult 5  = MessageBoxResultIgnore
fromWin32MessageBoxResult 7  = MessageBoxResultNo
fromWin32MessageBoxResult 1  = MessageBoxResultOK
fromWin32MessageBoxResult 4  = MessageBoxResultRetry
fromWin32MessageBoxResult 10 = MessageBoxResultTryAgain
fromWin32MessageBoxResult 6  = MessageBoxResultYes
fromWin32MessageBoxResult _  = error "Unknown MessageBox MBStatus."
