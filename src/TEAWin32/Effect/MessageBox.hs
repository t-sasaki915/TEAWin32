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

import           Data.Bits                                ((.|.))
import           Data.Maybe                               (fromMaybe)
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           GHC.Stack                                (HasCallStack)
import qualified Graphics.Win32                           as Win32
import           TEAWin32.Exception                       (TEAWin32Error (..),
                                                           errorTEAWin32)
import           TEAWin32.GUI                             (UniqueId (..))
import           TEAWin32.GUI.Component.ComponentRegistry (getComponentHWNDFromUniqueIdRegistryMaybe)
import qualified TEAWin32.Internal.Foreign                as Win32

showMessageBox :: HasCallStack => MessageBoxSettings -> IO MessageBoxResult
showMessageBox settings = do
    let msgBoxBtns          = toWin32MessageBoxButtons       (messageBoxButtons settings)
        msgBoxIcon          = toWin32MessageBoxIcon          (messageBoxIcon settings)
        msgBoxDefBtn        = toWin32MessageBoxDefaultButton (fromMaybe MessageBoxButton1 (messageBoxDefaultButton settings))
        msgBoxTaskModal     = if messageBoxTaskModal settings     then Win32.mB_TASKMODAL     else 0x00000000
        msgBoxRightText     = if messageBoxRightText settings     then Win32.mB_RIGHT         else 0x00000000
        msgBoxSetForeground = if messageBoxSetForeground settings then Win32.mB_SETFOREGROUND else 0x00000000
        msgBoxTopMost       = if messageBoxTopMost settings       then Win32.mB_TOPMOST       else 0x00000000

        msgBoxStyle = foldr (.|.) 0
            [msgBoxBtns, msgBoxIcon, msgBoxDefBtn, msgBoxTaskModal, msgBoxRightText, msgBoxSetForeground, msgBoxTopMost]

    maybeOwner <- case messageBoxOwnerUniqueId settings of
        Just uniqueId -> getComponentHWNDFromUniqueIdRegistryMaybe (UserUniqueId uniqueId)
        Nothing       -> pure Nothing

    fromWin32MessageBoxResult <$>
        Win32.messageBox maybeOwner (Text.unpack (messageBoxContent settings)) (Text.unpack (messageBoxTitle settings)) msgBoxStyle

data MessageBoxSettings = MessageBoxSettings
    { messageBoxTitle         :: Text
    , messageBoxContent       :: Text
    , messageBoxButtons       :: MessageBoxButtons
    , messageBoxIcon          :: MessageBoxIcon
    , messageBoxDefaultButton :: Maybe MessageBoxDefaultButton
    , messageBoxTaskModal     :: Bool
    , messageBoxOwnerUniqueId :: Maybe Text
    , messageBoxRightText     :: Bool
    , messageBoxSetForeground :: Bool
    , messageBoxTopMost       :: Bool
    } deriving (Show, Eq)

defaultMessageBoxSettings :: MessageBoxSettings
defaultMessageBoxSettings = MessageBoxSettings
    { messageBoxTitle         = ""
    , messageBoxContent       = ""
    , messageBoxButtons       = MessageBoxButtonsOK
    , messageBoxIcon          = MessageBoxIconInformation
    , messageBoxDefaultButton = Nothing
    , messageBoxTaskModal     = True
    , messageBoxOwnerUniqueId = Nothing
    , messageBoxRightText     = False
    , messageBoxSetForeground = True
    , messageBoxTopMost       = False
    }

data MessageBoxButtons = MessageBoxButtonsAbortRetryIgnore
                       | MessageBoxButtonsCancelTryAgainContinue
                       | MessageBoxButtonsOK
                       | MessageBoxButtonsOKCancel
                       | MessageBoxButtonsRetryCancel
                       | MessageBoxButtonsYesNo
                       | MessageBoxButtonsYesNoCancel
                       deriving (Show, Eq)

toWin32MessageBoxButtons :: MessageBoxButtons -> Win32.MBStyle
toWin32MessageBoxButtons MessageBoxButtonsAbortRetryIgnore       = Win32.mB_ABORTRETRYIGNORE
toWin32MessageBoxButtons MessageBoxButtonsCancelTryAgainContinue = Win32.mB_CANCELTRYIGNORE
toWin32MessageBoxButtons MessageBoxButtonsOK                     = Win32.mB_OK
toWin32MessageBoxButtons MessageBoxButtonsOKCancel               = Win32.mB_OKCANCEL
toWin32MessageBoxButtons MessageBoxButtonsRetryCancel            = Win32.mB_RETRYCANCEL
toWin32MessageBoxButtons MessageBoxButtonsYesNo                  = Win32.mB_YESNO
toWin32MessageBoxButtons MessageBoxButtonsYesNoCancel            = Win32.mB_YESNOCANCEL

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
toWin32MessageBoxIcon MessageBoxIconExclamation = Win32.mB_ICONEXCLAMATION
toWin32MessageBoxIcon MessageBoxIconWarning     = Win32.mB_ICONWARNING
toWin32MessageBoxIcon MessageBoxIconInformation = Win32.mB_ICONINFORMATION
toWin32MessageBoxIcon MessageBoxIconAsterisk    = Win32.mB_ICONASTERISK
toWin32MessageBoxIcon MessageBoxIconQuestion    = Win32.mB_ICONQUESTION
toWin32MessageBoxIcon MessageBoxIconStop        = Win32.mB_ICONSTOP
toWin32MessageBoxIcon MessageBoxIconError       = Win32.mB_ICONERROR
toWin32MessageBoxIcon MessageBoxIconHand        = Win32.mB_ICONHAND

data MessageBoxDefaultButton = MessageBoxButton1
                             | MessageBoxButton2
                             | MessageBoxButton3
                             deriving (Show, Eq)

toWin32MessageBoxDefaultButton :: MessageBoxDefaultButton -> Win32.MBStyle
toWin32MessageBoxDefaultButton MessageBoxButton1 = Win32.mB_DEFBUTTON1
toWin32MessageBoxDefaultButton MessageBoxButton2 = Win32.mB_DEFBUTTON2
toWin32MessageBoxDefaultButton MessageBoxButton3 = Win32.mB_DEFBUTTON3

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

fromWin32MessageBoxResult :: HasCallStack => Win32.MBStatus -> MessageBoxResult
fromWin32MessageBoxResult x
    | x == Win32.iDABORT    = MessageBoxResultAbort
    | x == Win32.iDCANCEL   = MessageBoxResultCancel
    | x == Win32.iDCONTINUE = MessageBoxResultContinue
    | x == Win32.iDIGNORE   = MessageBoxResultIgnore
    | x == Win32.iDNO       = MessageBoxResultNo
    | x == Win32.iDOK       = MessageBoxResultOK
    | x == Win32.iDRETRY    = MessageBoxResultRetry
    | x == Win32.iDTRYAGAIN = MessageBoxResultTryAgain
    | x == Win32.iDYES      = MessageBoxResultYes
    | otherwise             = errorTEAWin32 (InternalTEAWin32Error "Unknown MessageBox MBStatus.")
