module TEAWin32.Internal
    ( ApplicationErrorLocation (..)
    , throwTEAWin32ApplicationError
    , throwTEAWin32InternalError
    , try_
    ) where

import           Control.Exception (SomeException, try)
import           Data.Bits         ((.|.))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           GHC.IO            (unsafePerformIO)
import           GHC.Stack         (HasCallStack, callStack, prettyCallStack)
import qualified Graphics.Win32    as Win32

data ApplicationErrorLocation = Init
                              | Update
                              deriving Show

throwTEAWin32ApplicationError :: HasCallStack => ApplicationErrorLocation -> Text -> a
throwTEAWin32ApplicationError errLoc errorMsg = unsafePerformIO $ do
    let fullErrorMsg =
            "An unhandled application error occurred during " <> show errLoc <> ".\n\n" <> Text.unpack errorMsg

        msgBoxFlags =
            Win32.mB_OK .|. Win32.mB_SETFOREGROUND .|. Win32.mB_TASKMODAL .|. Win32.mB_ICONSTOP

    _ <- Win32.messageBox Nothing fullErrorMsg "Application Error" msgBoxFlags

    error fullErrorMsg

throwTEAWin32InternalError :: HasCallStack => Text -> a
throwTEAWin32InternalError errorMsg = unsafePerformIO $ do
    let fullErrorMsg =
            "An internal TEAWin32 error occurred.\n\n" <> Text.unpack errorMsg <> "\n\n" <> prettyCallStack callStack

        msgBoxFlags =
            Win32.mB_OK .|. Win32.mB_SETFOREGROUND .|. Win32.mB_TASKMODAL .|. Win32.mB_ICONSTOP

    _ <- Win32.messageBox Nothing fullErrorMsg "TEAWin32" msgBoxFlags

    error fullErrorMsg

try_ :: IO a -> IO (Either SomeException a)
try_ = try
