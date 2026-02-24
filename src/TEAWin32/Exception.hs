module TEAWin32.Exception
    ( ErrorLocation (..)
    , TEAWin32Error (..)
    , errorTEAWin32
    ) where

import           Control.Monad.Cont        (ContT (..), evalContT)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Foreign.C                 (withCWString)
import           GHC.IO                    (unsafePerformIO)
import           GHC.Stack                 (HasCallStack, callStack,
                                            prettyCallStack)
import           System.Exit               (exitFailure)
import qualified TEAWin32.Internal.Foreign as Win32

data ErrorLocation = Init
                   | Update
                   | View
                   deriving Show

data TEAWin32Error = InternalTEAWin32Error Text
                   | TEAWin32ApplicationError ErrorLocation Text

errorTEAWin32 :: HasCallStack => TEAWin32Error -> a
errorTEAWin32 (InternalTEAWin32Error errorMsg) = reportTEAWin32Error
    "Internal Framework Error" "An internal TEAWin32 error has occurred." errorMsg

errorTEAWin32 (TEAWin32ApplicationError errLoc errorMsg) = reportTEAWin32Error
    "Application Error" ("An unhandled application error has occurred during " <> Text.show errLoc <> ".") errorMsg

reportTEAWin32Error :: HasCallStack => Text -> Text -> Text -> a
reportTEAWin32Error dialogTitle shortMsg specificMsg = unsafePerformIO $ do
    let specificMsgWithStacktrace = Text.replace "\n" "\r\n" (specificMsg <> "\n\n" <> Text.pack (prettyCallStack callStack))
        fullMsg                   = shortMsg <> "\r\n\r\n" <> specificMsgWithStacktrace

    putStrLn (Text.unpack fullMsg)

    evalContT $ do
        dialogTitle'               <- ContT $ withCWString (Text.unpack dialogTitle)
        shortMsg'                  <- ContT $ withCWString (Text.unpack shortMsg)
        specificMsgWithStacktrace' <- ContT $ withCWString (Text.unpack specificMsgWithStacktrace)
        fullMsg'                   <- ContT $ withCWString (Text.unpack fullMsg)

        liftIO $ Win32.c_ShowErrorReporter dialogTitle' shortMsg' specificMsgWithStacktrace' fullMsg'

    exitFailure
