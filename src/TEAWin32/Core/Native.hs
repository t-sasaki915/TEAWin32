module TEAWin32.Core.Native
    ( withCWText
    , assumeNotNULL
    , makeEventEnqueuerFunPtr
    , c_MakeIntResourceW
    , c_InitialiseTEAWin32C
    , c_FinaliseTEAWin32C
    , c_RequestRender
    , c_StartWin32MessageLoop
    , c_StartErrorReporter
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Foreign                (FunPtr, Ptr, intPtrToPtr, nullPtr)
import           Foreign.C              (CWString, withCWString)
import           TEAWin32.Core.Types

withCWText :: Text -> (CWString -> IO a) -> IO a
withCWText text = withCWString (Text.unpack text)

assumeNotNULL :: MonadIO m => Ptr a -> m () -> m ()
assumeNotNULL ptr func =
    if ptr /= nullPtr
        then func
        else liftIO $ putStrLn "!?" -- TODO

c_MakeIntResourceW :: WORD -> LPCWSTR
c_MakeIntResourceW = intPtrToPtr . fromIntegral

foreign import ccall "wrapper"
    makeEventEnqueuerFunPtr :: EventEnqueuer -> IO (FunPtr EventEnqueuer)

foreign import ccall "InitialiseTEAWin32C"
    c_InitialiseTEAWin32C :: Ptr TEAWin32Settings -> FunPtr EventEnqueuer -> IO Bool

foreign import ccall "FinaliseTEAWin32C"
    c_FinaliseTEAWin32C :: IO ()

foreign import ccall unsafe "RequestRender"
    c_RequestRender :: Ptr () -> Int -> IO ()

foreign import ccall "StartWin32MessageLoop"
    c_StartWin32MessageLoop :: IO ()

foreign import ccall "StartErrorReporter"
    c_StartErrorReporter :: IO ()
