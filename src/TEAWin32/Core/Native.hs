module TEAWin32.Core.Native
    ( withCWText
    , makeEventEnqueuerFunPtr
    , c_MakeIntResourceW
    , c_InitialiseTEAWin32C
    , c_FinaliseTEAWin32C
    , c_ShowErrorReporter
    , c_RequestRender
    , c_StartWin32MessageLoop
    ) where

import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Foreign             (FunPtr, Ptr, intPtrToPtr)
import           Foreign.C           (CWString, withCWString)
import           TEAWin32.Core.Types

withCWText :: Text -> (CWString -> IO a) -> IO a
withCWText text = withCWString (Text.unpack text)

c_MakeIntResourceW :: WORD -> LPCWSTR
c_MakeIntResourceW = intPtrToPtr . fromIntegral

foreign import ccall "wrapper"
    makeEventEnqueuerFunPtr :: EventEnqueuer -> IO (FunPtr EventEnqueuer)

foreign import ccall "InitialiseTEAWin32C"
    c_InitialiseTEAWin32C :: Ptr TEAWin32Settings -> FunPtr EventEnqueuer -> IO Bool

foreign import ccall "FinaliseTEAWin32C"
    c_FinaliseTEAWin32C :: IO ()

foreign import ccall "ShowErrorReporter"
    c_ShowErrorReporter :: CWString -> CWString -> CWString -> CWString -> IO ()

foreign import ccall unsafe "RequestRender"
    c_RequestRender :: Ptr () -> Int -> IO ()

foreign import ccall "StartWin32MessageLoop"
    c_StartWin32MessageLoop :: IO ()

