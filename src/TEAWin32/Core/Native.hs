module TEAWin32.Core.Native
    ( withCWText
    , makeEventEnqueuerFunPtr
    , c_MakeIntResourceW
    , c_InitialiseTEAWin32C
    , c_FinaliseTEAWin32C
    ) where

import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Foreign   as TForeign
import           Foreign             (FunPtr, Ptr, allocaArray, castPtr,
                                      intPtrToPtr, pokeElemOff)
import           Foreign.C           (CWString)
import           TEAWin32.Core.Types

withCWText :: Text -> (CWString -> IO a) -> IO a
withCWText text func =
    let len = Text.length text in
        allocaArray (len + 1) $ \ptr -> do
            TForeign.unsafeCopyToPtr text ptr
            pokeElemOff ptr len 0

            func (castPtr ptr)

c_MakeIntResourceW :: WORD -> LPCWSTR
c_MakeIntResourceW = intPtrToPtr . fromIntegral

foreign import ccall "wrapper"
    makeEventEnqueuerFunPtr :: EventEnqueuer -> IO (FunPtr EventEnqueuer)

foreign import ccall "InitialiseTEAWin32C"
    c_InitialiseTEAWin32C :: Ptr TEAWin32Settings -> FunPtr EventEnqueuer -> IO ()

foreign import ccall "FinaliseTEAWin32C"
    c_FinaliseTEAWin32C :: IO ()
