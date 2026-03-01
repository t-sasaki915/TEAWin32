module TEAWin32.Core.Native
    ( SHSTOCKICONID
    , WORD
    , LPCWSTR
    , withCWText
    , c_MakeIntResourceW
    ) where

import           Data.Text         (Text)
import qualified Data.Text         as Text
import qualified Data.Text.Foreign as TForeign
import           Foreign           (Ptr, Word16, Word32, allocaArray, castPtr,
                                    intPtrToPtr, pokeElemOff)
import           Foreign.C         (CWString, CWchar)

type SHSTOCKICONID = Word32
type WORD = Word16
type LPCWSTR = Ptr CWchar

withCWText :: Text -> (CWString -> IO a) -> IO a
withCWText text func =
    let len = Text.length text in
        allocaArray (len + 1) $ \ptr -> do
            TForeign.unsafeCopyToPtr text ptr
            pokeElemOff ptr len 0

            func (castPtr ptr)

c_MakeIntResourceW :: WORD -> LPCWSTR
c_MakeIntResourceW = intPtrToPtr . fromIntegral
