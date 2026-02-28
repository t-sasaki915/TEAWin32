module TEAWin32.Internal.Native
    ( initialiseDPIAwareFunctions
    , getScaleFactorForHWND
    , enableVisualStyles
    , getImmediateChildWindows
    , getTopLevelWindows
    , isWindowTopLevel
    , getHighDPIIcon
    , showErrorReporter
    , createFontSimple
    , getResourceIcon
    , finaliseTEAWin32C
    ) where

import           Control.Monad.Cont        (ContT (..), evalContT)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Foreign                   (Ptr, allocaArray, peekArray)
import           Foreign.C                 (CWString, withCWString)
import qualified Graphics.Win32            as Win32
import qualified TEAWin32.Internal.Foreign as Win32

foreign import ccall unsafe "InitialiseDPIAwareFunctions"
    c_InitialiseDPIAwareFunctions :: IO ()

foreign import ccall unsafe "GetScaleFactorForHWND"
    c_GetScaleFactorForHWND :: Win32.HWND -> IO Double

foreign import ccall unsafe "EnableVisualStyles"
    c_EnableVisualStyles :: IO Win32.HANDLE

foreign import ccall unsafe "GetImmediateChildWindows"
    c_GetImmediateChildWindows :: Win32.HWND -> Ptr Win32.HWND -> Int -> IO Int

foreign import ccall unsafe "GetTopLevelWindows"
    c_GetTopLevelWindows :: Ptr Win32.HWND -> Int -> IO Int

foreign import ccall unsafe "IsWindowTopLevel"
    c_IsWindowTopLevel :: Win32.HWND -> IO Bool

foreign import ccall unsafe "GetHighDPIIcon"
    c_GetHighDPIIcon :: Win32.SHSTOCKICONID -> IO Win32.HICON

foreign import ccall "ShowErrorReporter"
    c_ShowErrorReporter :: CWString -> CWString -> CWString -> CWString -> IO ()

foreign import ccall unsafe "CreateFontSimple"
    c_CreateFontSimple :: Int -> CWString -> IO Win32.HFONT

foreign import ccall unsafe "GetResourceIcon"
    c_GetResourceIcon :: Int -> IO Win32.HICON

foreign import ccall unsafe "FinaliseTEAWin32C"
    c_FinaliseTEAWin32C :: IO ()

initialiseDPIAwareFunctions :: IO ()
initialiseDPIAwareFunctions = c_InitialiseDPIAwareFunctions

getScaleFactorForHWND :: Win32.HWND -> IO Double
getScaleFactorForHWND = c_GetScaleFactorForHWND

enableVisualStyles :: IO Win32.HANDLE
enableVisualStyles = c_EnableVisualStyles

getImmediateChildWindows :: Win32.HWND -> IO [Win32.HWND]
getImmediateChildWindows parent =
    let maxWindows = 1024 in
        allocaArray maxWindows $ \arrayPtr ->
            c_GetImmediateChildWindows parent arrayPtr maxWindows >>= \hwndCount ->
                peekArray hwndCount arrayPtr

getTopLevelWindows :: IO [Win32.HWND]
getTopLevelWindows =
    let maxWindows = 1024 in
        allocaArray maxWindows $ \arrayPtr ->
            c_GetTopLevelWindows arrayPtr maxWindows >>= \hwndCount ->
                peekArray hwndCount arrayPtr

isWindowTopLevel :: Win32.HWND -> IO Bool
isWindowTopLevel = c_IsWindowTopLevel

getHighDPIIcon :: Win32.SHSTOCKICONID -> IO Win32.HICON
getHighDPIIcon = c_GetHighDPIIcon

showErrorReporter :: Text -> Text -> Text -> Text -> IO ()
showErrorReporter dialogTitle shortMsg specificMsgWithStacktrace fullMsg =
    evalContT $ do
        dialogTitle'               <- ContT $ withCWString (Text.unpack dialogTitle)
        shortMsg'                  <- ContT $ withCWString (Text.unpack shortMsg)
        specificMsgWithStacktrace' <- ContT $ withCWString (Text.unpack specificMsgWithStacktrace)
        fullMsg'                   <- ContT $ withCWString (Text.unpack fullMsg)

        liftIO $ c_ShowErrorReporter dialogTitle' shortMsg' specificMsgWithStacktrace' fullMsg'

createFontSimple :: Int -> Text -> IO Win32.HFONT
createFontSimple fontSize fontName =
     withCWString (Text.unpack fontName) $ \fontName' ->
        c_CreateFontSimple fontSize fontName'

getResourceIcon :: Int -> IO Win32.HICON
getResourceIcon = c_GetResourceIcon

finaliseTEAWin32C :: IO ()
finaliseTEAWin32C = c_FinaliseTEAWin32C
