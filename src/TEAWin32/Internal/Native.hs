module TEAWin32.Internal.Native
    ( CreateManagedWindowArgs (..)
    , initialiseTEAWin32C
    , initialiseDPIAwareFunctions
    , getScaleFactorForHWND
    , enableVisualStyles
    , getImmediateChildWindows
    , getTopLevelWindows
    , isWindowTopLevel
    , getHighDPIIcon
    , enableDPIAware
    , showErrorReporter
    , createFontSimple
    , getResourceIcon
    , createManagedWindow
    , finaliseTEAWin32C
    ) where

import           Control.Monad.Cont        (ContT (..), evalContT)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Foreign                   (FunPtr, Ptr, Storable (..), alloca,
                                            allocaArray, fillBytes, peekArray)
import           Foreign.C                 (CWString, withCWString)
import qualified Graphics.Win32            as Win32
import qualified TEAWin32.Internal.Foreign as Win32

data CreateManagedWindowArgs = CreateManagedWindowArgs
    { managedWindowClassName   :: Win32.LPCWSTR
    , managedWindowClassStyles :: Win32.UINT
    , managedWindowExStyles    :: Win32.DWORD
    , managedWindowStyles      :: Win32.DWORD
    , managedWindowParentHWND  :: Win32.HWND
    }

instance Storable CreateManagedWindowArgs where
    sizeOf _ = 32

    alignment _ = 8

    poke ptr args = do
        fillBytes ptr 0 (sizeOf args)

        pokeByteOff ptr 0 (managedWindowClassName args)
        pokeByteOff ptr 8 (managedWindowClassStyles args)
        pokeByteOff ptr 12 (managedWindowExStyles args)
        pokeByteOff ptr 16 (managedWindowStyles args)
        pokeByteOff ptr 24 (managedWindowParentHWND args)

    peek ptr = do
        managedWindowClassName'   <- peekByteOff ptr 0
        managedWindowClassStyles' <- peekByteOff ptr 8
        managedWindowExStyles'    <- peekByteOff ptr 12
        managedWindowStyles'      <- peekByteOff ptr 16
        managedWindowParentHWND'  <- peekByteOff ptr 24

        pure $ CreateManagedWindowArgs
            { managedWindowClassName   = managedWindowClassName'
            , managedWindowClassStyles = managedWindowClassStyles'
            , managedWindowExStyles    = managedWindowExStyles'
            , managedWindowStyles      = managedWindowStyles'
            , managedWindowParentHWND  = managedWindowParentHWND'
            }

foreign import ccall unsafe "InitialiseTEAWin32C"
    c_InitialiseTEAWin32C :: FunPtr Win32.WNDPROC -> IO ()

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

foreign import ccall unsafe "EnableDPIAware"
    c_EnableDPIAware :: IO ()

foreign import ccall "ShowErrorReporter"
    c_ShowErrorReporter :: CWString -> CWString -> CWString -> CWString -> IO ()

foreign import ccall unsafe "CreateFontSimple"
    c_CreateFontSimple :: Int -> CWString -> IO Win32.HFONT

foreign import ccall unsafe "GetResourceIcon"
    c_GetResourceIcon :: Int -> IO Win32.HICON

foreign import ccall unsafe "CreateManagedWindow"
    c_CreateManagedWindow :: Ptr CreateManagedWindowArgs -> IO Win32.HWND

foreign import ccall unsafe "FinaliseTEAWin32C"
    c_FinaliseTEAWin32C :: IO ()

initialiseTEAWin32C :: FunPtr Win32.WNDPROC -> IO ()
initialiseTEAWin32C = c_InitialiseTEAWin32C

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

enableDPIAware :: IO ()
enableDPIAware = c_EnableDPIAware

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

createManagedWindow :: CreateManagedWindowArgs -> IO Win32.HWND
createManagedWindow args =
    alloca $ \argsPtr ->
        poke argsPtr args >>
            c_CreateManagedWindow argsPtr

finaliseTEAWin32C :: IO ()
finaliseTEAWin32C = c_FinaliseTEAWin32C
