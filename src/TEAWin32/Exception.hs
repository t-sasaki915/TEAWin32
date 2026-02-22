module TEAWin32.Exception
    ( ErrorLocation (..)
    , TEAWin32Error (..)
    , try_
    , errorTEAWin32
    ) where

import           Control.Exception         (SomeException, try)
import           Control.Monad             (unless, void, when)
import           Data.Bits                 ((.|.))
import           Data.IORef                (atomicModifyIORef', newIORef,
                                            readIORef)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Foreign                   (alloca, castPtr, copyBytes,
                                            ptrToWordPtr, wordPtrToPtr)
import           Foreign.C                 (withCWStringLen)
import           GHC.IO                    (unsafePerformIO)
import           GHC.Stack                 (HasCallStack, callStack,
                                            prettyCallStack)
import qualified Graphics.Win32            as Win32
import qualified System.Win32              as Win32
import qualified TEAWin32.Internal.Foreign as Win32

data ErrorLocation = Init
                   | Update
                   deriving Show

data TEAWin32Error = InternalTEAWin32Error Text
                   | TEAWin32ApplicationError ErrorLocation Text

try_ :: IO a -> IO (Either SomeException a)
try_ = try

errorTEAWin32 :: HasCallStack => TEAWin32Error -> a
errorTEAWin32 (InternalTEAWin32Error errorMsg) = reportTEAWin32Error
    "Internal Framework Error" "An internal TEAWin32 error has occurred." errorMsg

errorTEAWin32 (TEAWin32ApplicationError errLoc errorMsg) = reportTEAWin32Error
    "Application Error" ("An unhandled application error has occurred during " <> Text.show errLoc <> ".") errorMsg

reportTEAWin32Error :: HasCallStack => Text -> Text -> Text -> a
reportTEAWin32Error dialogTitle shortErrorMsg specificErrorMsg = unsafePerformIO $ do
    let details = Text.replace "\n" "\r\n" (specificErrorMsg <> "\n\n" <> Text.pack (prettyCallStack callStack))

    Win32.messageBeep (Just Win32.mB_ICONHAND)

    let windowClass = Win32.mkClassName "TEAWin32-ErrorReporter"
    mainInstance    <- Win32.getModuleHandle Nothing
    icon            <- Win32.getHighDPIIcon 80
    backgroundBrush <- Win32.createSolidBrush (Win32.rgb 255 255 255)

    scaleRatioRef <- newIORef 0.0
    uiFontRef     <- newIORef Win32.nullPtr

    isDetailVisibleRef <- newIORef False
    detailButtonRef    <- newIORef Win32.nullPtr
    detailBoxRef       <- newIORef Win32.nullPtr

    _ <- Win32.registerClass
        ( Win32.cS_VREDRAW + Win32.cS_HREDRAW
        , mainInstance
        , Just icon
        , Nothing
        , Just backgroundBrush
        , Nothing
        , windowClass
        )

    let wndProc hwnd wMsg wParam lParam
            | wMsg == Win32.wM_PAINT = do
                alloca $ \paintStructPtr -> do
                    hdc    <- Win32.beginPaint hwnd paintStructPtr
                    ratio  <- readIORef scaleRatioRef
                    uiFont <- readIORef uiFontRef

                    errorIcon <- Win32.getHighDPIIcon 80

                    _ <- Win32.c_DrawIconEx hdc (scale ratio 10) (scale ratio 10) errorIcon
                        (scale ratio 32) (scale ratio 32) 0 Win32.nullPtr 3

                    oldFont <- Win32.c_SelectObject hdc uiFont

                    _ <- Win32.setTextColor hdc (Win32.rgb 0 0 0)
                    _ <- Win32.setBkMode hdc 1

                    Win32.textOut hdc (scale ratio 53) (scale ratio 16) (Text.unpack shortErrorMsg)

                    void (Win32.c_SelectObject hdc oldFont)

                    Win32.endPaint hwnd paintStructPtr

                pure 0

            | wMsg == Win32.wM_COMMAND = do
                let wmId = Win32.lOWORD (fromIntegral wParam)

                case wmId of
                    100 ->
                        Win32.sendMessage hwnd Win32.wM_CLOSE 0 0 >>
                            pure 0

                    101 -> do
                        let content = shortErrorMsg <> "\r\n\r\n" <> details
                        withCWStringLen (Text.unpack content) $ \(pSrc, len) -> do
                            let size = (len + 1) * 2

                            hndl <- Win32.globalAlloc Win32.gMEM_FIXED (fromIntegral size)

                            unless (hndl == Win32.nullPtr) $ do
                                copyBytes (castPtr hndl) (castPtr pSrc) size

                                Win32.c_OpenClipboard hwnd >>= \case
                                    True -> do
                                        Win32.emptyClipboard
                                        res <- Win32.setClipboardData Win32.cF_UNICODETEXT hndl
                                        Win32.closeClipboard

                                        when (res == Win32.nullPtr) $ void (Win32.globalFree hndl)

                                    False ->
                                        void (Win32.globalFree hndl)

                        pure 0

                    102 -> do
                        detailButton <- readIORef detailButtonRef
                        detailBox    <- readIORef detailBoxRef

                        ratio <- readIORef scaleRatioRef

                        readIORef isDetailVisibleRef >>= \case
                            True -> do
                                Win32.setWindowText detailButton "\x25BC Expand Details"

                                _ <- Win32.c_SetWindowPos hwnd Win32.nullPtr
                                    0 0 (scale ratio 505) (scale ratio 150) Win32.sWP_NOMOVE

                                _ <- Win32.showWindow detailBox Win32.sW_HIDE

                                atomicModifyIORef' isDetailVisibleRef (const (False, ()))

                                pure 0

                            False -> do
                                Win32.setWindowText detailButton "\x25B2 Collapse Details"

                                _ <- Win32.c_SetWindowPos hwnd Win32.nullPtr
                                    0 0 (scale ratio 505) (scale ratio 363) Win32.sWP_NOMOVE

                                _ <- Win32.showWindow detailBox Win32.sW_SHOW

                                atomicModifyIORef' isDetailVisibleRef (const (True, ()))

                                pure 0

                    _ ->
                        Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

            | otherwise =
                Win32.defWindowProcSafe (Just hwnd) wMsg wParam lParam

    errorReporterWindow <- Win32.createWindowEx
        (Win32.wS_EX_TOPMOST .|. Win32.wS_EX_APPWINDOW)
        windowClass
        (Text.unpack dialogTitle)
        (Win32.wS_CLIPCHILDREN .|. Win32.wS_BORDER .|. Win32.wS_SYSMENU)
        Nothing
        Nothing
        (Just 0)
        (Just 0)
        Nothing
        Nothing
        mainInstance
        wndProc

    scaleRatio <- do
        hdc <- Win32.getDC (Just errorReporterWindow)
        dpiY <- Win32.c_GetDeviceCaps hdc 90
        Win32.releaseDC (Just errorReporterWindow) hdc

        pure (fromIntegral dpiY / 96.0)

    uiFont <- Win32.createFont (-scale scaleRatio 14)
        0 0 0 400 False False False 1 0 0 0 0 "Meiryo UI"

    editorFont <- Win32.createFont (-scale scaleRatio 12)
        0 0 0 400 False False False 1 0 0 0 0 "Consolas"

    _ <- Win32.c_SetWindowPos errorReporterWindow Win32.nullPtr
        0 0 (scale scaleRatio 505) (scale scaleRatio 150) Win32.sWP_NOMOVE

    atomicModifyIORef' scaleRatioRef (const (scaleRatio, ()))
    atomicModifyIORef' uiFontRef (const (uiFont, ()))

    closeButton <- Win32.createWindow
        (Win32.mkClassName "BUTTON")
        "Close"
        (Win32.wS_TABSTOP .|. Win32.wS_VISIBLE .|. Win32.wS_CHILD .|. Win32.bS_DEFPUSHBUTTON .|. Win32.wS_CLIPSIBLINGS)
        (Just (scale scaleRatio 375))
        (Just (scale scaleRatio 70))
        (Just (scale scaleRatio 100))
        (Just (scale scaleRatio 28))
        (Just errorReporterWindow)
        (Just (wordPtrToPtr 100))
        mainInstance
        (const $ const $ const $ const $ pure 0)

    void $ Win32.sendMessage closeButton Win32.wM_SETFONT (fromIntegral $ ptrToWordPtr uiFont) 1

    copyButton <- Win32.createWindow
        (Win32.mkClassName "BUTTON")
        "Copy Details"
        (Win32.wS_TABSTOP .|. Win32.wS_VISIBLE .|. Win32.wS_CHILD .|. Win32.bS_DEFPUSHBUTTON .|. Win32.wS_CLIPSIBLINGS)
        (Just (scale scaleRatio 185))
        (Just (scale scaleRatio 70))
        (Just (scale scaleRatio 100))
        (Just (scale scaleRatio 28))
        (Just errorReporterWindow)
        (Just (wordPtrToPtr 101))
        mainInstance
        (const $ const $ const $ const $ pure 0)

    void $ Win32.sendMessage copyButton Win32.wM_SETFONT (fromIntegral $ ptrToWordPtr uiFont) 1

    detailButton <- Win32.createWindow
        (Win32.mkClassName "BUTTON")
        "\x25BC Expand Details"
        (Win32.wS_TABSTOP .|. Win32.wS_VISIBLE .|. Win32.wS_CHILD .|. Win32.bS_DEFPUSHBUTTON .|. Win32.wS_CLIPSIBLINGS)
        (Just (scale scaleRatio 15))
        (Just (scale scaleRatio 70))
        (Just (scale scaleRatio 150))
        (Just (scale scaleRatio 28))
        (Just errorReporterWindow)
        (Just (wordPtrToPtr 102))
        mainInstance
        (const $ const $ const $ const $ pure 0)

    atomicModifyIORef' detailButtonRef (const (detailButton, ()))

    void $ Win32.sendMessage detailButton Win32.wM_SETFONT (fromIntegral $ ptrToWordPtr uiFont) 1

    detailBox <- Win32.createWindow
        (Win32.mkClassName "EDIT")
        (Text.unpack details)
        (Win32.wS_CHILD .|. Win32.wS_BORDER .|. Win32.wS_VSCROLL .|. Win32.wS_HSCROLL .|. Win32.eS_MULTILINE .|. Win32.eS_READONLY)
        (Just (scale scaleRatio 16))
        (Just (scale scaleRatio 110))
        (Just (scale scaleRatio 458))
        (Just (scale scaleRatio 200))
        (Just errorReporterWindow)
        Nothing
        mainInstance
        (const $ const $ const $ const $ pure 0)

    atomicModifyIORef' detailBoxRef (const (detailBox, ()))

    void $ Win32.sendMessage detailBox Win32.wM_SETFONT (fromIntegral $ ptrToWordPtr editorFont) 1

    _ <- Win32.showWindow errorReporterWindow Win32.sW_SHOWNORMAL
    Win32.updateWindow errorReporterWindow

    let messagePump msgPtr =
            try_ (Win32.getMessage msgPtr (Just errorReporterWindow)) >>= \case
                Right _ ->
                    Win32.translateMessage msgPtr >>
                        Win32.dispatchMessage msgPtr >>
                            messagePump msgPtr
                Left _ ->
                    pure ()

    Win32.allocaMessage messagePump

    _ <- Win32.c_DeleteObject uiFont
    _ <- Win32.c_DeleteObject editorFont

    error $ Text.unpack (shortErrorMsg <> "\n\n" <> specificErrorMsg)

    where
        scale :: Num a => Double -> Int -> a
        scale ratio v = fromIntegral (round (fromIntegral v * ratio) :: Int)
