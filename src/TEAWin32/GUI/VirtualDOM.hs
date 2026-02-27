module TEAWin32.GUI.VirtualDOM
    ( CCallRequest (..)
    , UpdatePosReq (..)
    , CreateWindowReq (..)
    , CreateButtonReq (..)
    , scheduleCCall
    , flushCCallRequests
    ) where

import           Control.Applicative                      ((<|>))
import           Control.Monad.Cont                       (ContT (..))
import           Control.Monad.IO.Class                   (liftIO)
import           Data.IORef                               (IORef,
                                                           atomicModifyIORef',
                                                           newIORef)
import qualified Data.List                                as List
import           Data.Maybe                               (fromMaybe, isJust)
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           Foreign                                  (Ptr, withArray)
import           Foreign.C                                (CDouble (..),
                                                           withCWString)
import           GHC.IO                                   (unsafePerformIO)
import qualified Graphics.Win32                           as Win32
import           TEAWin32.Exception                       (TEAWin32Error (InternalTEAWin32Error),
                                                           errorTEAWin32)
import           TEAWin32.GUI
import           TEAWin32.GUI.Component.ComponentRegistry (ComponentRegistryKey (ComponentScaleFactorRegKey),
                                                           getComponentRegistryEntryValue)
import           TEAWin32.GUI.VirtualDOM.Internal         (InternalCCallRequest (..))
import           TEAWin32.Util                            (applyScaleFactor)

foreign import ccall unsafe "ExecuteCCallRequests"
    c_ExecuteCCallRequests :: Ptr InternalCCallRequest -> Int -> Int -> IO ()

cCallScheduleListRef :: IORef [(Maybe Win32.HWND, CCallRequest)]
cCallScheduleListRef = unsafePerformIO (newIORef [])
{-# NOINLINE cCallScheduleListRef #-}

data UpdatePosReq = UpdatePosReq
    { newLocation           :: Maybe (ScalableValue, ScalableValue)
    , newSize               :: Maybe (ScalableValue, ScalableValue)
    , bringComponentToFront :: Bool
    } deriving Eq

data CreateWindowReq = CreateWindowReq
    { newWindowClassName  :: Text
    , newWindowExStyles   :: Win32.DWORD
    , newWindowStyles     :: Win32.DWORD
    , newWindowParentHWND :: Maybe Win32.HWND
    } deriving Eq

newtype CreateButtonReq = CreateButtonReq
    { newButtonParentHWND :: Win32.HWND
    } deriving Eq

data CCallRequest = CreateWindowRequest        CreateWindowReq
                  | CreateButtonRequest        CreateButtonReq
                  | DestroyComponentRequest    Win32.HWND
                  | UpdateTextRequest          Win32.HWND Text
                  | UpdatePosRequest           Win32.HWND UpdatePosReq
                  | UpdateFontRequest          Win32.HWND Font
                  | UpdateIconRequest          Win32.HWND Icon
                  | UpdateCursorRequest        Win32.HWND Cursor
                  | InvalidateRectFullyRequest Win32.HWND
                  deriving Eq

getHWNDFromReq :: CCallRequest -> Maybe Win32.HWND
getHWNDFromReq (CreateWindowRequest _)           = Nothing
getHWNDFromReq (CreateButtonRequest _)           = Nothing
getHWNDFromReq (DestroyComponentRequest hwnd)    = Just hwnd
getHWNDFromReq (UpdateTextRequest hwnd _)        = Just hwnd
getHWNDFromReq (UpdatePosRequest hwnd _)         = Just hwnd
getHWNDFromReq (UpdateFontRequest hwnd _)        = Just hwnd
getHWNDFromReq (UpdateIconRequest hwnd _)        = Just hwnd
getHWNDFromReq (UpdateCursorRequest hwnd _)      = Just hwnd
getHWNDFromReq (InvalidateRectFullyRequest hwnd) = Just hwnd

canCombine :: CCallRequest -> CCallRequest -> Bool
canCombine (DestroyComponentRequest h1)         (DestroyComponentRequest h2)         = h1 == h2
canCombine (UpdateTextRequest h1 _)             (UpdateTextRequest h2 _)             = h1 == h2
canCombine (UpdatePosRequest h1 _)              (UpdatePosRequest h2 _)              = h1 == h2
canCombine (UpdateFontRequest h1 _)             (UpdateFontRequest h2 _)             = h1 == h2
canCombine (UpdateIconRequest h1 _)             (UpdateIconRequest h2 _)             = h1 == h2
canCombine (UpdateCursorRequest h1 _)           (UpdateCursorRequest h2 _)           = h1 == h2
canCombine (InvalidateRectFullyRequest h1)      (InvalidateRectFullyRequest h2)      = h1 == h2
canCombine _ _                                                                       = False

isUpdatePosRequest :: CCallRequest -> Bool
isUpdatePosRequest (UpdatePosRequest _ _) = True
isUpdatePosRequest _                      = False

combineRequests :: CCallRequest -> CCallRequest -> CCallRequest
combineRequests newReq@(DestroyComponentRequest _)    (DestroyComponentRequest _)    = newReq
combineRequests newReq@(UpdateTextRequest _ _)        (UpdateTextRequest _ _)        = newReq
combineRequests newReq@(UpdateFontRequest _ _)        (UpdateFontRequest _ _)        = newReq
combineRequests newReq@(UpdateIconRequest _ _)        (UpdateIconRequest _ _)        = newReq
combineRequests newReq@(UpdateCursorRequest _ _)      (UpdateCursorRequest _ _)      = newReq
combineRequests newReq@(InvalidateRectFullyRequest _) (InvalidateRectFullyRequest _) = newReq
combineRequests (UpdatePosRequest hwnd newReq) (UpdatePosRequest _ oldReq) =
    UpdatePosRequest hwnd $
        UpdatePosReq
            { newLocation           = newLocation newReq <|> newLocation oldReq
            , newSize               = newSize newReq     <|> newSize oldReq
            , bringComponentToFront = bringComponentToFront newReq || bringComponentToFront oldReq
            }
combineRequests _ _ = errorTEAWin32 (InternalTEAWin32Error "Tried to combine incompatible CCallRequests.")

scheduleCCall :: CCallRequest -> IO ()
scheduleCCall newReq =
    atomicModifyIORef' cCallScheduleListRef $ \scheduleList ->
        case getHWNDFromReq newReq of
            Just hwnd ->
                case List.find (\(_, oldReq) -> canCombine newReq oldReq) scheduleList of
                    Just (_, oldReq) ->
                        let filteredList = filter (\(_, req) -> req /= oldReq) scheduleList in
                            ((Just hwnd, combineRequests newReq oldReq) : filteredList, ())

                    Nothing ->
                        ((Just hwnd, newReq) : scheduleList, ())

            Nothing ->
                ((Nothing, newReq) : scheduleList, ())

flushCCallRequests :: IO ()
flushCCallRequests = do
    requests <- atomicModifyIORef' cCallScheduleListRef (\list -> ([], map snd (reverse list)))

    let updatePosReqCount = length $ filter isUpdatePosRequest requests

    runContT (mapM marshallRequest requests) $ \storableRequests ->
        withArray storableRequests $ \arrayPtr ->
            c_ExecuteCCallRequests arrayPtr (length requests) updatePosReqCount

marshallRequest :: CCallRequest -> ContT a IO InternalCCallRequest
marshallRequest (CreateWindowRequest req) =
    ContT (withCWString (Text.unpack $ newWindowClassName req)) >>= \classNamePtr ->
        pure $ CreateWindowRequest'
            (fromMaybe Win32.nullPtr (newWindowParentHWND req))
            classNamePtr
            (newWindowExStyles req)
            (newWindowStyles req)

marshallRequest (CreateButtonRequest req) =
    pure (CreateButtonRequest' (newButtonParentHWND req))

marshallRequest (DestroyComponentRequest hwnd) =
    pure (DestroyComponentRequest' hwnd)

marshallRequest (UpdateTextRequest hwnd text) =
    ContT (withCWString (Text.unpack text)) >>= \textPtr ->
        pure (UpdateTextRequest' hwnd textPtr)

marshallRequest (UpdatePosRequest hwnd req) =
    liftIO (getComponentRegistryEntryValue ComponentScaleFactorRegKey hwnd) >>= \scaleFactor ->
        pure $ UpdatePosRequest'
            hwnd
            (isJust (newLocation req))
            (isJust (newSize req))
            (bringComponentToFront req)
            (fromIntegral . (`applyScaleFactor` scaleFactor) . fst <$> newLocation req)
            (fromIntegral . (`applyScaleFactor` scaleFactor) . snd <$> newLocation req)
            (fromIntegral . (`applyScaleFactor` scaleFactor) . fst <$> newSize req)
            (fromIntegral . (`applyScaleFactor` scaleFactor) . snd <$> newSize req)

marshallRequest (UpdateFontRequest hwnd font) =
    ContT (withCWString (Text.unpack $ fontName font)) >>= \fontNamePtr ->
        liftIO (getComponentRegistryEntryValue ComponentScaleFactorRegKey hwnd) >>= \scaleFactor ->
            pure $ UpdateFontRequest'
                hwnd
                fontNamePtr
                (fromIntegral $ applyScaleFactor (fontSize font) scaleFactor)
                (CDouble scaleFactor)
                0 -- TODO

marshallRequest (UpdateIconRequest hwnd icon) =
    liftIO (getComponentRegistryEntryValue ComponentScaleFactorRegKey hwnd) >>= \scaleFactor ->
        case getIconType icon of
            ResourceIcon ->
                pure $ UpdateIconRequest'
                    hwnd
                    ResourceIcon
                    (CDouble scaleFactor)
                    Nothing
                    (Just (toWin32Icon' icon))
            StockIcon ->
                pure $ UpdateIconRequest'
                    hwnd
                    StockIcon
                    (CDouble scaleFactor)
                    (Just (toStockIconId icon))
                    Nothing

marshallRequest (UpdateCursorRequest hwnd cursor) =
    pure (UpdateCursorRequest' hwnd (toWin32Cursor' cursor))

marshallRequest (InvalidateRectFullyRequest hwnd) =
    pure (InvalidateRectFullyRequest' hwnd)
