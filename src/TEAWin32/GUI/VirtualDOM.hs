module TEAWin32.GUI.VirtualDOM
    ( CCallRequest (..)
    , UpdatePosReq (..)
    , CreateWindowReq (..)
    , CreateButtonReq (..)
    , scheduleCCall
    , flushCCallRequests
    ) where

import           Control.Applicative              ((<|>))
import           Control.Monad.Cont               (ContT (..))
import           Control.Monad.IO.Class           (liftIO)
import           Data.IORef                       (IORef, atomicModifyIORef',
                                                   newIORef)
import qualified Data.List                        as List
import           Data.Maybe                       (isJust)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Foreign                          (Ptr, withArray)
import           Foreign.C                        (withCWString)
import           GHC.IO                           (unsafePerformIO)
import qualified Graphics.Win32                   as Win32
import           TEAWin32.Exception               (TEAWin32Error (InternalTEAWin32Error),
                                                   errorTEAWin32)
import           TEAWin32.GUI
import           TEAWin32.GUI.VirtualDOM.Internal (InternalCCallRequest (..))

foreign import ccall unsafe "ExecuteCCallRequests"
    c_ExecuteCCallRequests :: Ptr InternalCCallRequest -> Int -> Int -> IO ()

cCallScheduleListRef :: IORef [(Maybe UniqueId, CCallRequest)]
cCallScheduleListRef = unsafePerformIO (newIORef [])
{-# NOINLINE cCallScheduleListRef #-}

data UpdatePosReq = UpdatePosReq
    { newLocation           :: Maybe (ScalableValue, ScalableValue)
    , newSize               :: Maybe (ScalableValue, ScalableValue)
    , bringComponentToFront :: Bool
    } deriving Eq

data CreateWindowReq = CreateWindowReq
    { newWindowUniqueId       :: UniqueId
    , newWindowClassName      :: Text
    , newWindowExStyles       :: Win32.DWORD
    , newWindowStyles         :: Win32.DWORD
    , newWindowParentUniqueId :: Maybe UniqueId
    } deriving Eq

data CreateButtonReq = CreateButtonReq
    { newButtonUniqueId       :: UniqueId
    , newButtonParentUniqueId :: UniqueId
    } deriving Eq

data CCallRequest = CreateWindowRequest        CreateWindowReq
                  | CreateButtonRequest        CreateButtonReq
                  | DestroyComponentRequest    UniqueId
                  | UpdateTextRequest          UniqueId Text
                  | UpdatePosRequest           UniqueId UpdatePosReq
                  | UpdateFontRequest          UniqueId Font
                  | UpdateIconRequest          UniqueId Icon
                  | UpdateCursorRequest        UniqueId Cursor
                  | InvalidateRectFullyRequest UniqueId
                  | ShowWindowRequest          UniqueId
                  deriving Eq

getTargetFromReq :: CCallRequest -> Maybe UniqueId
getTargetFromReq (CreateWindowRequest _)               = Nothing
getTargetFromReq (CreateButtonRequest _)               = Nothing
getTargetFromReq (DestroyComponentRequest uniqueId)    = Just uniqueId
getTargetFromReq (UpdateTextRequest uniqueId _)        = Just uniqueId
getTargetFromReq (UpdatePosRequest uniqueId _)         = Just uniqueId
getTargetFromReq (UpdateFontRequest uniqueId _)        = Just uniqueId
getTargetFromReq (UpdateIconRequest uniqueId _)        = Just uniqueId
getTargetFromReq (UpdateCursorRequest uniqueId _)      = Just uniqueId
getTargetFromReq (InvalidateRectFullyRequest uniqueId) = Just uniqueId
getTargetFromReq (ShowWindowRequest uniqueId)          = Just uniqueId

canCombine :: CCallRequest -> CCallRequest -> Bool
canCombine (DestroyComponentRequest u1)    (DestroyComponentRequest u2)    = u1 == u2
canCombine (UpdateTextRequest u1 _)        (UpdateTextRequest u2 _)        = u1 == u2
canCombine (UpdatePosRequest u1 _)         (UpdatePosRequest u2 _)         = u1 == u2
canCombine (UpdateFontRequest u1 _)        (UpdateFontRequest u2 _)        = u1 == u2
canCombine (UpdateIconRequest u1 _)        (UpdateIconRequest u2 _)        = u1 == u2
canCombine (UpdateCursorRequest u1 _)      (UpdateCursorRequest u2 _)      = u1 == u2
canCombine (InvalidateRectFullyRequest u1) (InvalidateRectFullyRequest u2) = u1 == u2
canCombine (ShowWindowRequest u1)          (ShowWindowRequest u2)          = u1 == u2
canCombine _ _                                                             = False

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
combineRequests newReq@(ShowWindowRequest _)          (ShowWindowRequest _)          = newReq
combineRequests (UpdatePosRequest uniqueId newReq) (UpdatePosRequest _ oldReq) =
    UpdatePosRequest uniqueId $
        UpdatePosReq
            { newLocation           = newLocation newReq <|> newLocation oldReq
            , newSize               = newSize newReq     <|> newSize oldReq
            , bringComponentToFront = bringComponentToFront newReq || bringComponentToFront oldReq
            }
combineRequests _ _ = errorTEAWin32 (InternalTEAWin32Error "Tried to combine incompatible CCallRequests.")

scheduleCCall :: CCallRequest -> IO ()
scheduleCCall newReq =
    atomicModifyIORef' cCallScheduleListRef $ \scheduleList ->
        case getTargetFromReq newReq of
            Just targetUniqueId ->
                case List.find (\(_, oldReq) -> canCombine newReq oldReq) scheduleList of
                    Just (_, oldReq) ->
                        let filteredList = filter (\(_, req) -> req /= oldReq) scheduleList in
                            ((Just targetUniqueId, combineRequests newReq oldReq) : filteredList, ())

                    Nothing ->
                        ((Just targetUniqueId, newReq) : scheduleList, ())

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
            (newWindowUniqueId req)
            classNamePtr
            (newWindowExStyles req)
            (newWindowStyles req)
            (newWindowParentUniqueId req)

marshallRequest (CreateButtonRequest req) =
    pure (CreateButtonRequest' (newButtonUniqueId req) (newButtonParentUniqueId req))

marshallRequest (DestroyComponentRequest target) =
    pure (DestroyComponentRequest' target)

marshallRequest (UpdateTextRequest target text) =
    ContT (withCWString (Text.unpack text)) >>= \textPtr ->
        pure (UpdateTextRequest' target textPtr)

marshallRequest (UpdatePosRequest target req) =
    pure $ UpdatePosRequest'
        target
        (isJust (newLocation req))
        (isJust (newSize req))
        (bringComponentToFront req)
        (fst <$> newLocation req)
        (snd <$> newLocation req)
        (fst <$> newSize req)
        (snd <$> newSize req)

marshallRequest (UpdateFontRequest target font) =
    ContT (withCWString (Text.unpack $ fontName font)) >>= \fontNamePtr ->
        pure $ UpdateFontRequest'
            target
            fontNamePtr
            (fontSize font)
            0 -- TODO

marshallRequest (UpdateIconRequest target icon) =
    case getIconType icon of
        ResourceIcon ->
            pure $ UpdateIconRequest'
                target
                ResourceIcon
                Nothing
                (Just (toWin32Icon icon))
        StockIcon ->
            pure $ UpdateIconRequest'
                target
                StockIcon
                (Just (toStockIconId icon))
                Nothing

marshallRequest (UpdateCursorRequest target cursor) =
    pure (UpdateCursorRequest' target (toWin32Cursor cursor))

marshallRequest (InvalidateRectFullyRequest target) =
    pure (InvalidateRectFullyRequest' target)

marshallRequest (ShowWindowRequest target) =
    pure (ShowWindowRequest' target)
