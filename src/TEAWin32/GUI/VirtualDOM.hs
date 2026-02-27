module TEAWin32.GUI.VirtualDOM
    ( CCallRequest (..)
    , UpdatePosReq (..)
    , CreateWindowReq (..)
    , CreateButtonReq (..)
    , CachedIconType (..)
    , fromHaskellCachedIconType
    , scheduleCCall
    ) where

import                          Control.Applicative                      ((<|>))
import                          Data.IORef                               (IORef,
                                                                          atomicModifyIORef',
                                                                          newIORef)
import                qualified Data.List                                as List
import                          Data.Text                                (Text)
import                          Foreign.C                                (CDouble,
                                                                          CInt)
import                          GHC.IO                                   (unsafePerformIO)
import                qualified Graphics.Win32                           as Win32
import                          TEAWin32.Exception                       (TEAWin32Error (InternalTEAWin32Error),
                                                                          errorTEAWin32)
import                          TEAWin32.GUI                             (Cursor,
                                                                          Font,
                                                                          Icon)
import {-# SOURCE #-}           TEAWin32.GUI.VirtualDOM.StorableInstance ()
import                qualified TEAWin32.Internal.Foreign                as Win32

cCallScheduleListRef :: IORef [(Maybe Win32.HWND, CCallRequest)]
cCallScheduleListRef = unsafePerformIO (newIORef [])
{-# NOINLINE cCallScheduleListRef #-}

data UpdatePosReq = UpdatePosReq
    { newLocation           :: Maybe (Int, Int)
    , newSize               :: Maybe (Int, Int)
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

data CachedIconType = ResourceIcon | StockIcon deriving Eq

fromHaskellCachedIconType :: CachedIconType -> CInt
fromHaskellCachedIconType ResourceIcon = 0
fromHaskellCachedIconType StockIcon    = 1

data CCallRequest = CreateWindowRequest        CreateWindowReq
                  | CreateButtonRequest        CreateButtonReq
                  | DestroyComponentRequest    Win32.HWND
                  | UpdateTextRequest          Win32.HWND Text
                  | UpdatePosRequest           Win32.HWND UpdatePosReq
                  | UpdateFontRequest          Win32.HWND Font
                  | UpdateIconRequest          Win32.HWND Icon
                  | UpdateCursorRequest        Win32.HWND Cursor
                  | InvalidateRectFullyRequest Win32.HWND

                  | CreateWindowRequest'       Win32.HWND Win32.LPCWSTR Win32.DWORD Win32.DWORD
                  | CreateButtonRequest'       Win32.HWND
                  | UpdateTextRequest'         Win32.HWND Win32.LPCWSTR
                  | UpdatePosRequest'          Win32.HWND Win32.BOOL Win32.BOOL Win32.BOOL (Maybe CInt) (Maybe CInt) (Maybe CInt) (Maybe CInt)
                  | UpdateFontRequest'         Win32.HWND Win32.LPCWSTR CInt CDouble CInt
                  | UpdateIconRequest'         Win32.HWND CachedIconType CDouble (Maybe Win32.SHSTOCKICONID) (Maybe Win32.LPCWSTR)
                  | UpdateCursorRequest'       Win32.HWND Win32.LPCWSTR
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
getHWNDFromReq _                                 = Nothing

canCombine :: CCallRequest -> CCallRequest -> Bool
canCombine (DestroyComponentRequest h1)         (DestroyComponentRequest h2)         = h1 == h2
canCombine (UpdateTextRequest h1 _)             (UpdateTextRequest h2 _)             = h1 == h2
canCombine (UpdatePosRequest h1 _)              (UpdatePosRequest h2 _)              = h1 == h2
canCombine (UpdateFontRequest h1 _)             (UpdateFontRequest h2 _)             = h1 == h2
canCombine (UpdateIconRequest h1 _)             (UpdateIconRequest h2 _)             = h1 == h2
canCombine (UpdateCursorRequest h1 _)           (UpdateCursorRequest h2 _)           = h1 == h2
canCombine (InvalidateRectFullyRequest h1)      (InvalidateRectFullyRequest h2)      = h1 == h2
canCombine _ _                                                                       = False

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
