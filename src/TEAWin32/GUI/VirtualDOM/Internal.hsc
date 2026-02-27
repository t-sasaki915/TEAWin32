{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TEAWin32.GUI.VirtualDOM.Internal (InternalCCallRequest (..)) where

import           Control.Monad             (when)
import           Data.Maybe                (fromJust)
import           Foreign                   (Storable (..), Word32, fillBytes,
                                            plusPtr)
import           Foreign.C                 (CDouble, CInt)
import qualified Graphics.Win32            as Win32
import           TEAWin32.Exception        (TEAWin32Error (..), errorTEAWin32)
import           TEAWin32.GUI              (IconType (..))
import qualified TEAWin32.Internal.Foreign as Win32

#include "VirtualDOM.h"

data InternalCCallRequest = CreateWindowRequest'        Win32.HWND Win32.LPCWSTR Win32.DWORD Win32.DWORD
                          | CreateButtonRequest'        Win32.HWND
                          | DestroyComponentRequest'    Win32.HWND
                          | UpdateTextRequest'          Win32.HWND Win32.LPCWSTR
                          | UpdatePosRequest'           Win32.HWND Win32.BOOL Win32.BOOL Win32.BOOL (Maybe CInt) (Maybe CInt) (Maybe CInt) (Maybe CInt)
                          | UpdateFontRequest'          Win32.HWND Win32.LPCWSTR CInt CDouble CInt
                          | UpdateIconRequest'          Win32.HWND IconType CDouble (Maybe Win32.SHSTOCKICONID) (Maybe Win32.LPCWSTR)
                          | UpdateCursorRequest'        Win32.HWND Win32.LPCWSTR
                          | InvalidateRectFullyRequest' Win32.HWND

instance Storable InternalCCallRequest where
    sizeOf _ = #{size CCallRequest}

    alignment _ = #{alignment CCallRequest}

    peek _ = errorTEAWin32 (InternalTEAWin32Error "Tried to peek CCallRequest")

    poke ptr val = do
        fillBytes ptr 0 (sizeOf val)

        case val of
            (CreateWindowRequest' parentHWND className exStyles styles) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_CREATE_WINDOW} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr parentHWND

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.createWindowReq}

                #{poke CreateWindowReq, newWindowClassName} dataPtr className
                #{poke CreateWindowReq, newWindowExStyles}  dataPtr exStyles
                #{poke CreateWindowReq, newWindowStyles}    dataPtr styles

            (CreateButtonRequest' parentHWND) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_CREATE_BUTTON} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr parentHWND

            (DestroyComponentRequest' hwnd) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_DESTROY_COMPONENT} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd

            (UpdateTextRequest' hwnd textPtr) -> do
                #{poke CCallRequest, reqType}                  ptr (#{const REQ_UPDATE_TEXT} :: #{type RequestType})
                #{poke CCallRequest, targetHWND}               ptr hwnd
                #{poke CCallRequest, reqData.newComponentText} ptr textPtr

            (UpdatePosRequest' hwnd hasNewLoc hasNewSize bringFront maybeNewX maybeNewY maybeNewWidth maybeNewHeight) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_UPDATE_POS} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.updatePosReq}

                #{poke UpdatePosReq, hasNewLocation}        dataPtr hasNewLoc
                #{poke UpdatePosReq, hasNewSize}            dataPtr hasNewSize
                #{poke UpdatePosReq, bringComponentToFront} dataPtr bringFront

                when hasNewLoc $ do
                    #{poke UpdatePosReq, newX} dataPtr (fromJust maybeNewX)
                    #{poke UpdatePosReq, newY} dataPtr (fromJust maybeNewY)
                
                when hasNewSize $ do
                    #{poke UpdatePosReq, newWidth}  dataPtr (fromJust maybeNewWidth)
                    #{poke UpdatePosReq, newHeight} dataPtr (fromJust maybeNewHeight)

            (UpdateFontRequest' hwnd fontName fontSize scaleRatio fontStyle) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_UPDATE_FONT} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.newFontCacheKey}

                #{poke CachedFont, fontName}   dataPtr fontName
                #{poke CachedFont, fontSize}   dataPtr fontSize
                #{poke CachedFont, scaleRatio} dataPtr scaleRatio
                #{poke CachedFont, fontStyle}  dataPtr fontStyle

            (UpdateIconRequest' hwnd iconType scaleRatio maybeStockIconId maybeResourceId) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_UPDATE_ICON} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd
                
                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.newIconCacheKey}

                #{poke CachedIcon, scaleRatio} dataPtr scaleRatio

                case iconType of
                    StockIcon -> do
                        #{poke CachedIcon, iconType}           dataPtr (1 :: CInt)
                        #{poke CachedIcon, iconId.stockIconId} dataPtr (fromJust maybeStockIconId)
                    ResourceIcon -> do
                        #{poke CachedIcon, iconType}           dataPtr (0 :: CInt)
                        #{poke CachedIcon, iconId.resourceId}  dataPtr (fromJust maybeResourceId)

            (UpdateCursorRequest' hwnd cursorId) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_UPDATE_CURSOR} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.newCursorCacheKey}

                #{poke CachedCursor, cursorKey} dataPtr cursorId

            (InvalidateRectFullyRequest' hwnd) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_INVALIDATE_RECT_FULLY} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd
