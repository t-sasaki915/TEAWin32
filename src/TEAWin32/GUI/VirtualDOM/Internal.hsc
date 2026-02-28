{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TEAWin32.GUI.VirtualDOM.Internal (InternalCCallRequest (..)) where

import           Control.Monad             (when)
import           Data.Maybe                (fromJust)
import           Foreign                   (Storable (..), Word32, fillBytes,
                                            plusPtr)
import           Foreign.C                 (CInt)
import qualified Graphics.Win32            as Win32
import           TEAWin32.Exception        (TEAWin32Error (..), errorTEAWin32)
import           TEAWin32.GUI              (IconType (..), ScalableValue (..), UniqueId (..))
import           TEAWin32.GUI.Instances    ()
import qualified TEAWin32.Internal.Foreign as Win32

#include "VirtualDOM.h"

data InternalCCallRequest = CreateWindowRequest'        UniqueId Win32.LPCWSTR Win32.DWORD Win32.DWORD (Maybe UniqueId)
                          | CreateButtonRequest'        UniqueId UniqueId
                          | DestroyComponentRequest'    UniqueId
                          | UpdateTextRequest'          UniqueId Win32.LPCWSTR
                          | UpdatePosRequest'           UniqueId Win32.BOOL Win32.BOOL Win32.BOOL (Maybe ScalableValue) (Maybe ScalableValue) (Maybe ScalableValue) (Maybe ScalableValue)
                          | UpdateFontRequest'          UniqueId Win32.LPCWSTR ScalableValue CInt
                          | UpdateIconRequest'          UniqueId IconType (Maybe Win32.SHSTOCKICONID) (Maybe Win32.LPCWSTR)
                          | UpdateCursorRequest'        UniqueId Win32.LPCWSTR
                          | InvalidateRectFullyRequest' UniqueId
                          | ShowWindowRequest'          UniqueId

instance Storable InternalCCallRequest where
    sizeOf _ = #{size CCallRequest}

    alignment _ = #{alignment CCallRequest}

    peek _ = errorTEAWin32 (InternalTEAWin32Error "Tried to peek CCallRequest")

    poke ptr val = do
        fillBytes ptr 0 (sizeOf val)

        case val of
            (CreateWindowRequest' (UniqueId uniqueId) className exStyles styles maybeParent) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_CREATE_WINDOW} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.createWindowReq}

                #{poke CreateWindowReq, newWindowClassName} dataPtr className
                #{poke CreateWindowReq, newWindowExStyles}  dataPtr exStyles
                #{poke CreateWindowReq, newWindowStyles}    dataPtr styles
                
                case maybeParent of
                    Just (UniqueId parent) ->
                        #{poke CreateWindowReq, newWindowParentUniqueId} dataPtr (fromIntegral parent :: CInt)

                    Nothing ->
                        #{poke CreateWindowReq, newWindowParentUniqueId} dataPtr (0 :: CInt)

            (CreateButtonRequest' (UniqueId uniqueId) (UniqueId parentId)) -> do
                #{poke CCallRequest, reqType}                         ptr (#{const REQ_CREATE_BUTTON} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId}                  ptr (fromIntegral uniqueId :: CInt)
                #{poke CCallRequest, reqData.newButtonParentUniqueId} ptr (fromIntegral parentId :: CInt)

            (DestroyComponentRequest' (UniqueId uniqueId)) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_DESTROY_COMPONENT} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)

            (UpdateTextRequest' (UniqueId uniqueId) textPtr) -> do
                #{poke CCallRequest, reqType}                  ptr (#{const REQ_UPDATE_TEXT} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId}           ptr (fromIntegral uniqueId :: CInt)
                #{poke CCallRequest, reqData.newComponentText} ptr textPtr

            (UpdatePosRequest' (UniqueId uniqueId) hasNewLoc hasNewSize bringFront maybeNewX maybeNewY maybeNewWidth maybeNewHeight) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_UPDATE_POS} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)

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

            (UpdateFontRequest' (UniqueId uniqueId) fontName fontSize fontStyle) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_UPDATE_FONT} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.newFontCacheKey}

                #{poke CachedFont, fontName}  dataPtr fontName
                #{poke CachedFont, fontSize}  dataPtr fontSize
                #{poke CachedFont, fontStyle} dataPtr fontStyle

            (UpdateIconRequest' (UniqueId uniqueId) iconType maybeStockIconId maybeResourceId) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_UPDATE_ICON} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)
                
                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.newIconCacheKey}

                case iconType of
                    StockIcon -> do
                        #{poke CachedIcon, iconType}           dataPtr (1 :: CInt)
                        #{poke CachedIcon, iconId.stockIconId} dataPtr (fromJust maybeStockIconId)
                    ResourceIcon -> do
                        #{poke CachedIcon, iconType}           dataPtr (0 :: CInt)
                        #{poke CachedIcon, iconId.resourceId}  dataPtr (fromJust maybeResourceId)

            (UpdateCursorRequest' (UniqueId uniqueId) cursorId) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_UPDATE_CURSOR} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.newCursorCacheKey}

                #{poke CachedCursor, cursorKey} dataPtr cursorId

            (InvalidateRectFullyRequest' (UniqueId uniqueId)) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_INVALIDATE_RECT_FULLY} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)

            (ShowWindowRequest' (UniqueId uniqueId)) -> do
                #{poke CCallRequest, reqType}        ptr (#{const REQ_SHOW_WINDOW} :: #{type RequestType})
                #{poke CCallRequest, targetUniqueId} ptr (fromIntegral uniqueId :: CInt)
