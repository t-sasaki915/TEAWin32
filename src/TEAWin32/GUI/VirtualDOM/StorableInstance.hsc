{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TEAWin32.GUI.VirtualDOM.StorableInstance () where

import                          Control.Monad           (when)
import                          Data.Maybe              (fromJust)
import                          Foreign                 (Storable (..), Word32, fillBytes,
                                                         plusPtr)
import {-# SOURCE #-}           TEAWin32.GUI.VirtualDOM

#include "VirtualDOM.h"

instance Storable CCallRequest where
    sizeOf _ = #{size CCallRequest}

    alignment _ = #{alignment CCallRequest}

    peek _ = error "NOT IMPLEMENTED"

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

            (DestroyComponentRequest hwnd) -> do
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

                #{poke CachedIcon, iconType}   dataPtr (fromHaskellCachedIconType iconType)
                #{poke CachedIcon, scaleRatio} dataPtr scaleRatio

                case iconType of
                    StockIcon ->
                        #{poke CachedIcon, iconId.stockIconId} dataPtr (fromJust maybeStockIconId)
                    ResourceIcon ->
                        #{poke CachedIcon, iconId.resourceId}  dataPtr (fromJust maybeResourceId)

            (UpdateCursorRequest' hwnd cursorId) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_UPDATE_CURSOR} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd

                let dataPtr = ptr `plusPtr` #{offset CCallRequest, reqData.newCursorCacheKey}

                #{poke CachedCursor, cursorKey} dataPtr cursorId

            (InvalidateRectFullyRequest hwnd) -> do
                #{poke CCallRequest, reqType}    ptr (#{const REQ_INVALIDATE_RECT_FULLY} :: #{type RequestType})
                #{poke CCallRequest, targetHWND} ptr hwnd

            (CreateWindowRequest _) ->
                error "CreateWindowRequest"

            (CreateButtonRequest _) ->
                error "CreateButtonRequest"

            (UpdateTextRequest _ _) ->
                error "UpdateTextRequest"

            (UpdatePosRequest _ _) ->
                error "UpdatePosRequest"

            (UpdateFontRequest _ _) ->
                error "UpdateFontRequest"

            (UpdateIconRequest _ _) ->
                error "UpdateIconRequest"

            (UpdateCursorRequest _ _) ->
                error "UpdateCursorRequest"
