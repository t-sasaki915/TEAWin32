{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TEAWin32.Core.Native.Constants where

import           Data.Word (Word16, Word32)
import           Foreign.C (CInt)

#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "TEAWin32.h"
#include "VirtualDOM.h"

#include <windows.h>

size_TEAWin32Settings :: Int
size_TEAWin32Settings = #size TEAWin32Settings

alignment_TEAWin32Settings :: Int
alignment_TEAWin32Settings = #alignment TEAWin32Settings

offset_TEAWin32Settings_useVisualStyles :: Int
offset_TEAWin32Settings_useVisualStyles = #offset TEAWin32Settings, useVisualStyles

size_ScalableValue :: Int
size_ScalableValue = #size ScalableValue

alignment_ScalableValue :: Int
alignment_ScalableValue = #alignment ScalableValue

offset_ScalableValue_value :: Int
offset_ScalableValue_value = #offset ScalableValue, value

offset_ScalableValue_isScalable :: Int
offset_ScalableValue_isScalable = #offset ScalableValue, isScalable

size_CreateWindowReq :: Int
size_CreateWindowReq = #size CreateWindowReq

alignment_CreateWindowReq :: Int
alignment_CreateWindowReq = #alignment CreateWindowReq

offset_CreateWindowReq_newWindowParentUniqueId :: Int
offset_CreateWindowReq_newWindowParentUniqueId = #offset CreateWindowReq, newWindowParentUniqueId

offset_CreateWindowReq_newWindowClassName :: Int
offset_CreateWindowReq_newWindowClassName = #offset CreateWindowReq, newWindowClassName

offset_CreateWindowReq_newWindowExStyles :: Int
offset_CreateWindowReq_newWindowExStyles = #offset CreateWindowReq, newWindowExStyles

offset_CreateWindowReq_newWindowStyles :: Int
offset_CreateWindowReq_newWindowStyles = #offset CreateWindowReq, newWindowStyles

size_UpdatePosReq :: Int
size_UpdatePosReq = #size UpdatePosReq

alignment_UpdatePosReq :: Int
alignment_UpdatePosReq = #alignment UpdatePosReq

offset_UpdatePosReq_hasNewLocation :: Int
offset_UpdatePosReq_hasNewLocation = #offset UpdatePosReq, hasNewLocation

offset_UpdatePosReq_hasNewSize :: Int
offset_UpdatePosReq_hasNewSize = #offset UpdatePosReq, hasNewSize

offset_UpdatePosReq_bringComponentToFront :: Int
offset_UpdatePosReq_bringComponentToFront = #offset UpdatePosReq, bringComponentToFront

offset_UpdatePosReq_newX :: Int
offset_UpdatePosReq_newX = #offset UpdatePosReq, newX

offset_UpdatePosReq_newY :: Int
offset_UpdatePosReq_newY = #offset UpdatePosReq, newY

offset_UpdatePosReq_newWidth :: Int
offset_UpdatePosReq_newWidth = #offset UpdatePosReq, newWidth

offset_UpdatePosReq_newHeight :: Int
offset_UpdatePosReq_newHeight = #offset UpdatePosReq, newHeight

size_CachedFont :: Int
size_CachedFont = #size CachedFont

alignment_CachedFont :: Int
alignment_CachedFont = #alignment CachedFont

offset_CachedFont_fontName :: Int
offset_CachedFont_fontName = #offset CachedFont, fontName

offset_CachedFont_fontSize :: Int
offset_CachedFont_fontSize = #offset CachedFont, fontSize

offset_CachedFont_isItalic :: Int
offset_CachedFont_isItalic = #offset CachedFont, isItalic

offset_CachedFont_isUnderline :: Int
offset_CachedFont_isUnderline = #offset CachedFont, isUnderline

offset_CachedFont_isStrikeOut :: Int
offset_CachedFont_isStrikeOut = #offset CachedFont, isStrikeOut

size_CachedCursor :: Int
size_CachedCursor = #size CachedCursor

alignment_CachedCursor :: Int
alignment_CachedCursor = #alignment CachedCursor

offset_CachedCursor_cursorKey :: Int
offset_CachedCursor_cursorKey = #offset CachedCursor, cursorKey

size_CachedIcon :: Int
size_CachedIcon = #size CachedIcon

alignment_CachedIcon :: Int
alignment_CachedIcon = #alignment CachedIcon

offset_CachedIcon_iconType :: Int
offset_CachedIcon_iconType = #offset CachedIcon, iconType

offset_CachedIcon_iconId_stockIconId :: Int
offset_CachedIcon_iconId_stockIconId = #offset CachedIcon, iconId.stockIconId

offset_CachedIcon_iconId_resourceId :: Int
offset_CachedIcon_iconId_resourceId = #offset CachedIcon, iconId.resourceId

size_CCallRequest :: Int
size_CCallRequest = #size CCallRequest

alignment_CCallRequest :: Int
alignment_CCallRequest = #alignment CCallRequest

offset_CCallRequest_reqType :: Int
offset_CCallRequest_reqType = #offset CCallRequest, reqType

offset_CCallRequest_targetUniqueId :: Int
offset_CCallRequest_targetUniqueId = #offset CCallRequest, targetUniqueId

offset_CCallRequest_reqData_createWindowReq :: Int
offset_CCallRequest_reqData_createWindowReq = #offset CCallRequest, reqData.createWindowReq

offset_CCallRequest_reqData_newButtonParentUniqueId :: Int
offset_CCallRequest_reqData_newButtonParentUniqueId = #offset CCallRequest, reqData.newButtonParentUniqueId

offset_CCallRequest_reqData_newComponentText :: Int
offset_CCallRequest_reqData_newComponentText = #offset CCallRequest, reqData.newComponentText

offset_CCallRequest_reqData_updatePosReq :: Int
offset_CCallRequest_reqData_updatePosReq = #offset CCallRequest, reqData.updatePosReq

offset_CCallRequest_reqData_newFontCacheKey :: Int
offset_CCallRequest_reqData_newFontCacheKey = #offset CCallRequest, reqData.newFontCacheKey

offset_CCallRequest_reqData_newIconCacheKey :: Int
offset_CCallRequest_reqData_newIconCacheKey = #offset CCallRequest, reqData.newIconCacheKey

offset_CCallRequest_reqData_newCursorCacheKey :: Int
offset_CCallRequest_reqData_newCursorCacheKey = #offset CCallRequest, reqData.newCursorCacheKey

size_EventQueueEntry :: Int
size_EventQueueEntry = #size EventQueueEntry

alignment_EventQueueEntry :: Int
alignment_EventQueueEntry = #alignment EventQueueEntry

offset_EventQueueEntry_eventType :: Int
offset_EventQueueEntry_eventType = #offset EventQueueEntry, eventType

offset_EventQueueEntry_eventData_fatalErrorEventData_errorType :: Int
offset_EventQueueEntry_eventData_fatalErrorEventData_errorType = #offset EventQueueEntry, eventData.fatalErrorEventData.errorType

offset_EventQueueEntry_eventData_fatalErrorEventData_errorCode :: Int
offset_EventQueueEntry_eventData_fatalErrorEventData_errorCode = #offset EventQueueEntry, eventData.fatalErrorEventData.errorCode

offset_EventQueueEntry_eventData_fatalErrorEventData_errorLocation :: Int
offset_EventQueueEntry_eventData_fatalErrorEventData_errorLocation = #offset EventQueueEntry, eventData.fatalErrorEventData.errorLocation

const_RESOURCE_ICON :: CInt
const_RESOURCE_ICON = #const RESOURCE_ICON

const_STOCK_ICON :: CInt
const_STOCK_ICON = #const STOCK_ICON

const_REQ_CREATE_WINDOW :: CInt
const_REQ_CREATE_WINDOW = #const REQ_CREATE_WINDOW

const_REQ_CREATE_BUTTON :: CInt
const_REQ_CREATE_BUTTON = #const REQ_CREATE_BUTTON

const_REQ_DESTROY_COMPONENT :: CInt
const_REQ_DESTROY_COMPONENT = #const REQ_DESTROY_COMPONENT

const_REQ_UPDATE_TEXT :: CInt
const_REQ_UPDATE_TEXT = #const REQ_UPDATE_TEXT

const_REQ_UPDATE_POS :: CInt
const_REQ_UPDATE_POS = #const REQ_UPDATE_POS

const_REQ_UPDATE_FONT :: CInt
const_REQ_UPDATE_FONT = #const REQ_UPDATE_FONT

const_REQ_UPDATE_ICON :: CInt
const_REQ_UPDATE_ICON = #const REQ_UPDATE_ICON

const_REQ_UPDATE_CURSOR :: CInt
const_REQ_UPDATE_CURSOR = #const REQ_UPDATE_CURSOR

const_REQ_INVALIDATE_RECT_FULLY :: CInt
const_REQ_INVALIDATE_RECT_FULLY = #const REQ_INVALIDATE_RECT_FULLY

const_REQ_SHOW_WINDOW :: CInt
const_REQ_SHOW_WINDOW = #const REQ_SHOW_WINDOW

const_SIID_DOCNOASSOC :: Word32
const_SIID_DOCNOASSOC = #const SIID_DOCNOASSOC

const_SIID_DOCASSOC :: Word32
const_SIID_DOCASSOC = #const SIID_DOCASSOC

const_SIID_APPLICATION :: Word32
const_SIID_APPLICATION = #const SIID_APPLICATION

const_SIID_FOLDER :: Word32
const_SIID_FOLDER = #const SIID_FOLDER

const_SIID_FOLDEROPEN :: Word32
const_SIID_FOLDEROPEN = #const SIID_FOLDEROPEN

const_SIID_DRIVE525 :: Word32
const_SIID_DRIVE525 = #const SIID_DRIVE525

const_SIID_DRIVE35 :: Word32
const_SIID_DRIVE35 = #const SIID_DRIVE35

const_SIID_DRIVEREMOVE :: Word32
const_SIID_DRIVEREMOVE = #const SIID_DRIVEREMOVE

const_SIID_DRIVEFIXED :: Word32
const_SIID_DRIVEFIXED = #const SIID_DRIVEFIXED

const_SIID_DRIVENET :: Word32
const_SIID_DRIVENET = #const SIID_DRIVENET

const_SIID_DRIVENETDISABLED :: Word32
const_SIID_DRIVENETDISABLED = #const SIID_DRIVENETDISABLED

const_SIID_DRIVECD :: Word32
const_SIID_DRIVECD = #const SIID_DRIVECD

const_SIID_DRIVERAM :: Word32
const_SIID_DRIVERAM = #const SIID_DRIVERAM

const_SIID_WORLD :: Word32
const_SIID_WORLD = #const SIID_WORLD

const_SIID_SERVER :: Word32
const_SIID_SERVER = #const SIID_SERVER

const_SIID_PRINTER :: Word32
const_SIID_PRINTER = #const SIID_PRINTER

const_SIID_MYNETWORK :: Word32
const_SIID_MYNETWORK = #const SIID_MYNETWORK

const_SIID_FIND :: Word32
const_SIID_FIND = #const SIID_FIND

const_SIID_HELP :: Word32
const_SIID_HELP = #const SIID_HELP

const_SIID_SHARE :: Word32
const_SIID_SHARE = #const SIID_SHARE

const_SIID_LINK :: Word32
const_SIID_LINK = #const SIID_LINK

const_SIID_SLOWFILE :: Word32
const_SIID_SLOWFILE = #const SIID_SLOWFILE

const_SIID_RECYCLER :: Word32
const_SIID_RECYCLER = #const SIID_RECYCLER

const_SIID_RECYCLERFULL :: Word32
const_SIID_RECYCLERFULL = #const SIID_RECYCLERFULL

const_SIID_MEDIACDAUDIO :: Word32
const_SIID_MEDIACDAUDIO = #const SIID_MEDIACDAUDIO

const_SIID_LOCK :: Word32
const_SIID_LOCK = #const SIID_LOCK

const_SIID_AUTOLIST :: Word32
const_SIID_AUTOLIST = #const SIID_AUTOLIST

const_SIID_PRINTERNET :: Word32
const_SIID_PRINTERNET = #const SIID_PRINTERNET

const_SIID_SERVERSHARE :: Word32
const_SIID_SERVERSHARE = #const SIID_SERVERSHARE

const_SIID_PRINTERFAX :: Word32
const_SIID_PRINTERFAX = #const SIID_PRINTERFAX

const_SIID_PRINTERFAXNET :: Word32
const_SIID_PRINTERFAXNET = #const SIID_PRINTERFAXNET

const_SIID_PRINTERFILE :: Word32
const_SIID_PRINTERFILE = #const SIID_PRINTERFILE

const_SIID_STACK :: Word32
const_SIID_STACK = #const SIID_STACK

const_SIID_MEDIASVCD :: Word32
const_SIID_MEDIASVCD = #const SIID_MEDIASVCD

const_SIID_STUFFEDFOLDER :: Word32
const_SIID_STUFFEDFOLDER = #const SIID_STUFFEDFOLDER

const_SIID_DRIVEUNKNOWN :: Word32
const_SIID_DRIVEUNKNOWN = #const SIID_DRIVEUNKNOWN

const_SIID_DRIVEDVD :: Word32
const_SIID_DRIVEDVD = #const SIID_DRIVEDVD

const_SIID_MEDIADVD :: Word32
const_SIID_MEDIADVD = #const SIID_MEDIADVD

const_SIID_MEDIADVDRAM :: Word32
const_SIID_MEDIADVDRAM = #const SIID_MEDIADVDRAM

const_SIID_MEDIADVDRW :: Word32
const_SIID_MEDIADVDRW = #const SIID_MEDIADVDRW

const_SIID_MEDIADVDR :: Word32
const_SIID_MEDIADVDR = #const SIID_MEDIADVDR

const_SIID_MEDIADVDROM :: Word32
const_SIID_MEDIADVDROM = #const SIID_MEDIADVDROM

const_SIID_MEDIACDAUDIOPLUS :: Word32
const_SIID_MEDIACDAUDIOPLUS = #const SIID_MEDIACDAUDIOPLUS

const_SIID_MEDIACDRW :: Word32
const_SIID_MEDIACDRW = #const SIID_MEDIACDRW

const_SIID_MEDIACDR :: Word32
const_SIID_MEDIACDR = #const SIID_MEDIACDR

const_SIID_MEDIACDBURN :: Word32
const_SIID_MEDIACDBURN = #const SIID_MEDIACDBURN

const_SIID_MEDIABLANKCD :: Word32
const_SIID_MEDIABLANKCD = #const SIID_MEDIABLANKCD

const_SIID_MEDIACDROM :: Word32
const_SIID_MEDIACDROM = #const SIID_MEDIACDROM

const_SIID_AUDIOFILES :: Word32
const_SIID_AUDIOFILES = #const SIID_AUDIOFILES

const_SIID_IMAGEFILES :: Word32
const_SIID_IMAGEFILES = #const SIID_IMAGEFILES

const_SIID_VIDEOFILES :: Word32
const_SIID_VIDEOFILES = #const SIID_VIDEOFILES

const_SIID_MIXEDFILES :: Word32
const_SIID_MIXEDFILES = #const SIID_MIXEDFILES

const_SIID_FOLDERBACK :: Word32
const_SIID_FOLDERBACK = #const SIID_FOLDERBACK

const_SIID_FOLDERFRONT :: Word32
const_SIID_FOLDERFRONT = #const SIID_FOLDERFRONT

const_SIID_SHIELD :: Word32
const_SIID_SHIELD = #const SIID_SHIELD

const_SIID_WARNING :: Word32
const_SIID_WARNING = #const SIID_WARNING

const_SIID_INFO :: Word32
const_SIID_INFO = #const SIID_INFO

const_SIID_ERROR :: Word32
const_SIID_ERROR = #const SIID_ERROR

const_SIID_KEY :: Word32
const_SIID_KEY = #const SIID_KEY

const_SIID_SOFTWARE :: Word32
const_SIID_SOFTWARE = #const SIID_SOFTWARE

const_SIID_RENAME :: Word32
const_SIID_RENAME = #const SIID_RENAME

const_SIID_DELETE :: Word32
const_SIID_DELETE = #const SIID_DELETE

const_SIID_MEDIAAUDIODVD :: Word32
const_SIID_MEDIAAUDIODVD = #const SIID_MEDIAAUDIODVD

const_SIID_MEDIAMOVIEDVD :: Word32
const_SIID_MEDIAMOVIEDVD = #const SIID_MEDIAMOVIEDVD

const_SIID_MEDIAENHANCEDCD :: Word32
const_SIID_MEDIAENHANCEDCD = #const SIID_MEDIAENHANCEDCD

const_SIID_MEDIAENHANCEDDVD :: Word32
const_SIID_MEDIAENHANCEDDVD = #const SIID_MEDIAENHANCEDDVD

const_SIID_MEDIAHDDVD :: Word32
const_SIID_MEDIAHDDVD = #const SIID_MEDIAHDDVD

const_SIID_MEDIABLURAY :: Word32
const_SIID_MEDIABLURAY = #const SIID_MEDIABLURAY

const_SIID_MEDIAVCD :: Word32
const_SIID_MEDIAVCD = #const SIID_MEDIAVCD

const_SIID_MEDIADVDPLUSR :: Word32
const_SIID_MEDIADVDPLUSR = #const SIID_MEDIADVDPLUSR

const_SIID_MEDIADVDPLUSRW :: Word32
const_SIID_MEDIADVDPLUSRW = #const SIID_MEDIADVDPLUSRW

const_SIID_DESKTOPPC :: Word32
const_SIID_DESKTOPPC = #const SIID_DESKTOPPC

const_SIID_MOBILEPC :: Word32
const_SIID_MOBILEPC = #const SIID_MOBILEPC

const_SIID_USERS :: Word32
const_SIID_USERS = #const SIID_USERS

const_SIID_MEDIASMARTMEDIA :: Word32
const_SIID_MEDIASMARTMEDIA = #const SIID_MEDIASMARTMEDIA

const_SIID_MEDIACOMPACTFLASH :: Word32
const_SIID_MEDIACOMPACTFLASH = #const SIID_MEDIACOMPACTFLASH

const_SIID_DEVICECELLPHONE :: Word32
const_SIID_DEVICECELLPHONE = #const SIID_DEVICECELLPHONE

const_SIID_DEVICECAMERA :: Word32
const_SIID_DEVICECAMERA = #const SIID_DEVICECAMERA

const_SIID_DEVICEVIDEOCAMERA :: Word32
const_SIID_DEVICEVIDEOCAMERA = #const SIID_DEVICEVIDEOCAMERA

const_SIID_DEVICEAUDIOPLAYER :: Word32
const_SIID_DEVICEAUDIOPLAYER = #const SIID_DEVICEAUDIOPLAYER

const_SIID_NETWORKCONNECT :: Word32
const_SIID_NETWORKCONNECT = #const SIID_NETWORKCONNECT

const_SIID_INTERNET :: Word32
const_SIID_INTERNET = #const SIID_INTERNET

const_SIID_ZIPFILE :: Word32
const_SIID_ZIPFILE = #const SIID_ZIPFILE

const_SIID_SETTINGS :: Word32
const_SIID_SETTINGS = #const SIID_SETTINGS

const_SIID_DRIVEHDDVD :: Word32
const_SIID_DRIVEHDDVD = #const SIID_DRIVEHDDVD

const_SIID_DRIVEBD :: Word32
const_SIID_DRIVEBD = #const SIID_DRIVEBD

const_SIID_MEDIAHDDVDROM :: Word32
const_SIID_MEDIAHDDVDROM = #const SIID_MEDIAHDDVDROM

const_SIID_MEDIAHDDVDR :: Word32
const_SIID_MEDIAHDDVDR = #const SIID_MEDIAHDDVDR

const_SIID_MEDIAHDDVDRAM :: Word32
const_SIID_MEDIAHDDVDRAM = #const SIID_MEDIAHDDVDRAM

const_SIID_MEDIABDROM :: Word32
const_SIID_MEDIABDROM = #const SIID_MEDIABDROM

const_SIID_MEDIABDR :: Word32
const_SIID_MEDIABDR = #const SIID_MEDIABDR

const_SIID_MEDIABDRE :: Word32
const_SIID_MEDIABDRE = #const SIID_MEDIABDRE

const_SIID_CLUSTEREDDRIVE :: Word32
const_SIID_CLUSTEREDDRIVE = #const SIID_CLUSTEREDDRIVE

const_SIID_MAX_ICONS :: Word32
const_SIID_MAX_ICONS = #const SIID_MAX_ICONS

const_IDC_ARROW :: Word16
const_IDC_ARROW = #const IDC_ARROW

const_IDC_IBEAM :: Word16
const_IDC_IBEAM = #const IDC_IBEAM

const_IDC_WAIT :: Word16
const_IDC_WAIT = #const IDC_WAIT

const_IDC_CROSS :: Word16
const_IDC_CROSS = #const IDC_CROSS

const_IDC_UPARROW :: Word16
const_IDC_UPARROW = #const IDC_UPARROW

const_IDC_SIZENWSE :: Word16
const_IDC_SIZENWSE = #const IDC_SIZENWSE

const_IDC_SIZENESW :: Word16
const_IDC_SIZENESW = #const IDC_SIZENESW

const_IDC_SIZEWE :: Word16
const_IDC_SIZEWE = #const IDC_SIZEWE

const_IDC_SIZENS :: Word16
const_IDC_SIZENS = #const IDC_SIZENS

const_CW_USEDEFAULT :: CInt
const_CW_USEDEFAULT = #const CW_USEDEFAULT

const_WS_POPUP :: Word32
const_WS_POPUP = #const WS_POPUP

const_WS_OVERLAPPEDWINDOW :: Word32
const_WS_OVERLAPPEDWINDOW = #const WS_OVERLAPPEDWINDOW

const_WS_CHILD :: Word32
const_WS_CHILD = #const WS_CHILD

pattern EventTypeTestEvent :: CInt
pattern EventTypeTestEvent = #const EVENT_TYPE_TEST_EVENT

pattern EventTypeFatalError :: CInt
pattern EventTypeFatalError = #const EVENT_TYPE_FATAL_ERROR

{-# COMPLETE EventTypeTestEvent, EventTypeFatalError #-}
