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

offset_TEAWin32Settings_isDebugMode :: Int
offset_TEAWin32Settings_isDebugMode = #offset TEAWin32Settings, isDebugMode

size_ScalableValue :: Int
size_ScalableValue = #size ScalableValue

alignment_ScalableValue :: Int
alignment_ScalableValue = #alignment ScalableValue

offset_ScalableValue_value :: Int
offset_ScalableValue_value = #offset ScalableValue, value

offset_ScalableValue_isScalable :: Int
offset_ScalableValue_isScalable = #offset ScalableValue, isScalable

size_RenderProcedure :: Int
size_RenderProcedure = #size RenderProcedure

offset_RenderProcedure_procType :: Int
offset_RenderProcedure_procType = #offset RenderProcedure, procType

offset_RenderProcedure_targetUniqueId :: Int
offset_RenderProcedure_targetUniqueId = #offset RenderProcedure, targetUniqueId

offset_RenderProcedure_procData_createWindowData_newWindowParentUniqueId :: Int
offset_RenderProcedure_procData_createWindowData_newWindowParentUniqueId = #offset RenderProcedure, procData.createWindowData.newWindowParentUniqueId

offset_RenderProcedure_procData_createWindowData_newWindowClassName :: Int
offset_RenderProcedure_procData_createWindowData_newWindowClassName = #offset RenderProcedure, procData.createWindowData.newWindowClassName

offset_RenderProcedure_procData_createWindowData_newWindowExStyles :: Int
offset_RenderProcedure_procData_createWindowData_newWindowExStyles = #offset RenderProcedure, procData.createWindowData.newWindowExStyles

offset_RenderProcedure_procData_createWindowData_newWindowStyles :: Int
offset_RenderProcedure_procData_createWindowData_newWindowStyles = #offset RenderProcedure, procData.createWindowData.newWindowStyles

offset_RenderProcedure_procData_newButtonParentUniqueId :: Int
offset_RenderProcedure_procData_newButtonParentUniqueId = #offset RenderProcedure, procData.newButtonParentUniqueId

offset_RenderProcedure_procData_newComponentText :: Int
offset_RenderProcedure_procData_newComponentText = #offset RenderProcedure, procData.newComponentText

offset_RenderProcedure_procData_updatePosData_hasNewLocation :: Int
offset_RenderProcedure_procData_updatePosData_hasNewLocation = #offset RenderProcedure, procData.updatePosData.hasNewLocation

offset_RenderProcedure_procData_updatePosData_hasNewSize :: Int
offset_RenderProcedure_procData_updatePosData_hasNewSize = #offset RenderProcedure, procData.updatePosData.hasNewSize

offset_RenderProcedure_procData_updatePosData_bringComponentToFront :: Int
offset_RenderProcedure_procData_updatePosData_bringComponentToFront = #offset RenderProcedure, procData.updatePosData.bringComponentToFront

offset_RenderProcedure_procData_updatePosData_newX :: Int
offset_RenderProcedure_procData_updatePosData_newX = #offset RenderProcedure, procData.updatePosData.newX

offset_RenderProcedure_procData_updatePosData_newY :: Int
offset_RenderProcedure_procData_updatePosData_newY = #offset RenderProcedure, procData.updatePosData.newY

offset_RenderProcedure_procData_updatePosData_newWidth :: Int
offset_RenderProcedure_procData_updatePosData_newWidth = #offset RenderProcedure, procData.updatePosData.newWidth

offset_RenderProcedure_procData_updatePosData_newHeight :: Int
offset_RenderProcedure_procData_updatePosData_newHeight = #offset RenderProcedure, procData.updatePosData.newHeight

offset_RenderProcedure_procData_newFontCacheKey_fontName :: Int
offset_RenderProcedure_procData_newFontCacheKey_fontName = #offset RenderProcedure, procData.newFontCacheKey.fontName

offset_RenderProcedure_procData_newFontCacheKey_fontSize :: Int
offset_RenderProcedure_procData_newFontCacheKey_fontSize = #offset RenderProcedure, procData.newFontCacheKey.fontSize

offset_RenderProcedure_procData_newFontCacheKey_isItalic :: Int
offset_RenderProcedure_procData_newFontCacheKey_isItalic = #offset RenderProcedure, procData.newFontCacheKey.isItalic

offset_RenderProcedure_procData_newFontCacheKey_isUnderline :: Int
offset_RenderProcedure_procData_newFontCacheKey_isUnderline = #offset RenderProcedure, procData.newFontCacheKey.isUnderline

offset_RenderProcedure_procData_newFontCacheKey_isStrikeOut :: Int
offset_RenderProcedure_procData_newFontCacheKey_isStrikeOut = #offset RenderProcedure, procData.newFontCacheKey.isStrikeOut

offset_RenderProcedure_procData_newIconCacheKey_iconType :: Int
offset_RenderProcedure_procData_newIconCacheKey_iconType = #offset RenderProcedure, procData.newIconCacheKey.iconType

offset_RenderProcedure_procData_newIconCacheKey_iconId_stockIconId :: Int
offset_RenderProcedure_procData_newIconCacheKey_iconId_stockIconId = #offset RenderProcedure, procData.newIconCacheKey.iconId.stockIconId

offset_RenderProcedure_procData_newIconCacheKey_iconId_resourceId :: Int
offset_RenderProcedure_procData_newIconCacheKey_iconId_resourceId = #offset RenderProcedure, procData.newIconCacheKey.iconId.resourceId

offset_RenderProcedure_procData_newCursorCacheKey_cursorKey :: Int
offset_RenderProcedure_procData_newCursorCacheKey_cursorKey = #offset RenderProcedure, procData.newCursorCacheKey.cursorKey

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

const_RENDER_PROC_TYPE_CREATE_WINDOW :: CInt
const_RENDER_PROC_TYPE_CREATE_WINDOW = #const RENDER_PROC_TYPE_CREATE_WINDOW

const_RENDER_PROC_TYPE_CREATE_BUTTON :: CInt
const_RENDER_PROC_TYPE_CREATE_BUTTON = #const RENDER_PROC_TYPE_CREATE_BUTTON

const_RENDER_PROC_TYPE_DESTROY_COMPONENT :: CInt
const_RENDER_PROC_TYPE_DESTROY_COMPONENT = #const RENDER_PROC_TYPE_DESTROY_COMPONENT

const_RENDER_PROC_TYPE_UPDATE_TEXT :: CInt
const_RENDER_PROC_TYPE_UPDATE_TEXT = #const RENDER_PROC_TYPE_UPDATE_TEXT

const_RENDER_PROC_TYPE_UPDATE_POS :: CInt
const_RENDER_PROC_TYPE_UPDATE_POS = #const RENDER_PROC_TYPE_UPDATE_POS

const_RENDER_PROC_TYPE_UPDATE_FONT :: CInt
const_RENDER_PROC_TYPE_UPDATE_FONT = #const RENDER_PROC_TYPE_UPDATE_FONT

const_RENDER_PROC_TYPE_UPDATE_ICON :: CInt
const_RENDER_PROC_TYPE_UPDATE_ICON = #const RENDER_PROC_TYPE_UPDATE_ICON

const_RENDER_PROC_TYPE_UPDATE_CURSOR :: CInt
const_RENDER_PROC_TYPE_UPDATE_CURSOR = #const RENDER_PROC_TYPE_UPDATE_CURSOR

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
