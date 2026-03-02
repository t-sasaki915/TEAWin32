module TEAWin32.Core.Marshall (marshallCCallRequest) where

import           Control.Monad                  (when)
import           Control.Monad.Cont             (ContT (..))
import           Data.Maybe                     (fromJust, isJust)
import           Data.Word                      (Word32)
import           Foreign                        (Storable (..), fillBytes,
                                                 fromBool, toBool)
import           Foreign.C                      (CBool, CInt, CWString)
import qualified TEAWin32.Core.Native           as Native
import qualified TEAWin32.Core.Native.Constants as Native
import           TEAWin32.Core.Types

marshallCCallRequest :: CCallRequest -> ContT a IO InternalCCallRequest
marshallCCallRequest (CreateWindowRequest req) =
    ContT (Native.withCWText (newWindowClassName req)) >>= \classNamePtr ->
        pure $ CreateWindowRequest' (newWindowUniqueId req) $
            InternalCreateWindowReq
                { newWindowParentUniqueId' = newWindowParentUniqueId req
                , newWindowClassName'      = classNamePtr
                , newWindowExStyles'       = newWindowExStyles req
                , newWindowStyles'         = newWindowStyles req
                }

marshallCCallRequest (CreateButtonRequest req) =
    pure (CreateButtonRequest' (newButtonUniqueId req) (newButtonParentUniqueId req))

marshallCCallRequest (DestroyComponentRequest target) =
    pure (DestroyComponentRequest' target)

marshallCCallRequest (UpdateTextRequest target newText) =
    ContT (Native.withCWText newText) >>= \newTextPtr ->
        pure (UpdateTextRequest' target newTextPtr)

marshallCCallRequest (UpdatePosRequest target req) =
    pure $ UpdatePosRequest' target $
        InternalUpdatePosReq
            { hasNewLocation'        = fromBool $ isJust (newLocation req)
            , hasNewSize'            = fromBool $ isJust (newSize req)
            , bringComponentToFront' = fromBool $ bringComponentToFront req
            , maybeNewX'             = fst <$> newLocation req
            , maybeNewY'             = snd <$> newLocation req
            , maybeNewWidth'         = fst <$> newSize req
            , maybeNewHeight'        = snd <$> newSize req
            }

marshallCCallRequest (UpdateFontRequest target font) =
    ContT (marshallFont font) >>= \font' ->
        pure (UpdateFontRequest' target font')

marshallCCallRequest (UpdateIconRequest target icon) =
    pure (UpdateIconRequest' target (marshallIcon icon))

marshallCCallRequest (UpdateCursorRequest target cursor) =
    pure (UpdateCursorRequest' target (marshallCursor cursor))

marshallCCallRequest (InvalidateRectFullyRequest target) =
    pure (InvalidateRectFullyRequest' target)

marshallCCallRequest (ShowWindowRequest target) =
    pure (ShowWindowRequest' target)

marshallFont :: Font -> (InternalCachedFont -> IO a) -> IO a
marshallFont DefaultGUIFont func =
    Native.withCWText "MS Shell Dlg" $ \fontNamePtr ->
        func $ InternalCachedFont
            { fontName'    = fontNamePtr
            , fontSize'    = 9
            , isItalic'    = fromBool False
            , isUnderline' = fromBool False
            , isStrikeOut' = fromBool False
            }

marshallFont (Font fontName fontSize) func =
    Native.withCWText fontName $ \fontNamePtr ->
        func $ InternalCachedFont
            { fontName'    = fontNamePtr
            , fontSize'    = fontSize
            , isItalic'    = fromBool False
            , isUnderline' = fromBool False
            , isStrikeOut' = fromBool False
            }

marshallFont (Font' fontName fontSize fontSettings) func =
    Native.withCWText fontName $ \fontNamePtr ->
        func $ InternalCachedFont
            { fontName'    = fontNamePtr
            , fontSize'    = fontSize
            , isItalic'    = fromBool $ isItalic fontSettings
            , isUnderline' = fromBool $ isUnderline fontSettings
            , isStrikeOut' = fromBool $ isStrikeOut fontSettings
            }

marshallIcon :: Icon -> InternalCachedIcon
marshallIcon (IconFromResourceFile iconId) =
    InternalCachedIcon
        { iconType' = ResourceIcon
        , maybeStockIconId = Nothing
        , maybeResourceId = Just (Native.c_MakeIntResourceW $ fromIntegral iconId)
        }
marshallIcon icon =
    InternalCachedIcon
        { iconType'        = StockIcon
        , maybeStockIconId = Just stockIconId
        , maybeResourceId  = Nothing
        }
    where
        stockIconId = case icon of
            IconDocNoAssoc        -> Native.const_SIID_DOCNOASSOC
            IconDocAssoc          -> Native.const_SIID_DOCASSOC
            IconApplication       -> Native.const_SIID_APPLICATION
            IconFolder            -> Native.const_SIID_FOLDER
            IconFolderOpen        -> Native.const_SIID_FOLDEROPEN
            IconDrive525          -> Native.const_SIID_DRIVE525
            IconDrive35           -> Native.const_SIID_DRIVE35
            IconDriveRemove       -> Native.const_SIID_DRIVEREMOVE
            IconDriveFixed        -> Native.const_SIID_DRIVEFIXED
            IconDriveNet          -> Native.const_SIID_DRIVENET
            IconDriveNetDisabled  -> Native.const_SIID_DRIVENETDISABLED
            IconDriveCD           -> Native.const_SIID_DRIVECD
            IconDriveRAM          -> Native.const_SIID_DRIVERAM
            IconWorld             -> Native.const_SIID_WORLD
            IconServer            -> Native.const_SIID_SERVER
            IconPrinter           -> Native.const_SIID_PRINTER
            IconMyNetwork         -> Native.const_SIID_MYNETWORK
            IconFind              -> Native.const_SIID_FIND
            IconHelp              -> Native.const_SIID_HELP
            IconShare             -> Native.const_SIID_SHARE
            IconLink              -> Native.const_SIID_LINK
            IconSlowFile          -> Native.const_SIID_SLOWFILE
            IconRecycler          -> Native.const_SIID_RECYCLER
            IconRecyclerFull      -> Native.const_SIID_RECYCLERFULL
            IconMediaCDAudio      -> Native.const_SIID_MEDIACDAUDIO
            IconLock              -> Native.const_SIID_LOCK
            IconAutoList          -> Native.const_SIID_AUTOLIST
            IconPrinterNet        -> Native.const_SIID_PRINTERNET
            IconServerShare       -> Native.const_SIID_SERVERSHARE
            IconPrinterFax        -> Native.const_SIID_PRINTERFAX
            IconPrinterFaxNet     -> Native.const_SIID_PRINTERFAXNET
            IconPrinterFile       -> Native.const_SIID_PRINTERFILE
            IconStack             -> Native.const_SIID_STACK
            IconMediaSVCD         -> Native.const_SIID_MEDIASVCD
            IconStuffedFolder     -> Native.const_SIID_STUFFEDFOLDER
            IconDriveUnknown      -> Native.const_SIID_DRIVEUNKNOWN
            IconDriveDVD          -> Native.const_SIID_DRIVEDVD
            IconMediaDVD          -> Native.const_SIID_MEDIADVD
            IconMediaDVDRAM       -> Native.const_SIID_MEDIADVDRAM
            IconMediaDVDRW        -> Native.const_SIID_MEDIADVDRW
            IconMediaDVDR         -> Native.const_SIID_MEDIADVDR
            IconMediaDVDROM       -> Native.const_SIID_MEDIADVDROM
            IconMediaCDAudioPlus  -> Native.const_SIID_MEDIACDAUDIOPLUS
            IconMediaCDRW         -> Native.const_SIID_MEDIACDRW
            IconMediaCDR          -> Native.const_SIID_MEDIACDR
            IconMediaCDBurn       -> Native.const_SIID_MEDIACDBURN
            IconMediaBlankCD      -> Native.const_SIID_MEDIABLANKCD
            IconMediaCDROM        -> Native.const_SIID_MEDIACDROM
            IconAudioFiles        -> Native.const_SIID_AUDIOFILES
            IconImageFiles        -> Native.const_SIID_IMAGEFILES
            IconVideoFiles        -> Native.const_SIID_VIDEOFILES
            IconMixedFiles        -> Native.const_SIID_MIXEDFILES
            IconFolderBack        -> Native.const_SIID_FOLDERBACK
            IconFolderFront       -> Native.const_SIID_FOLDERFRONT
            IconShield            -> Native.const_SIID_SHIELD
            IconWarning           -> Native.const_SIID_WARNING
            IconInfo              -> Native.const_SIID_INFO
            IconError             -> Native.const_SIID_ERROR
            IconKey               -> Native.const_SIID_KEY
            IconSoftware          -> Native.const_SIID_SOFTWARE
            IconRename            -> Native.const_SIID_RENAME
            IconDelete            -> Native.const_SIID_DELETE
            IconMediaAudioDVD     -> Native.const_SIID_MEDIAAUDIODVD
            IconMediaMovieDVD     -> Native.const_SIID_MEDIAMOVIEDVD
            IconMediaEnhancedCD   -> Native.const_SIID_MEDIAENHANCEDCD
            IconMediaEnhancedDVD  -> Native.const_SIID_MEDIAENHANCEDDVD
            IconMediaHDDVD        -> Native.const_SIID_MEDIAHDDVD
            IconMediaBluray       -> Native.const_SIID_MEDIABLURAY
            IconMediaVCD          -> Native.const_SIID_MEDIAVCD
            IconMediaDVDPlusR     -> Native.const_SIID_MEDIADVDPLUSR
            IconMediaDVDPlusRW    -> Native.const_SIID_MEDIADVDPLUSRW
            IconDesktopPC         -> Native.const_SIID_DESKTOPPC
            IconMobilePC          -> Native.const_SIID_MOBILEPC
            IconUsers             -> Native.const_SIID_USERS
            IconMediaSmartMedia   -> Native.const_SIID_MEDIASMARTMEDIA
            IconMediaCompactFlash -> Native.const_SIID_MEDIACOMPACTFLASH
            IconDeviceCellPhone   -> Native.const_SIID_DEVICECELLPHONE
            IconDeviceCamera      -> Native.const_SIID_DEVICECAMERA
            IconDeviceVideoCamera -> Native.const_SIID_DEVICEVIDEOCAMERA
            IconDeviceAudioPlayer -> Native.const_SIID_DEVICEAUDIOPLAYER
            IconNetworkConnect    -> Native.const_SIID_NETWORKCONNECT
            IconInternet          -> Native.const_SIID_INTERNET
            IconZipFile           -> Native.const_SIID_ZIPFILE
            IconSettings          -> Native.const_SIID_SETTINGS
            IconDriveHDDVD        -> Native.const_SIID_DRIVEHDDVD
            IconDriveBD           -> Native.const_SIID_DRIVEBD
            IconMediaHDDVDROM     -> Native.const_SIID_MEDIAHDDVDROM
            IconMediaHDDVDR       -> Native.const_SIID_MEDIAHDDVDR
            IconMediaHDDVDRAM     -> Native.const_SIID_MEDIAHDDVDRAM
            IconMediaBDROM        -> Native.const_SIID_MEDIABDROM
            IconMediaBDR          -> Native.const_SIID_MEDIABDR
            IconMediaBDRE         -> Native.const_SIID_MEDIABDRE
            IconClusteredDrive    -> Native.const_SIID_CLUSTEREDDRIVE

marshallCursor :: Cursor -> InternalCachedCursor
marshallCursor cursor = InternalCachedCursor { cursorKey' = Native.c_MakeIntResourceW cur }
    where
        cur = case cursor of
            CursorArrow    -> Native.const_IDC_ARROW
            CursorIBeam    -> Native.const_IDC_IBEAM
            CursorWait     -> Native.const_IDC_WAIT
            CursorCross    -> Native.const_IDC_CROSS
            CursorUparrow  -> Native.const_IDC_UPARROW
            CursorSizeNWSE -> Native.const_IDC_SIZENWSE
            CursorSizeNESW -> Native.const_IDC_SIZENESW
            CursorSizeWE   -> Native.const_IDC_SIZEWE
            CursorSizeNS   -> Native.const_IDC_SIZENS

data InternalCreateWindowReq = InternalCreateWindowReq
    { newWindowParentUniqueId' :: Maybe UniqueId
    , newWindowClassName'      :: CWString
    , newWindowExStyles'       :: Word32
    , newWindowStyles'         :: Word32
    }

instance Storable InternalCreateWindowReq where
    sizeOf _ = Native.size_CreateWindowReq

    alignment _ = Native.alignment_CreateWindowReq

    peek _ = undefined

    poke ptr val = do
        fillBytes ptr 0 Native.size_CreateWindowReq

        case newWindowParentUniqueId' val of
            Just parent ->
                pokeByteOff ptr Native.offset_CreateWindowReq_newWindowParentUniqueId parent

            Nothing ->
                pokeByteOff ptr Native.offset_CreateWindowReq_newWindowParentUniqueId (0 :: CInt)

        pokeByteOff ptr Native.offset_CreateWindowReq_newWindowClassName (newWindowClassName' val)
        pokeByteOff ptr Native.offset_CreateWindowReq_newWindowExStyles  (newWindowExStyles' val)
        pokeByteOff ptr Native.offset_CreateWindowReq_newWindowStyles    (newWindowStyles' val)

data InternalUpdatePosReq = InternalUpdatePosReq
    { hasNewLocation'        :: CBool
    , hasNewSize'            :: CBool
    , bringComponentToFront' :: CBool
    , maybeNewX'             :: Maybe ScalableValue
    , maybeNewY'             :: Maybe ScalableValue
    , maybeNewWidth'         :: Maybe ScalableValue
    , maybeNewHeight'        :: Maybe ScalableValue
    }

instance Storable InternalUpdatePosReq where
    sizeOf _ = Native.size_UpdatePosReq

    alignment _ = Native.alignment_UpdatePosReq

    peek _ = undefined

    poke ptr val = do
        fillBytes ptr 0 Native.size_UpdatePosReq

        pokeByteOff ptr Native.offset_UpdatePosReq_hasNewLocation        (hasNewLocation' val)
        pokeByteOff ptr Native.offset_UpdatePosReq_hasNewSize            (hasNewSize' val)
        pokeByteOff ptr Native.offset_UpdatePosReq_bringComponentToFront (bringComponentToFront' val)

        when (toBool $ hasNewLocation' val) $ do
            pokeByteOff ptr Native.offset_UpdatePosReq_newX (fromJust $ maybeNewX' val)
            pokeByteOff ptr Native.offset_UpdatePosReq_newY (fromJust $ maybeNewY' val)

        when (toBool $ hasNewSize' val) $ do
            pokeByteOff ptr Native.offset_UpdatePosReq_newWidth  (fromJust $ maybeNewWidth' val)
            pokeByteOff ptr Native.offset_UpdatePosReq_newHeight (fromJust $ maybeNewHeight' val)

data InternalCachedFont = InternalCachedFont
    { fontName'    :: CWString
    , fontSize'    :: ScalableValue
    , isItalic'    :: CBool
    , isUnderline' :: CBool
    , isStrikeOut' :: CBool
    }

instance Storable InternalCachedFont where
    sizeOf _ = Native.size_CachedFont

    alignment _ = Native.alignment_CachedFont

    peek _ = undefined

    poke ptr val = do
        fillBytes ptr 0 Native.size_UpdatePosReq

        pokeByteOff ptr Native.offset_CachedFont_fontName    (fontName' val)
        pokeByteOff ptr Native.offset_CachedFont_fontSize    (fontSize' val)
        pokeByteOff ptr Native.offset_CachedFont_isItalic    (isItalic' val)
        pokeByteOff ptr Native.offset_CachedFont_isUnderline (isUnderline' val)
        pokeByteOff ptr Native.offset_CachedFont_isStrikeOut (isStrikeOut' val)

data InternalCachedIcon = InternalCachedIcon
    { iconType'        :: IconType
    , maybeStockIconId :: Maybe SHSTOCKICONID
    , maybeResourceId  :: Maybe CWString
    }

instance Storable InternalCachedIcon where
    sizeOf _ = Native.size_CachedIcon

    alignment _ = Native.alignment_CachedIcon

    peek _ = undefined

    poke ptr val = do
        fillBytes ptr 0 Native.size_UpdatePosReq

        case iconType' val of
            StockIcon -> do
                pokeByteOff ptr Native.offset_CachedIcon_iconType           Native.const_STOCK_ICON
                pokeByteOff ptr Native.offset_CachedIcon_iconId_stockIconId (fromJust $ maybeStockIconId val)
            ResourceIcon -> do
                pokeByteOff ptr Native.offset_CachedIcon_iconType          Native.const_RESOURCE_ICON
                pokeByteOff ptr Native.offset_CachedIcon_iconId_resourceId (fromJust $ maybeResourceId val)


newtype InternalCachedCursor = InternalCachedCursor
    { cursorKey' :: CWString
    }

instance Storable InternalCachedCursor where
    sizeOf _ = Native.size_CachedCursor

    alignment _ = Native.alignment_CachedCursor

    peek _ = undefined

    poke ptr val = do
        fillBytes ptr 0 Native.size_UpdatePosReq

        pokeByteOff ptr Native.offset_CachedCursor_cursorKey (cursorKey' val)

data InternalCCallRequest = CreateWindowRequest'        UniqueId InternalCreateWindowReq
                          | CreateButtonRequest'        UniqueId UniqueId
                          | DestroyComponentRequest'    UniqueId
                          | UpdateTextRequest'          UniqueId CWString
                          | UpdatePosRequest'           UniqueId InternalUpdatePosReq
                          | UpdateFontRequest'          UniqueId InternalCachedFont
                          | UpdateIconRequest'          UniqueId InternalCachedIcon
                          | UpdateCursorRequest'        UniqueId InternalCachedCursor
                          | InvalidateRectFullyRequest' UniqueId
                          | ShowWindowRequest'          UniqueId

instance Storable InternalCCallRequest where
    sizeOf _ = Native.size_CCallRequest

    alignment _ = Native.alignment_CCallRequest

    peek _ = undefined

    poke ptr val = do
        fillBytes ptr 0 Native.size_CCallRequest

        case val of
            (CreateWindowRequest' uniqueId req) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_CREATE_WINDOW
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

                pokeByteOff ptr Native.offset_CCallRequest_reqData_createWindowReq req

            (CreateButtonRequest' uniqueId parentId) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_CREATE_BUTTON
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

                pokeByteOff ptr Native.offset_CCallRequest_reqData_newButtonParentUniqueId parentId

            (DestroyComponentRequest' uniqueId) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_DESTROY_COMPONENT
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

            (UpdateTextRequest' uniqueId newText) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_UPDATE_TEXT
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

                pokeByteOff ptr Native.offset_CCallRequest_reqData_newComponentText newText

            (UpdatePosRequest' uniqueId req) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_UPDATE_POS
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

                pokeByteOff ptr Native.offset_CCallRequest_reqData_updatePosReq req

            (UpdateFontRequest' uniqueId req) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_UPDATE_FONT
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

                pokeByteOff ptr Native.offset_CCallRequest_reqData_newFontCacheKey req

            (UpdateIconRequest' uniqueId req) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_UPDATE_ICON
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

                pokeByteOff ptr Native.offset_CCallRequest_reqData_newIconCacheKey req

            (UpdateCursorRequest' uniqueId req) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_UPDATE_CURSOR
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

                pokeByteOff ptr Native.offset_CCallRequest_reqData_newCursorCacheKey req

            (InvalidateRectFullyRequest' uniqueId) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_INVALIDATE_RECT_FULLY
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

            (ShowWindowRequest' uniqueId) -> do
                pokeByteOff ptr Native.offset_CCallRequest_reqType        Native.const_REQ_SHOW_WINDOW
                pokeByteOff ptr Native.offset_CCallRequest_targetUniqueId uniqueId

