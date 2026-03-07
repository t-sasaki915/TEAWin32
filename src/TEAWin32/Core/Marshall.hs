module TEAWin32.Core.Marshall (dispatchRenderProcedures) where

import           Control.Monad                  (foldM)
import           Control.Monad.Cont             (ContT (..), evalContT)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Bits                      ((.|.))
import           Data.Maybe                     (isJust)
import           Data.Word                      (Word32)
import           Foreign                        (Ptr, allocaBytes, castPtr,
                                                 fillBytes, pokeByteOff)
import           Foreign.C                      (CInt, CWString)
import qualified TEAWin32.Core.Native           as Native
import qualified TEAWin32.Core.Native.Constants as Native
import           TEAWin32.Core.Types
import           TEAWin32.Core.Util             (whenJust)

dispatchRenderProcedures :: [(UniqueId, RenderProcedure)] -> IO ()
dispatchRenderProcedures procedures = do
    let totalSize = length procedures * Native.size_RenderProcedure

    allocaBytes totalSize $ \ptr -> do
        fillBytes ptr 0 totalSize

        let func deferWindowPosCount (index, procedure) =
                pokeRenderProcedure ptr procedure index >>= \case
                    True  -> pure (deferWindowPosCount + 1)
                    False -> pure deferWindowPosCount

        evalContT $ do
            deferWindowPosCount <- foldM func (0 :: Int) (zip [0..] procedures)

            liftIO $ Native.c_RequestRender (castPtr ptr) (length procedures) deferWindowPosCount

pokeRenderProcedure :: Ptr () -> (UniqueId, RenderProcedure) -> Int -> ContT a IO Bool
pokeRenderProcedure ptr (uniqueId, procedure) procOffset = do
    let offset = procOffset * Native.size_RenderProcedure

        pokeData relativeOffset val = liftIO $ pokeByteOff ptr (offset + relativeOffset) val

    case procedure of
        (CreateWindow className windowStyle maybeParent) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_CREATE_WINDOW
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            case maybeParent of
                Just parent ->
                    pokeData Native.offset_RenderProcedure_procData_createWindowData_newWindowParentUniqueId parent

                Nothing ->
                    pokeData Native.offset_RenderProcedure_procData_createWindowData_newWindowParentUniqueId (0 :: CInt)

            ContT (Native.withCWText className) >>= \className' ->
                pokeData Native.offset_RenderProcedure_procData_createWindowData_newWindowClassName className'

            let (exStyles, styles) = marshallWindowStyle windowStyle

            pokeData Native.offset_RenderProcedure_procData_createWindowData_newWindowExStyles exStyles
            pokeData Native.offset_RenderProcedure_procData_createWindowData_newWindowStyles   styles

            pure False

        (CreateButton parent) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_CREATE_BUTTON
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            pokeData Native.offset_RenderProcedure_procData_newButtonParentUniqueId parent

            pure False

        (SetComponentText newText) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_UPDATE_TEXT
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            ContT (Native.withCWText newText) >>= \newText' ->
                pokeData Native.offset_RenderProcedure_procData_newComponentText newText'

            pure False

        (SetComponentPos maybeNewPos maybeNewSize bringCompToFront) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_UPDATE_POS
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            pokeData Native.offset_RenderProcedure_procData_updatePosData_hasNewLocation        (isJust maybeNewPos)
            pokeData Native.offset_RenderProcedure_procData_updatePosData_hasNewSize            (isJust maybeNewSize)
            pokeData Native.offset_RenderProcedure_procData_updatePosData_bringComponentToFront bringCompToFront

            whenJust maybeNewPos $ \(newX, newY) -> do
                pokeData Native.offset_RenderProcedure_procData_updatePosData_newX newX
                pokeData Native.offset_RenderProcedure_procData_updatePosData_newY newY

            whenJust maybeNewSize $ \(newWidth, newHeight) -> do
                pokeData Native.offset_RenderProcedure_procData_updatePosData_newWidth  newWidth
                pokeData Native.offset_RenderProcedure_procData_updatePosData_newHeight newHeight

            pure True

        (SetComponentFont newFont) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_UPDATE_FONT
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            let fontName = case newFont of
                    DefaultGUIFont    -> "MS Shell Dlg"
                    (Font fName _)    -> fName
                    (Font' fName _ _) -> fName

                fontSize = case newFont of
                    DefaultGUIFont    -> 9
                    (Font _ fSize)    -> fSize
                    (Font' _ fSize _) -> fSize

                italic = case newFont of
                    DefaultGUIFont                   -> False
                    (Font _ _)                       -> False
                    (Font' _ _ (FontSettings i _ _)) -> i

                underline = case newFont of
                    DefaultGUIFont                   -> False
                    (Font _ _)                       -> False
                    (Font' _ _ (FontSettings _ u _)) -> u

                strikeOut = case newFont of
                    DefaultGUIFont                   -> False
                    (Font _ _)                       -> False
                    (Font' _ _ (FontSettings _ _ s)) -> s

            ContT (Native.withCWText fontName) >>= \fontName' ->
                    pokeData Native.offset_RenderProcedure_procData_newFontCacheKey_fontName fontName'

            pokeData Native.offset_RenderProcedure_procData_newFontCacheKey_fontSize    fontSize
            pokeData Native.offset_RenderProcedure_procData_newFontCacheKey_isItalic    italic
            pokeData Native.offset_RenderProcedure_procData_newFontCacheKey_isUnderline underline
            pokeData Native.offset_RenderProcedure_procData_newFontCacheKey_isStrikeOut strikeOut

            pure False

        (SetComponentIcon newIcon) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_UPDATE_ICON
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            case newIcon of
                (IconFromResourceFile rsId) -> do
                    pokeData Native.offset_RenderProcedure_procData_newIconCacheKey_iconType Native.const_RESOURCE_ICON

                    pokeData Native.offset_RenderProcedure_procData_newIconCacheKey_iconId_resourceId (Native.c_MakeIntResourceW (fromIntegral rsId))

                stockIcon -> do
                    pokeData Native.offset_RenderProcedure_procData_newIconCacheKey_iconType Native.const_STOCK_ICON

                    pokeData Native.offset_RenderProcedure_procData_newIconCacheKey_iconId_resourceId (marshallStockIcon stockIcon)

            pure False

        (SetComponentCursor newCursor) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_UPDATE_CURSOR
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            pokeData Native.offset_RenderProcedure_procData_newCursorCacheKey_cursorKey (marshallCursor newCursor)

            pure False

        (SetComponentBackgroundColour _) -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_UPDATE_BACKGROUND_COLOUR
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            -- TODO

            pure False

        DestroyComponent -> do
            pokeData Native.offset_RenderProcedure_procType       Native.const_RENDER_PROC_TYPE_DESTROY_COMPONENT
            pokeData Native.offset_RenderProcedure_targetUniqueId uniqueId

            pure False


marshallWindowStyle :: WindowStyle -> (Word32, Word32)
marshallWindowStyle WindowStyleBorderless      = (0, Native.const_WS_POPUP)
marshallWindowStyle WindowStyleNormal          = (0, Native.const_WS_OVERLAPPEDWINDOW)
marshallWindowStyle WindowStyleBorderlessChild = (0, Native.const_WS_CHILD)
marshallWindowStyle WindowStyleNormalChild     = (0, Native.const_WS_OVERLAPPEDWINDOW .|. Native.const_WS_CHILD)

marshallStockIcon :: Icon -> SHSTOCKICONID
marshallStockIcon stockIcon = case stockIcon of
    IconDocNoAssoc           -> Native.const_SIID_DOCNOASSOC
    IconDocAssoc             -> Native.const_SIID_DOCASSOC
    IconApplication          -> Native.const_SIID_APPLICATION
    IconFolder               -> Native.const_SIID_FOLDER
    IconFolderOpen           -> Native.const_SIID_FOLDEROPEN
    IconDrive525             -> Native.const_SIID_DRIVE525
    IconDrive35              -> Native.const_SIID_DRIVE35
    IconDriveRemove          -> Native.const_SIID_DRIVEREMOVE
    IconDriveFixed           -> Native.const_SIID_DRIVEFIXED
    IconDriveNet             -> Native.const_SIID_DRIVENET
    IconDriveNetDisabled     -> Native.const_SIID_DRIVENETDISABLED
    IconDriveCD              -> Native.const_SIID_DRIVECD
    IconDriveRAM             -> Native.const_SIID_DRIVERAM
    IconWorld                -> Native.const_SIID_WORLD
    IconServer               -> Native.const_SIID_SERVER
    IconPrinter              -> Native.const_SIID_PRINTER
    IconMyNetwork            -> Native.const_SIID_MYNETWORK
    IconFind                 -> Native.const_SIID_FIND
    IconHelp                 -> Native.const_SIID_HELP
    IconShare                -> Native.const_SIID_SHARE
    IconLink                 -> Native.const_SIID_LINK
    IconSlowFile             -> Native.const_SIID_SLOWFILE
    IconRecycler             -> Native.const_SIID_RECYCLER
    IconRecyclerFull         -> Native.const_SIID_RECYCLERFULL
    IconMediaCDAudio         -> Native.const_SIID_MEDIACDAUDIO
    IconLock                 -> Native.const_SIID_LOCK
    IconAutoList             -> Native.const_SIID_AUTOLIST
    IconPrinterNet           -> Native.const_SIID_PRINTERNET
    IconServerShare          -> Native.const_SIID_SERVERSHARE
    IconPrinterFax           -> Native.const_SIID_PRINTERFAX
    IconPrinterFaxNet        -> Native.const_SIID_PRINTERFAXNET
    IconPrinterFile          -> Native.const_SIID_PRINTERFILE
    IconStack                -> Native.const_SIID_STACK
    IconMediaSVCD            -> Native.const_SIID_MEDIASVCD
    IconStuffedFolder        -> Native.const_SIID_STUFFEDFOLDER
    IconDriveUnknown         -> Native.const_SIID_DRIVEUNKNOWN
    IconDriveDVD             -> Native.const_SIID_DRIVEDVD
    IconMediaDVD             -> Native.const_SIID_MEDIADVD
    IconMediaDVDRAM          -> Native.const_SIID_MEDIADVDRAM
    IconMediaDVDRW           -> Native.const_SIID_MEDIADVDRW
    IconMediaDVDR            -> Native.const_SIID_MEDIADVDR
    IconMediaDVDROM          -> Native.const_SIID_MEDIADVDROM
    IconMediaCDAudioPlus     -> Native.const_SIID_MEDIACDAUDIOPLUS
    IconMediaCDRW            -> Native.const_SIID_MEDIACDRW
    IconMediaCDR             -> Native.const_SIID_MEDIACDR
    IconMediaCDBurn          -> Native.const_SIID_MEDIACDBURN
    IconMediaBlankCD         -> Native.const_SIID_MEDIABLANKCD
    IconMediaCDROM           -> Native.const_SIID_MEDIACDROM
    IconAudioFiles           -> Native.const_SIID_AUDIOFILES
    IconImageFiles           -> Native.const_SIID_IMAGEFILES
    IconVideoFiles           -> Native.const_SIID_VIDEOFILES
    IconMixedFiles           -> Native.const_SIID_MIXEDFILES
    IconFolderBack           -> Native.const_SIID_FOLDERBACK
    IconFolderFront          -> Native.const_SIID_FOLDERFRONT
    IconShield               -> Native.const_SIID_SHIELD
    IconWarning              -> Native.const_SIID_WARNING
    IconInfo                 -> Native.const_SIID_INFO
    IconError                -> Native.const_SIID_ERROR
    IconKey                  -> Native.const_SIID_KEY
    IconSoftware             -> Native.const_SIID_SOFTWARE
    IconRename               -> Native.const_SIID_RENAME
    IconDelete               -> Native.const_SIID_DELETE
    IconMediaAudioDVD        -> Native.const_SIID_MEDIAAUDIODVD
    IconMediaMovieDVD        -> Native.const_SIID_MEDIAMOVIEDVD
    IconMediaEnhancedCD      -> Native.const_SIID_MEDIAENHANCEDCD
    IconMediaEnhancedDVD     -> Native.const_SIID_MEDIAENHANCEDDVD
    IconMediaHDDVD           -> Native.const_SIID_MEDIAHDDVD
    IconMediaBluray          -> Native.const_SIID_MEDIABLURAY
    IconMediaVCD             -> Native.const_SIID_MEDIAVCD
    IconMediaDVDPlusR        -> Native.const_SIID_MEDIADVDPLUSR
    IconMediaDVDPlusRW       -> Native.const_SIID_MEDIADVDPLUSRW
    IconDesktopPC            -> Native.const_SIID_DESKTOPPC
    IconMobilePC             -> Native.const_SIID_MOBILEPC
    IconUsers                -> Native.const_SIID_USERS
    IconMediaSmartMedia      -> Native.const_SIID_MEDIASMARTMEDIA
    IconMediaCompactFlash    -> Native.const_SIID_MEDIACOMPACTFLASH
    IconDeviceCellPhone      -> Native.const_SIID_DEVICECELLPHONE
    IconDeviceCamera         -> Native.const_SIID_DEVICECAMERA
    IconDeviceVideoCamera    -> Native.const_SIID_DEVICEVIDEOCAMERA
    IconDeviceAudioPlayer    -> Native.const_SIID_DEVICEAUDIOPLAYER
    IconNetworkConnect       -> Native.const_SIID_NETWORKCONNECT
    IconInternet             -> Native.const_SIID_INTERNET
    IconZipFile              -> Native.const_SIID_ZIPFILE
    IconSettings             -> Native.const_SIID_SETTINGS
    IconDriveHDDVD           -> Native.const_SIID_DRIVEHDDVD
    IconDriveBD              -> Native.const_SIID_DRIVEBD
    IconMediaHDDVDROM        -> Native.const_SIID_MEDIAHDDVDROM
    IconMediaHDDVDR          -> Native.const_SIID_MEDIAHDDVDR
    IconMediaHDDVDRAM        -> Native.const_SIID_MEDIAHDDVDRAM
    IconMediaBDROM           -> Native.const_SIID_MEDIABDROM
    IconMediaBDR             -> Native.const_SIID_MEDIABDR
    IconMediaBDRE            -> Native.const_SIID_MEDIABDRE
    IconClusteredDrive       -> Native.const_SIID_CLUSTEREDDRIVE
    (IconFromResourceFile _) -> undefined

marshallCursor :: Cursor -> CWString
marshallCursor cursor =  Native.c_MakeIntResourceW $ case cursor of
    CursorArrow    -> Native.const_IDC_ARROW
    CursorIBeam    -> Native.const_IDC_IBEAM
    CursorWait     -> Native.const_IDC_WAIT
    CursorCross    -> Native.const_IDC_CROSS
    CursorUparrow  -> Native.const_IDC_UPARROW
    CursorSizeNWSE -> Native.const_IDC_SIZENWSE
    CursorSizeNESW -> Native.const_IDC_SIZENESW
    CursorSizeWE   -> Native.const_IDC_SIZEWE
    CursorSizeNS   -> Native.const_IDC_SIZENS
