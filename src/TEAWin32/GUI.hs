{-# LANGUAGE InstanceSigs #-}
module TEAWin32.GUI
    ( UniqueId (..)
    , WindowStyle (..)
    , IconType (..)
    , Icon (..)
    , Cursor (..)
    , Font (..)
    , ScalableValue (..)
    , CanBeRawValue (..)
    , toWin32WindowStyle
    , getIconType
    , toWin32Icon
    , toStockIconId
    , toWin32Cursor
    ) where

import           Data.Bits                 ((.|.))
import           Data.Text                 (Text)
import qualified Graphics.Win32            as Win32
import           TEAWin32.Exception        (TEAWin32Error (InternalTEAWin32Error),
                                            errorTEAWin32)
import qualified TEAWin32.Internal.Foreign as Win32

newtype UniqueId = UniqueId Int deriving (Show, Eq, Ord)

data WindowStyle = WindowStyleBorderless
                 | WindowStyleNormal
                 | WindowStyleBorderlessChild
                 | WindowStyleNormalChild
                 deriving (Show, Eq, Ord)

toWin32WindowStyle :: WindowStyle -> Win32.WindowStyle
toWin32WindowStyle WindowStyleBorderless =
    Win32.wS_POPUP .|. Win32.wS_CLIPCHILDREN

toWin32WindowStyle WindowStyleNormal =
    Win32.wS_OVERLAPPEDWINDOW .|. Win32.wS_CLIPCHILDREN

toWin32WindowStyle WindowStyleBorderlessChild =
    Win32.wS_CHILD .|. Win32.wS_CLIPSIBLINGS .|. Win32.wS_CLIPCHILDREN

toWin32WindowStyle WindowStyleNormalChild =
    Win32.wS_OVERLAPPEDWINDOW .|. Win32.wS_CHILD .|. Win32.wS_TABSTOP .|. Win32.wS_CLIPSIBLINGS .|. Win32.wS_CLIPCHILDREN

data IconType = ResourceIcon | StockIcon deriving Eq

data Icon = IconDocNoAssoc
          | IconDocAssoc
          | IconApplication
          | IconFolder
          | IconFolderOpen
          | IconDrive525
          | IconDrive35
          | IconDriveRemove
          | IconDriveFixed
          | IconDriveNet
          | IconDriveNetDisabled
          | IconDriveCD
          | IconDriveRAM
          | IconWorld
          | IconServer
          | IconPrinter
          | IconMyNetwork
          | IconFind
          | IconHelp
          | IconShare
          | IconLink
          | IconSlowFile
          | IconRecycler
          | IconRecyclerFull
          | IconMediaCDAudio
          | IconLock
          | IconAutoList
          | IconPrinterNet
          | IconServerShare
          | IconPrinterFax
          | IconPrinterFaxNet
          | IconPrinterFile
          | IconStack
          | IconMediaSVCD
          | IconStuffedFolder
          | IconDriveUnknown
          | IconDriveDVD
          | IconMediaDVD
          | IconMediaDVDRAM
          | IconMediaDVDRW
          | IconMediaDVDR
          | IconMediaDVDROM
          | IconMediaCDAudioPlus
          | IconMediaCDRW
          | IconMediaCDR
          | IconMediaCDBurn
          | IconMediaBlankCD
          | IconMediaCDROM
          | IconAudioFiles
          | IconImageFiles
          | IconVideoFiles
          | IconMixedFiles
          | IconFolderBack
          | IconFolderFront
          | IconShield
          | IconWarning
          | IconInfo
          | IconError
          | IconKey
          | IconSoftware
          | IconRename
          | IconDelete
          | IconMediaAudioDVD
          | IconMediaMovieDVD
          | IconMediaEnhancedCD
          | IconMediaEnhancedDVD
          | IconMediaHDDVD
          | IconMediaBluray
          | IconMediaVCD
          | IconMediaDVDPlusR
          | IconMediaDVDPlusRW
          | IconDesktopPC
          | IconMobilePC
          | IconUsers
          | IconMediaSmartMedia
          | IconMediaCompactFlash
          | IconDeviceCellPhone
          | IconDeviceCamera
          | IconDeviceVideoCamera
          | IconDeviceAudioPlayer
          | IconNetworkConnect
          | IconInternet
          | IconZipFile
          | IconSettings
          | IconDriveHDDVD
          | IconDriveBD
          | IconMediaHDDVDROM
          | IconMediaHDDVDR
          | IconMediaHDDVDRAM
          | IconMediaBDROM
          | IconMediaBDR
          | IconMediaBDRE
          | IconClusteredDrive
          | IconFromResource Int
          deriving (Show, Eq, Ord)

getIconType :: Icon -> IconType
getIconType (IconFromResource _) = ResourceIcon
getIconType _                    = StockIcon

toWin32Icon :: Icon -> Win32.Icon
toWin32Icon (IconFromResource i) = Win32.c_MakeIntResourceW (fromIntegral i)
toWin32Icon _                    = errorTEAWin32 (InternalTEAWin32Error "Impossible pattern matching")

toStockIconId :: Icon -> Win32.SHSTOCKICONID
toStockIconId icon = case icon of
    IconDocNoAssoc        -> Win32.sIID_DOCNOASSOC
    IconDocAssoc          -> Win32.sIID_DOCASSOC
    IconApplication       -> Win32.sIID_APPLICATION
    IconFolder            -> Win32.sIID_FOLDER
    IconFolderOpen        -> Win32.sIID_FOLDEROPEN
    IconDrive525          -> Win32.sIID_DRIVE525
    IconDrive35           -> Win32.sIID_DRIVE35
    IconDriveRemove       -> Win32.sIID_DRIVEREMOVE
    IconDriveFixed        -> Win32.sIID_DRIVEFIXED
    IconDriveNet          -> Win32.sIID_DRIVENET
    IconDriveNetDisabled  -> Win32.sIID_DRIVENETDISABLED
    IconDriveCD           -> Win32.sIID_DRIVECD
    IconDriveRAM          -> Win32.sIID_DRIVERAM
    IconWorld             -> Win32.sIID_WORLD
    IconServer            -> Win32.sIID_SERVER
    IconPrinter           -> Win32.sIID_PRINTER
    IconMyNetwork         -> Win32.sIID_MYNETWORK
    IconFind              -> Win32.sIID_FIND
    IconHelp              -> Win32.sIID_HELP
    IconShare             -> Win32.sIID_SHARE
    IconLink              -> Win32.sIID_LINK
    IconSlowFile          -> Win32.sIID_SLOWFILE
    IconRecycler          -> Win32.sIID_RECYCLER
    IconRecyclerFull      -> Win32.sIID_RECYCLERFULL
    IconMediaCDAudio      -> Win32.sIID_MEDIACDAUDIO
    IconLock              -> Win32.sIID_LOCK
    IconAutoList          -> Win32.sIID_AUTOLIST
    IconPrinterNet        -> Win32.sIID_PRINTERNET
    IconServerShare       -> Win32.sIID_SERVERSHARE
    IconPrinterFax        -> Win32.sIID_PRINTERFAX
    IconPrinterFaxNet     -> Win32.sIID_PRINTERFAXNET
    IconPrinterFile       -> Win32.sIID_PRINTERFILE
    IconStack             -> Win32.sIID_STACK
    IconMediaSVCD         -> Win32.sIID_MEDIASVCD
    IconStuffedFolder     -> Win32.sIID_STUFFEDFOLDER
    IconDriveUnknown      -> Win32.sIID_DRIVEUNKNOWN
    IconDriveDVD          -> Win32.sIID_DRIVEDVD
    IconMediaDVD          -> Win32.sIID_MEDIADVD
    IconMediaDVDRAM       -> Win32.sIID_MEDIADVDRAM
    IconMediaDVDRW        -> Win32.sIID_MEDIADVDRW
    IconMediaDVDR         -> Win32.sIID_MEDIADVDR
    IconMediaDVDROM       -> Win32.sIID_MEDIADVDROM
    IconMediaCDAudioPlus  -> Win32.sIID_MEDIACDAUDIOPLUS
    IconMediaCDRW         -> Win32.sIID_MEDIACDRW
    IconMediaCDR          -> Win32.sIID_MEDIACDR
    IconMediaCDBurn       -> Win32.sIID_MEDIACDBURN
    IconMediaBlankCD      -> Win32.sIID_MEDIABLANKCD
    IconMediaCDROM        -> Win32.sIID_MEDIACDROM
    IconAudioFiles        -> Win32.sIID_AUDIOFILES
    IconImageFiles        -> Win32.sIID_IMAGEFILES
    IconVideoFiles        -> Win32.sIID_VIDEOFILES
    IconMixedFiles        -> Win32.sIID_MIXEDFILES
    IconFolderBack        -> Win32.sIID_FOLDERBACK
    IconFolderFront       -> Win32.sIID_FOLDERFRONT
    IconShield            -> Win32.sIID_SHIELD
    IconWarning           -> Win32.sIID_WARNING
    IconInfo              -> Win32.sIID_INFO
    IconError             -> Win32.sIID_ERROR
    IconKey               -> Win32.sIID_KEY
    IconSoftware          -> Win32.sIID_SOFTWARE
    IconRename            -> Win32.sIID_RENAME
    IconDelete            -> Win32.sIID_DELETE
    IconMediaAudioDVD     -> Win32.sIID_MEDIAAUDIODVD
    IconMediaMovieDVD     -> Win32.sIID_MEDIAMOVIEDVD
    IconMediaEnhancedCD   -> Win32.sIID_MEDIAENHANCEDCD
    IconMediaEnhancedDVD  -> Win32.sIID_MEDIAENHANCEDDVD
    IconMediaHDDVD        -> Win32.sIID_MEDIAHDDVD
    IconMediaBluray       -> Win32.sIID_MEDIABLURAY
    IconMediaVCD          -> Win32.sIID_MEDIAVCD
    IconMediaDVDPlusR     -> Win32.sIID_MEDIADVDPLUSR
    IconMediaDVDPlusRW    -> Win32.sIID_MEDIADVDPLUSRW
    IconDesktopPC         -> Win32.sIID_DESKTOPPC
    IconMobilePC          -> Win32.sIID_MOBILEPC
    IconUsers             -> Win32.sIID_USERS
    IconMediaSmartMedia   -> Win32.sIID_MEDIASMARTMEDIA
    IconMediaCompactFlash -> Win32.sIID_MEDIACOMPACTFLASH
    IconDeviceCellPhone   -> Win32.sIID_DEVICECELLPHONE
    IconDeviceCamera      -> Win32.sIID_DEVICECAMERA
    IconDeviceVideoCamera -> Win32.sIID_DEVICEVIDEOCAMERA
    IconDeviceAudioPlayer -> Win32.sIID_DEVICEAUDIOPLAYER
    IconNetworkConnect    -> Win32.sIID_NETWORKCONNECT
    IconInternet          -> Win32.sIID_INTERNET
    IconZipFile           -> Win32.sIID_ZIPFILE
    IconSettings          -> Win32.sIID_SETTINGS
    IconDriveHDDVD        -> Win32.sIID_DRIVEHDDVD
    IconDriveBD           -> Win32.sIID_DRIVEBD
    IconMediaHDDVDROM     -> Win32.sIID_MEDIAHDDVDROM
    IconMediaHDDVDR       -> Win32.sIID_MEDIAHDDVDR
    IconMediaHDDVDRAM     -> Win32.sIID_MEDIAHDDVDRAM
    IconMediaBDROM        -> Win32.sIID_MEDIABDROM
    IconMediaBDR          -> Win32.sIID_MEDIABDR
    IconMediaBDRE         -> Win32.sIID_MEDIABDRE
    IconClusteredDrive    -> Win32.sIID_CLUSTEREDDRIVE
    (IconFromResource _)  -> errorTEAWin32 (InternalTEAWin32Error "Impossible pattern matching")

data Cursor = CursorArrow
            | CursorIBeam
            | CursorWait
            | CursorCross
            | CursorUparrow
            | CursorSizeNWSE
            | CursorSizeNESW
            | CursorSizeWE
            | CursorSizeNS
            deriving (Show, Eq, Ord)

toWin32Cursor :: Cursor -> Win32.Cursor
toWin32Cursor CursorArrow    = Win32.iDC_ARROW
toWin32Cursor CursorIBeam    = Win32.iDC_IBEAM
toWin32Cursor CursorWait     = Win32.iDC_WAIT
toWin32Cursor CursorCross    = Win32.iDC_CROSS
toWin32Cursor CursorUparrow  = Win32.iDC_UPARROW
toWin32Cursor CursorSizeNWSE = Win32.iDC_SIZENWSE
toWin32Cursor CursorSizeNESW = Win32.iDC_SIZENESW
toWin32Cursor CursorSizeWE   = Win32.iDC_SIZEWE
toWin32Cursor CursorSizeNS   = Win32.iDC_SIZENS

data Font = Font
    { fontName :: Text
    , fontSize :: ScalableValue
    } deriving (Show, Eq, Ord)

data ScalableValue = RawValue      Double
                   | ScalableValue Double
                   deriving (Show, Eq, Ord)

class CanBeRawValue a where
    raw :: a -> ScalableValue

instance CanBeRawValue Integer where
    raw = RawValue . fromIntegral

instance CanBeRawValue Int where
    raw = RawValue . fromIntegral

instance CanBeRawValue Double where
    raw = RawValue

instance Num ScalableValue where
    fromInteger :: Integer -> ScalableValue
    fromInteger a = ScalableValue (fromInteger a)

    (ScalableValue a) + (ScalableValue b) = ScalableValue (a + b)
    (RawValue a)      + (RawValue b)      = RawValue      (a + b)
    (ScalableValue a) + (RawValue b)      = ScalableValue (a + b)
    (RawValue a)      + (ScalableValue b) = RawValue      (a + b)

    (ScalableValue a) * (ScalableValue b) = ScalableValue (a * b)
    (RawValue a)      * (RawValue b)      = RawValue      (a * b)
    (ScalableValue a) * (RawValue b)      = ScalableValue (a * b)
    (RawValue a)      * (ScalableValue b) = RawValue      (a * b)

    abs (ScalableValue a) = ScalableValue (abs a)
    abs (RawValue a)      = RawValue      (abs a)

    signum (ScalableValue a) = ScalableValue (signum a)
    signum (RawValue a)      = RawValue      (signum a)

    negate (ScalableValue a) = ScalableValue (negate a)
    negate (RawValue a)      = RawValue      (negate a)

instance Fractional ScalableValue where
    fromRational a = ScalableValue (fromRational a)

    (ScalableValue a) / (ScalableValue b) = ScalableValue (a / b)
    (RawValue a)      / (RawValue b)      = RawValue      (a / b)
    (ScalableValue a) / (RawValue b)      = ScalableValue (a / b)
    (RawValue a)      / (ScalableValue b) = RawValue      (a / b)
