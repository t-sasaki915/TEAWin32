module TEAWin32.GUI (Icon (..), Cursor (..), Font (..)) where

import           Data.Text (Text)

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

instance Ord Icon

data Cursor = CursorArrow
            | CursorIBeam
            | CursorWait
            | CursorCross
            | CursorUparrow
            | CursorSizeNWSE
            | CursorSizeNESW
            | CursorSizeWE
            | CursorSizeNS

instance Ord Cursor

data Font = Font
    { fontName :: Text
    , fontSize :: ScalableValue
    }

instance Ord Font

data ScalableValue = RawValue      Double
                   | ScalableValue Double
