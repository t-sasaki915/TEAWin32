{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.Core.Types
    ( UniqueId (..)
    , Colour (..)
    , TEAWin32Settings (..)
    , ScalableValue (..)
    , RawValue (..)
    , WindowStyle (..)
    , IconType (..)
    , Icon (..)
    , Cursor (..)
    , Font (..)
    , ComponentProperty (..)
    , ComponentTitle (..)
    , ComponentSize (..)
    , ComponentPosition (..)
    , ComponentFont (..)
    , WindowProperty (..)
    , WindowIcon (..)
    , WindowCursor (..)
    , WindowBackgroundColour (..)
    , ButtonProperty (..)
    , defaultTEAWin32Settings
    ) where

import           Data.Data (Typeable)
import           Data.Text (Text)
import           Foreign   (Storable (..), fillBytes)
import           Foreign.C (CInt)

newtype UniqueId = UniqueId Int

instance Storable UniqueId where
    sizeOf _ = sizeOf (0 :: CInt)

    alignment _ = alignment (0 :: CInt)

    peek ptr = UniqueId <$> peekByteOff ptr 0

    poke ptr (UniqueId uniqueId) =
        fillBytes ptr 0 (sizeOf (undefined :: UniqueId)) >>
            pokeByteOff ptr 0 uniqueId

data Colour = RGB Int Int Int deriving (Show, Eq)

newtype TEAWin32Settings = TEAWin32Settings
    { useVisualStyles :: Bool
    }

defaultTEAWin32Settings :: TEAWin32Settings
defaultTEAWin32Settings = TEAWin32Settings
    { useVisualStyles = True
    }

data ScalableValue = ScalableValue Double
                   | RawValue      Double
                   deriving (Show, Eq, Ord)

class RawValue a where
    raw :: a -> ScalableValue

instance RawValue Integer where
    raw = RawValue . fromIntegral

instance RawValue Int where
    raw = RawValue . fromIntegral

instance RawValue Double where
    raw = RawValue

instance Num ScalableValue where
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

data WindowStyle = WindowStyleBorderless
                 | WindowStyleNormal
                 | WindowStyleBorderlessChild
                 | WindowStyleNormalChild
                 deriving (Show, Eq, Ord)

data IconType = ResourceIcon | StockIcon deriving Eq

data Icon = IconDocNoAssoc | IconDocAssoc | IconApplication | IconFolder | IconFolderOpen | IconDrive525 | IconDrive35 | IconDriveRemove
          | IconDriveFixed | IconDriveNet | IconDriveNetDisabled | IconDriveCD | IconDriveRAM | IconWorld | IconServer | IconPrinter
          | IconMyNetwork | IconFind | IconHelp | IconShare | IconLink | IconSlowFile | IconRecycler | IconRecyclerFull | IconMediaCDAudio
          | IconLock | IconAutoList | IconPrinterNet | IconServerShare | IconPrinterFax | IconPrinterFaxNet | IconPrinterFile | IconStack
          | IconMediaSVCD | IconStuffedFolder | IconDriveUnknown | IconDriveDVD | IconMediaDVD | IconMediaDVDRAM | IconMediaDVDRW
          | IconMediaDVDR | IconMediaDVDROM | IconMediaCDAudioPlus | IconMediaCDRW | IconMediaCDR | IconMediaCDBurn | IconMediaBlankCD
          | IconMediaCDROM | IconAudioFiles | IconImageFiles | IconVideoFiles | IconMixedFiles | IconFolderBack | IconFolderFront
          | IconShield | IconWarning | IconInfo | IconError | IconKey | IconSoftware | IconRename | IconDelete | IconMediaAudioDVD
          | IconMediaMovieDVD | IconMediaEnhancedCD | IconMediaEnhancedDVD | IconMediaHDDVD | IconMediaBluray | IconMediaVCD
          | IconMediaDVDPlusR | IconMediaDVDPlusRW | IconDesktopPC | IconMobilePC | IconUsers | IconMediaSmartMedia | IconMediaCompactFlash
          | IconDeviceCellPhone | IconDeviceCamera | IconDeviceVideoCamera | IconDeviceAudioPlayer | IconNetworkConnect | IconInternet
          | IconZipFile | IconSettings | IconDriveHDDVD | IconDriveBD | IconMediaHDDVDROM | IconMediaHDDVDR | IconMediaHDDVDRAM | IconMediaBDROM
          | IconMediaBDR | IconMediaBDRE | IconClusteredDrive | IconFromResourceFile Int
          deriving (Show, Eq, Ord)

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

data Font = Font
    { fontName    :: Text
    , fontSize    :: ScalableValue
    , isItalic    :: Bool
    , isUnderline :: Bool
    , isStrikeOut :: Bool
    } deriving (Show, Eq, Ord)

data ComponentProperty = forall a. (Typeable a, Show a, IsComponentProperty a) => ComponentProperty a

class IsComponentProperty a

newtype ComponentTitle    = ComponentTitle    Text                           deriving (Show, Eq)
newtype ComponentSize     = ComponentSize     (ScalableValue, ScalableValue) deriving (Show, Eq)
newtype ComponentPosition = ComponentPosition (ScalableValue, ScalableValue) deriving (Show, Eq)
newtype ComponentFont     = ComponentFont     Font                           deriving (Show, Eq)

instance IsComponentProperty ComponentTitle
instance IsComponentProperty ComponentSize
instance IsComponentProperty ComponentPosition
instance IsComponentProperty ComponentFont

data WindowProperty = forall a. (Typeable a, Show a, IsComponentProperty a, IsWindowProperty a) => WindowProperty a

instance IsComponentProperty WindowProperty

class IsWindowProperty a

newtype WindowIcon             = WindowIcon             Icon   deriving (Show, Eq)
newtype WindowCursor           = WindowCursor           Cursor deriving (Show, Eq)
newtype WindowBackgroundColour = WindowBackgroundColour Colour deriving (Show, Eq)

instance IsComponentProperty WindowIcon
instance IsComponentProperty WindowCursor
instance IsComponentProperty WindowBackgroundColour

instance IsWindowProperty WindowIcon
instance IsWindowProperty WindowCursor
instance IsWindowProperty WindowBackgroundColour
instance IsWindowProperty ComponentTitle
instance IsWindowProperty ComponentSize
instance IsWindowProperty ComponentPosition
instance IsWindowProperty ComponentFont

data ButtonProperty = forall a. (Typeable a, Show a, IsComponentProperty a, IsButtonProperty a) => ButtonProperty a

instance IsComponentProperty ButtonProperty

class IsButtonProperty a

instance IsButtonProperty ComponentTitle
instance IsButtonProperty ComponentSize
instance IsButtonProperty ComponentPosition
instance IsButtonProperty ComponentFont
