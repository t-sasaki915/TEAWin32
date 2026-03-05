{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module TEAWin32.Core.Types
    ( SHSTOCKICONID
    , WORD
    , LPCWSTR
    , Model (..)
    , Msg (..)
    , InternalState (..)
    , EventEnqueuer
    , EventQueueEntry (..)
    , UniqueIdInternState (..)
    , DSLState (..)
    , DSL
    , View
    , UniqueId (..)
    , Colour (..)
    , TEAWin32Settings (..)
    , ScalableValue (..)
    , RawValue (..)
    , WindowStyle (..)
    , IconType (..)
    , Icon (..)
    , Cursor (..)
    , Font (..)
    , FontSettings (..)
    , GUIComponent (..)
    , IsGUIComponent
    , IsChildWrapper (..)
    , TopLevelComponent (..)
    , IsTopLevelComponent
    , Window (..)
    , WindowChild (..)
    , IsWindowChild
    , Button (..)
    , GUIComponentProperty (..)
    , IsGUIComponentProperty
    , ComponentTitle (..)
    , ComponentSize (..)
    , ComponentPosition (..)
    , ComponentFont (..)
    , ComponentIcon (..)
    , ComponentCursor (..)
    , ComponentBackgroundColour (..)
    , IsPropertyWrapper (..)
    , WindowProperty (..)
    , IsWindowProperty
    , ButtonProperty (..)
    , IsButtonProperty
    , UpdatePosReq (..)
    , CreateWindowReq (..)
    , CreateButtonReq (..)
    , CCallRequest (..)
    ) where

import           Control.Concurrent.STM         (TQueue)
import           Control.Monad.State.Strict     (State)
import           Control.Monad.Writer.Strict    (WriterT)
import           Data.Data                      (Typeable, cast)
import           Data.Map.Strict                (Map)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Foreign                        (Ptr, Storable (..), Word16,
                                                 Word32, fillBytes)
import           Foreign.C                      (CInt, CWchar, peekCWString)
import qualified TEAWin32.Core.Native.Constants as Native

type SHSTOCKICONID = Word32
type WORD          = Word16
type LPCWSTR       = Ptr CWchar

data Model = forall a. Typeable a => Model a
data Msg = forall a. (Typeable a, Eq a, Show a) => Msg a

data InternalState = InternalState
    { eventQueue        :: TQueue EventQueueEntry
    , lastGUIComponents :: [GUIComponent]
    , updateFunction    :: Msg -> Model -> IO Model
    , viewFunction      :: Model -> View
    , currentModel      :: Model
    }

type EventEnqueuer = Ptr EventQueueEntry -> IO ()

data EventQueueEntry = TestEvent
                     | FatalErrorEvent Text Word32 Text
                     deriving Show

instance Storable EventQueueEntry where
    sizeOf _ = Native.size_EventQueueEntry

    alignment _ = Native.alignment_EventQueueEntry

    peek ptr =
        peekByteOff ptr Native.offset_EventQueueEntry_eventType >>= \case
            Native.EventTypeTestEvent ->
                pure TestEvent

            Native.EventTypeFatalError -> do
                errorType     <- peekByteOff ptr Native.offset_EventQueueEntry_eventData_fatalErrorEventData_errorType >>= peekCWString
                errorCode     <- peekByteOff ptr Native.offset_EventQueueEntry_eventData_fatalErrorEventData_errorCode
                errorLocation <- peekByteOff ptr Native.offset_EventQueueEntry_eventData_fatalErrorEventData_errorLocation >>= peekCWString

                pure (FatalErrorEvent (Text.pack errorType) errorCode (Text.pack errorLocation))

    poke = undefined

data UniqueIdInternState = UniqueIdInternState
    { internedUserUniqueIdMap      :: Map Text Int
    , nextUserUniqueIdInternNumber :: Int
    }

data DSLState = DSLState
    { nextAutoUniqueId      :: Int
    , userUniqueIdsAppeared :: [Text]
    , uniqueIdInternState   :: UniqueIdInternState
    }

type DSL a b = WriterT [a] (State DSLState) b

type View = DSL TopLevelComponent ()

newtype UniqueId = UniqueId Int deriving (Show, Eq)

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

instance Storable TEAWin32Settings where
    sizeOf _ = Native.size_TEAWin32Settings

    alignment _ = Native.alignment_TEAWin32Settings

    peek = undefined

    poke ptr val = do
        fillBytes ptr 0 Native.size_TEAWin32Settings

        pokeByteOff ptr Native.offset_TEAWin32Settings_useVisualStyles (useVisualStyles val)

data ScalableValue = ScalableValue Double
                   | RawValue      Double
                   deriving (Show, Eq, Ord)

class RawValue a where
    raw :: a -> ScalableValue

instance RawValue Integer where
    raw = RawValue . fromIntegral

instance RawValue Int where
    raw = RawValue . fromIntegral

instance RawValue CInt where
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

instance Storable ScalableValue where
    sizeOf _ = Native.size_ScalableValue

    alignment _ = Native.alignment_ScalableValue

    peek ptr = do
        value <- peekByteOff ptr Native.offset_ScalableValue_value
        isScalable <- peekByteOff ptr Native.offset_ScalableValue_isScalable

        if (isScalable :: CInt) == 0
            then pure (RawValue value)
            else pure (ScalableValue value)

    poke ptr (RawValue v) = do
        fillBytes ptr 0 Native.size_ScalableValue

        pokeByteOff ptr Native.offset_ScalableValue_value      v
        pokeByteOff ptr Native.offset_ScalableValue_isScalable False

    poke ptr (ScalableValue v) = do
        fillBytes ptr 0 Native.size_ScalableValue

        pokeByteOff ptr Native.offset_ScalableValue_value      v
        pokeByteOff ptr Native.offset_ScalableValue_isScalable True

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

data Font = DefaultGUIFont
          | Font Text ScalableValue
          | Font' Text ScalableValue FontSettings
          deriving (Show, Eq, Ord)

data FontSettings = FontSettings
    { isItalic    :: Bool
    , isUnderline :: Bool
    , isStrikeOut :: Bool
    } deriving (Show, Eq, Ord)

{-data GUIOperation = CreateWindow Text WindowStyle
                  | CreateButton
                  | -}

data GUIComponent = forall a. (Typeable a, Show a, Eq a, IsGUIComponent a) => GUIComponent a

instance Show GUIComponent where
    show (GUIComponent a) = "GUIComponent " <> show a

instance Eq GUIComponent where
    (GUIComponent a) == (GUIComponent b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

class IsGUIComponent a

instance IsGUIComponent GUIComponent

class IsChildWrapper a b where
    wrapChild :: b -> a

data TopLevelComponent = forall a. (Typeable a, Show a, Eq a, IsGUIComponent a, IsTopLevelComponent a) => TopLevelComponent a

instance Show TopLevelComponent where
    show (TopLevelComponent a) = "TopLevelComponent " <> show a

instance Eq TopLevelComponent where
    (TopLevelComponent a) == (TopLevelComponent b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

class IsTopLevelComponent a

instance IsTopLevelComponent Window

instance (Typeable a, Show a, Eq a, IsGUIComponent a, IsTopLevelComponent a) => IsChildWrapper TopLevelComponent a where
    wrapChild = TopLevelComponent

data Window = Window UniqueId Text WindowStyle [WindowProperty] [WindowChild] deriving (Show, Eq)

instance IsGUIComponent Window

data WindowChild = forall a. (Typeable a, Show a, Eq a, IsGUIComponent a, IsWindowChild a) => WindowChild a

instance Show WindowChild where
    show (WindowChild a) = "WindowChild " <> show a

instance Eq WindowChild where
    (WindowChild a) == (WindowChild b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

class IsWindowChild a

instance IsWindowChild Window
instance IsWindowChild Button

instance (Typeable a, Show a, Eq a, IsGUIComponent a, IsWindowChild a) => IsChildWrapper WindowChild a where
    wrapChild = WindowChild

data Button = Button UniqueId [ButtonProperty] deriving (Show, Eq)

instance IsGUIComponent Button

data GUIComponentProperty = forall a. (Typeable a, Show a, Eq a, IsGUIComponentProperty a) => GUIComponentProperty a

instance Show GUIComponentProperty where
    show (GUIComponentProperty a) = "GUIComponentProperty " <> show a

instance Eq GUIComponentProperty where
    (GUIComponentProperty a) == (GUIComponentProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

class IsGUIComponentProperty a

instance IsGUIComponentProperty GUIComponentProperty

newtype ComponentTitle            = ComponentTitle            Text                           deriving (Show, Eq)
newtype ComponentSize             = ComponentSize             (ScalableValue, ScalableValue) deriving (Show, Eq)
newtype ComponentPosition         = ComponentPosition         (ScalableValue, ScalableValue) deriving (Show, Eq)
newtype ComponentFont             = ComponentFont             Font                           deriving (Show, Eq)
newtype ComponentIcon             = ComponentIcon             Icon                           deriving (Show, Eq)
newtype ComponentCursor           = ComponentCursor           Cursor                         deriving (Show, Eq)
newtype ComponentBackgroundColour = ComponentBackgroundColour Colour                         deriving (Show, Eq)

instance IsGUIComponentProperty ComponentTitle
instance IsGUIComponentProperty ComponentSize
instance IsGUIComponentProperty ComponentPosition
instance IsGUIComponentProperty ComponentFont
instance IsGUIComponentProperty ComponentIcon
instance IsGUIComponentProperty ComponentCursor
instance IsGUIComponentProperty ComponentBackgroundColour

class IsPropertyWrapper a b where
    wrapProperty :: b -> a

data WindowProperty = forall a. (Typeable a, Show a, Eq a, IsGUIComponentProperty a, IsWindowProperty a) => WindowProperty a

instance Show WindowProperty where
    show (WindowProperty a) = "WindowProperty " <> show a

instance Eq WindowProperty where
    (WindowProperty a) == (WindowProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance IsGUIComponentProperty WindowProperty

class IsWindowProperty a

instance IsWindowProperty ComponentTitle
instance IsWindowProperty ComponentSize
instance IsWindowProperty ComponentPosition
instance IsWindowProperty ComponentIcon
instance IsWindowProperty ComponentCursor
instance IsWindowProperty ComponentBackgroundColour

instance (Typeable a, Show a, Eq a, IsGUIComponentProperty a, IsWindowProperty a) => IsPropertyWrapper WindowProperty a where
    wrapProperty = WindowProperty

data ButtonProperty = forall a. (Typeable a, Show a, Eq a, IsGUIComponentProperty a, IsButtonProperty a) => ButtonProperty a

instance Show ButtonProperty where
    show (ButtonProperty a) = "ButtonProperty " <> show a

instance Eq ButtonProperty where
    (ButtonProperty a) == (ButtonProperty b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

instance IsGUIComponentProperty ButtonProperty

class IsButtonProperty a

instance IsButtonProperty ComponentTitle
instance IsButtonProperty ComponentSize
instance IsButtonProperty ComponentPosition
instance IsButtonProperty ComponentFont

instance (Typeable a, Show a, Eq a, IsGUIComponentProperty a, IsButtonProperty a) => IsPropertyWrapper ButtonProperty a where
    wrapProperty = ButtonProperty

data UpdatePosReq = UpdatePosReq
    { newLocation           :: Maybe (ScalableValue, ScalableValue)
    , newSize               :: Maybe (ScalableValue, ScalableValue)
    , bringComponentToFront :: Bool
    } deriving (Show, Eq)

data CreateWindowReq = CreateWindowReq
    { newWindowUniqueId       :: UniqueId
    , newWindowClassName      :: Text
    , newWindowStyles         :: WindowStyle
    , newWindowParentUniqueId :: Maybe UniqueId
    } deriving (Show, Eq)

data CreateButtonReq = CreateButtonReq
    { newButtonUniqueId       :: UniqueId
    , newButtonParentUniqueId :: UniqueId
    } deriving (Show, Eq)

data CCallRequest = CreateWindowRequest        CreateWindowReq
                  | CreateButtonRequest        CreateButtonReq
                  | DestroyComponentRequest    UniqueId
                  | UpdateTextRequest          UniqueId Text
                  | UpdatePosRequest           UniqueId UpdatePosReq
                  | UpdateFontRequest          UniqueId Font
                  | UpdateIconRequest          UniqueId Icon
                  | UpdateCursorRequest        UniqueId Cursor
                  | InvalidateRectFullyRequest UniqueId
                  | ShowWindowRequest          UniqueId
                  deriving (Show, Eq)
