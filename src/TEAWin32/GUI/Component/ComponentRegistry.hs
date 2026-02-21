{-# LANGUAGE GADTs #-}

module TEAWin32.GUI.Component.ComponentRegistry
    ( ComponentRegistryKey (..)
    , ComponentRegistryEntry (..)
    , registerComponentToRegistry
    , unregisterComponentFromRegistry
    , isComponentManaged
    , addComponentRegistryEntry
    , removeComponentRegistryEntry
    , updateComponentRegistryEntry
    , getComponentRegistryEntryValue
    , getComponentRegistryEntryValueMaybe
    , whenComponentHasRegistryKey
    ) where

import                          Control.Concurrent            (MVar, modifyMVar,
                                                               newMVar,
                                                               readMVar)
import                          Control.Exception             (throw)
import                          Data.IntMap.Strict            (IntMap)
import                qualified Data.IntMap.Strict            as IntMap
import                          Data.Text                     (Text)
import                qualified Data.Text                     as Text
import                          Foreign                       (ptrToIntPtr)
import                          GHC.IO                        (unsafePerformIO)
import                          GHC.Stack                     (HasCallStack)
import                qualified Graphics.Win32                as Win32
import {-# SOURCE #-} qualified TEAWin32.Application.Internal as ApplicationInternal
import                          TEAWin32.Drawing              (Colour)
import                          TEAWin32.Exception            (InternalTEAWin32Exception (InternalTEAWin32Exception))
import                          TEAWin32.GUI
import                          TEAWin32.GUI.Component        (ComponentType)

data ComponentRegistryKey a where
    ComponentUniqueIdRegKey          :: ComponentRegistryKey UniqueId
    ComponentTypeRegKey              :: ComponentRegistryKey ComponentType
    ComponentCurrentDPIRegKey        :: ComponentRegistryKey Int
    ComponentClickEventHandlerRegKey :: ComponentRegistryKey ApplicationInternal.Msg
    ComponentBackgroundColourRegKey  :: ComponentRegistryKey Colour
    ComponentFontRegKey              :: ComponentRegistryKey Font
    ComponentTitleRegKey             :: ComponentRegistryKey Text
    ComponentSizeRegKey              :: ComponentRegistryKey (ScalableValue, ScalableValue)
    ComponentPositionRegKey          :: ComponentRegistryKey (ScalableValue, ScalableValue)
    ComponentZIndexRegKey            :: ComponentRegistryKey Int
    WindowClassNameRegKey            :: ComponentRegistryKey Text
    WindowStyleRegKey                :: ComponentRegistryKey WindowStyle
    WindowCursorRegKey               :: ComponentRegistryKey Cursor
    WindowIconRegKey                 :: ComponentRegistryKey Icon

instance Show (ComponentRegistryKey a) where
    show ComponentUniqueIdRegKey          = "ComponentUniqueIdRegKey"
    show ComponentTypeRegKey              = "ComponentTypeRegKey"
    show ComponentCurrentDPIRegKey        = "ComponentCurrentDPIRegKey"
    show ComponentClickEventHandlerRegKey = "ComponentClickEventHandlerRegKey"
    show ComponentBackgroundColourRegKey  = "ComponentBackgroundColourRegKey"
    show ComponentFontRegKey              = "ComponentFontRegKey"
    show ComponentTitleRegKey             = "ComponentTitleRegKey"
    show ComponentSizeRegKey              = "ComponentSizeRegKey"
    show ComponentPositionRegKey          = "ComponentPositionRegKey"
    show ComponentZIndexRegKey            = "ComponentZIndexRegKey"
    show WindowClassNameRegKey            = "WindowClassNameRegKey"
    show WindowStyleRegKey                = "WindowStyleRegKey"
    show WindowCursorRegKey               = "WindowCursorRegKey"
    show WindowIconRegKey                 = "WindowIconRegKey"

keyToInt :: ComponentRegistryKey a -> Int
keyToInt ComponentUniqueIdRegKey          = 0
keyToInt ComponentTypeRegKey              = 1
keyToInt ComponentCurrentDPIRegKey        = 2
keyToInt ComponentClickEventHandlerRegKey = 3
keyToInt ComponentBackgroundColourRegKey  = 4
keyToInt ComponentFontRegKey              = 5
keyToInt ComponentTitleRegKey             = 6
keyToInt ComponentSizeRegKey              = 7
keyToInt ComponentPositionRegKey          = 8
keyToInt ComponentZIndexRegKey            = 9
keyToInt WindowClassNameRegKey            = 10
keyToInt WindowStyleRegKey                = 11
keyToInt WindowCursorRegKey               = 12
keyToInt WindowIconRegKey                 = 13

data ComponentRegistryEntry = ComponentUniqueIdReg          !UniqueId
                            | ComponentTypeReg              !ComponentType
                            | ComponentCurrentDPIReg        !Int
                            | ComponentClickEventHandlerReg !ApplicationInternal.Msg
                            | ComponentBackgroundColourReg  !Colour
                            | ComponentFontReg              !Font
                            | ComponentTitleReg             !Text
                            | ComponentSizeReg              !(ScalableValue, ScalableValue)
                            | ComponentPositionReg          !(ScalableValue, ScalableValue)
                            | ComponentZIndexReg            !Int
                            | WindowClassNameReg            !Text
                            | WindowStyleReg                !WindowStyle
                            | WindowCursorReg               !Cursor
                            | WindowIconReg                 !Icon
                            deriving Show

projectEntry :: HasCallStack => ComponentRegistryKey a -> ComponentRegistryEntry -> a
projectEntry ComponentUniqueIdRegKey          (ComponentUniqueIdReg v)          = v
projectEntry ComponentTypeRegKey              (ComponentTypeReg v)              = v
projectEntry ComponentCurrentDPIRegKey        (ComponentCurrentDPIReg v)        = v
projectEntry ComponentClickEventHandlerRegKey (ComponentClickEventHandlerReg v) = v
projectEntry ComponentBackgroundColourRegKey  (ComponentBackgroundColourReg v)  = v
projectEntry ComponentFontRegKey              (ComponentFontReg v)              = v
projectEntry ComponentTitleRegKey             (ComponentTitleReg v)             = v
projectEntry ComponentSizeRegKey              (ComponentSizeReg v)              = v
projectEntry ComponentPositionRegKey          (ComponentPositionReg v)          = v
projectEntry ComponentZIndexRegKey            (ComponentZIndexReg v)            = v
projectEntry WindowClassNameRegKey            (WindowClassNameReg v)            = v
projectEntry WindowStyleRegKey                (WindowStyleReg v)                = v
projectEntry WindowCursorRegKey               (WindowCursorReg v)               = v
projectEntry WindowIconRegKey                 (WindowIconReg v)                 = v
projectEntry key ent =
    throw (InternalTEAWin32Exception $ "Type mismatch in registry. Key: " <> Text.show key <> " Entry: " <> Text.show ent)

componentRegistryRef :: MVar (IntMap (IntMap ComponentRegistryEntry))
componentRegistryRef = unsafePerformIO (newMVar IntMap.empty)
{-# NOINLINE componentRegistryRef #-}

hwndToInt :: Win32.HWND -> Int
hwndToInt hwnd = fromIntegral (ptrToIntPtr hwnd)

registerComponentToRegistry :: Win32.HWND -> IO ()
registerComponentToRegistry hwnd =
    modifyMVar componentRegistryRef $ \componentRegistry ->
        pure (IntMap.insert (hwndToInt hwnd) IntMap.empty componentRegistry, ())

unregisterComponentFromRegistry :: Win32.HWND -> IO ()
unregisterComponentFromRegistry hwnd =
    modifyMVar componentRegistryRef $ \componentRegistry ->
        pure (IntMap.delete (hwndToInt hwnd) componentRegistry, ())

isComponentManaged :: Win32.HWND -> IO Bool
isComponentManaged hwnd =
    readMVar componentRegistryRef >>= \componentRegistry ->
        pure (hwndToInt hwnd `IntMap.member` componentRegistry)

addComponentRegistryEntry :: HasCallStack => ComponentRegistryKey a -> ComponentRegistryEntry -> Win32.HWND -> IO ()
addComponentRegistryEntry regKey regEntry hwnd =
    modifyMVar componentRegistryRef $ \componentRegistry ->
        let hwnd' = hwndToInt hwnd in
            case IntMap.lookup hwnd' componentRegistry of
                Just entries ->
                    let newEntries = IntMap.insert (keyToInt regKey) regEntry entries in
                        pure (IntMap.insert hwnd' newEntries componentRegistry, ())

                Nothing ->
                    throw $ InternalTEAWin32Exception $
                        "Tried to add a registry entry to HWND that was not in ComponentRegistry: "
                            <> Text.show regKey <> " Entry: " <> Text.show regEntry <> " HWND: " <> Text.show hwnd

removeComponentRegistryEntry :: HasCallStack => ComponentRegistryKey a -> Win32.HWND -> IO ()
removeComponentRegistryEntry regKey hwnd =
    modifyMVar componentRegistryRef $ \componentRegistry ->
        let hwnd' = hwndToInt hwnd in
            case IntMap.lookup hwnd' componentRegistry of
                Just entries ->
                    let newEntries = IntMap.delete (keyToInt regKey) entries in
                        pure (IntMap.insert hwnd' newEntries componentRegistry, ())

                Nothing ->
                    throw $ InternalTEAWin32Exception $
                        "Tried to remove a registry entry from HWND that was not in ComponentRegistry: "
                            <> Text.show regKey <> " HWND: " <> Text.show hwnd

updateComponentRegistryEntry :: HasCallStack => ComponentRegistryKey a -> ComponentRegistryEntry -> Win32.HWND -> IO ()
updateComponentRegistryEntry regKey newRegEntry hwnd =
    modifyMVar componentRegistryRef $ \componentRegistry ->
        let hwnd' = hwndToInt hwnd in
            case IntMap.lookup hwnd' componentRegistry of
                Just entries ->
                    let newEntries = IntMap.insert (keyToInt regKey) newRegEntry entries in
                        pure (IntMap.insert hwnd' newEntries componentRegistry, ())

                Nothing ->
                    throw $ InternalTEAWin32Exception $
                        "Tried to update a registry entry of HWND that was not in ComponentRegistry: "
                            <> Text.show regKey <> " NewEntry: " <> Text.show newRegEntry <> " HWND: " <> Text.show hwnd

withComponentRegistryEntries :: HasCallStack => Win32.HWND -> (IntMap ComponentRegistryEntry -> IO a) -> IO a
withComponentRegistryEntries hwnd func =
    readMVar componentRegistryRef >>= \componentRegistry ->
        case IntMap.lookup (hwndToInt hwnd) componentRegistry of
            Just entries ->
                func entries

            Nothing ->
                throw $ InternalTEAWin32Exception $
                    "Tried to access the registry entries of HWND that was not in ComponentRegistry: " <> Text.show hwnd

getComponentRegistryEntryValue :: HasCallStack => ComponentRegistryKey a -> Win32.HWND -> IO a
getComponentRegistryEntryValue regKey hwnd =
    withComponentRegistryEntries hwnd $ \entries ->
        case IntMap.lookup (keyToInt regKey) entries of
            Just entry ->
                pure (projectEntry regKey entry)

            Nothing ->
                throw $ InternalTEAWin32Exception $
                    "Tried to access a registry entry that was not in ComponentRegistry: " <> Text.show regKey <> " HWND: " <> Text.show hwnd

getComponentRegistryEntryValueMaybe :: HasCallStack => ComponentRegistryKey a -> Win32.HWND -> IO (Maybe a)
getComponentRegistryEntryValueMaybe regKey hwnd =
    withComponentRegistryEntries hwnd $ \entries ->
        case IntMap.lookup (keyToInt regKey) entries of
            Just entry ->
                pure $ Just (projectEntry regKey entry)

            Nothing ->
                pure Nothing

whenComponentHasRegistryKey :: HasCallStack => ComponentRegistryKey a -> Win32.HWND -> (a -> IO ()) -> IO ()
whenComponentHasRegistryKey regKey hwnd func =
    withComponentRegistryEntries hwnd $ \entries ->
        case IntMap.lookup (keyToInt regKey) entries of
            Just entry ->
                func (projectEntry regKey entry)

            Nothing ->
                pure ()
