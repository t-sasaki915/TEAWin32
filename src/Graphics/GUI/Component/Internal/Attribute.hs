module Graphics.GUI.Component.Internal.Attribute
    ( ComponentType (..)
    , EventType (..)
    , ComponentFlagKey (..)
    , ComponentAttribute (..)
    , registerHWNDToAttributeMap
    , unregisterHWNDFromAttributeMap
    , addAttributeToHWND
    , updateAttributeOfHWND
    , removeAttributeFromHWND
    , getAttributesFromHWND
    , isManagedByTEAWin32GUI
    , getComponentUniqueIdFromHWND
    , getComponentTypeFromHWND
    , doesHWNDHaveFlag
    , getEventHandlerFromHWNDMaybe
    , getEventHandlerFromHWND
    , getComponentBackgroundColourFromHWNDMaybe
    , getComponentBackgroundColourFromHWND
    , getComponentFontFromHWND
    , getComponentTitleFromHWND
    , getWindowClassNameFromHWND
    , getWindowStyleFromHWND
    , getWindowCursorFromHWND
    , getWindowIconFromHWND
    ) where

import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef)
import qualified Data.List              as List
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Framework.TEA.Internal as TEAInternal
import           GHC.IO                 (unsafePerformIO)
import           Graphics.Drawing       (Colour)
import           Graphics.GUI           (Cursor, Font, Icon, UniqueId,
                                         WindowStyle)
import qualified Graphics.Win32         as Win32

data ComponentType = ComponentWindow
                   | ComponentButton
                   deriving (Eq, Show, Ord)

data EventType = ComponentClickEvent
               deriving (Eq, Show, Ord)

data ComponentFlagKey = ComponentTitleSet
                      | ComponentSizeSet
                      | ComponentPositionSet
                      | ComponentFontSet
                      | ComponentOnClickSet
                      | ComponentChildrenSet
                      | ComponentBackgroundColourSet
                      | WindowIconSet
                      | WindowCursorSet
                      | WindowBackgroundColourSet
                      deriving (Eq, Show, Ord)

data ComponentAttribute = ComponentUniqueIdAttr         UniqueId
                        | ComponentTypeAttr             ComponentType
                        | ComponentFlagAttr             ComponentFlagKey
                        | ComponentEventHandlerAttr     EventType TEAInternal.Msg
                        | ComponentBackgroundColourAttr Colour
                        | ComponentFontAttr             Font
                        | ComponentTitleAttr            Text
                        | WindowClassNameAttr           Text
                        | WindowStyleAttr               WindowStyle
                        | WindowCursorAttr              Cursor
                        | WindowIconAttr                Icon
                        deriving (Eq, Show)

isSameKind :: ComponentAttribute -> ComponentAttribute -> Bool
isSameKind (ComponentUniqueIdAttr _) (ComponentUniqueIdAttr _)                 = True
isSameKind (ComponentTypeAttr _) (ComponentTypeAttr _)                         = True
isSameKind (ComponentFlagAttr _) (ComponentFlagAttr _)                         = True
isSameKind (ComponentEventHandlerAttr _ _) (ComponentEventHandlerAttr _ _)     = True
isSameKind (ComponentBackgroundColourAttr _) (ComponentBackgroundColourAttr _) = True
isSameKind (ComponentFontAttr _) (ComponentFontAttr _)                         = True
isSameKind (ComponentTitleAttr _) (ComponentTitleAttr _)                       = True
isSameKind (WindowClassNameAttr _) (WindowClassNameAttr _)                     = True
isSameKind (WindowStyleAttr _) (WindowStyleAttr _)                             = True
isSameKind (WindowCursorAttr _) (WindowCursorAttr _)                           = True
isSameKind (WindowIconAttr _) (WindowIconAttr _)                               = True
isSameKind _ _                                                                 = False

attributeMapRef :: IORef (Map Win32.HWND [ComponentAttribute])
attributeMapRef = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE attributeMapRef #-}

registerHWNDToAttributeMap :: Win32.HWND -> IO ()
registerHWNDToAttributeMap hwnd =
    atomicModifyIORef' attributeMapRef $ \attrMap ->
        let newMap = Map.insert hwnd [] attrMap in
            (newMap, ())

unregisterHWNDFromAttributeMap :: Win32.HWND -> IO ()
unregisterHWNDFromAttributeMap hwnd =
    atomicModifyIORef' attributeMapRef $ \attrMap ->
        let newMap = Map.delete hwnd attrMap in
            (newMap, ())

addAttributeToHWND :: Win32.HWND -> ComponentAttribute -> IO ()
addAttributeToHWND hwnd attr =
    atomicModifyIORef' attributeMapRef $ \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs ->
                let newHWNDAttrs = hwndAttrs ++ [attr]
                    newMap = Map.insert hwnd newHWNDAttrs attrMap in
                        (newMap, ())
            Nothing ->
                error $ "AttributeMap for " <> show hwnd <> " is not initialised."

updateAttributeOfHWND :: Win32.HWND -> ComponentAttribute -> IO ()
updateAttributeOfHWND hwnd newAttr =
    atomicModifyIORef' attributeMapRef $ \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs ->
                let newHWNDAttrs = [attr | attr <- hwndAttrs, not (attr `isSameKind` newAttr)] ++ [newAttr]
                    newMap = Map.insert hwnd newHWNDAttrs attrMap in
                        (newMap, ())

            Nothing ->
                error $ "AttributeMap for " <> show hwnd <> " is not initialised."

removeAttributeFromHWND :: Win32.HWND -> ComponentAttribute -> IO ()
removeAttributeFromHWND hwnd attr =
    atomicModifyIORef' attributeMapRef $ \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs ->
                let newHWNDAttrs = List.delete attr hwndAttrs
                    newMap = Map.insert hwnd newHWNDAttrs attrMap in
                        (newMap, ())
            Nothing ->
                error $ "AttributeMap for " <> show hwnd <> " is not initialised."

getAttributesFromHWND :: Win32.HWND -> IO [ComponentAttribute]
getAttributesFromHWND hwnd =
    readIORef attributeMapRef >>= \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs -> pure hwndAttrs
            Nothing        -> error $ "AttributeMap for " <> show hwnd <> " is not initialised."

isManagedByTEAWin32GUI :: Win32.HWND -> IO Bool
isManagedByTEAWin32GUI hwnd =
    readIORef attributeMapRef >>= \attrMap ->
        pure $ hwnd `Map.member` attrMap

getComponentUniqueIdFromHWND :: Win32.HWND -> IO UniqueId
getComponentUniqueIdFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ uniqueId | ComponentUniqueIdAttr uniqueId <- attrs ] of
            [ uniqueId ] -> pure uniqueId
            x            -> error $ "Illegal AttributeMap state: " <> show x

getComponentTypeFromHWND :: Win32.HWND -> IO ComponentType
getComponentTypeFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ componentType | ComponentTypeAttr componentType <- attrs ] of
            [ componentType ] -> pure componentType
            x                 -> error $ "Illegal AttributeMap state: " <> show x

doesHWNDHaveFlag :: ComponentFlagKey -> Win32.HWND -> IO Bool
doesHWNDHaveFlag flagKey hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ () | ComponentFlagAttr flagKey' <- attrs, flagKey' == flagKey ] of
            [ () ] -> pure True
            []     -> pure False
            x      -> error $ "Illegal AttributeMap state: " <> show x

getEventHandlerFromHWNDMaybe :: EventType -> Win32.HWND -> IO (Maybe TEAInternal.Msg)
getEventHandlerFromHWNDMaybe eventType hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ msg | ComponentEventHandlerAttr eventType' msg <- attrs, eventType' == eventType ] of
            [ msg ] -> pure (Just msg)
            []      -> pure Nothing
            x       -> error $ "Illegal AttributeMap state: " <> show x

getEventHandlerFromHWND :: EventType -> Win32.HWND -> IO TEAInternal.Msg
getEventHandlerFromHWND eventType hwnd =
    getEventHandlerFromHWNDMaybe eventType hwnd >>= \case
        Just msg -> pure msg
        Nothing  -> error "Illegal AttributeMap state: []"

getComponentBackgroundColourFromHWNDMaybe :: Win32.HWND -> IO (Maybe Colour)
getComponentBackgroundColourFromHWNDMaybe hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ colour | ComponentBackgroundColourAttr colour <- attrs ] of
            [ colour ] -> pure (Just colour)
            []         -> pure Nothing
            x          -> error $ "Illegal AttributeMap state: " <> show x

getComponentBackgroundColourFromHWND :: Win32.HWND -> IO Colour
getComponentBackgroundColourFromHWND hwnd =
    getComponentBackgroundColourFromHWNDMaybe hwnd >>= \case
        Just colour -> pure colour
        Nothing     -> error "Illegal AttributeMap state: []"

getComponentFontFromHWND :: Win32.HWND -> IO Font
getComponentFontFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ font | ComponentFontAttr font <- attrs ] of
            [ font ] -> pure font
            x        -> error $ "Illegal AttributeMap state: " <> show x

getComponentTitleFromHWND :: Win32.HWND -> IO Text
getComponentTitleFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ title | ComponentTitleAttr title <- attrs ] of
            [ title ] -> pure title
            x         -> error $ "Illegal AttributeMap state: " <> show x

getWindowClassNameFromHWND :: Win32.HWND -> IO Text
getWindowClassNameFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ className | WindowClassNameAttr className <- attrs ] of
            [ className ] -> pure className
            x             -> error $ "Illegal AttributeMap state: " <> show x

getWindowStyleFromHWND :: Win32.HWND -> IO WindowStyle
getWindowStyleFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ windowStyle | WindowStyleAttr windowStyle <- attrs ] of
            [ windowStyle ] -> pure windowStyle
            x               -> error $ "Illegal AttributeMap state: " <> show x

getWindowCursorFromHWND :: Win32.HWND -> IO Cursor
getWindowCursorFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ cursor | WindowCursorAttr cursor <- attrs ] of
            [ cursor ] -> pure cursor
            x          -> error $ "Illegal AttributeMap state: " <> show x

getWindowIconFromHWND :: Win32.HWND -> IO Icon
getWindowIconFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ icon | WindowIconAttr icon <- attrs ] of
            [ icon ] -> pure icon
            x        -> error $ "Illegal AttributeMap state: " <> show x
