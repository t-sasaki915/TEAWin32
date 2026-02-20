module TEAWin32.GUI.Component.Internal.Attribute
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
    , isManagedByTEAWin32
    , isComponentWindow
    , getComponentUniqueIdFromHWND
    , getComponentTypeFromHWND
    , getComponentCurrentDPIFromHWND
    , doesHWNDHaveFlag
    , getEventHandlerFromHWNDMaybe
    , getEventHandlerFromHWND
    , getComponentBackgroundColourFromHWNDMaybe
    , getComponentBackgroundColourFromHWND
    , getComponentFontFromHWND
    , getComponentTitleFromHWND
    , getComponentSizeFromHWND
    , getComponentPositionFromHWND
    , getComponentZIndexFromHWND
    , getWindowClassNameFromHWND
    , getWindowStyleFromHWND
    , getWindowCursorFromHWND
    , getWindowIconFromHWND
    ) where

import           Control.Concurrent            (MVar, modifyMVar, newMVar,
                                                readMVar)
import           Data.Functor                  ((<&>))
import qualified Data.List                     as List
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           GHC.IO                        (unsafePerformIO)
import           GHC.Stack                     (HasCallStack)
import qualified Graphics.Win32                as Win32
import qualified TEAWin32.Application.Internal as ApplicationInternal
import           TEAWin32.Drawing              (Colour)
import           TEAWin32.GUI                  (Cursor, Font, Icon,
                                                ScalableValue, UniqueId,
                                                WindowStyle)
import           TEAWin32.GUI.Component        (ComponentType (..))
import           TEAWin32.Internal             (throwTEAWin32InternalError)

data EventType = ComponentClickEvent
               deriving (Eq, Show, Ord)

data ComponentFlagKey = ComponentTitleSet
                      | ComponentSizeSet
                      | ComponentPositionSet
                      | ComponentFontSet
                      | ComponentOnClickSet
                      | ComponentChildrenSet
                      | ComponentBackgroundColourSet
                      | ComponentZIndexSet
                      | WindowIconSet
                      | WindowCursorSet
                      | WindowBackgroundColourSet
                      deriving (Eq, Show, Ord)

data ComponentAttribute = ComponentUniqueIdAttr         UniqueId
                        | ComponentTypeAttr             ComponentType
                        | ComponentCurrentDPIAttr       Int
                        | ComponentFlagAttr             ComponentFlagKey
                        | ComponentEventHandlerAttr     EventType ApplicationInternal.Msg
                        | ComponentBackgroundColourAttr Colour
                        | ComponentFontAttr             Font
                        | ComponentTitleAttr            Text
                        | ComponentSizeAttr             (ScalableValue, ScalableValue)
                        | ComponentPositionAttr         (ScalableValue, ScalableValue)
                        | ComponentZIndexAttr           Int
                        | WindowClassNameAttr           Text
                        | WindowStyleAttr               WindowStyle
                        | WindowCursorAttr              Cursor
                        | WindowIconAttr                Icon
                        deriving (Eq, Show)

isSameKind :: ComponentAttribute -> ComponentAttribute -> Bool
isSameKind (ComponentUniqueIdAttr _) (ComponentUniqueIdAttr _)                 = True
isSameKind (ComponentTypeAttr _) (ComponentTypeAttr _)                         = True
isSameKind (ComponentCurrentDPIAttr _) (ComponentCurrentDPIAttr _)             = True
isSameKind (ComponentFlagAttr _) (ComponentFlagAttr _)                         = True
isSameKind (ComponentEventHandlerAttr _ _) (ComponentEventHandlerAttr _ _)     = True
isSameKind (ComponentBackgroundColourAttr _) (ComponentBackgroundColourAttr _) = True
isSameKind (ComponentFontAttr _) (ComponentFontAttr _)                         = True
isSameKind (ComponentTitleAttr _) (ComponentTitleAttr _)                       = True
isSameKind (ComponentSizeAttr _) (ComponentSizeAttr _)                         = True
isSameKind (ComponentPositionAttr _) (ComponentPositionAttr _)                 = True
isSameKind (WindowClassNameAttr _) (WindowClassNameAttr _)                     = True
isSameKind (WindowStyleAttr _) (WindowStyleAttr _)                             = True
isSameKind (WindowCursorAttr _) (WindowCursorAttr _)                           = True
isSameKind (WindowIconAttr _) (WindowIconAttr _)                               = True
isSameKind _ _                                                                 = False

attributeMapRef :: MVar (Map Win32.HWND [ComponentAttribute])
attributeMapRef = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE attributeMapRef #-}

registerHWNDToAttributeMap :: Win32.HWND -> IO ()
registerHWNDToAttributeMap hwnd =
    modifyMVar attributeMapRef $ \attrMap ->
        let newMap = Map.insert hwnd [] attrMap in
            pure (newMap, ())

unregisterHWNDFromAttributeMap :: Win32.HWND -> IO ()
unregisterHWNDFromAttributeMap hwnd =
    modifyMVar attributeMapRef $ \attrMap ->
        let newMap = Map.delete hwnd attrMap in
            pure (newMap, ())

addAttributeToHWND :: HasCallStack => Win32.HWND -> ComponentAttribute -> IO ()
addAttributeToHWND hwnd attr =
    modifyMVar attributeMapRef $ \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs ->
                let newHWNDAttrs = hwndAttrs ++ [attr]
                    newMap = Map.insert hwnd newHWNDAttrs attrMap in
                        pure (newMap, ())
            Nothing ->
                throwTEAWin32InternalError $ "AttributeMap for " <> Text.show hwnd <> " is not initialised."

updateAttributeOfHWND :: HasCallStack => Win32.HWND -> ComponentAttribute -> IO ()
updateAttributeOfHWND hwnd newAttr =
    modifyMVar attributeMapRef $ \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs ->
                let newHWNDAttrs = [attr | attr <- hwndAttrs, not (attr `isSameKind` newAttr)] ++ [newAttr]
                    newMap = Map.insert hwnd newHWNDAttrs attrMap in
                        pure (newMap, ())

            Nothing ->
                throwTEAWin32InternalError $ "AttributeMap for " <> Text.show hwnd <> " is not initialised."

removeAttributeFromHWND :: HasCallStack => Win32.HWND -> ComponentAttribute -> IO ()
removeAttributeFromHWND hwnd attr =
    modifyMVar attributeMapRef $ \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs ->
                let newHWNDAttrs = List.delete attr hwndAttrs
                    newMap = Map.insert hwnd newHWNDAttrs attrMap in
                        pure (newMap, ())
            Nothing ->
                throwTEAWin32InternalError $ "AttributeMap for " <> Text.show hwnd <> " is not initialised."

getAttributesFromHWND :: HasCallStack => Win32.HWND -> IO [ComponentAttribute]
getAttributesFromHWND hwnd =
    readMVar attributeMapRef >>= \attrMap ->
        case Map.lookup hwnd attrMap of
            Just hwndAttrs -> pure hwndAttrs
            Nothing        -> throwTEAWin32InternalError $ "AttributeMap for " <> Text.show hwnd <> " is not initialised."

isManagedByTEAWin32 :: Win32.HWND -> IO Bool
isManagedByTEAWin32 hwnd =
    readMVar attributeMapRef >>= \attrMap ->
        pure $ hwnd `Map.member` attrMap

isComponentWindow :: Win32.HWND -> IO Bool
isComponentWindow hwnd =
    getComponentTypeFromHWND hwnd <&> (== ComponentWindow)

getComponentUniqueIdFromHWND :: HasCallStack => Win32.HWND -> IO UniqueId
getComponentUniqueIdFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ uniqueId | ComponentUniqueIdAttr uniqueId <- attrs ] of
            [ uniqueId ] -> pure uniqueId
            x            -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getComponentTypeFromHWND :: HasCallStack => Win32.HWND -> IO ComponentType
getComponentTypeFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ componentType | ComponentTypeAttr componentType <- attrs ] of
            [ componentType ] -> pure componentType
            x                 -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getComponentCurrentDPIFromHWND :: HasCallStack => Win32.HWND -> IO Int
getComponentCurrentDPIFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ currentDPI | ComponentCurrentDPIAttr currentDPI <- attrs ] of
            [ currentDPI ] -> pure currentDPI
            x              -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

doesHWNDHaveFlag :: HasCallStack => ComponentFlagKey -> Win32.HWND -> IO Bool
doesHWNDHaveFlag flagKey hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ () | ComponentFlagAttr flagKey' <- attrs, flagKey' == flagKey ] of
            [ () ] -> pure True
            []     -> pure False
            x      -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getEventHandlerFromHWNDMaybe :: HasCallStack => EventType -> Win32.HWND -> IO (Maybe ApplicationInternal.Msg)
getEventHandlerFromHWNDMaybe eventType hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ msg | ComponentEventHandlerAttr eventType' msg <- attrs, eventType' == eventType ] of
            [ msg ] -> pure (Just msg)
            []      -> pure Nothing
            x       -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getEventHandlerFromHWND :: HasCallStack => EventType -> Win32.HWND -> IO ApplicationInternal.Msg
getEventHandlerFromHWND eventType hwnd =
    getEventHandlerFromHWNDMaybe eventType hwnd >>= \case
        Just msg -> pure msg
        Nothing  -> throwTEAWin32InternalError "Illegal AttributeMap state: []"

getComponentBackgroundColourFromHWNDMaybe :: HasCallStack => Win32.HWND -> IO (Maybe Colour)
getComponentBackgroundColourFromHWNDMaybe hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ colour | ComponentBackgroundColourAttr colour <- attrs ] of
            [ colour ] -> pure (Just colour)
            []         -> pure Nothing
            x          -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getComponentBackgroundColourFromHWND :: HasCallStack => Win32.HWND -> IO Colour
getComponentBackgroundColourFromHWND hwnd =
    getComponentBackgroundColourFromHWNDMaybe hwnd >>= \case
        Just colour -> pure colour
        Nothing     -> throwTEAWin32InternalError "Illegal AttributeMap state: []"

getComponentFontFromHWND :: HasCallStack => Win32.HWND -> IO Font
getComponentFontFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ font | ComponentFontAttr font <- attrs ] of
            [ font ] -> pure font
            x        -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getComponentTitleFromHWND :: HasCallStack => Win32.HWND -> IO Text
getComponentTitleFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ title | ComponentTitleAttr title <- attrs ] of
            [ title ] -> pure title
            x         -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getComponentSizeFromHWND :: HasCallStack => Win32.HWND -> IO (ScalableValue, ScalableValue)
getComponentSizeFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ size | ComponentSizeAttr size <- attrs ] of
            [ size ] -> pure size
            x        -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getComponentPositionFromHWND :: HasCallStack => Win32.HWND -> IO (ScalableValue, ScalableValue)
getComponentPositionFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ position | ComponentPositionAttr position <- attrs ] of
            [ position ] -> pure position
            x            -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getComponentZIndexFromHWND :: HasCallStack => Win32.HWND -> IO Int
getComponentZIndexFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ zIndex | ComponentZIndexAttr zIndex <- attrs ] of
            [ zIndex ] -> pure zIndex
            x          -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getWindowClassNameFromHWND :: HasCallStack => Win32.HWND -> IO Text
getWindowClassNameFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ className | WindowClassNameAttr className <- attrs ] of
            [ className ] -> pure className
            x             -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getWindowStyleFromHWND :: HasCallStack => Win32.HWND -> IO WindowStyle
getWindowStyleFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ windowStyle | WindowStyleAttr windowStyle <- attrs ] of
            [ windowStyle ] -> pure windowStyle
            x               -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getWindowCursorFromHWND :: HasCallStack => Win32.HWND -> IO Cursor
getWindowCursorFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ cursor | WindowCursorAttr cursor <- attrs ] of
            [ cursor ] -> pure cursor
            x          -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x

getWindowIconFromHWND :: HasCallStack => Win32.HWND -> IO Icon
getWindowIconFromHWND hwnd =
    getAttributesFromHWND hwnd >>= \attrs ->
        case [ icon | WindowIconAttr icon <- attrs ] of
            [ icon ] -> pure icon
            x        -> throwTEAWin32InternalError $ "Illegal AttributeMap state: " <> Text.show x
