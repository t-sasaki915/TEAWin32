module Graphics.GUI.Component.Internal
    ( windowClassPrefix
    , compareGUIComponents
    , setComponentType
    , getComponentType
    , unregisterComponentType
    , setUniqueIdToHWND
    , getUniqueIdFromHWND
    , unregisterUniqueIdFromHWND
    , getClassName
    , getWindowTitle
    , setFlag
    , isFlagSet
    , unsetFlag
    , unsetFlags
    , getRelativeRect
    , getWindowStyle
    , getWindowCursor
    , setEventHandler
    , getEventHandler
    , getEventHandlerMaybe
    , unregisterEventHandler
    , unregisterEventHandlers
    , attachGDI
    , getGDI
    , unattachGDI
    , unattachGDIs
    , withProps
    , isManagedWindow
    , restoreComponentFromHWND
    ) where

import                          Control.Monad                          (void,
                                                                        (>=>))
import                          Data.Functor                           ((<&>))
import                          Data.IORef                             (modifyIORef,
                                                                        newIORef,
                                                                        readIORef)
import                          Data.List                              (isPrefixOf)
import                qualified Data.Map                               as Map
import                          Data.Text                              (Text)
import                qualified Data.Text                              as Text
import                          Foreign                                hiding
                                                                       (new,
                                                                        void)
import                qualified Framework.TEA.Internal                 as TEAInternal
import                          Graphics.GUI
import                          Graphics.GUI.Component                 (GUIComponent,
                                                                        IsGUIComponent (..))
import {-# SOURCE #-}           Graphics.GUI.Component.Button.Internal (restoreButtonFromHWND)
import {-# SOURCE #-}           Graphics.GUI.Component.Window.Internal (restoreWindowFromHWND)
import                qualified Graphics.GUI.Foreign                   as Win32
import                qualified Graphics.GUI.Internal                  as GUIInternal
import                qualified Graphics.Win32                         as Win32

componentTypePropName :: String
componentTypePropName = "TEAWIN32GUI_COMPONENT_TYPE"

componentUniqueIdPropName :: String
componentUniqueIdPropName = "TEAWIN32GUI_COMPONENT_UNIQUE_ID"

componentFlagPropNamePrefix :: String
componentFlagPropNamePrefix = "TEAWIN32GUI_COMPONENT_FLAG_"

componentEventHandlerPropNamePrefix :: String
componentEventHandlerPropNamePrefix = "TEAWIN32GUI_COMPONENT_EVENTHANDLER_"

componentGDIPropNamePrefix :: String
componentGDIPropNamePrefix = "TEAWIN32GUI_COMPONENT_GDI_"

windowClassPrefix :: String
windowClassPrefix = "TEAWIN32GUI_WINDOW_"

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [(GUIComponent, GUIComponent)])
compareGUIComponents new old = (added, deleted, redraw, propertyChanged)
    where
        newMap = Map.fromList [ (getUniqueId x, x) | x <- new ]
        oldMap = Map.fromList [ (getUniqueId x, x) | x <- old ]

        added   = Map.elems $ Map.difference newMap oldMap
        deleted = Map.elems $ Map.difference oldMap newMap

        commonKeys = Map.keys $ Map.intersection newMap oldMap
        (redraw, propertyChanged) = foldr (checkChange newMap oldMap) ([], []) commonKeys

        checkChange nMap oMap k (redr, propc) =
            let newValue = nMap Map.! k
                oldValue = oMap Map.! k in
                    if newValue == oldValue
                        then (redr, propc)
                        else
                            if doesNeedToRedraw oldValue newValue
                                then (newValue : redr, propc)
                                else (redr, (newValue, oldValue) : propc)

setComponentType :: Text -> Win32.HWND -> IO ()
setComponentType componentType hwnd =
    Win32.withTString componentTypePropName $ \pName ->
        newStablePtr componentType >>= \componentTypePtr ->
            void $ Win32.c_SetProp hwnd pName (castStablePtrToPtr componentTypePtr)

getComponentType :: Win32.HWND -> IO Text
getComponentType hwnd =
    Win32.withTString componentTypePropName $
        Win32.c_GetProp hwnd >=> deRefStablePtr . castPtrToStablePtr

unregisterComponentType :: Win32.HWND -> IO ()
unregisterComponentType hwnd =
    Win32.withTString componentTypePropName $
        Win32.c_RemoveProp hwnd >=> freeStablePtr . castPtrToStablePtr

setUniqueIdToHWND :: UniqueId -> Win32.HWND -> IO ()
setUniqueIdToHWND uniqueId hwnd =
    Win32.withTString componentUniqueIdPropName $ \pName ->
        newStablePtr uniqueId >>= \uniqueIdPtr ->
            void $ Win32.c_SetProp hwnd pName (castStablePtrToPtr uniqueIdPtr)

getUniqueIdFromHWND :: Win32.HWND -> IO UniqueId
getUniqueIdFromHWND hwnd =
    Win32.withTString componentUniqueIdPropName $
        Win32.c_GetProp hwnd >=> deRefStablePtr . castPtrToStablePtr

unregisterUniqueIdFromHWND :: Win32.HWND -> IO ()
unregisterUniqueIdFromHWND hwnd =
    Win32.withTString componentUniqueIdPropName $
        Win32.c_RemoveProp hwnd >=> freeStablePtr . castPtrToStablePtr

getClassName :: Win32.HWND -> IO Text
getClassName hwnd =
    allocaArray 256 $ \pBuf -> do
        Win32.c_GetClassName hwnd pBuf 256 >>
            Win32.peekTString pBuf <&> Text.pack

getWindowTitle :: Win32.HWND -> IO Text
getWindowTitle hwnd =
    Win32.c_GetWindowTextLength hwnd >>= \case
        0   -> pure ""
        len -> allocaArray (len + 1) $ \pBuf ->
            Win32.c_GetWindowText hwnd pBuf (len + 1) >>
                Text.pack <$> Win32.peekTString pBuf

getWindowStyle :: Win32.HWND -> IO WindowStyle
getWindowStyle hwnd =
    fromWin32WindowStyle . fromIntegral <$> Win32.c_GetWindowLongPtr hwnd Win32.gWL_STYLE

getWindowCursor :: Win32.HWND -> IO Cursor
getWindowCursor hwnd =
    fromWin32Cursor . intPtrToPtr . fromIntegral <$> Win32.c_GetClassLongPtr hwnd Win32.gCLP_HCURSOR

setFlag :: String -> Win32.HWND -> IO ()
setFlag flagName hwnd =
    Win32.withTString (componentFlagPropNamePrefix <> flagName) $ \pName ->
        void $ Win32.c_SetProp hwnd pName (intPtrToPtr 1)

isFlagSet :: String -> Win32.HWND -> IO Bool
isFlagSet flagName hwnd =
    Win32.withTString (componentFlagPropNamePrefix <> flagName) $
        fmap (/= Win32.nullPtr) . Win32.c_GetProp hwnd

unsetFlag :: String -> Win32.HWND -> IO ()
unsetFlag flagName hwnd =
    void $ Win32.withTString (componentFlagPropNamePrefix <> flagName) $ Win32.c_RemoveProp hwnd

unsetFlags :: Win32.HWND -> IO ()
unsetFlags hwnd =
    withProps hwnd $ mapM_ $ \case
        (propName, _) | componentFlagPropNamePrefix `isPrefixOf` propName ->
            void $ Win32.withTString propName $ Win32.c_RemoveProp hwnd

        _ -> pure ()

getRelativeRect :: Win32.HWND -> IO (Int, Int, Int, Int)
getRelativeRect hwnd = do
    (l', t', r', b') <- Win32.getWindowRect hwnd
    let (l, t, r, b) = (fromIntegral l', fromIntegral t', fromIntegral r', fromIntegral b')

    GUIInternal.isTopLevelWindow hwnd >>= \case
        True  -> pure (l, t, r - l, b - t)
        False -> do
            parentHWND <- Win32.getParent hwnd
            (x, y) <- Win32.screenToClient parentHWND (fromIntegral l, fromIntegral t)

            pure (fromIntegral x, fromIntegral y, r - l, b - t)

setEventHandler :: String -> TEAInternal.Msg -> Win32.HWND -> IO ()
setEventHandler eventType msg hwnd =
    Win32.withTString (componentEventHandlerPropNamePrefix <> eventType) $ \pName ->
        newStablePtr msg >>= \componentTypePtr ->
            void $ Win32.c_SetProp hwnd pName (castStablePtrToPtr componentTypePtr)

getEventHandler :: String -> Win32.HWND -> IO TEAInternal.Msg
getEventHandler eventType hwnd =
    Win32.withTString (componentEventHandlerPropNamePrefix <> eventType) $
        Win32.c_GetProp hwnd >=> deRefStablePtr . castPtrToStablePtr

getEventHandlerMaybe :: String -> Win32.HWND -> IO (Maybe TEAInternal.Msg)
getEventHandlerMaybe eventType hwnd =
    Win32.withTString (componentEventHandlerPropNamePrefix <> eventType) $ \pName -> do
        ptr <- Win32.c_GetProp hwnd pName
        if ptr /= Win32.nullPtr
            then Just <$> deRefStablePtr (castPtrToStablePtr ptr)
            else pure Nothing

unregisterEventHandler :: String -> Win32.HWND -> IO ()
unregisterEventHandler eventType hwnd =
    Win32.withTString (componentEventHandlerPropNamePrefix <> eventType) $
        Win32.c_RemoveProp hwnd >=> freeStablePtr . castPtrToStablePtr

unregisterEventHandlers :: Win32.HWND -> IO ()
unregisterEventHandlers hwnd =
    withProps hwnd $ mapM_ $ \case
        (propName, _) | componentEventHandlerPropNamePrefix `isPrefixOf` propName ->
            Win32.withTString propName $
                Win32.c_RemoveProp hwnd >=> freeStablePtr . castPtrToStablePtr

        _ -> pure ()

attachGDI :: String -> Win32.HANDLE -> Win32.HWND -> IO ()
attachGDI gdiName hndl hwnd =
    Win32.withTString (componentGDIPropNamePrefix <> gdiName) $ \pName ->
        void $ Win32.c_SetProp hwnd pName hndl

getGDI :: String -> Win32.HWND -> IO Win32.HANDLE
getGDI gdiName hwnd =
    Win32.withTString (componentGDIPropNamePrefix <> gdiName) $ Win32.c_GetProp hwnd

unattachGDI :: String -> Win32.HWND -> IO ()
unattachGDI gdiName hwnd =
    Win32.withTString (componentGDIPropNamePrefix <> gdiName) $
        Win32.c_RemoveProp hwnd >=> void . Win32.c_DeleteObject . castPtr

unattachGDIs :: Win32.HWND -> IO ()
unattachGDIs hwnd =
    withProps hwnd $ mapM_ $ \case
        (propName, _) | componentGDIPropNamePrefix `isPrefixOf` propName ->
            Win32.withTString propName $
                Win32.c_RemoveProp hwnd >=> void . Win32.c_DeleteObject . castPtr

        _ -> pure ()

withProps :: Win32.HWND -> ([(String, Win32.HANDLE)] -> IO a) -> IO a
withProps hwnd func = do
    propsRef <- newIORef []

    let callback _ hLabel hData _ =
            Win32.peekTString hLabel >>= \label ->
                modifyIORef propsRef ((label, hData) :) >>
                    pure True

    enumProc <- Win32.makePropEnumProcEx callback
    _        <- Win32.c_EnumPropsEx hwnd enumProc 0

    props <- readIORef propsRef
    x     <- func props

    freeHaskellFunPtr enumProc

    pure x

isManagedWindow :: Win32.HWND -> IO Bool
isManagedWindow hwnd =
    getClassName hwnd >>= \className ->
        pure $ windowClassPrefix `isPrefixOf` Text.unpack className

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent
restoreComponentFromHWND hwnd =
    getComponentType hwnd >>= \case
        "BUTTON" -> restoreButtonFromHWND hwnd
        "WINDOW" -> restoreWindowFromHWND hwnd
        _        -> error "Unrecognisable component type"

