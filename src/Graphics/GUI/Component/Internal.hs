module Graphics.GUI.Component.Internal
    ( windowClassPrefix
    , compareGUIComponents
    , setComponentType
    , getComponentType
    , setUniqueIdToHWND
    , getUniqueIdFromHWND
    , getClassName
    , getWindowTitle
    , setFlag
    , isFlagSet
    , unsetFlag
    , getRelativeRect
    , getWindowStyle
    , getWindowCursor
    , getWindowIcon
    , setEventHandler
    , getEventHandler
    , getEventHandlerMaybe
    , unregisterEventHandler
    , attachGDI
    , getGDI
    , unattachGDI
    , isManagedWindow
    , restoreComponentFromHWND
    ) where

import                          Data.Functor                           ((<&>))
import                          Data.List                              (isPrefixOf)
import                qualified Data.Map                               as Map
import                          Data.Maybe                             (isJust)
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
import                          Graphics.GUI.Component.Internal.Prop
import {-# SOURCE #-}           Graphics.GUI.Component.Window.Internal (restoreWindowFromHWND)
import                qualified Graphics.GUI.Foreign                   as Win32
import                qualified Graphics.GUI.Internal                  as GUIInternal
import                qualified Graphics.Win32                         as Win32

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
    setProp hwnd "ComponentType" (ComponentType componentType)

getComponentType :: Win32.HWND -> IO Text
getComponentType hwnd =
    getProp hwnd "ComponentType" >>= \case
        Just (ComponentType cType) -> pure cType
        _                          -> error "ComponentType not found."

setUniqueIdToHWND :: UniqueId -> Win32.HWND -> IO ()
setUniqueIdToHWND uniqueId hwnd =
    setProp hwnd "ComponentUniqueId" (ComponentUniqueId uniqueId)

getUniqueIdFromHWND :: Win32.HWND -> IO UniqueId
getUniqueIdFromHWND hwnd =
    getProp hwnd "ComponentUniqueId" >>= \case
        Just (ComponentUniqueId uniqueId) -> pure uniqueId
        _                                 -> error "ComponentUniqueId not found."

setFlag :: Text -> Win32.HWND -> IO ()
setFlag flagName hwnd = setProp hwnd ("ComponentFlag_" <> flagName) ComponentFlag

isFlagSet :: Text -> Win32.HWND -> IO Bool
isFlagSet flagName hwnd = isJust <$> getProp hwnd ("ComponentFlag_" <> flagName)

unsetFlag :: Text -> Win32.HWND -> IO ()
unsetFlag flagName hwnd = removeProp hwnd ("ComponentFlag_" <> flagName)

setEventHandler :: Text -> TEAInternal.Msg -> Win32.HWND -> IO ()
setEventHandler eventType msg hwnd =
    setProp hwnd ("ComponentEventHandler_" <> eventType) (ComponentEventHandler msg)

getEventHandler :: Text -> Win32.HWND -> IO TEAInternal.Msg
getEventHandler eventType hwnd =
    getProp hwnd ("ComponentEventHandler_" <> eventType) >>= \case
        Just (ComponentEventHandler msg) -> pure msg
        _                                -> error "ComponentEventHandler not found."

getEventHandlerMaybe :: Text -> Win32.HWND -> IO (Maybe TEAInternal.Msg)
getEventHandlerMaybe eventType hwnd =
    getProp hwnd ("ComponentEventHandler_" <> eventType) >>= \case
        Just (ComponentEventHandler msg) -> pure (Just msg)
        Nothing                          -> pure Nothing
        _                                -> error "ComponentEventHandler not found."

unregisterEventHandler :: Text -> Win32.HWND -> IO ()
unregisterEventHandler eventType hwnd =
    removeProp hwnd ("ComponentEventHandler_" <> eventType)

attachGDI :: Text -> Win32.HANDLE -> Win32.HWND -> IO ()
attachGDI gdiName hndl hwnd =
    setProp hwnd ("ComponentGDIResource_" <> gdiName) (ComponentGDIResource hndl)

getGDI :: Text -> Win32.HWND -> IO Win32.HANDLE
getGDI gdiName hwnd =
    getProp hwnd ("ComponentGDIResource_" <> gdiName) >>= \case
        Just (ComponentGDIResource hndl) -> pure hndl
        _                                -> error "ComponentGDIResource not found."

unattachGDI :: Text -> Win32.HWND -> IO ()
unattachGDI gdiName hwnd =
    let propName = ("ComponentGDIResource_" <> gdiName) in
        getProp hwnd propName >>= \case
            Just x@(ComponentGDIResource _) ->
                finalisePropValue x >>
                    removeProp hwnd propName

            _ -> error "ComponentGDIResource not found."

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

getWindowIcon :: Win32.HWND -> IO Icon
getWindowIcon hwnd =
    Win32.c_GetClassLongPtr hwnd Win32.gCLP_HICON >>=
        fromWin32Icon . intPtrToPtr . fromIntegral

getWindowCursor :: Win32.HWND -> IO Cursor
getWindowCursor hwnd =
    Win32.c_GetClassLongPtr hwnd Win32.gCLP_HCURSOR >>=
        fromWin32Cursor . intPtrToPtr . fromIntegral

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

