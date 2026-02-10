module Graphics.GUI.Component.Internal
    ( compareGUIComponents
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
    , getRelativeRect
    , restoreComponentFromHWND
    ) where

import                          Control.Monad                          (void,
                                                                        (>=>))
import                qualified Data.Map                               as Map
import                          Data.Text                              (Text)
import                qualified Data.Text                              as Text
import                          Foreign                                hiding
                                                                       (new,
                                                                        void)
import                          Graphics.GUI                           (UniqueId)
import                          Graphics.GUI.Component                 (GUIComponent,
                                                                        IsGUIComponent (..))
import {-# SOURCE #-}           Graphics.GUI.Component.Button.Internal (restoreButtonFromHWND)
import                qualified Graphics.GUI.Foreign                   as Win32
import                qualified Graphics.Win32                         as Win32

componentTypePropName :: String
componentTypePropName = "TEAWIN32GUI_COMPONENT_TYPE"

componentUniqueIdPropName :: String
componentUniqueIdPropName = "TEAWIN32GUI_COMPONENT_UNIQUE_ID"

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
    allocaArray 256 $ \pBuf ->
        Text.pack <$> (Win32.c_GetClassName hwnd pBuf 256 >>= Win32.peekTString . intPtrToPtr . fromIntegral)

getWindowTitle :: Win32.HWND -> IO Text
getWindowTitle hwnd = do
    Win32.getWindowTextLength hwnd >>= \textLength ->
        Text.pack <$> Win32.getWindowText hwnd (textLength + 1)

setFlag :: String -> Win32.HWND -> IO ()
setFlag flagName hwnd =
    Win32.withTString flagName $ \pName ->
        void $ Win32.c_SetProp hwnd pName (intPtrToPtr 1)

isFlagSet :: String -> Win32.HWND -> IO Bool
isFlagSet flagName hwnd =
    Win32.withTString flagName $
        fmap (/= Win32.nullPtr) . Win32.c_GetProp hwnd

unsetFlag :: String -> Win32.HWND -> IO ()
unsetFlag flagName hwnd =
    Win32.withTString flagName $ \pName ->
        void $ Win32.c_RemoveProp hwnd pName

getRelativeRect :: Win32.HWND -> IO (Int, Int, Int, Int)
getRelativeRect hwnd = do
    (l', t', r', b') <- Win32.getWindowRect hwnd
    let (l, t, r, b) = (fromIntegral l', fromIntegral t', fromIntegral r', fromIntegral b')

    parentHWND <- Win32.getParent hwnd

    if parentHWND == Win32.nullPtr
        then pure (l, t, r - l, b - t)
        else do
            (x, y) <- Win32.screenToClient parentHWND (fromIntegral l, fromIntegral t)

            pure (fromIntegral x, fromIntegral y, r - l, b - t)

restoreComponentFromHWND :: Win32.HWND -> IO (Maybe GUIComponent)
restoreComponentFromHWND hwnd =
    getComponentType hwnd >>= \case
        "BUTTON" -> Just <$> restoreButtonFromHWND hwnd
        _        -> pure Nothing

