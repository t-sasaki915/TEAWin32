module Graphics.GUI.Component.Internal
    ( compareGUIComponents
    , getRelativeRectFromHWNDUsingWin32
    , setComponentTitle
    , setComponentPosition
    , setComponentSize
    , setComponentFont
    , useDefaultFont
    , setWindowIcon
    , setWindowCursor
    , requestRedraw
    , restoreComponentFromHWND
    ) where

import                          Data.Functor                              (void)
import                qualified Data.Map                                  as Map
import                          Data.Text                                 (Text)
import                qualified Data.Text                                 as Text
import                          Foreign                                   hiding
                                                                          (new,
                                                                           void)
import                          Graphics.GUI
import                          Graphics.GUI.Component                    (GUIComponent,
                                                                           IsGUIComponent (..))
import {-# SOURCE #-}           Graphics.GUI.Component.Button.Internal    (restoreButtonFromHWND)
import                          Graphics.GUI.Component.Internal.Attribute
import {-# SOURCE #-}           Graphics.GUI.Component.Window.Internal    (restoreWindowFromHWND)
import                qualified Graphics.GUI.Foreign                      as Win32
import                qualified Graphics.GUI.Internal                     as GUIInternal
import                qualified Graphics.Win32                            as Win32

data UpdateAction = Render
                  | UpdateProperties
                  | Redraw
                  | Delete
                  | NoChange

compareGUIComponents' :: [GUIComponent] -> [GUIComponent] -> [(UpdateAction, GUIComponent)]
compareGUIComponents' newComponents oldComponents =
    let newComponentsWithUniqueId = Map.fromList [ (getUniqueId x, x) | x <- newComponents ]
        oldComponentsWithUniqueId = Map.fromList [ (getUniqueId x, x) | x <- oldComponents ]
        deletedComponents = [ (Delete, x) | x <- Map.elems $ Map.difference oldComponentsWithUniqueId newComponentsWithUniqueId ]
        newComponentsWithAction =
            flip map newComponents $ \newComponent ->
                case Map.lookup (getUniqueId newComponent) oldComponentsWithUniqueId of
                    Just oldComponent | newComponent == oldComponent ->
                        (NoChange, newComponent)

                    Just oldComponent | doesNeedToRedraw oldComponent newComponent ->
                        (Redraw, newComponent)

                    Just _ ->
                        (UpdateProperties, newComponent)

                    Nothing ->
                        (Render, newComponent)

    in deletedComponents ++ newComponentsWithAction

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

getRelativeRectFromHWNDUsingWin32 :: Win32.HWND -> IO (Int, Int, Int, Int)
getRelativeRectFromHWNDUsingWin32 hwnd = do
    (l', t', r', b') <- Win32.getWindowRect hwnd
    let (l, t, r, b) = (fromIntegral l', fromIntegral t', fromIntegral r', fromIntegral b')

    GUIInternal.isTopLevelWindow hwnd >>= \case
        True  -> pure (l, t, r - l, b - t)
        False -> do
            parentHWND <- Win32.getParent hwnd
            (x, y) <- Win32.screenToClient parentHWND (fromIntegral l, fromIntegral t)

            pure (fromIntegral x, fromIntegral y, r - l, b - t)

setComponentTitle :: Text -> Win32.HWND -> IO ()
setComponentTitle title hwnd = Win32.setWindowText hwnd (Text.unpack title)

setComponentPosition :: Int -> Int -> Win32.HWND -> IO ()
setComponentPosition x y hwnd = void $ Win32.c_SetWindowPos
    hwnd
    Win32.nullPtr
    (fromIntegral x)
    (fromIntegral y)
    0
    0
    (Win32.sWP_NOSIZE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

setComponentSize :: Int -> Int -> Win32.HWND -> IO ()
setComponentSize width height hwnd = void $ Win32.c_SetWindowPos
    hwnd
    Win32.nullPtr
    0
    0
    (fromIntegral width)
    (fromIntegral height)
    (Win32.sWP_NOMOVE .|. Win32.sWP_NOZORDER .|. Win32.sWP_NOACTIVATE)

setComponentFont :: Font -> Win32.HWND -> IO ()
setComponentFont font hwnd =
    toWin32Font font >>= \font' ->
        void $ Win32.sendMessage hwnd Win32.wM_SETFONT (fromIntegral $ ptrToWordPtr font') 1

useDefaultFont :: Win32.HWND -> IO ()
useDefaultFont = setComponentFont DefaultGUIFont

setWindowIcon :: Icon -> Win32.HWND -> IO ()
setWindowIcon icon hwnd =
    toWin32Icon icon >>= \icon' ->
        void $ Win32.c_SetClassLongPtr hwnd Win32.gCLP_HICON icon'

setWindowCursor :: Cursor -> Win32.HWND -> IO ()
setWindowCursor cursor hwnd =
    toWin32Cursor cursor >>= \cursor' ->
        void $ Win32.c_SetClassLongPtr hwnd Win32.gCLP_HCURSOR cursor'

requestRedraw :: Win32.HWND -> IO ()
requestRedraw hwnd =
    Win32.invalidateRect (Just hwnd) Nothing True

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent
restoreComponentFromHWND hwnd =
    getComponentTypeFromHWND hwnd >>= \case
        ComponentButton -> restoreButtonFromHWND hwnd
        ComponentWindow -> restoreWindowFromHWND hwnd
