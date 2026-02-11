{-# LANGUAGE ExistentialQuantification #-}

module Framework.TEA.Internal
    ( Model (..)
    , Msg (..)
    , modelRef
    , updateFuncRef
    , viewFuncRef
    , uniqueIdAndHWNDMapRef
    , registerHWND
    , unregisterHWND
    , issueMsg
    , updateChildren
    ) where

import                          Control.Monad                            (forM_,
                                                                          void)
import                          Control.Monad.Writer                     (execWriter)
import                          Data.Data                                (Typeable,
                                                                          cast)
import                          Data.IORef                               (IORef,
                                                                          atomicModifyIORef',
                                                                          newIORef,
                                                                          readIORef)
import                          Data.Map                                 (Map)
import                qualified Data.Map                                 as Map
import                          GHC.IO                                   (unsafePerformIO)
import                          Graphics.GUI                             (UniqueId)
import                          Graphics.GUI.Component                   (GUIComponent,
                                                                          GUIComponents,
                                                                          IsGUIComponent (..))
import {-# SOURCE #-} qualified Graphics.GUI.Component.Internal          as ComponentInternal
import                          Graphics.GUI.Component.Property          (IsGUIComponentProperty (..))
import                          Graphics.GUI.Component.Property.Internal (compareProperties)
import                qualified Graphics.GUI.Internal                    as GUIInternal
import                qualified Graphics.Win32                           as Win32

data Model = forall a. Typeable a => Model a
data Msg = forall a. (Typeable a, Eq a, Show a) => Msg a

instance Show Msg where
    show (Msg a) = show a

instance Eq Msg where
    (Msg a) == (Msg b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

modelRef :: IORef Model
modelRef = unsafePerformIO (newIORef (error "TEA is not initialised."))
{-# NOINLINE modelRef #-}

updateFuncRef :: IORef (Msg -> Model -> IO Model)
updateFuncRef = unsafePerformIO (newIORef (error "TEA is not initialised."))
{-# NOINLINE updateFuncRef #-}

viewFuncRef :: IORef (Model -> GUIComponents)
viewFuncRef = unsafePerformIO (newIORef (error "TEA is not initialised."))
{-# NOINLINE viewFuncRef #-}

uniqueIdAndHWNDMapRef :: IORef (Map UniqueId Win32.HWND)
uniqueIdAndHWNDMapRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE uniqueIdAndHWNDMapRef #-}

registerHWND :: UniqueId -> Win32.HWND -> IO ()
registerHWND uniqueId hwnd =
    void $ atomicModifyIORef' uniqueIdAndHWNDMapRef $ \hwndMap ->
        let newHWNDMap = Map.insert uniqueId hwnd hwndMap in
            (newHWNDMap, newHWNDMap)

unregisterHWND :: Win32.HWND -> IO ()
unregisterHWND hwnd =
    void $ atomicModifyIORef' uniqueIdAndHWNDMapRef $ \hwndMap ->
        let newHWNDMap = Map.filter (/= hwnd) hwndMap in
            (newHWNDMap, newHWNDMap)

issueMsg :: Msg -> IO ()
issueMsg msg = do
    updateFunc   <- readIORef updateFuncRef
    currentModel <- readIORef modelRef

    newModel <- updateFunc msg currentModel
    _        <- atomicModifyIORef' modelRef (const (newModel, newModel))

    currentGUIComponents <- GUIInternal.withParentWindows (mapM ComponentInternal.restoreComponentFromHWND)
    uniqueIdAndHWNDMap   <- readIORef uniqueIdAndHWNDMapRef

    viewFunc <- readIORef viewFuncRef
    let newGUIComponents = execWriter (viewFunc newModel)

    let (added, deleted, redraw, propertyChanged) = ComponentInternal.compareGUIComponents newGUIComponents currentGUIComponents

    forM_ added $ \addedComponent ->
        render addedComponent Nothing

    forM_ deleted $ \deletedComponent ->
        case Map.lookup (getUniqueId deletedComponent) uniqueIdAndHWNDMap of
            Just hwnd -> Win32.destroyWindow hwnd
            Nothing   -> error "Tried to delete a component that was not in the map."

    forM_ redraw $ \componentToRedraw ->
        case Map.lookup (getUniqueId componentToRedraw) uniqueIdAndHWNDMap of
            Just hwnd -> render componentToRedraw Nothing >> Win32.destroyWindow hwnd
            Nothing   -> error "Tried to redraw a component that was not in the map."

    forM_ propertyChanged $ \(newComponent, oldComponent) -> do
        let uniqueId = getUniqueId newComponent

        case Map.lookup uniqueId uniqueIdAndHWNDMap of
            Just hwnd -> do
                let (addedProps, deletedProps, changedProps) = compareProperties (getProperties newComponent) (getProperties oldComponent)

                mapM_ (`applyProperty` hwnd) addedProps
                mapM_ (`unapplyProperty` hwnd) deletedProps

                forM_ changedProps $ \(newProp, oldProp) ->
                    updateProperty newProp oldProp hwnd

            Nothing ->
                error "Tried to update the properties of a component that was not in the map."

updateChildren :: [GUIComponent] -> [GUIComponent] -> Win32.HWND -> IO ()
updateChildren newChildren oldChildren targetHWND = do
    let (added, deleted, redraw, propertyChanged) = ComponentInternal.compareGUIComponents newChildren oldChildren

    uniqueIdAndHWNDMap <- readIORef uniqueIdAndHWNDMapRef

    forM_ added $ \addedComponent ->
        render addedComponent (Just targetHWND)

    forM_ deleted $ \deletedComponent ->
        case Map.lookup (getUniqueId deletedComponent) uniqueIdAndHWNDMap of
            Just hwnd -> Win32.destroyWindow hwnd
            Nothing   -> error "Tried to delete a component that was not in the map."

    forM_ redraw $ \componentToRedraw ->
        case Map.lookup (getUniqueId componentToRedraw) uniqueIdAndHWNDMap of
            Just hwnd -> render componentToRedraw (Just targetHWND) >> Win32.destroyWindow hwnd
            Nothing   -> error "Tried to redraw a component that was not in the map."

    forM_ propertyChanged $ \(newComponent, oldComponent) -> do
        let uniqueId = getUniqueId newComponent

        case Map.lookup uniqueId uniqueIdAndHWNDMap of
            Just hwnd -> do
                let (addedProps, deletedProps, changedProps) = compareProperties (getProperties newComponent) (getProperties oldComponent)

                mapM_ (`applyProperty` hwnd) addedProps
                mapM_ (`unapplyProperty` hwnd) deletedProps

                forM_ changedProps $ \(newProp, oldProp) ->
                    updateProperty newProp oldProp hwnd

            Nothing ->
                error "Tried to update the properties of a component that was not in the map."
