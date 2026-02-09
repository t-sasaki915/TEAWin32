{-# LANGUAGE ExistentialQuantification #-}

module Framework.TEA.Internal
    ( IsModel
    , IsMsg
    , Model (..)
    , Msg (..)
    , modelRef
    , buttonClickEventHandlersRef
    , updateFuncRef
    , viewFuncRef
    , guiComponentMapRef
    , canTerminateProgrammeRef
    , performUpdate
    ) where

import           Control.Monad                   (forM_, void)
import           Control.Monad.Writer            (runWriter)
import           Data.Data                       (Typeable)
import           Data.IORef                      (IORef, atomicModifyIORef',
                                                  newIORef, readIORef)
import qualified Data.List                       as List
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           GHC.IO                          (unsafePerformIO)
import           Graphics.GUI                    (UniqueId)
import           Graphics.GUI.Component          (GUIComponent, GUIComponents,
                                                  IsGUIComponent (..))
import           Graphics.GUI.Component.Property (GUIComponentProperty,
                                                  IsGUIComponentProperty (..))
import qualified Graphics.Win32                  as Win32

class IsModel a
class Eq a => IsMsg a

data Model = forall a. (Typeable a, IsModel a) => Model a | NoModel
data Msg = forall a. (Typeable a, IsMsg a) => Msg a

modelRef :: IORef Model
modelRef = unsafePerformIO (newIORef NoModel)
{-# NOINLINE modelRef #-}

updateFuncRef :: IORef (Msg -> Model -> IO Model)
updateFuncRef = unsafePerformIO (newIORef (const $ const $ pure NoModel))
{-# NOINLINE updateFuncRef #-}

viewFuncRef :: IORef (Model -> GUIComponents)
viewFuncRef = unsafePerformIO (newIORef (const $ pure ()))
{-# NOINLINE viewFuncRef #-}

buttonClickEventHandlersRef :: IORef (Map Win32.HWND Msg)
buttonClickEventHandlersRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE buttonClickEventHandlersRef #-}

guiComponentMapRef :: IORef (Map UniqueId GUIComponent)
guiComponentMapRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE guiComponentMapRef #-}

uniqueIdAndHWNDMapRef :: IORef (Map UniqueId Win32.HWND)
uniqueIdAndHWNDMapRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE uniqueIdAndHWNDMapRef #-}

canTerminateProgrammeRef :: IORef Bool
canTerminateProgrammeRef = unsafePerformIO (newIORef True)
{-# NOINLINE canTerminateProgrammeRef #-}

performUpdate :: Msg -> IO ()
performUpdate msg = do
    updateFunc   <- readIORef updateFuncRef
    currentModel <- readIORef modelRef

    newModel <- updateFunc msg currentModel
    _        <- atomicModifyIORef' modelRef (const (newModel, newModel))

    guiComponentMap    <- readIORef guiComponentMapRef
    uniqueIdAndHWNDMap <- readIORef uniqueIdAndHWNDMapRef

    viewFunc <- readIORef viewFuncRef
    let newGUIComponents     = snd $ runWriter (viewFunc newModel)
        currentGUIComponents = Map.elems guiComponentMap

    let (added, deleted, redraw, propertyChanged) = compareGUIComponents newGUIComponents currentGUIComponents

    _ <- atomicModifyIORef' canTerminateProgrammeRef (const (False, False))

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

    forM_ propertyChanged $ \newComponent -> do
        let uniqueId = getUniqueId newComponent
            oldComponent =
                case Map.lookup uniqueId guiComponentMap of
                    Just comp -> comp
                    Nothing   -> error "Tried to update the properties of a component that was not in the map."

        case Map.lookup uniqueId uniqueIdAndHWNDMap of
            Just hwnd -> do
                let (addedProps, deletedProps, changedProps) = compareProperties (getProperties newComponent) (getProperties oldComponent)

                mapM_ (`applyProperty` hwnd) addedProps
                mapM_ (`unapplyProperty` hwnd) deletedProps
                mapM_ (`updateProperty` hwnd) changedProps

                void $ atomicModifyIORef' guiComponentMapRef $ \compMap ->
                    let newCompMap = Map.update (const $ Just newComponent) uniqueId compMap in
                        (newCompMap, newCompMap)

            Nothing ->
                error "Tried to update the properties of a component that was not in the map."

    void $ atomicModifyIORef' canTerminateProgrammeRef (const (True, True))

    where
        updateChildren :: [GUIComponent] -> [GUIComponent] -> Win32.HWND -> IO ()
        updateChildren newComponents oldComponents parentHWND = do
            let (added, deleted, redraw, propertyChanged) = compareGUIComponents newComponents oldComponents

            uniqueIdAndHWNDMap <- readIORef uniqueIdAndHWNDMapRef

            forM_ added $ \addedComponent ->
                render addedComponent (Just parentHWND)

            forM_ deleted $ \deletedComponent ->
                case Map.lookup (getUniqueId deletedComponent) uniqueIdAndHWNDMap of
                    Just hwnd -> Win32.destroyWindow hwnd
                    Nothing   -> error "Tried to delete a component that was not in the map."

            forM_ redraw $ \componentToRedraw ->
                case Map.lookup (getUniqueId componentToRedraw) uniqueIdAndHWNDMap of
                    Just hwnd -> render componentToRedraw (Just parentHWND) >> Win32.destroyWindow hwnd
                    Nothing   -> error "Tried to redraw a component that was not in the map."

            forM_ propertyChanged $ \newComponent -> do
                let uniqueId = getUniqueId newComponent
                    oldComponent =
                        case List.find (\x -> getUniqueId x == uniqueId) oldComponents of
                            Just comp -> comp
                            Nothing   -> error "Tried to update the properties of a component that was not in the map."

                case Map.lookup uniqueId uniqueIdAndHWNDMap of
                    Just hwnd -> do
                        let (addedProps, deletedProps, changedProps) = compareProperties (getProperties newComponent) (getProperties oldComponent)

                        mapM_ (`applyProperty` hwnd) addedProps
                        mapM_ (`unapplyProperty` hwnd) deletedProps
                        mapM_ (`updateProperty` hwnd) changedProps

                    Nothing ->
                        error "Tried to update the properties of a component that was not in the map."


compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [GUIComponent])
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
                                else (redr, newValue : propc)

compareProperties :: [GUIComponentProperty] -> [GUIComponentProperty] -> ([GUIComponentProperty], [GUIComponentProperty], [GUIComponentProperty])
compareProperties new old = (added, deleted, changed)
    where
        newMap = Map.fromList [ (getPropertyName x, x) | x <- new ]
        oldMap = Map.fromList [ (getPropertyName x, x) | x <- old ]

        added   = Map.elems $ Map.difference newMap oldMap
        deleted = Map.elems $ Map.difference oldMap newMap

        commonKeys = Map.keys $ Map.intersection newMap oldMap
        changed = foldr (checkChange newMap oldMap) [] commonKeys

        checkChange nMap oMap k chgd
            | nMap Map.! k == oMap Map.! k = chgd
            | otherwise                    = nMap Map.! k : chgd
