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
    , uniqueIdAndHWNDMapRef
    , canTerminateProgrammeRef
    , performUpdate
    ) where

import           Control.Monad                   (forM_, void)
import           Control.Monad.Writer            (runWriter)
import           Data.Data                       (Typeable)
import           Data.IORef                      (IORef, atomicModifyIORef',
                                                  newIORef, readIORef)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           GHC.IO                          (unsafePerformIO)
import           Graphics.GUI                    (UniqueId)
import           Graphics.GUI.Component          (GUIComponent, GUIComponents,
                                                  IsGUIComponent (..),
                                                  compareGUIComponents)
import           Graphics.GUI.Component.Property (IsGUIComponentProperty (..),
                                                  compareProperties)
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

guiComponentMapRef :: IORef (Map UniqueId (Win32.HWND, GUIComponent))
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

    guiComponentMap <- readIORef guiComponentMapRef

    viewFunc <- readIORef viewFuncRef
    let newGUIComponents     = snd $ runWriter (viewFunc newModel)
        currentGUIComponents = snd <$> Map.elems guiComponentMap

    let (added, deleted, redraw, propertyChanged) = compareGUIComponents newGUIComponents currentGUIComponents

    _ <- atomicModifyIORef' canTerminateProgrammeRef (const (False, False))

    forM_ added $ \addedComponent ->
        render addedComponent Nothing

    forM_ deleted $ \deletedComponent ->
        case Map.lookup (getUniqueId deletedComponent) guiComponentMap of
            Just (hwnd, _) -> Win32.destroyWindow hwnd
            Nothing        -> error "Tried to delete a component that was not in the map."

    forM_ redraw $ \componentToRedraw ->
        case Map.lookup (getUniqueId componentToRedraw) guiComponentMap of
            Just (hwnd, _) -> render componentToRedraw Nothing >> Win32.destroyWindow hwnd
            Nothing        -> error "Tried to redraw a component that was not in the map."

    forM_ propertyChanged $ \(newComponent, oldComponent) -> do
        let uniqueId = getUniqueId newComponent

        case Map.lookup uniqueId guiComponentMap of
            Just (hwnd, _) -> do
                let (addedProps, deletedProps, changedProps) = compareProperties (getProperties newComponent) (getProperties oldComponent)

                mapM_ (`applyProperty` hwnd) addedProps
                mapM_ (`unapplyProperty` hwnd) deletedProps

                forM_ changedProps $ \(newProp, oldProp) ->
                    updateProperty newProp oldProp hwnd

                void $ atomicModifyIORef' guiComponentMapRef $ \compMap ->
                    let newCompMap = Map.update (const $ Just (hwnd, newComponent)) uniqueId compMap in
                        (newCompMap, newCompMap)

            Nothing ->
                error "Tried to update the properties of a component that was not in the map."

    void $ atomicModifyIORef' canTerminateProgrammeRef (const (True, True))
