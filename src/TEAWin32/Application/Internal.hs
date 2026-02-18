{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.Application.Internal
    ( Model (..)
    , Msg (..)
    , modelRef
    , updateFuncRef
    , viewFuncRef
    , registerHWND
    , unregisterHWND
    , issueMsg
    , updateComponents
    ) where

import                          Control.Monad                             (forM_,
                                                                           unless,
                                                                           void)
import                          Control.Monad.Writer                      (execWriter)
import                          Data.Data                                 (Typeable,
                                                                           cast)
import                          Data.IORef                                (IORef,
                                                                           atomicModifyIORef',
                                                                           newIORef,
                                                                           readIORef)
import                          Data.Map                                  (Map)
import                qualified Data.Map                                  as Map
import                          Data.Maybe                                (catMaybes)
import                          GHC.IO                                    (unsafePerformIO)
import                qualified Graphics.Win32                            as Win32
import                          TEAWin32.GUI                              (UniqueId)
import                          TEAWin32.GUI.Component                    (GUIComponent,
                                                                           GUIComponents,
                                                                           IsGUIComponent (..))
import {-# SOURCE #-} qualified TEAWin32.GUI.Component.Internal           as ComponentInternal
import {-# SOURCE #-}           TEAWin32.GUI.Component.Internal.Attribute (isManagedByTEAWin32)
import                          TEAWin32.GUI.Component.Property           (IsGUIComponentProperty (..))
import                          TEAWin32.GUI.Component.Property.Internal  as PropertyInternal
import                qualified TEAWin32.GUI.Internal                     as GUIInternal

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

getHWNDByUniqueId :: UniqueId -> IO (Maybe Win32.HWND)
getHWNDByUniqueId uniqueId =
    readIORef uniqueIdAndHWNDMapRef >>= \uniqueIdAndHWNDMap ->
        pure $ Map.lookup uniqueId uniqueIdAndHWNDMap

registerHWND :: UniqueId -> Win32.HWND -> IO ()
registerHWND uniqueId hwnd =
    atomicModifyIORef' uniqueIdAndHWNDMapRef $ \hwndMap ->
        let newHWNDMap = Map.insert uniqueId hwnd hwndMap in
            (newHWNDMap, ())

unregisterHWND :: Win32.HWND -> IO ()
unregisterHWND hwnd =
    atomicModifyIORef' uniqueIdAndHWNDMapRef $ \hwndMap ->
        let newHWNDMap = Map.filter (/= hwnd) hwndMap in
            (newHWNDMap, ())

issueMsg :: Msg -> IO ()
issueMsg msg = do
    updateFunc   <- readIORef updateFuncRef
    currentModel <- readIORef modelRef

    newModel <- updateFunc msg currentModel
    atomicModifyIORef' modelRef (const (newModel, ()))

    currentGUIComponents <- fmap catMaybes $ GUIInternal.withTopLevelWindows $ mapM $ \topLevelWindow ->
        isManagedByTEAWin32 topLevelWindow >>= \case
            True  -> Just <$> ComponentInternal.restoreComponentFromHWND topLevelWindow
            False -> pure Nothing

    viewFunc <- readIORef viewFuncRef
    let newGUIComponents = execWriter (viewFunc newModel)

    unless (newGUIComponents == currentGUIComponents) $
        updateComponents newGUIComponents currentGUIComponents Nothing

updateComponents :: [GUIComponent] -> [GUIComponent] -> Maybe Win32.HWND -> IO ()
updateComponents newChildren oldChildren parentHWND = do
    print newChildren
    ComponentInternal.sortComponentsWithZIndex newChildren parentHWND >>= \sortedNewChildren ->
        forM_ (ComponentInternal.compareGUIComponents sortedNewChildren oldChildren) $ \case
            ComponentInternal.NoComponentChange ->
                pure ()

            (ComponentInternal.RenderComponent addedComponent) ->
                void (render addedComponent parentHWND)

            (ComponentInternal.DeleteComponent deletedComponent) ->
                getHWNDByUniqueId (getUniqueId deletedComponent) >>= \case
                    Just hwnd -> Win32.destroyWindow hwnd
                    Nothing   -> error "Tried to delete a component that was not in the map."

            (ComponentInternal.RedrawComponent modifiedComponent) ->
                getHWNDByUniqueId (getUniqueId modifiedComponent) >>= \case
                    Just hwnd -> Win32.destroyWindow hwnd >> void (render modifiedComponent parentHWND)
                    Nothing   -> error "Tried to redraw a component that was not in the map."

            (ComponentInternal.UpdateProperties newComponent oldComponent) ->
                getHWNDByUniqueId (getUniqueId oldComponent) >>= \case
                    Just hwnd ->
                        forM_ (PropertyInternal.compareProperties (getProperties newComponent) (getProperties oldComponent)) $ \case
                            PropertyInternal.NoPropertyChange ->
                                pure ()

                            (PropertyInternal.AddProperty addedProperty) ->
                                applyProperty addedProperty hwnd

                            (PropertyInternal.DeleteProperty deletedProperty) ->
                                unapplyProperty deletedProperty hwnd

                            (PropertyInternal.UpdateProperty newProperty oldProperty) ->
                                updateProperty newProperty oldProperty hwnd

                    Nothing ->
                        error "Tried to update the properties of a component that was not in the map."
