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
    , lastGUIComponentsRef
    , guiComponentMapRef
    , performUpdate
    ) where

import           Control.Monad.Writer   (runWriter)
import           Data.Data              (Typeable)
import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           GHC.IO                 (unsafePerformIO)
import           Graphics.GUI           (UniqueId)
import           Graphics.GUI.Component (GUIComponent, GUIComponents,
                                         IsGUIComponent (..))
import qualified Graphics.Win32         as Win32

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

lastGUIComponentsRef :: IORef [GUIComponent]
lastGUIComponentsRef = unsafePerformIO (newIORef [])
{-# NOINLINE lastGUIComponentsRef #-}

buttonClickEventHandlersRef :: IORef (Map Win32.HWND Msg)
buttonClickEventHandlersRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE buttonClickEventHandlersRef #-}

guiComponentMapRef :: IORef (Map UniqueId (Win32.HWND, GUIComponent))
guiComponentMapRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE guiComponentMapRef #-}

performUpdate :: Msg -> IO ()
performUpdate msg = do
    updateFunc   <- readIORef updateFuncRef
    currentModel <- readIORef modelRef

    newModel <- updateFunc msg currentModel
    _        <- atomicModifyIORef' modelRef (const (newModel, newModel))

    viewFunc          <- readIORef viewFuncRef
    lastGUIComponents <- readIORef lastGUIComponentsRef
    let newGUIComponents = snd $ runWriter (viewFunc newModel)

    let (added, deleted, recreate, propertyChanged) = compareGUIComponents newGUIComponents lastGUIComponents

    print (added)
    print (deleted)
    print ( recreate)
    print ( propertyChanged)

    pure ()

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [GUIComponent])
compareGUIComponents new old = (added, deleted, recreate, propertyChanged)
    where
        newMap = Map.fromList [ (getUniqueId x, x) | x <- new ]
        oldMap = Map.fromList [ (getUniqueId x, x) | x <- old ]

        added   = Map.elems $ Map.difference newMap oldMap
        deleted = Map.elems $ Map.difference oldMap newMap

        commonKeys = Map.keys $ Map.intersection newMap oldMap
        (recreate, propertyChanged) = foldr (checkChange newMap oldMap) ([], []) commonKeys

        checkChange nMap oMap k (recr, propc) =
            let newValue = nMap Map.! k
                oldValue = oMap Map.! k in
                    if newValue == oldValue
                        then (recreate, propertyChanged)
                        else
                            if getProperties newValue == getProperties oldValue
                                then (newValue : recr, propc)
                                else (recr, newValue : propc)
