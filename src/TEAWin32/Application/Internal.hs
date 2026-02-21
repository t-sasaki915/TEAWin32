{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.Application.Internal
    ( Model (..)
    , Msg (..)
    , modelRef
    , updateFuncRef
    , viewFuncRef
    , isUpdateProgressing
    , issueMsg
    , updateComponents
    ) where

import           Control.Monad                            (filterM, forM, forM_,
                                                           void, when)
import           Control.Monad.State.Strict               (evalState)
import           Control.Monad.Writer.Strict              (execWriterT)
import           Data.Data                                (Typeable, cast)
import           Data.IntMap                              ((!))
import qualified Data.IntMap                              as IntMap
import           Data.IORef                               (IORef,
                                                           atomicModifyIORef',
                                                           newIORef, readIORef)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (isNothing)
import           GHC.IO                                   (unsafePerformIO)
import           GHC.Stack                                (HasCallStack)
import qualified Graphics.Win32                           as Win32
import           TEAWin32.GUI.Component                   (GUIComponent,
                                                           GUIComponents,
                                                           IsGUIComponent (..))
import           TEAWin32.GUI.Component.ComponentRegistry
import qualified TEAWin32.GUI.Component.Internal          as ComponentInternal
import qualified TEAWin32.GUI.Internal                    as GUIInternal
import           TEAWin32.Internal                        (throwTEAWin32InternalError)

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
modelRef = unsafePerformIO (newIORef (throwTEAWin32InternalError "TEA is not initialised."))
{-# NOINLINE modelRef #-}

updateFuncRef :: IORef (Msg -> Model -> IO Model)
updateFuncRef = unsafePerformIO (newIORef (throwTEAWin32InternalError "TEA is not initialised."))
{-# NOINLINE updateFuncRef #-}

viewFuncRef :: IORef (Model -> GUIComponents)
viewFuncRef = unsafePerformIO (newIORef (throwTEAWin32InternalError "TEA is not initialised."))
{-# NOINLINE viewFuncRef #-}

isUpdateProgressingRef :: IORef Bool
isUpdateProgressingRef = unsafePerformIO (newIORef False)
{-# NOINLINE isUpdateProgressingRef #-}

isUpdateProgressing :: IO Bool
isUpdateProgressing = readIORef isUpdateProgressingRef

issueMsg :: HasCallStack => Msg -> IO ()
issueMsg msg = do
    atomicModifyIORef' isUpdateProgressingRef (const (True, ()))

    updateFunc   <- readIORef updateFuncRef
    currentModel <- readIORef modelRef
    newModel     <- updateFunc msg currentModel
    atomicModifyIORef' modelRef (const (newModel, ()))

    viewFunc <- readIORef viewFuncRef
    let newGUIComponents = evalState (execWriterT $ viewFunc newModel) 0

    updateComponents newGUIComponents Nothing

    atomicModifyIORef' isUpdateProgressingRef (const (False, ()))

updateComponents :: HasCallStack => [GUIComponent] -> Maybe Win32.HWND -> IO ()
updateComponents newGUIComponents maybeParent = do
    childrenWithUniqueId <- Map.fromList <$>
        case maybeParent of
            Just parent ->
                GUIInternal.withImmediateChildWindows parent $ mapM $ \child ->
                    getComponentRegistryEntryValue ComponentUniqueIdRegKey child >>= \uniqueId ->
                        pure (uniqueId, child)

            Nothing ->
                GUIInternal.withTopLevelWindows $ mapM $ \child ->
                    getComponentRegistryEntryValue ComponentUniqueIdRegKey child >>= \uniqueId ->
                        pure (uniqueId, child)

    let newGUIComponentsWithUniqueId = Map.fromList [ (getUniqueId comp, comp) | comp <- newGUIComponents ]

    _ <- flip Map.traverseWithKey childrenWithUniqueId $ \uniqueId child ->
        when (isNothing (Map.lookup uniqueId newGUIComponentsWithUniqueId)) $
            ComponentInternal.destroyComponent child

    _ <- flip Map.traverseWithKey newGUIComponentsWithUniqueId $ \uniqueId newGUIComponent ->
        case Map.lookup uniqueId childrenWithUniqueId of
            Just currentHWND ->
                putStrLn $ "UPDATE " <> show uniqueId

            Nothing ->
                void (render newGUIComponent maybeParent)

    pure ()

