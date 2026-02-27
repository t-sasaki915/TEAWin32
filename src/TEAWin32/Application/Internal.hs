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

import           Control.Monad.State.Strict  (evalState)
import           Control.Monad.Writer.Strict (execWriterT)
import           Data.Data                   (Typeable, cast)
import           Data.IORef                  (IORef, atomicModifyIORef',
                                              newIORef, readIORef)
import qualified Data.Map                    as Map
import           GHC.IO                      (unsafePerformIO)
import           GHC.Stack                   (HasCallStack)
import           TEAWin32.Exception          (TEAWin32Error (..), errorTEAWin32)
import           TEAWin32.GUI                (UniqueId)
import           TEAWin32.GUI.Component      (GUIComponent)
import           TEAWin32.GUI.DSL.Internal   (DSL, DSLState (..),
                                              UniqueIdInternState (UniqueIdInternState))

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
modelRef = unsafePerformIO (newIORef (errorTEAWin32 (InternalTEAWin32Error "TEA is not initialised.")))
{-# NOINLINE modelRef #-}

updateFuncRef :: IORef (Msg -> Model -> IO Model)
updateFuncRef = unsafePerformIO (newIORef (errorTEAWin32 (InternalTEAWin32Error "TEA is not initialised.")))
{-# NOINLINE updateFuncRef #-}

viewFuncRef :: IORef (Model -> DSL)
viewFuncRef = unsafePerformIO (newIORef (errorTEAWin32 (InternalTEAWin32Error "TEA is not initialised.")))
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
    let newGUIComponents = evalState (execWriterT $ viewFunc newModel) (DSLState 1 [] (UniqueIdInternState Map.empty 1))

    updateComponents newGUIComponents Nothing

    atomicModifyIORef' isUpdateProgressingRef (const (False, ()))

updateComponents :: HasCallStack => [GUIComponent] -> Maybe UniqueId -> IO ()
updateComponents newGUIComponents maybeParent =
    pure ()

