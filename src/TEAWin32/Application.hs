module TEAWin32.Application
    ( Settings (..)
    , defaultSettings
    , runTEA
    ) where

import           Control.Exception              (Exception (displayException))
import           Control.Monad                  (forM_)
import           Control.Monad.State.Strict     (evalState)
import           Control.Monad.Writer.Strict    (execWriterT)
import           Data.Data                      (Typeable, cast)
import           Data.IORef                     (atomicModifyIORef')
import qualified Data.Map                       as Map
import qualified Data.Text                      as Text
import           Foreign                        (Ptr, Storable (poke), alloca)
import           GHC.Stack                      (HasCallStack)
import           Prelude                        hiding (init)
import           TEAWin32.Application.Instances ()
import           TEAWin32.Application.Internal
import           TEAWin32.Application.WndProc   (HaskellWndProcCallbacks,
                                                 createHaskellWndProcCallbacks,
                                                 freeHaskellWndProcCallbacks)
import           TEAWin32.Exception             (ErrorLocation (..),
                                                 TEAWin32Error (..),
                                                 errorTEAWin32)
import           TEAWin32.GUI.Component         (IsGUIComponent (scheduleRendering))
import           TEAWin32.GUI.DSL.Internal      (DSL, DSLState (..),
                                                 UniqueIdInternState (UniqueIdInternState))
import           TEAWin32.GUI.VirtualDOM        (flushCCallRequests)
import qualified TEAWin32.Internal.Native       as Native
import           TEAWin32.Util                  (try_)

newtype Settings = Settings
    { useVisualStyles :: Bool
    }

defaultSettings :: Settings
defaultSettings = Settings
    { useVisualStyles = True
    }

runTEA :: (HasCallStack, Typeable model, Typeable msg) => Settings -> IO model -> (msg -> model -> IO model) -> (model -> DSL) -> IO ()
runTEA settings init update view = do
    wndProcCallbacks <- createHaskellWndProcCallbacks
    alloca $ \callbacksPtr -> do
        poke callbacksPtr wndProcCallbacks
        c_InitialiseTEAWin32C callbacksPtr

    --let preFunc = if useVisualStyles settings then GUIInternal.withVisualStyles else id

    runTEA' init update view

    Native.finaliseTEAWin32C

    freeHaskellWndProcCallbacks wndProcCallbacks

runTEA' :: (HasCallStack, Typeable model, Typeable msg) => IO model -> (msg -> model -> IO model) -> (model -> DSL) -> IO ()
runTEA' init update view = do
    initModel <- try_ init >>= \case
        Right x -> pure x
        Left err ->
            errorTEAWin32 (TEAWin32ApplicationError Init (Text.pack $ displayException err))

    atomicModifyIORef' modelRef (const (Model initModel, ()))

    let
        update' (Msg msg) (Model model) =
            case (cast msg, cast model) of
                (Just msg', Just model') ->
                    try_ (update msg' model') >>= \case
                        Right x  -> pure (Model x)
                        Left err -> errorTEAWin32 (TEAWin32ApplicationError Update (Text.pack $ displayException err))
                _ ->
                    errorTEAWin32 (InternalTEAWin32Error "Failed to cast Msg and Model.")

        view' (Model model) =
            case cast model of
                Just model' -> view model'
                _           -> errorTEAWin32 (InternalTEAWin32Error "Failed to cast Model.")

    atomicModifyIORef' updateFuncRef (const (update', ()))
    atomicModifyIORef' viewFuncRef (const (view', ()))

    let initGUIComponents = evalState (execWriterT $ view' (Model initModel)) (DSLState 1 [] (UniqueIdInternState Map.empty 1))

    forM_ initGUIComponents $ \guiComponent ->
        scheduleRendering guiComponent Nothing -- TODO

    flushCCallRequests

    c_StartMessagePump

foreign import ccall unsafe "InitialiseTEAWin32C"
    c_InitialiseTEAWin32C :: Ptr HaskellWndProcCallbacks -> IO ()

foreign import ccall "StartMessagePump"
    c_StartMessagePump :: IO ()
