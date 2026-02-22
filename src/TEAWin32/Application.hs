module TEAWin32.Application
    ( Settings (..)
    , defaultSettings
    , runTEA
    ) where

import           Control.Exception               (Exception (displayException))
import           Control.Monad                   (forM_)
import           Control.Monad.State.Strict      (evalState)
import           Control.Monad.Writer.Strict     (execWriterT)
import           Data.Data                       (Typeable, cast)
import           Data.IORef                      (atomicModifyIORef')
import qualified Data.Text                       as Text
import           GHC.Stack                       (HasCallStack)
import qualified Graphics.Win32                  as Win32
import           Prelude                         hiding (init)
import           TEAWin32.Application.Internal
import           TEAWin32.Exception              (ErrorLocation (..),
                                                  TEAWin32Error (..),
                                                  errorTEAWin32, try_)
import           TEAWin32.GUI                    (withVisualStyles)
import           TEAWin32.GUI.Component          (DSLState (..), GUIComponents,
                                                  IsGUIComponent (render))
import qualified TEAWin32.GUI.Component.Internal as ComponentInternal
import           TEAWin32.GUI.Internal           (finaliseFontCache,
                                                  initialiseCursorCache,
                                                  initialiseDPIStrategy,
                                                  initialiseIconCache,
                                                  setProcessDPIAware)

newtype Settings = Settings
    { useVisualStyles :: Bool
    }

defaultSettings :: Settings
defaultSettings = Settings
    { useVisualStyles = True
    }

runTEA :: (HasCallStack, Typeable model, Typeable msg) => Settings -> IO model -> (msg -> model -> IO model) -> (model -> GUIComponents) -> IO ()
runTEA settings init update view = do
    setProcessDPIAware

    initialiseDPIStrategy
    initialiseCursorCache
    initialiseIconCache

    let preFunc = if useVisualStyles settings then withVisualStyles else id

    preFunc (runTEA' init update view)

    finaliseFontCache

runTEA' :: (HasCallStack, Typeable model, Typeable msg) => IO model -> (msg -> model -> IO model) -> (model -> GUIComponents) -> IO ()
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

    let initGUIComponents = evalState (execWriterT $ view' (Model initModel)) (DSLState 0 [])

    sortedInitGUIComponents <- ComponentInternal.sortComponentsWithZIndex initGUIComponents Nothing
    forM_ sortedInitGUIComponents $ \guiComponent ->
        render guiComponent Nothing

    flushWindowPosScheduleList

    messagePump

messagePump :: IO ()
messagePump = Win32.allocaMessage messagePump'
    where
        messagePump' msgPtr =
            try_ (Win32.getMessage msgPtr Nothing) >>= \case
                Right True ->
                    Win32.translateMessage msgPtr >>
                        Win32.dispatchMessage msgPtr >>
                            messagePump' msgPtr

                _ ->
                    pure ()
