module TEAWin32.Application
    ( Settings (..)
    , defaultSettings
    , runTEA
    ) where

import           Control.Exception             (SomeException, try)
import           Control.Monad                 (forM_)
import           Control.Monad.Writer          (execWriter)
import           Data.Data                     (Typeable, cast)
import           Data.Functor                  ((<&>))
import           Data.IORef                    (atomicModifyIORef')
import qualified Graphics.Win32                as Win32
import           Prelude                       hiding (init)
import           TEAWin32.Application.Internal
import           TEAWin32.GUI                  (withVisualStyles)
import           TEAWin32.GUI.Component        (GUIComponents,
                                                IsGUIComponent (render))
import           TEAWin32.GUI.Internal         (finaliseFontCache,
                                                initialiseCursorCache,
                                                initialiseIconCache)

newtype Settings = Settings
    { useVisualStyles :: Bool
    }

defaultSettings :: Settings
defaultSettings = Settings
    { useVisualStyles = True
    }

runTEA :: (Typeable model, Typeable msg) => Settings -> IO model -> (msg -> model -> IO model) -> (model -> GUIComponents) -> IO ()
runTEA settings init update view =
    if useVisualStyles settings
        then withVisualStyles (runTEA' init update view)
        else runTEA' init update view

runTEA' :: (Typeable model, Typeable msg) => IO model -> (msg -> model -> IO model) -> (model -> GUIComponents) -> IO ()
runTEA' init update view = do
    initialiseCursorCache
    initialiseIconCache

    initModel <- init

    atomicModifyIORef' modelRef (const (Model initModel, ()))

    let
        update' (Msg msg) (Model model) =
            case (cast msg, cast model) of
                (Just msg', Just model') -> update msg' model' <&> Model
                _                        -> error "Failed to cast Msg and Model."

        view' (Model model) =
            case cast model of
                Just model' -> view model'
                _           -> error "Failed to cast Model."

    atomicModifyIORef' updateFuncRef (const (update', ()))
    atomicModifyIORef' viewFuncRef (const (view', ()))

    let initGUIComponents = execWriter (view initModel)

    forM_ (reverse initGUIComponents) $ \guiComponent ->
        render guiComponent Nothing

    messagePump

    finaliseFontCache

messagePump :: IO ()
messagePump = Win32.allocaMessage messagePump'
    where
        messagePump' msgPtr =
            (try (Win32.getMessage msgPtr Nothing) :: IO (Either SomeException Bool)) >>= \case
                Right True ->
                    Win32.translateMessage msgPtr >>
                        Win32.dispatchMessage msgPtr >>
                            messagePump' msgPtr

                _ ->
                    pure ()
