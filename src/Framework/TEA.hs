module Framework.TEA
    ( Settings (..)
    , defaultSettings
    , runTEA
    ) where

import           Control.Exception      (SomeException, try)
import           Control.Monad          (forM_, void, when)
import           Control.Monad.Writer   (execWriter)
import           Data.Data              (Typeable, cast)
import           Data.Functor           ((<&>))
import           Data.IORef             (atomicModifyIORef')
import           Framework.TEA.Internal
import           Graphics.GUI           (withVisualStyles)
import           Graphics.GUI.Component (GUIComponents, IsGUIComponent (render))
import           Graphics.GUI.Internal  (finaliseFontCache,
                                         initialiseCursorCache,
                                         initialiseIconCache)
import qualified Graphics.Win32         as Win32
import           Prelude                hiding (init)

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

    forM_ initGUIComponents $ \guiComponent ->
        render guiComponent Nothing

    messagePump

    finaliseFontCache

messagePump :: IO ()
messagePump =
    Win32.allocaMessage $ \msg ->
        let pump =
                (try $ Win32.getMessage msg Nothing :: IO (Either SomeException Bool)) >>= \r ->
                    when (or r) $
                        void $ Win32.translateMessage msg >>
                            Win32.dispatchMessage msg >>
                                pump
        in pump
