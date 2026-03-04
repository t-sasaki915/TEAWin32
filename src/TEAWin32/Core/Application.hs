module TEAWin32.Core.Application
    ( defaultTEAWin32Settings
    , runTEAWin32
    ) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (bracket_, uninterruptibleMask_)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, evalStateT)
import           Data.Data                  (Typeable, cast)
import           Foreign                    (alloca, nullPtr, poke)
import           Prelude                    hiding (init)
import qualified TEAWin32.Core.Native       as Native
import           TEAWin32.Core.Types

defaultTEAWin32Settings :: TEAWin32Settings
defaultTEAWin32Settings = TEAWin32Settings
    { useVisualStyles = True
    }

runTEAWin32 :: (Typeable model, Typeable msg) => TEAWin32Settings -> IO model -> (msg -> model -> IO model) -> (model -> DSL) -> IO ()
runTEAWin32 settings init update view = bracket_
    initialiseC
    (uninterruptibleMask_ Native.c_FinaliseTEAWin32C)
    $ do
        initModel <- init

        let update' (Msg msg) (Model model) =
                case (cast msg, cast model) of
                    (Just msg', Just model') -> Model <$> update msg' model'
                    _                        -> error "" -- TODO
            view' (Model model) =
                case cast model of
                    Just model' -> view model'
                    _           -> error "" -- TODO

            internalState = InternalState
                { lastGUIComponents = []
                , updateFunction    = update'
                , viewFunction      = view'
                , currentModel      = Model initModel
                }

        evalStateT mainLoop internalState

    where
        initialiseC =
            alloca $ \settingsPtr ->
                poke settingsPtr settings >>
                    Native.c_InitialiseTEAWin32C settingsPtr nullPtr

mainLoop :: StateT InternalState IO ()
mainLoop = do
    liftIO (threadDelay 16000)
    liftIO (putStrLn "LOOP")

    mainLoop
