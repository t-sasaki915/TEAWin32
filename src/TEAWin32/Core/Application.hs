module TEAWin32.Core.Application
    ( defaultTEAWin32Settings
    , runTEAWin32
    ) where

import           Control.Concurrent         (forkOS, threadDelay)
import           Control.Concurrent.STM     (TQueue, atomically, newTQueueIO,
                                             tryReadTQueue, writeTQueue)
import           Control.Exception          (bracket, uninterruptibleMask_)
import           Control.Monad              (unless, void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets,
                                             modify')
import           Data.Data                  (Typeable, cast)
import           Foreign                    (FunPtr, Ptr, freeHaskellFunPtr,
                                             nullPtr, peek, with)
import           Prelude                    hiding (init)
import           TEAWin32.Core.DSL          (runDSL)
import           TEAWin32.Core.Marshall     (dispatchRenderProcedures)
import qualified TEAWin32.Core.Native       as Native
import           TEAWin32.Core.Types
import           TEAWin32.Core.VirtualDOM   (optimiseRenderProcedures)

defaultTEAWin32Settings :: TEAWin32Settings
defaultTEAWin32Settings = TEAWin32Settings
    { useVisualStyles = True
    }

processEvents :: TQueue EventQueueEntry -> StateT InternalState IO ()
processEvents queue =
    liftIO (atomically $ tryReadTQueue queue) >>= \case
        Just StopMainLoopEvent ->
            modify' $ \s -> s { mainLoopContinue = False }

        Just InitialRenderEvent -> do
            viewFunc    <- gets viewFunction
            currentMdl  <- gets currentModel
            internState <- gets lastUniqueIdInternState

            let (test1, _) = runDSL (viewFunc currentMdl) internState

            liftIO (dispatchRenderProcedures (optimiseRenderProcedures test1))

            processEvents queue

        Nothing ->
            pure ()

mainLoop :: StateT InternalState IO ()
mainLoop = do
    currentState <- get

    when (mainLoopContinue currentState) $ do

        processEvents (eventQueue currentState)

        --liftIO (putStrLn "LOOP")

        liftIO (threadDelay 16000)
        mainLoop

withEventEnqueuer :: ((TQueue EventQueueEntry, FunPtr (Ptr EventQueueEntry -> IO ())) -> IO a) -> IO a
withEventEnqueuer func = do
    evtQueue <- newTQueueIO

    let enqueuer entryPtr =
            unless (entryPtr == nullPtr) $
                peek entryPtr >>=
                    atomically . writeTQueue evtQueue

    bracket (Native.makeEventEnqueuerFunPtr enqueuer)
            freeHaskellFunPtr
            (\enqueuerPtr -> func (evtQueue, enqueuerPtr))

runTEAWin32 :: (Typeable model, Typeable msg) => TEAWin32Settings -> IO model -> (msg -> model -> IO model) -> (model -> View) -> IO ()
runTEAWin32 settings init update view =
    withEventEnqueuer $ \(evtQueue, eventEnqueuer) ->
        bracket
            (with settings $ \settingsPtr -> Native.c_InitialiseTEAWin32C settingsPtr eventEnqueuer)
            (const $ uninterruptibleMask_ Native.c_FinaliseTEAWin32C)
            $ \case
                False ->
                    liftIO Native.c_StartErrorReporter

                True -> do
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
                            { eventQueue              = evtQueue
                            , mainLoopContinue        = True
                            , lastRenderProcedures    = []
                            , lastUniqueIdInternState = UniqueIdInternState { internedUserUniqueIdMap = mempty, nextUserUniqueIdInternNumber = 1 }
                            , updateFunction          = update'
                            , viewFunction            = view'
                            , currentModel            = Model initModel
                            }

                    void $ forkOS (evalStateT mainLoop internalState)

                    liftIO Native.c_StartWin32MessageLoop
