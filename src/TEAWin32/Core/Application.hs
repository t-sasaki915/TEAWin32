module TEAWin32.Core.Application
    ( defaultTEAWin32Settings
    , runTEAWin32
    ) where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.STM     (TQueue, atomically, newTQueueIO,
                                             tryReadTQueue, writeTQueue)
import           Control.Exception          (bracket, bracket_,
                                             uninterruptibleMask_)
import           Control.Monad              (unless)
import           Control.Monad.Cont         (ContT (..), evalContT)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, evalStateT, get)
import           Data.Data                  (Typeable, cast)
import qualified Data.Text                  as Text
import           Foreign                    (FunPtr, Ptr, freeHaskellFunPtr,
                                             nullPtr, peek, with)
import           Prelude                    hiding (init)
import           System.Exit                (exitFailure)
import           TEAWin32.Core.DSL          (runDSL)
import qualified TEAWin32.Core.Native       as Native
import           TEAWin32.Core.Types

defaultTEAWin32Settings :: TEAWin32Settings
defaultTEAWin32Settings = TEAWin32Settings
    { useVisualStyles = True
    }

processEvents :: TQueue EventQueueEntry -> StateT InternalState IO ()
processEvents queue =
    liftIO (atomically $ tryReadTQueue queue) >>= \case
        Just (FatalErrorEvent errorType errorCode errorLocation) -> do
            let dialogTitle        = "Internal Framework Error"
                shortMsg           = "An internal error has occurred in native C programme."
                specificMsgWithLoc = errorType <> ": " <> Text.show errorCode <> "\r\n" <> "Location: " <> errorLocation
                fullMsg            = dialogTitle <> "\r\n\r\n" <> shortMsg <> "\r\n" <> specificMsgWithLoc

            liftIO $ evalContT $ do
                dialogTitle'        <- ContT $ Native.withCWText dialogTitle
                shortMsg'           <- ContT $ Native.withCWText shortMsg
                specificMsgWithLoc' <- ContT $ Native.withCWText specificMsgWithLoc
                fullMsg'            <- ContT $ Native.withCWText fullMsg

                liftIO (Native.c_ShowErrorReporter dialogTitle' shortMsg' specificMsgWithLoc' fullMsg')

            liftIO exitFailure

        Just event -> do
            liftIO (putStrLn $ "RECEIVED AN EVENT!!!!!!!!!!!!!! " <> show event)

            processEvents queue

        Nothing ->
            pure ()

mainLoop :: StateT InternalState IO ()
mainLoop = do
    currentState <- get

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
        bracket_
            (with settings $ \settingsPtr -> Native.c_InitialiseTEAWin32C settingsPtr eventEnqueuer)
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
                        { eventQueue              = evtQueue
                        , lastRenderProcedures    = []
                        , lastUniqueIdInternState = UniqueIdInternState { internedUserUniqueIdMap = mempty, nextUserUniqueIdInternNumber = 1 }
                        , updateFunction          = update'
                        , viewFunction            = view'
                        , currentModel            = Model initModel
                        }

                    (test1, test2) = runDSL (view' (currentModel internalState)) (lastUniqueIdInternState internalState)

                liftIO (print test1)
                liftIO (print test2)

                evalStateT mainLoop internalState
