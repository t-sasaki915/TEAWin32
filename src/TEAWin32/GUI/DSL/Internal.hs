module TEAWin32.GUI.DSL.Internal
    ( recordUserUniqueId
    , getNextSystemUniqueId
    , resolveChildren
    ) where

import           Control.Monad.State.Strict      (State, lift, state)
import           Control.Monad.Writer.Strict     (WriterT, execWriterT)
import qualified Data.List                       as List
import           Data.Text                       (Text)
import           TEAWin32.Exception              (ErrorLocation (View),
                                                  TEAWin32Error (..),
                                                  errorTEAWin32)
import           TEAWin32.GUI                    (UniqueId (..))
import           TEAWin32.GUI.Component          (DSLState (..), GUIComponent,
                                                  GUIComponents)
import           TEAWin32.GUI.Component.Property (ComponentChildren (..))

recordUserUniqueId :: Text -> WriterT [GUIComponent] (State DSLState) ()
recordUserUniqueId uniqueId =
    state $ \(DSLState currentCount usrUniqueIds) ->
        case uniqueId `List.elem` usrUniqueIds of
            False ->
                let newUsrUniqueIds = usrUniqueIds ++ [uniqueId] in
                    ((), DSLState currentCount newUsrUniqueIds)

            True ->
                errorTEAWin32 (TEAWin32ApplicationError View ("Duplicated UserUniqueId: " <> uniqueId))

getNextSystemUniqueId :: WriterT [GUIComponent] (State DSLState) UniqueId
getNextSystemUniqueId =
    state $ \(DSLState currentCount usrUniqueIds) ->
        let newCount = currentCount + 1 in
            (SystemUniqueId newCount, DSLState newCount usrUniqueIds)

resolveChildren :: (ComponentChildren -> a) -> [a] -> GUIComponents -> WriterT [GUIComponent] (State DSLState) [a]
resolveChildren wrapper properties children =
    lift (execWriterT children) >>= \children' ->
        pure (properties ++ [ wrapper (ComponentChildren children') | not (null children') ])
