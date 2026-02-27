module TEAWin32.GUI.DSL.Internal
    ( DSLT
    , DSL
    , UniqueIdInternState (..)
    , DSLState (..)
    , generateNextUniqueId
    , internUserUniqueId
    , resolveChildren
    ) where

import           Control.Monad.State.Strict      (State, lift, state)
import           Control.Monad.Writer.Strict     (WriterT, execWriterT)
import qualified Data.List                       as List
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import           TEAWin32.Exception              (ErrorLocation (View),
                                                  TEAWin32Error (..),
                                                  errorTEAWin32)
import           TEAWin32.GUI                    (UniqueId (..))
import           TEAWin32.GUI.Component          (GUIComponent)
import           TEAWin32.GUI.Component.Property (ComponentChildren (..))

type DSLT a = WriterT [GUIComponent] (State DSLState) a
type DSL = DSLT ()

data UniqueIdInternState = UniqueIdInternState
    { internedUserUniqueIdMap      :: Map Text Int
    , nextUserUniqueIdInternNumber :: Int
    }

data DSLState = DSLState
    { nextAutoUniqueId      :: Int
    , userUniqueIdsAppeared :: [Text]
    , uniqueIdInternState   :: UniqueIdInternState
    }

generateNextUniqueId :: DSLT UniqueId
generateNextUniqueId =
    state $ \dState ->
        let autoUniqueId = nextAutoUniqueId dState
            newDSLState = dState { nextAutoUniqueId = autoUniqueId + 1 }
        in
        (UniqueId autoUniqueId, newDSLState)

internUserUniqueId :: Text -> DSLT UniqueId
internUserUniqueId userUniqueId =
    state $ \dState ->
        case userUniqueId `List.elem` userUniqueIdsAppeared dState of
            False ->
                let internMap = internedUserUniqueIdMap (uniqueIdInternState dState) in
                    case Map.lookup userUniqueId internMap of
                        Just n ->
                            (UniqueId n, dState)

                        Nothing ->
                            let internNumber = nextUserUniqueIdInternNumber (uniqueIdInternState dState)
                                newDSLState = dState
                                    { uniqueIdInternState = UniqueIdInternState
                                        { internedUserUniqueIdMap      = Map.insert userUniqueId internNumber internMap
                                        , nextUserUniqueIdInternNumber = internNumber + 1
                                        }
                                    }
                            in
                            (UniqueId (-internNumber), newDSLState)

            True ->
                errorTEAWin32 (TEAWin32ApplicationError View ("Duplicated UserUniqueId: " <> userUniqueId))

resolveChildren :: (ComponentChildren -> a) -> [a] -> DSL -> DSLT [a]
resolveChildren wrapper properties children =
    lift (execWriterT children) >>= \children' ->
        pure (properties ++ [ wrapper (ComponentChildren children') | not (null children') ])
