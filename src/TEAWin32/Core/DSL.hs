{-# LANGUAGE FlexibleContexts #-}

module TEAWin32.Core.DSL
    ( runDSL
    , noChildren
    , title_
    , size_
    , pos_
    , font_
    , icon_
    , cursor_
    , bgColour_
    , onClick_
    , window_'
    , window_
    , button_'
    , button_
    ) where

import           Control.Monad.Reader        (ask, runReader)
import           Control.Monad.State.Strict  (gets, lift, modify', runState,
                                              state)
import           Control.Monad.Writer.Strict (execWriterT, tell)
import           Data.Data                   (Typeable)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import           TEAWin32.Core.Types
import           TEAWin32.Core.Util          (whenJust)

runDSL :: DSL a () -> UniqueIdInternState -> ([(UniqueId, RenderProcedure)], UniqueIdInternState)
runDSL dsl internState =
    let dslState = DSLState { nextAutoUniqueId = 1, userUniqueIdsAppeared = [], parentUniqueId = Nothing, uniqueIdInternState = internState }
        (result, dslState') = runState (execWriterT dsl) dslState in
            (snd <$> result, uniqueIdInternState dslState')

noChildren :: DSL a ()
noChildren = pure ()

title_ :: (IsPropertyWrapper a ComponentTitle) => Text -> PropertyDSL a ()
title_ text = tellProperty ComponentTitle (SetComponentText text)

size_ :: (IsPropertyWrapper a ComponentSize) => (ScalableValue, ScalableValue) -> PropertyDSL a ()
size_ size = tellProperty ComponentSize (SetComponentPos Nothing (Just size) False)

pos_ :: (IsPropertyWrapper a ComponentPosition) => (ScalableValue, ScalableValue) -> PropertyDSL a ()
pos_ pos = tellProperty ComponentPosition (SetComponentPos (Just pos) Nothing False)

font_ :: (IsPropertyWrapper a ComponentFont) => Font -> PropertyDSL a ()
font_ font = tellProperty ComponentFont (SetComponentFont font)

icon_ :: (IsPropertyWrapper a ComponentIcon) => Icon -> PropertyDSL a ()
icon_ icon = tellProperty ComponentIcon (SetComponentIcon icon)

cursor_ :: (IsPropertyWrapper a ComponentCursor) => Cursor -> PropertyDSL a ()
cursor_ cursor = tellProperty ComponentCursor (SetComponentCursor cursor)

bgColour_ :: (IsPropertyWrapper a ComponentBackgroundColour) => Colour -> PropertyDSL a ()
bgColour_ colour = tellProperty ComponentBackgroundColour (SetComponentBackgroundColour colour)

onClick_ :: (IsPropertyWrapper a ComponentOnClick, Typeable msg, Eq msg, Show msg) => msg -> PropertyDSL a ()
onClick_ clickMsg = tellProperty ComponentOnClick (SetComponentClickEvent (Msg clickMsg))

window_' :: (IsChildWrapper a Window) => Text -> Text -> WindowStyle -> PropertyDSL WindowProperty () -> DSL WindowChild () -> DSL a ()
window_' windowUniqueId windowClass windowStyle windowProperties windowChildren =
    tellComponent Window (UidProvidedByUser windowUniqueId) (CreateWindow windowClass windowStyle) (Just windowProperties) (Just windowChildren)

window_ :: (IsChildWrapper a Window) => Text -> WindowStyle -> PropertyDSL WindowProperty () -> DSL WindowChild () -> DSL a ()
window_ windowClass windowStyle windowProperties windowChildren =
    tellComponent Window UidProvidedBySystem (CreateWindow windowClass windowStyle) (Just windowProperties) (Just windowChildren)

button_' :: (IsChildWrapper a Button) => Text -> PropertyDSL ButtonProperty () -> DSL a ()
button_' uniqueId properties =
    tellComponent Button (UidProvidedByUser uniqueId) (CreateButton . fromJust) (Just properties) Nothing

button_ :: (IsChildWrapper a Button) => PropertyDSL ButtonProperty () -> DSL a ()
button_ properties =
    tellComponent Button UidProvidedBySystem (CreateButton . fromJust) (Just properties) Nothing

data UniqueIdProvider = UidProvidedBySystem | UidProvidedByUser Text

tellProperty :: IsPropertyWrapper a b => b -> RenderProcedure -> PropertyDSL a ()
tellProperty a b = ask >>= \parentUid ->
    tell [(wrapProperty a, (parentUid, b))]

tellComponent :: IsChildWrapper a b => b -> UniqueIdProvider -> (Maybe UniqueId -> RenderProcedure) -> Maybe (PropertyDSL c ()) -> Maybe (DSL d ()) -> DSL a ()
tellComponent a uidProvider f mPropertyDSL mChildrenDSL =
    let mUid = case uidProvider of
            UidProvidedBySystem        -> nextUniqueId
            (UidProvidedByUser usrUid) -> internUserUniqueId usrUid in do
                uid       <- mUid
                parentUid <- gets parentUniqueId

                tell [(wrapChild a, (uid, f parentUid))]

                whenJust mPropertyDSL $ \propertyDSL ->
                    concatProperties uid a propertyDSL

                whenJust mChildrenDSL $ \childrenDSL ->
                    concatChildren uid a childrenDSL

concatProperties :: (IsChildWrapper b a) => UniqueId -> a -> PropertyDSL c () -> DSL b ()
concatProperties uniqueId dummyVal propDSL =
    let properties = map (\(_, a) -> (wrapChild dummyVal, a)) (runReader (execWriterT propDSL) uniqueId) in
        tell properties

concatChildren :: (IsChildWrapper b a) => UniqueId -> a -> DSL c () -> DSL b ()
concatChildren parentUid dummyVal childrenDSL = do
    oldParentUniqueId <- gets parentUniqueId
    modify' $ \s -> s { parentUniqueId = Just parentUid }

    children <- lift (execWriterT childrenDSL)

    modify' $ \s -> s { parentUniqueId = oldParentUniqueId }

    let children' = map (\(_, a) -> (wrapChild dummyVal, a)) children
    tell children'

nextUniqueId :: DSL a UniqueId
nextUniqueId =
    state $ \dState ->
        let autoUniqueId = nextAutoUniqueId dState
            newDSLState = dState { nextAutoUniqueId = autoUniqueId + 1 } in
                (UniqueId autoUniqueId, newDSLState)

internUserUniqueId :: Text -> DSL a UniqueId
internUserUniqueId userUniqueId = state $ \dState ->
    case userUniqueId `elem` userUniqueIdsAppeared dState of
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
            --errorTEAWin32 (TEAWin32ApplicationError View ("Duplicated UserUniqueId: " <> userUniqueId))
            error "" -- TODO
