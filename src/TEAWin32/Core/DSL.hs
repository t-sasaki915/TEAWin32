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
    , window_'
    , window_
    , button_'
    , button_
    ) where

import           Control.Monad.Reader        (ask, runReader)
import           Control.Monad.State.Strict  (lift, runState, state)
import           Control.Monad.Writer.Strict (execWriterT, tell)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           TEAWin32.Core.Types

runDSL :: DSL a () -> UniqueIdInternState -> ([(UniqueId, RenderProcedure)], UniqueIdInternState)
runDSL dsl internState =
    let dslState = DSLState { nextAutoUniqueId = 1, userUniqueIdsAppeared = [], uniqueIdInternState = internState }
        (result, dslState') = runState (execWriterT dsl) dslState in
            (snd <$> result, uniqueIdInternState dslState')

noChildren :: DSL a ()
noChildren = pure ()

title_ :: (IsPropertyWrapper a ComponentTitle) => Text -> PropertyDSL a ()
title_ text =
    ask >>= \parentUniqueId ->
        tell [(wrapProperty ComponentTitle, (parentUniqueId, SetComponentText text))]

size_ :: (IsPropertyWrapper a ComponentSize) => (ScalableValue, ScalableValue) -> PropertyDSL a ()
size_ size =
    ask >>= \parentUniqueId ->
        tell [(wrapProperty ComponentSize, (parentUniqueId, SetComponentSize size))]

pos_ :: (IsPropertyWrapper a ComponentPosition) => (ScalableValue, ScalableValue) -> PropertyDSL a ()
pos_ pos =
    ask >>= \parentUniqueId ->
        tell [(wrapProperty ComponentPosition, (parentUniqueId, SetComponentPosition pos))]

font_ :: (IsPropertyWrapper a ComponentFont) => Font -> PropertyDSL a ()
font_ font =
    ask >>= \parentUniqueId ->
        tell [(wrapProperty ComponentFont, (parentUniqueId, SetComponentFont font))]

icon_ :: (IsPropertyWrapper a ComponentIcon) => Icon -> PropertyDSL a ()
icon_ icon =
    ask >>= \parentUniqueId ->
        tell [(wrapProperty ComponentIcon, (parentUniqueId, SetComponentIcon icon))]

cursor_ :: (IsPropertyWrapper a ComponentCursor) => Cursor -> PropertyDSL a ()
cursor_ cursor =
    ask >>= \parentUniqueId ->
        tell [(wrapProperty ComponentCursor, (parentUniqueId, SetComponentCursor cursor))]

bgColour_ :: (IsPropertyWrapper a ComponentBackgroundColour) => Colour -> PropertyDSL a ()
bgColour_ colour =
    ask >>= \parentUniqueId ->
        tell [(wrapProperty ComponentBackgroundColour, (parentUniqueId, SetComponentBackgroundColour colour))]

window_' :: (IsChildWrapper a Window) => Text -> Text -> WindowStyle -> PropertyDSL WindowProperty () -> DSL WindowChild () -> DSL a ()
window_' windowUniqueId windowClass windowStyle windowProperties windowChildren =
    internUserUniqueId windowUniqueId >>= \uid ->
        tell [(wrapChild Window, (uid, CreateWindow windowClass windowStyle))] >>
            concatProperties uid Window windowProperties >>
                concatChildren Window windowChildren

window_ :: (IsChildWrapper a Window) => Text -> WindowStyle -> PropertyDSL WindowProperty () -> DSL WindowChild () -> DSL a ()
window_ windowClass windowStyle windowProperties windowChildren =
    nextUniqueId >>= \uid ->
        tell [(wrapChild Window, (uid, CreateWindow windowClass windowStyle))] >>
            concatProperties uid Window windowProperties >>
                concatChildren Window windowChildren

button_' :: (IsChildWrapper a Button) => Text -> PropertyDSL ButtonProperty () -> DSL a ()
button_' uniqueId properties =
    internUserUniqueId uniqueId >>= \uid ->
        tell [(wrapChild Button, (uid, CreateButton))] >>
            concatProperties uid Button properties

button_ :: (IsChildWrapper a Button) => PropertyDSL ButtonProperty () -> DSL a ()
button_ properties =
    nextUniqueId >>= \uid ->
        tell [(wrapChild Button, (uid, CreateButton))] >>
            concatProperties uid Button properties

concatProperties :: (IsChildWrapper b a) => UniqueId -> a -> PropertyDSL c () -> DSL b ()
concatProperties uniqueId dummyVal propDSL =
    let properties = map (\(_, a) -> (wrapChild dummyVal, a)) (runReader (execWriterT propDSL) uniqueId) in
        tell properties

concatChildren :: (IsChildWrapper b a) => a -> DSL c () -> DSL b ()
concatChildren dummyVal childrenDSL =
    lift (execWriterT childrenDSL) >>= \children ->
        let children' = map (\(_, a) -> (wrapChild dummyVal, a)) children in
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
