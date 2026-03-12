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

runDSL :: DSL a () -> UniqueIdInternState -> ([(UniqueId, RenderProcedure)], UniqueIdInternState)
runDSL dsl internState =
    let dslState = DSLState { nextAutoUniqueId = 1, userUniqueIdsAppeared = [], parentUniqueId = Nothing, uniqueIdInternState = internState }
        (result, dslState') = runState (execWriterT dsl) dslState in
            (snd <$> result, uniqueIdInternState dslState')

noChildren :: DSL a ()
noChildren = pure ()

title_ :: (IsPropertyWrapper a ComponentTitle) => Text -> PropertyDSL a ()
title_ text =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentTitle, (parentUid, SetComponentText text))]

size_ :: (IsPropertyWrapper a ComponentSize) => (ScalableValue, ScalableValue) -> PropertyDSL a ()
size_ size =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentSize, (parentUid, SetComponentPos Nothing (Just size) False))]

pos_ :: (IsPropertyWrapper a ComponentPosition) => (ScalableValue, ScalableValue) -> PropertyDSL a ()
pos_ pos =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentPosition, (parentUid, SetComponentPos (Just pos) Nothing False))]

font_ :: (IsPropertyWrapper a ComponentFont) => Font -> PropertyDSL a ()
font_ font =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentFont, (parentUid, SetComponentFont font))]

icon_ :: (IsPropertyWrapper a ComponentIcon) => Icon -> PropertyDSL a ()
icon_ icon =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentIcon, (parentUid, SetComponentIcon icon))]

cursor_ :: (IsPropertyWrapper a ComponentCursor) => Cursor -> PropertyDSL a ()
cursor_ cursor =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentCursor, (parentUid, SetComponentCursor cursor))]

bgColour_ :: (IsPropertyWrapper a ComponentBackgroundColour) => Colour -> PropertyDSL a ()
bgColour_ colour =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentBackgroundColour, (parentUid, SetComponentBackgroundColour colour))]

onClick_ :: (IsPropertyWrapper a ComponentOnClick, Typeable msg, Eq msg, Show msg) => msg -> PropertyDSL a ()
onClick_ clickMsg =
    ask >>= \parentUid ->
        tell [(wrapProperty ComponentOnClick, (parentUid, SetComponentClickEvent (Msg clickMsg)))]

window_' :: (IsChildWrapper a Window) => Text -> Text -> WindowStyle -> PropertyDSL WindowProperty () -> DSL WindowChild () -> DSL a ()
window_' windowUniqueId windowClass windowStyle windowProperties windowChildren =
    internUserUniqueId windowUniqueId >>= \uid ->
        gets parentUniqueId >>= \parentUid ->
            tell [(wrapChild Window, (uid, CreateWindow windowClass windowStyle parentUid))] >>
                concatProperties uid Window windowProperties >>
                    concatChildren uid Window windowChildren

window_ :: (IsChildWrapper a Window) => Text -> WindowStyle -> PropertyDSL WindowProperty () -> DSL WindowChild () -> DSL a ()
window_ windowClass windowStyle windowProperties windowChildren =
    nextUniqueId >>= \uid ->
        gets parentUniqueId >>= \parentUid ->
            tell [(wrapChild Window, (uid, CreateWindow windowClass windowStyle parentUid))] >>
                concatProperties uid Window windowProperties >>
                    concatChildren uid Window windowChildren

button_' :: (IsChildWrapper a Button) => Text -> PropertyDSL ButtonProperty () -> DSL a ()
button_' uniqueId properties =
    internUserUniqueId uniqueId >>= \uid ->
        gets parentUniqueId >>= \parentUid ->
            tell [(wrapChild Button, (uid, CreateButton (fromJust parentUid)))] >>
                concatProperties uid Button properties

button_ :: (IsChildWrapper a Button) => PropertyDSL ButtonProperty () -> DSL a ()
button_ properties =
    nextUniqueId >>= \uid ->
        gets parentUniqueId >>= \parentUid ->
            tell [(wrapChild Button, (uid, CreateButton (fromJust parentUid)))] >>
                concatProperties uid Button properties

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
