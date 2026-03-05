{-# LANGUAGE FlexibleContexts #-}

module TEAWin32.Core.DSL
    ( noChildren
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

import           Control.Monad.State.Strict  (lift, state)
import           Control.Monad.Writer.Strict (execWriterT, tell)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           TEAWin32.Core.Types

noChildren :: DSL a ()
noChildren = pure ()

title_ :: (IsPropertyWrapper a ComponentTitle) => Text -> a
title_ = wrapComponentProperty . ComponentTitle

size_ :: (IsPropertyWrapper a ComponentSize) => (ScalableValue, ScalableValue) -> a
size_ = wrapComponentProperty . ComponentSize

pos_ :: (IsPropertyWrapper a ComponentPosition) => (ScalableValue, ScalableValue) -> a
pos_ = wrapComponentProperty . ComponentPosition

font_ :: (IsPropertyWrapper a ComponentFont) => Font -> a
font_ = wrapComponentProperty . ComponentFont

icon_ :: (IsPropertyWrapper a ComponentIcon) => Icon -> a
icon_ = wrapComponentProperty . ComponentIcon

cursor_ :: (IsPropertyWrapper a ComponentCursor) => Cursor -> a
cursor_ = wrapComponentProperty . ComponentCursor

bgColour_ :: (IsPropertyWrapper a ComponentBackgroundColour) => Colour -> a
bgColour_ = wrapComponentProperty . ComponentBackgroundColour

window_' :: (IsChildWrapper a Window) => Text -> Text -> WindowStyle -> [WindowProperty] -> DSL WindowChild () -> DSL a ()
window_' windowUniqueId windowClass windowStyle windowProperties windowChildren =
    internUserUniqueId windowUniqueId >>= \uid ->
        lift (execWriterT windowChildren) >>= \children ->
            tell [wrapChild (Window uid windowClass windowStyle windowProperties children)]

window_ :: (IsChildWrapper a Window) => Text -> WindowStyle -> [WindowProperty] -> DSL WindowChild () -> DSL a ()
window_ windowClass windowStyle windowProperties windowChildren =
    nextUniqueId >>= \uid ->
        lift (execWriterT windowChildren) >>= \children ->
            tell [wrapChild (Window uid windowClass windowStyle windowProperties children)]

button_' :: (IsChildWrapper a Button) => Text -> [ButtonProperty] -> DSL a ()
button_' uniqueId properties =
    internUserUniqueId uniqueId >>= \uid ->
        tell [wrapChild (Button uid properties)]

button_ :: (IsChildWrapper a Button) => [ButtonProperty] -> DSL a ()
button_ properties =
    nextUniqueId >>= \uid ->
        tell [wrapChild (Button uid properties)]

nextUniqueId :: DSL a UniqueId
nextUniqueId =
    state $ \dState ->
        let autoUniqueId = nextAutoUniqueId dState
            newDSLState = dState { nextAutoUniqueId = autoUniqueId + 1 }
        in
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
