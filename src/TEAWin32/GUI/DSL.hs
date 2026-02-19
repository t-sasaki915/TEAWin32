{-# LANGUAGE FlexibleContexts #-}

module TEAWin32.GUI.DSL
    ( module TEAWin32.Drawing
    , module TEAWin32.GUI
    , module TEAWin32.GUI.Component.Button.DSL
    , module TEAWin32.GUI.Component.Window.DSL
    , GUIComponents
    , noChildren
    , title_
    , size_
    , pos_
    , font_
    , zIndex_
    , onClick_
    ) where

import           Data.Data                         (Typeable)
import           Data.Text                         (Text)
import           TEAWin32.Drawing                  (Colour (..))
import           TEAWin32.GUI                      (Cursor (..), Font (..),
                                                    Icon (..), ScalableValue,
                                                    WindowStyle (..), raw)
import           TEAWin32.GUI.Component            (GUIComponents)
import           TEAWin32.GUI.Component.Button.DSL
import           TEAWin32.GUI.Component.Property
import           TEAWin32.GUI.Component.Window.DSL

noChildren :: GUIComponents
noChildren = pure ()

title_ :: (IsPropertyWrapper a ComponentTitle) => Text -> a
title_ = wrapComponentProperty . ComponentTitle

size_ :: (IsPropertyWrapper a ComponentSize) => (ScalableValue, ScalableValue) -> a
size_ = wrapComponentProperty . ComponentSize

pos_ :: (IsPropertyWrapper a ComponentPosition) => (ScalableValue, ScalableValue) -> a
pos_ = wrapComponentProperty . ComponentPosition

font_ :: (IsPropertyWrapper a ComponentFont) => Font -> a
font_ = wrapComponentProperty . ComponentFont

zIndex_ :: (IsPropertyWrapper a ComponentZIndex) => Int -> a
zIndex_ = wrapComponentProperty . ComponentZIndex

onClick_ :: (IsPropertyWrapper a ComponentOnClick, Typeable b, Show b, Eq b) => b -> a
onClick_ = wrapComponentProperty . ComponentOnClick
