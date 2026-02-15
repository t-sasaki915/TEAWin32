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
    , position_
    , font_
    , onClick_
    ) where

import           Data.Data                         (Typeable)
import           Data.Text                         (Text)
import           TEAWin32.Drawing                  (Colour (..))
import           TEAWin32.GUI                      (Cursor (..), Font (..),
                                                    Icon (..), WindowStyle (..))
import           TEAWin32.GUI.Component            (GUIComponents)
import           TEAWin32.GUI.Component.Button.DSL
import           TEAWin32.GUI.Component.Property
import           TEAWin32.GUI.Component.Window.DSL

noChildren :: GUIComponents
noChildren = pure ()

title_ :: (IsPropertyWrapper a ComponentTitle) => Text -> a
title_ = wrapComponentProperty . ComponentTitle

size_ :: (IsPropertyWrapper a ComponentSize) => (Int, Int) -> a
size_ = wrapComponentProperty . ComponentSize

position_ :: (IsPropertyWrapper a ComponentPosition) => (Int, Int) -> a
position_ = wrapComponentProperty . ComponentPosition

font_ :: (IsPropertyWrapper a ComponentFont) => Font -> a
font_ = wrapComponentProperty . ComponentFont

onClick_ :: (IsPropertyWrapper a ComponentOnClick, Typeable b, Show b, Eq b) => b -> a
onClick_ = wrapComponentProperty . ComponentOnClick
