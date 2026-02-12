{-# LANGUAGE FlexibleContexts #-}

module Graphics.GUI.DSL
    ( module Graphics.Drawing
    , module Graphics.GUI
    , module Graphics.GUI.Component.Button.DSL
    , module Graphics.GUI.Component.Window.DSL
    , noChildren
    , title_
    , size_
    , position_
    , onClick_
    ) where

import           Data.Data                         (Typeable)
import           Data.Text                         (Text)
import           Graphics.Drawing                  (Colour (..))
import           Graphics.GUI                      (Cursor (..), Icon (..),
                                                    WindowStyle (..))
import           Graphics.GUI.Component            (GUIComponents)
import           Graphics.GUI.Component.Button.DSL
import           Graphics.GUI.Component.Property
import           Graphics.GUI.Component.Window.DSL

noChildren :: GUIComponents
noChildren = pure ()

title_ :: (IsPropertyWrapper a ComponentTitle) => Text -> a
title_ = wrapComponentProperty . ComponentTitle

size_ :: (IsPropertyWrapper a ComponentSize) => (Int, Int) -> a
size_ = wrapComponentProperty . ComponentSize

position_ :: (IsPropertyWrapper a ComponentPosition) => (Int, Int) -> a
position_ = wrapComponentProperty . ComponentPosition

onClick_ :: (IsPropertyWrapper a ComponentOnClick, Typeable b, Show b, Eq b) => b -> a
onClick_ = wrapComponentProperty . ComponentOnClick
