module Graphics.GUI.DSL
    ( WindowStyle (..)
    , Icon (..)
    , Cursor (..)
    , Colour (..)
    , windowTitle
    , windowIcon
    , windowCursor
    , windowSize
    , windowPosition
    , windowBackgroundColour
    , windowChildren
    , window
    , buttonLabel
    , buttonSize
    , buttonPosition
    , buttonClicked
    , button
    ) where

import           Graphics.Drawing                  (Colour (..))
import           Graphics.GUI
import           Graphics.GUI.Component.Button.DSL
import           Graphics.GUI.Component.Window.DSL
