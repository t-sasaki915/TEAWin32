module Graphics.GUI.Component.Window.DSL
    ( windowTitle
    , windowIcon
    , windowCursor
    , windowSize
    , windowPosition
    , windowBackgroundColour
    , windowChildren
    , window
    ) where

import           Control.Monad.Writer                   (MonadWriter (tell),
                                                         Writer, execWriter)
import           Data.Text                              (Text)
import           Graphics.Drawing                       (Colour)
import           Graphics.GUI                           (Cursor, Icon,
                                                         UniqueId (..),
                                                         WindowStyle)
import           Graphics.GUI.Component                 (GUIComponent (GUIComponent),
                                                         GUIComponents)
import           Graphics.GUI.Component.Window          (Window (Window))
import           Graphics.GUI.Component.Window.Property

windowTitle :: Text -> Writer [WindowProperty] ()
windowTitle = tell . pure . WindowProperty . WindowTitle

windowIcon :: Icon -> Writer [WindowProperty] ()
windowIcon = tell . pure . WindowProperty . WindowIcon

windowCursor :: Cursor -> Writer [WindowProperty] ()
windowCursor = tell . pure . WindowProperty . WindowCursor

windowSize :: (Int, Int) -> Writer [WindowProperty] ()
windowSize = tell . pure . WindowProperty . WindowSize

windowPosition :: (Int, Int) -> Writer [WindowProperty] ()
windowPosition = tell . pure . WindowProperty . WindowPosition

windowBackgroundColour :: Colour -> Writer [WindowProperty] ()
windowBackgroundColour = tell . pure . WindowProperty . WindowBackgroundColour

windowChildren :: Writer [GUIComponent] () -> Writer [WindowProperty] ()
windowChildren children =
    tell $ pure $ WindowProperty $ WindowChildren (execWriter children)

window :: Text -> Text -> WindowStyle -> Writer [WindowProperty] () -> GUIComponents
window windowUniqueId windowClass windowStyle windowProperties =
    tell $ pure $ GUIComponent $
        Window (UniqueId windowUniqueId) windowClass windowStyle (execWriter windowProperties)
