module Graphics.GUI.Component.Window.DSL
    ( icon_
    , cursor_
    , backgroundColour_
    , window_
    ) where

import           Control.Monad                          (unless)
import           Control.Monad.Writer                   (MonadWriter (tell),
                                                         execWriter)
import           Data.Text                              (Text)
import           Graphics.Drawing                       (Colour)
import           Graphics.GUI                           (Cursor, Icon,
                                                         UniqueId (..),
                                                         WindowStyle)
import           Graphics.GUI.Component                 (GUIComponent (GUIComponent),
                                                         GUIComponents)
import           Graphics.GUI.Component.Property        (ComponentChildren (ComponentChildren))
import           Graphics.GUI.Component.Window          (Window (Window))
import           Graphics.GUI.Component.Window.Property

icon_ :: Icon -> WindowProperty
icon_ = WindowProperty . WindowIcon

cursor_ :: Cursor -> WindowProperty
cursor_ = WindowProperty . WindowCursor

backgroundColour_ :: Colour -> WindowProperty
backgroundColour_ = WindowProperty . WindowBackgroundColour

window_ :: Text -> Text -> WindowStyle -> [WindowProperty] -> GUIComponents -> GUIComponents
window_ windowUniqueId windowClass windowStyle windowProperties windowChildren = do
    let properties = execWriter $
            tell windowProperties >>
                let children = execWriter windowChildren in
                    unless (null children) $
                        tell [WindowProperty $ ComponentChildren children]

    tell $ pure $ GUIComponent $
        Window (UniqueId windowUniqueId) windowClass windowStyle properties
