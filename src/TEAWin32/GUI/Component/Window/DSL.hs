module TEAWin32.GUI.Component.Window.DSL
    ( icon_
    , cursor_
    , backgroundColour_
    , window_
    ) where

import           Control.Monad                          (unless)
import           Control.Monad.Writer                   (MonadWriter (tell),
                                                         execWriter)
import           Data.Text                              (Text)
import           TEAWin32.Drawing                       (Colour)
import           TEAWin32.GUI                           (Cursor, Icon,
                                                         UniqueId (..),
                                                         WindowStyle)
import           TEAWin32.GUI.Component                 (GUIComponent (GUIComponent),
                                                         GUIComponents)
import           TEAWin32.GUI.Component.Property        (ComponentChildren (ComponentChildren))
import           TEAWin32.GUI.Component.Window          (Window (Window))
import           TEAWin32.GUI.Component.Window.Property

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
