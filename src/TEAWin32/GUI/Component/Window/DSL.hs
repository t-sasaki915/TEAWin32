module TEAWin32.GUI.Component.Window.DSL
    ( icon_
    , cursor_
    , backgroundColour_
    , window_'
    , window_
    ) where

import           Control.Monad.Writer.Strict            (tell)
import           Data.Text                              (Text)
import           TEAWin32.Drawing                       (Colour)
import           TEAWin32.GUI                           (Cursor, Icon,
                                                         UniqueId (..),
                                                         WindowStyle)
import           TEAWin32.GUI.Component                 (GUIComponent (GUIComponent),
                                                         GUIComponents)
import           TEAWin32.GUI.Component.Window          (Window (Window))
import           TEAWin32.GUI.Component.Window.Property
import           TEAWin32.GUI.DSL.Internal              (getNextSystemUniqueId,
                                                         resolveChildren)

icon_ :: Icon -> WindowProperty
icon_ = WindowProperty . WindowIcon

cursor_ :: Cursor -> WindowProperty
cursor_ = WindowProperty . WindowCursor

backgroundColour_ :: Colour -> WindowProperty
backgroundColour_ = WindowProperty . WindowBackgroundColour

window_' :: Text -> Text -> WindowStyle -> [WindowProperty] -> GUIComponents -> GUIComponents
window_' windowUniqueId windowClass windowStyle windowProperties windowChildren =
    resolveChildren WindowProperty windowProperties windowChildren >>= \properties ->
        tell [GUIComponent (Window (UserUniqueId windowUniqueId) windowClass windowStyle properties)]

window_ :: Text -> WindowStyle -> [WindowProperty] -> GUIComponents -> GUIComponents
window_ windowClass windowStyle windowProperties windowChildren =
    resolveChildren WindowProperty windowProperties windowChildren >>= \properties ->
        getNextSystemUniqueId >>= \uniqueId ->
            tell [GUIComponent (Window uniqueId windowClass windowStyle properties)]
