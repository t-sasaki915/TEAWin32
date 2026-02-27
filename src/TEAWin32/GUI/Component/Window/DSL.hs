module TEAWin32.GUI.Component.Window.DSL
    ( icon_
    , cursor_
    , bgColour_
    , window_'
    , window_
    ) where

import           Control.Monad.Writer.Strict            (tell)
import           Data.Text                              (Text)
import           TEAWin32.Drawing                       (Colour)
import           TEAWin32.GUI                           (Cursor, Icon,
                                                         WindowStyle)
import           TEAWin32.GUI.Component                 (GUIComponent (GUIComponent))
import           TEAWin32.GUI.Component.Window          (Window (Window))
import           TEAWin32.GUI.Component.Window.Property
import           TEAWin32.GUI.DSL.Internal              (DSL,
                                                         generateNextUniqueId,
                                                         internUserUniqueId,
                                                         resolveChildren)

icon_ :: Icon -> WindowProperty
icon_ = WindowProperty . WindowIcon

cursor_ :: Cursor -> WindowProperty
cursor_ = WindowProperty . WindowCursor

bgColour_ :: Colour -> WindowProperty
bgColour_ = WindowProperty . WindowBackgroundColour

window_' :: Text -> Text -> WindowStyle -> [WindowProperty] -> DSL -> DSL
window_' windowUniqueId windowClass windowStyle windowProperties windowChildren =
    internUserUniqueId windowUniqueId >>= \uid ->
        resolveChildren WindowProperty windowProperties windowChildren >>= \properties ->
            tell [GUIComponent (Window uid windowClass windowStyle properties)]

window_ :: Text -> WindowStyle -> [WindowProperty] -> DSL -> DSL
window_ windowClass windowStyle windowProperties windowChildren =
    generateNextUniqueId >>= \uid ->
        resolveChildren WindowProperty windowProperties windowChildren >>= \properties ->
            tell [GUIComponent (Window uid windowClass windowStyle properties)]
