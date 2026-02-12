module Graphics.GUI.Component.Window.Internal (restoreWindowFromHWND) where

import           Control.Monad                          (when)
import           Control.Monad.Writer                   (MonadIO (liftIO),
                                                         execWriterT, tell)
import qualified Data.Text                              as Text
import           Graphics.GUI.Component                 (GUIComponent (GUIComponent))
import qualified Graphics.GUI.Component.Internal        as ComponentInternal
import           Graphics.GUI.Component.Property
import           Graphics.GUI.Component.Window          (Window (Window))
import           Graphics.GUI.Component.Window.Property
import qualified Graphics.GUI.Internal                  as Internal
import qualified Graphics.Win32                         as Win32

restoreWindowFromHWND :: Win32.HWND -> IO GUIComponent
restoreWindowFromHWND hwnd = do
    windowUniqueId  <- ComponentInternal.getUniqueIdFromHWND hwnd
    windowStyle     <- ComponentInternal.getWindowStyle hwnd
    windowClassName <-
        ComponentInternal.getClassName hwnd >>= \className ->
            case Text.stripPrefix (Text.pack ComponentInternal.windowClassPrefix) className of
                Just className' -> pure className'
                Nothing         -> error "Tried to process a window that is not managed by TEAWin32GUI."

    isComponentTitleSet         <- ComponentInternal.isFlagSet "COMPONENTTITLE_SET"         hwnd
    isWindowIconSet             <- ComponentInternal.isFlagSet "WINDOWICON_SET"             hwnd
    isWindowCursorSet           <- ComponentInternal.isFlagSet "WINDOWCURSOR_SET"           hwnd
    isComponentSizeSet          <- ComponentInternal.isFlagSet "COMPONENTSIZE_SET"          hwnd
    isComponentPositionSet      <- ComponentInternal.isFlagSet "COMPONENTPOSITION_SET"      hwnd
    isWindowBackgroundColourSet <- ComponentInternal.isFlagSet "WINDOWBACKGROUNDCOLOUR_SET" hwnd
    isComponentChildrenSet      <- ComponentInternal.isFlagSet "COMPONENTCHILDREN_SET"      hwnd

    properties <- execWriterT $ do
        when isComponentTitleSet $
            liftIO (ComponentInternal.getWindowTitle hwnd) >>= \windowTitle ->
                tell [WindowProperty $ ComponentTitle windowTitle]

        when isWindowIconSet $
            liftIO (ComponentInternal.getWindowIcon hwnd) >>= \windowIcon ->
                tell [WindowProperty $ WindowIcon windowIcon]

        when isWindowCursorSet $
            liftIO (ComponentInternal.getWindowCursor hwnd) >>= \windowCursor ->
                tell [WindowProperty $ WindowCursor windowCursor]

        when isComponentSizeSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(_, _, w, h) ->
                tell [WindowProperty $ ComponentSize (w, h)]

        when isComponentPositionSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(x, y, _, _) ->
                tell [WindowProperty $ ComponentPosition (x, y)]

        when isWindowBackgroundColourSet $
            liftIO (ComponentInternal.getWindowBackgroundColour hwnd) >>= \backgroundColour ->
                tell [WindowProperty $ WindowBackgroundColour backgroundColour]

        when isComponentChildrenSet $
            liftIO (Internal.withImmediateChildWindows hwnd (mapM ComponentInternal.restoreComponentFromHWND)) >>= \children ->
                tell [WindowProperty $ ComponentChildren children]

    pure $ GUIComponent $ Window windowUniqueId windowClassName windowStyle properties
