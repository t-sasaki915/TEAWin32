module Graphics.GUI.Component.Window.Internal (restoreWindowFromHWND) where

import           Control.Monad                          (when)
import           Control.Monad.Writer                   (MonadIO (liftIO),
                                                         execWriterT, tell)
import           Graphics.GUI.Component                 (GUIComponent (GUIComponent))
import qualified Graphics.GUI.Component.Internal        as ComponentInternal
import           Graphics.GUI.Component.Window          (Window (Window))
import           Graphics.GUI.Component.Window.Property
import qualified Graphics.GUI.Internal                  as Internal
import qualified Graphics.Win32                         as Win32

restoreWindowFromHWND :: Win32.HWND -> IO GUIComponent
restoreWindowFromHWND hwnd = do
    windowUniqueId  <- ComponentInternal.getUniqueIdFromHWND hwnd
    windowClassName <- ComponentInternal.getClassName hwnd
    windowStyle     <- ComponentInternal.getWindowStyle hwnd

    isWindowTitleSet    <- ComponentInternal.isFlagSet "WINDOWTITLE_SET"    hwnd
    isWindowIconSet     <- ComponentInternal.isFlagSet "WINDOWICON_SET"     hwnd
    isWindowCursorSet   <- ComponentInternal.isFlagSet "WINDOWCURSOR_SET"   hwnd
    isWindowSizeSet     <- ComponentInternal.isFlagSet "WINDOWSIZE_SET"     hwnd
    isWindowPositionSet <- ComponentInternal.isFlagSet "WINDOWPOSITION_SET" hwnd
    isWindowBrushSet    <- ComponentInternal.isFlagSet "WINDOWBRUSH_SET"    hwnd
    isWindowChildrenSet <- ComponentInternal.isFlagSet "WINDOWCHILDREN_SET" hwnd

    properties <- execWriterT $ do
        when isWindowTitleSet $
            liftIO (ComponentInternal.getWindowTitle hwnd) >>= \windowTitle ->
                tell [WindowProperty $ WindowTitle windowTitle]

        when isWindowCursorSet $
            liftIO (ComponentInternal.getWindowCursor hwnd) >>= \windowCursor ->
                tell [WindowProperty $ WindowCursor windowCursor]

        when isWindowSizeSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(_, _, w, h) ->
                tell [WindowProperty $ WindowSize (w, h)]

        when isWindowPositionSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(x, y, _, _) ->
                tell [WindowProperty $ WindowPosition (x, y)]

        when isWindowChildrenSet $
            liftIO (Internal.withChildWindows hwnd (mapM ComponentInternal.restoreComponentFromHWND)) >>= \children ->
                tell [WindowProperty $ WindowChildren children]

    pure $ GUIComponent $ Window windowUniqueId windowClassName windowStyle properties
