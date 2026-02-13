module Graphics.GUI.Component.Window.Internal (restoreWindowFromHWND) where

import           Control.Monad                             (when)
import           Control.Monad.Writer                      (MonadIO (liftIO),
                                                            execWriterT, tell)
import           Graphics.GUI.Component                    (GUIComponent (GUIComponent))
import qualified Graphics.GUI.Component.Internal           as ComponentInternal
import           Graphics.GUI.Component.Internal.Attribute
import           Graphics.GUI.Component.Property
import           Graphics.GUI.Component.Window             (Window (Window))
import           Graphics.GUI.Component.Window.Property
import qualified Graphics.GUI.Internal                     as Internal
import qualified Graphics.Win32                            as Win32

restoreWindowFromHWND :: Win32.HWND -> IO GUIComponent
restoreWindowFromHWND hwnd = do
    windowUniqueId  <- getComponentUniqueIdFromHWND hwnd
    windowClassName <- getWindowClassNameFromHWND hwnd
    windowStyle     <- getWindowStyleFromHWND hwnd

    isComponentTitleSet         <- doesHWNDHaveFlag ComponentTitleSet            hwnd
    isWindowIconSet             <- doesHWNDHaveFlag WindowIconSet                hwnd
    isWindowCursorSet           <- doesHWNDHaveFlag WindowCursorSet              hwnd
    isComponentSizeSet          <- doesHWNDHaveFlag ComponentSizeSet             hwnd
    isComponentPositionSet      <- doesHWNDHaveFlag ComponentPositionSet         hwnd
    isWindowBackgroundColourSet <- doesHWNDHaveFlag ComponentBackgroundColourSet hwnd
    isComponentChildrenSet      <- doesHWNDHaveFlag ComponentChildrenSet         hwnd

    properties <- execWriterT $ do
        when isComponentTitleSet $
            liftIO (getComponentTitleFromHWND hwnd) >>= \windowTitle ->
                tell [WindowProperty $ ComponentTitle windowTitle]

        when isWindowIconSet $
            liftIO (getWindowIconFromHWND hwnd) >>= \windowIcon ->
                tell [WindowProperty $ WindowIcon windowIcon]

        when isWindowCursorSet $
            liftIO (getWindowCursorFromHWND hwnd) >>= \windowCursor ->
                tell [WindowProperty $ WindowCursor windowCursor]

        when isComponentSizeSet $
            liftIO (ComponentInternal.getRelativeRectFromHWNDUsingWin32 hwnd) >>= \(_, _, w, h) ->
                tell [WindowProperty $ ComponentSize (w, h)]

        when isComponentPositionSet $
            liftIO (ComponentInternal.getRelativeRectFromHWNDUsingWin32 hwnd) >>= \(x, y, _, _) ->
                tell [WindowProperty $ ComponentPosition (x, y)]

        when isWindowBackgroundColourSet $
            liftIO (getComponentBackgroundColourFromHWND hwnd) >>= \backgroundColour ->
                tell [WindowProperty $ WindowBackgroundColour backgroundColour]

        when isComponentChildrenSet $
            liftIO (Internal.withImmediateChildWindows hwnd (mapM ComponentInternal.restoreComponentFromHWND)) >>= \children ->
                tell [WindowProperty $ ComponentChildren children]

    pure $ GUIComponent $ Window windowUniqueId windowClassName windowStyle properties
