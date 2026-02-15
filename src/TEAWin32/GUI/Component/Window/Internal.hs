module TEAWin32.GUI.Component.Window.Internal (restoreWindowFromHWND) where

import           Control.Monad                             (when)
import           Control.Monad.Writer                      (MonadIO (liftIO),
                                                            execWriterT, tell)
import qualified Graphics.Win32                            as Win32
import           TEAWin32.GUI.Component                    (GUIComponent (GUIComponent))
import qualified TEAWin32.GUI.Component.Internal           as ComponentInternal
import           TEAWin32.GUI.Component.Internal.Attribute
import           TEAWin32.GUI.Component.Property
import           TEAWin32.GUI.Component.Window             (Window (Window))
import           TEAWin32.GUI.Component.Window.Property
import qualified TEAWin32.GUI.Internal                     as Internal

restoreWindowFromHWND :: Win32.HWND -> IO GUIComponent
restoreWindowFromHWND hwnd = do
    windowUniqueId  <- getComponentUniqueIdFromHWND hwnd
    windowClassName <- getWindowClassNameFromHWND hwnd
    windowStyle     <- getWindowStyleFromHWND hwnd

    isComponentTitleSet            <- doesHWNDHaveFlag ComponentTitleSet            hwnd
    isComponentSizeSet             <- doesHWNDHaveFlag ComponentSizeSet             hwnd
    isComponentPositionSet         <- doesHWNDHaveFlag ComponentPositionSet         hwnd
    isComponentFontSet             <- doesHWNDHaveFlag ComponentFontSet             hwnd
    isComponentBackgroundColourSet <- doesHWNDHaveFlag ComponentBackgroundColourSet hwnd
    isComponentChildrenSet         <- doesHWNDHaveFlag ComponentChildrenSet         hwnd
    isWindowIconSet                <- doesHWNDHaveFlag WindowIconSet                hwnd
    isWindowCursorSet              <- doesHWNDHaveFlag WindowCursorSet              hwnd

    properties <- execWriterT $ do
        when isComponentTitleSet $
            liftIO (getComponentTitleFromHWND hwnd) >>= \windowTitle ->
                tell [WindowProperty $ ComponentTitle windowTitle]

        when (isComponentSizeSet || isComponentPositionSet) $
            liftIO (ComponentInternal.getRelativeRectFromHWNDUsingWin32 hwnd) >>= \(x, y, w, h) -> do
                when isComponentSizeSet $
                    tell [WindowProperty $ ComponentSize (w, h)]

                when isComponentPositionSet $
                    tell [WindowProperty $ ComponentPosition (x, y)]

        when isComponentFontSet $
            liftIO (getComponentFontFromHWND hwnd) >>= \font ->
                tell [WindowProperty $ ComponentFont font]

        when isComponentBackgroundColourSet $
            liftIO (getComponentBackgroundColourFromHWND hwnd) >>= \backgroundColour ->
                tell [WindowProperty $ WindowBackgroundColour backgroundColour]

        when isComponentChildrenSet $
            liftIO (Internal.withImmediateChildWindows hwnd (mapM ComponentInternal.restoreComponentFromHWND)) >>= \children ->
                tell [WindowProperty $ ComponentChildren children]

        when isWindowIconSet $
            liftIO (getWindowIconFromHWND hwnd) >>= \windowIcon ->
                tell [WindowProperty $ WindowIcon windowIcon]

        when isWindowCursorSet $
            liftIO (getWindowCursorFromHWND hwnd) >>= \windowCursor ->
                tell [WindowProperty $ WindowCursor windowCursor]


    pure $ GUIComponent $ Window windowUniqueId windowClassName windowStyle properties
