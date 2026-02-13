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
