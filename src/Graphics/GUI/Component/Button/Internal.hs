module Graphics.GUI.Component.Button.Internal (restoreButtonFromHWND) where

import           Control.Monad                             (when)
import           Control.Monad.Writer                      (MonadIO (liftIO),
                                                            execWriterT, tell)
import           Graphics.GUI.Component                    (GUIComponent (..))
import           Graphics.GUI.Component.Button             (Button (Button))
import           Graphics.GUI.Component.Button.Property
import qualified Graphics.GUI.Component.Internal           as ComponentInternal
import           Graphics.GUI.Component.Internal.Attribute
import           Graphics.GUI.Component.Property
import qualified Graphics.Win32                            as Win32

restoreButtonFromHWND :: Win32.HWND -> IO GUIComponent
restoreButtonFromHWND hwnd = do
    buttonUniqueId <- getComponentUniqueIdFromHWND hwnd

    isComponentTitleSet    <- doesHWNDHaveFlag ComponentTitleSet    hwnd
    isComponentSizeSet     <- doesHWNDHaveFlag ComponentSizeSet     hwnd
    isComponentPositionSet <- doesHWNDHaveFlag ComponentPositionSet hwnd
    isComponentFontSet     <- doesHWNDHaveFlag ComponentFontSet     hwnd
    isComponentOnClickSet  <- doesHWNDHaveFlag ComponentOnClickSet  hwnd

    properties <- execWriterT $ do
        when isComponentTitleSet $
            liftIO (getComponentTitleFromHWND hwnd) >>= \buttonLabel ->
                tell [ButtonProperty $ ComponentTitle buttonLabel]

        when (isComponentSizeSet || isComponentPositionSet) $
            liftIO (ComponentInternal.getRelativeRectFromHWNDUsingWin32 hwnd) >>= \(x, y, w, h) -> do
                when isComponentSizeSet $
                    tell [ButtonProperty $ ComponentSize (w, h)]

                when isComponentPositionSet $
                    tell [ButtonProperty $ ComponentPosition (x, y)]

        when isComponentFontSet $
            liftIO (getComponentFontFromHWND hwnd) >>= \font ->
                tell [ButtonProperty $ ComponentFont font]

        when isComponentOnClickSet $
            liftIO (getEventHandlerFromHWND ComponentClickEvent hwnd) >>= \msg ->
                tell [ButtonProperty $ ComponentOnClick msg]

    pure $ GUIComponent $ Button buttonUniqueId properties
