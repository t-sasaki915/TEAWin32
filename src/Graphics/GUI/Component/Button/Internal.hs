module Graphics.GUI.Component.Button.Internal (restoreButtonFromHWND) where

import           Control.Monad                          (when)
import           Control.Monad.Writer                   (MonadIO (liftIO),
                                                         execWriterT, tell)
import           Graphics.GUI.Component                 (GUIComponent (..))
import           Graphics.GUI.Component.Button          (Button (Button))
import           Graphics.GUI.Component.Button.Property
import qualified Graphics.GUI.Component.Internal        as ComponentInternal
import           Graphics.GUI.Component.Property
import qualified Graphics.Win32                         as Win32

restoreButtonFromHWND :: Win32.HWND -> IO GUIComponent
restoreButtonFromHWND hwnd = do
    buttonUniqueId  <- ComponentInternal.getUniqueIdFromHWND hwnd

    isComponentTitleSet    <- ComponentInternal.isFlagSet "COMPONENTTITLE_SET"    hwnd
    isComponentSizeSet     <- ComponentInternal.isFlagSet "COMPONENTSIZE_SET"     hwnd
    isComponentPositionSet <- ComponentInternal.isFlagSet "COMPONENTPOSITION_SET" hwnd
    isComponentOnClickSet  <- ComponentInternal.isFlagSet "COMPONENTONCLICK_SET"  hwnd

    properties <- execWriterT $ do
        when isComponentTitleSet $
            liftIO (ComponentInternal.getWindowTitle hwnd) >>= \buttonLabel ->
                tell [ButtonProperty $ ComponentTitle buttonLabel]

        when isComponentSizeSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(_, _, w, h) ->
                tell [ButtonProperty $ ComponentSize (w, h)]

        when isComponentPositionSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(x, y, _, _) ->
                tell [ButtonProperty $ ComponentPosition (x, y)]

        when isComponentOnClickSet $
            liftIO (ComponentInternal.getEventHandler "COMPONENTONCLICK" hwnd) >>= \msg ->
                tell [ButtonProperty $ ComponentOnClick msg]

    pure $ GUIComponent $ Button buttonUniqueId properties
