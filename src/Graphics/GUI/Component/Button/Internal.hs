module Graphics.GUI.Component.Button.Internal (restoreButtonFromHWND) where

import           Control.Monad                          (when)
import           Control.Monad.Writer                   (MonadIO (liftIO),
                                                         execWriterT, tell)
import           Graphics.GUI.Component                 (GUIComponent (..))
import           Graphics.GUI.Component.Button          (Button (Button))
import           Graphics.GUI.Component.Button.Property
import qualified Graphics.GUI.Component.Internal        as ComponentInternal
import qualified Graphics.Win32                         as Win32

restoreButtonFromHWND :: Win32.HWND -> IO GUIComponent
restoreButtonFromHWND hwnd = do
    buttonUniqueId  <- ComponentInternal.getUniqueIdFromHWND hwnd

    isButtonLabelSet    <- ComponentInternal.isFlagSet "BUTTONLABEL_SET"    hwnd
    isButtonSizeSet     <- ComponentInternal.isFlagSet "BUTTONSIZE_SET"     hwnd
    isButtonPositionSet <- ComponentInternal.isFlagSet "BUTTONPOSITION_SET" hwnd
    isButtonClickedSet  <- ComponentInternal.isFlagSet "BUTTONCLICKED_SET"  hwnd

    properties <- execWriterT $ do
        when isButtonLabelSet $
            liftIO (ComponentInternal.getWindowTitle hwnd) >>= \buttonLabel ->
                tell [ButtonProperty $ ButtonLabel buttonLabel]

        when isButtonSizeSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(_, _, w, h) ->
                tell [ButtonProperty $ ButtonSize (w, h)]

        when isButtonPositionSet $
            liftIO (ComponentInternal.getRelativeRect hwnd) >>= \(x, y, _, _) ->
                tell [ButtonProperty $ ButtonPosition (x, y)]

        when isButtonClickedSet $
            liftIO (ComponentInternal.getEventHandler "BUTTONCLICKED" hwnd) >>= \msg ->
                tell [ButtonProperty $ ButtonClicked msg]

    pure $ GUIComponent $ Button buttonUniqueId properties
