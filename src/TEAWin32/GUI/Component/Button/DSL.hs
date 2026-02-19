module TEAWin32.GUI.Component.Button.DSL (button_', button_) where

import           Control.Monad.Writer.Strict            (MonadWriter (tell))
import           Data.Text                              (Text)
import           TEAWin32.GUI                           (UniqueId (..))
import           TEAWin32.GUI.Component                 (GUIComponent (..),
                                                         GUIComponents)
import           TEAWin32.GUI.Component.Button          (Button (Button))
import           TEAWin32.GUI.Component.Button.Property
import           TEAWin32.GUI.DSL.Internal              (getNextSystemUniqueId)

button_' :: Text -> [ButtonProperty] -> GUIComponents
button_' uniqueId = tell . pure . GUIComponent . Button (UserUniqueId uniqueId)

button_ :: [ButtonProperty] -> GUIComponents
button_ properties =
    getNextSystemUniqueId >>= \uniqueId ->
        tell [GUIComponent (Button uniqueId properties)]
