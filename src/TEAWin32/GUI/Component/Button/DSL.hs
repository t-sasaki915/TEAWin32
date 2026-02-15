module TEAWin32.GUI.Component.Button.DSL (button_) where

import           Control.Monad.Writer                   (MonadWriter (tell))
import           Data.Text                              (Text)
import           TEAWin32.GUI                           (UniqueId (..))
import           TEAWin32.GUI.Component                 (GUIComponent (..),
                                                         GUIComponents)
import           TEAWin32.GUI.Component.Button          (Button (Button))
import           TEAWin32.GUI.Component.Button.Property

button_ :: Text -> [ButtonProperty] -> GUIComponents
button_ uniqueId = tell . pure . GUIComponent . Button (UniqueId uniqueId)
