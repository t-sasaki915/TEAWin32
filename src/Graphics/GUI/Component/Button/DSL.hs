module Graphics.GUI.Component.Button.DSL (button_) where

import           Control.Monad.Writer                   (MonadWriter (tell))
import           Data.Text                              (Text)
import           Graphics.GUI                           (UniqueId (..))
import           Graphics.GUI.Component                 (GUIComponent (..),
                                                         GUIComponents)
import           Graphics.GUI.Component.Button          (Button (Button))
import           Graphics.GUI.Component.Button.Property

button_ :: Text -> [ButtonProperty] -> GUIComponents
button_ uniqueId = tell . pure . GUIComponent . Button (UniqueId uniqueId)
