module TEAWin32.GUI.Component.Button.DSL (button_', button_) where

import           Control.Monad.Writer.Strict            (MonadWriter (tell))
import           Data.Text                              (Text)
import           TEAWin32.GUI.Component                 (GUIComponent (..))
import           TEAWin32.GUI.Component.Button          (Button (Button))
import           TEAWin32.GUI.Component.Button.Property
import           TEAWin32.GUI.DSL.Internal              (DSL,
                                                         generateNextUniqueId,
                                                         internUserUniqueId)

button_' :: Text -> [ButtonProperty] -> DSL
button_' uniqueId properties =
    internUserUniqueId uniqueId >>= \uid ->
        tell [GUIComponent (Button uid properties)]

button_ :: [ButtonProperty] -> DSL
button_ properties =
    generateNextUniqueId >>= \uid ->
        tell [GUIComponent (Button uid properties)]
