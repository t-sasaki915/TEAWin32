module TEAWin32.GUI.DSL.Internal
    ( getNextSystemUniqueId
    , resolveChildren
    ) where

import           Control.Monad.State.Strict      (State, lift, state)
import           Control.Monad.Writer.Strict     (WriterT, execWriterT)
import           TEAWin32.GUI                    (UniqueId (..))
import           TEAWin32.GUI.Component          (GUIComponent, GUIComponents)
import           TEAWin32.GUI.Component.Property (ComponentChildren (..))

getNextSystemUniqueId :: WriterT [GUIComponent] (State Int) UniqueId
getNextSystemUniqueId =
    state $ \currentCount ->
        let newCount = currentCount + 1 in
            (SystemUniqueId newCount, newCount)

resolveChildren :: (ComponentChildren -> a) -> [a] -> GUIComponents -> WriterT [GUIComponent] (State Int) [a]
resolveChildren wrapper properties children =
    lift (execWriterT children) >>= \children' ->
        pure (properties ++ [ wrapper (ComponentChildren children') | not (null children') ])
