module TEAWin32.Core.Util (whenJust) where

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing _  = pure ()
