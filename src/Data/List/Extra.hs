module Data.List.Extra (firstJust) where

import           Data.Maybe (listToMaybe, mapMaybe)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f
