{-# LANGUAGE TemplateHaskell #-}

module Model
    ( Model (..)
    , displayWidth
    , displayHeight
    , clickedCount
    ) where

import           Control.Lens (makeLenses)

data Model = Model
    { _displayWidth  :: Int
    , _displayHeight :: Int
    , _clickedCount  :: Int
    } deriving Show

makeLenses ''Model
