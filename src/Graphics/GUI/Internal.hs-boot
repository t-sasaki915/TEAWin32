module Graphics.GUI.Internal (cursorCacheRef, iconCacheRef) where

import                          Control.Concurrent (MVar)
import                          Data.Bimap         (Bimap)
import {-# SOURCE #-}           Graphics.GUI       (Cursor, Icon)
import                qualified Graphics.Win32     as Win32

cursorCacheRef :: MVar (Bimap Cursor Win32.HANDLE)

iconCacheRef :: MVar (Bimap Icon Win32.HANDLE)
