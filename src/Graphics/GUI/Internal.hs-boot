module Graphics.GUI.Internal (cursorCacheRef, iconCacheRef, fontCacheRef) where

import                          Control.Concurrent (MVar)
import                          Data.Map           (Map)
import {-# SOURCE #-}           Graphics.GUI       (Cursor, Font, Icon)
import                qualified Graphics.Win32     as Win32

cursorCacheRef :: MVar (Map Cursor Win32.HANDLE)

iconCacheRef :: MVar (Map Icon Win32.HANDLE)

fontCacheRef :: MVar (Map Font Win32.HANDLE)
