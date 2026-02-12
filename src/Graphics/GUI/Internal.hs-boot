module Graphics.GUI.Internal (cursorCacheRef, iconCacheRef, fontCacheRef) where

import                          Control.Concurrent (MVar)
import                          Data.Bimap         (Bimap)
import {-# SOURCE #-}           Graphics.GUI       (Cursor, Font, Icon)
import                qualified Graphics.Win32     as Win32

cursorCacheRef :: MVar (Bimap Cursor Win32.HANDLE)

iconCacheRef :: MVar (Bimap Icon Win32.HANDLE)

fontCacheRef :: MVar (Bimap Font Win32.HANDLE)
