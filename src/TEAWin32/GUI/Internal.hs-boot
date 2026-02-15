module TEAWin32.GUI.Internal (cursorCacheRef, iconCacheRef, fontCacheRef) where

import                          Control.Concurrent (MVar)
import                          Data.Map           (Map)
import                qualified Graphics.Win32     as Win32
import {-# SOURCE #-}           TEAWin32.GUI       (Cursor, Font, Icon)

cursorCacheRef :: MVar (Map Cursor Win32.HANDLE)

iconCacheRef :: MVar (Map Icon Win32.HANDLE)

fontCacheRef :: MVar (Map Font Win32.HANDLE)
