module Graphics.GUI.Internal (cursorCacheRef) where

import                          Data.Bimap     (Bimap)
import                          Data.IORef     (IORef)
import {-# SOURCE #-}           Graphics.GUI   (Cursor)
import                qualified Graphics.Win32 as Win32

cursorCacheRef :: IORef (Bimap Cursor Win32.HANDLE)
