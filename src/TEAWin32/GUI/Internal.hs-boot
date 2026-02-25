module TEAWin32.GUI.Internal
    ( cursorCacheRef
    , resourceIconCacheRef
    , stockIconCacheRef
    , fontCacheRef
    ) where

import                          Control.Concurrent (MVar)
import                          Data.Map           (Map)
import                qualified Graphics.Win32     as Win32
import {-# SOURCE #-}           TEAWin32.GUI       (Cursor, Font, Icon)

cursorCacheRef :: MVar (Map Cursor Win32.HCURSOR)

resourceIconCacheRef :: MVar (Map Icon Win32.HICON)

stockIconCacheRef :: MVar (Map Icon Win32.HICON)

fontCacheRef :: MVar (Map Font Win32.HFONT)
