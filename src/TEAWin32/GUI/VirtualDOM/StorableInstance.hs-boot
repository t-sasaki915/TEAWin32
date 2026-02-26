module TEAWin32.GUI.VirtualDOM.StorableInstance () where

import                          Foreign                 (Storable)
import {-# SOURCE #-}           TEAWin32.GUI.VirtualDOM (CCallRequest)

instance Storable CCallRequest
