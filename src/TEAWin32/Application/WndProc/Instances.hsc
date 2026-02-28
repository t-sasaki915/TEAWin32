{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TEAWin32.Application.WndProc.Instances () where

import           Foreign                      (Storable (..), fillBytes)
import           TEAWin32.Application.WndProc (HaskellWndProcCallbacks (..))
import           TEAWin32.Exception           (TEAWin32Error (..), errorTEAWin32)

#include "TEAWin32.h"

instance Storable HaskellWndProcCallbacks where
    sizeOf _ = #{size HaskellWndProcCallbacks}

    alignment _ = #{alignment HaskellWndProcCallbacks}

    peek _ = errorTEAWin32 (InternalTEAWin32Error "Tried to peek HaskellWndProcCallbacks")

    poke ptr callbacks = do
        fillBytes ptr 0 #{size HaskellWndProcCallbacks}

        #{poke HaskellWndProcCallbacks, onWindowDestroy} ptr (onWindowDestroyCallback callbacks)
