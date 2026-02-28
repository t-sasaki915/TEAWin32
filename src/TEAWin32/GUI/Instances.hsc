{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TEAWin32.GUI.Instances () where

import           Foreign      (Storable (..), fillBytes, Int32)
import           Foreign.C    (CDouble (..), CInt)
import           TEAWin32.GUI (ScalableValue (..))

#include "DPIAware.h"

instance Storable ScalableValue where
    sizeOf _ = #{size CScalableValue}

    alignment _ = #{alignment CScalableValue}

    peek ptr = do
        value      <- #{peek CScalableValue, value}      ptr
        isScalable <- #{peek CScalableValue, isScalable} ptr

        if (isScalable :: CInt) == 0
            then pure (RawValue value)
            else pure (ScalableValue value)

    poke ptr (RawValue v) = do
        fillBytes ptr 0 #{size CScalableValue}

        #{poke CScalableValue, value}      ptr (CDouble v)
        #{poke CScalableValue, isScalable} ptr (#{const FALSE} :: #{type BOOL})

    poke ptr (ScalableValue v) = do
        fillBytes ptr 0 #{size CScalableValue}

        #{poke CScalableValue, value}      ptr (CDouble v)
        #{poke CScalableValue, isScalable} ptr (#{const TRUE} :: #{type BOOL})
