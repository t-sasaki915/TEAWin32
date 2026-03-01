{-# LANGUAGE CPP #-}

module TEAWin32.Core.Native.Constants 
    ( size_ScalableValue
    , alignment_ScalableValue
    , offset_ScalableValue_Value
    , offset_ScalableValue_IsScalable
    ) where

#include "DPIAware.h"

size_ScalableValue :: Int
size_ScalableValue = #size ScalableValue

alignment_ScalableValue :: Int
alignment_ScalableValue = #size ScalableValue

offset_ScalableValue_Value :: Int
offset_ScalableValue_Value = #offset ScalableValue, value

offset_ScalableValue_IsScalable :: Int
offset_ScalableValue_IsScalable = #offset ScalableValue, isScalable
