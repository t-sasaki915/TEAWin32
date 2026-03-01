module TEAWin32.Core.Types
    ( UniqueId (..)
    , TEAWin32Settings (..)
    , ScalableValue (..)
    , RawValue (..)
    , defaultTEAWin32Settings
    ) where

import           Foreign   (Storable (..), fillBytes)
import           Foreign.C (CInt)

newtype UniqueId = UniqueId Int

instance Storable UniqueId where
    sizeOf _ = sizeOf (0 :: CInt)

    alignment _ = alignment (0 :: CInt)

    peek ptr = UniqueId <$> peekByteOff ptr 0

    poke ptr (UniqueId uniqueId) =
        fillBytes ptr 0 (sizeOf (undefined :: UniqueId)) >>
            pokeByteOff ptr 0 uniqueId

newtype TEAWin32Settings = TEAWin32Settings
    { useVisualStyles :: Bool
    }

defaultTEAWin32Settings :: TEAWin32Settings
defaultTEAWin32Settings = TEAWin32Settings
    { useVisualStyles = True
    }

data ScalableValue = ScalableValue Double
                   | RawValue      Double
                   deriving Eq

class RawValue a where
    raw :: a -> ScalableValue

instance RawValue Integer where
    raw = RawValue . fromIntegral

instance RawValue Int where
    raw = RawValue . fromIntegral

instance RawValue Double where
    raw = RawValue

instance Num ScalableValue where
    fromInteger a = ScalableValue (fromInteger a)

    (ScalableValue a) + (ScalableValue b) = ScalableValue (a + b)
    (RawValue a)      + (RawValue b)      = RawValue      (a + b)
    (ScalableValue a) + (RawValue b)      = ScalableValue (a + b)
    (RawValue a)      + (ScalableValue b) = RawValue      (a + b)

    (ScalableValue a) * (ScalableValue b) = ScalableValue (a * b)
    (RawValue a)      * (RawValue b)      = RawValue      (a * b)
    (ScalableValue a) * (RawValue b)      = ScalableValue (a * b)
    (RawValue a)      * (ScalableValue b) = RawValue      (a * b)

    abs (ScalableValue a) = ScalableValue (abs a)
    abs (RawValue a)      = RawValue      (abs a)

    signum (ScalableValue a) = ScalableValue (signum a)
    signum (RawValue a)      = RawValue      (signum a)

    negate (ScalableValue a) = ScalableValue (negate a)
    negate (RawValue a)      = RawValue      (negate a)

instance Fractional ScalableValue where
    fromRational a = ScalableValue (fromRational a)

    (ScalableValue a) / (ScalableValue b) = ScalableValue (a / b)
    (RawValue a)      / (RawValue b)      = RawValue      (a / b)
    (ScalableValue a) / (RawValue b)      = ScalableValue (a / b)
    (RawValue a)      / (ScalableValue b) = RawValue      (a / b)


