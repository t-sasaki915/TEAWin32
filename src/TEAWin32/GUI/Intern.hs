module TEAWin32.GUI.Intern (internUniqueId) where

import           Data.IORef      (IORef, atomicModifyIORef', newIORef)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import           Foreign.C.Types (CInt)
import           GHC.IO          (unsafePerformIO)
import           TEAWin32.GUI    (UniqueId (..))

data UniqueIdInternStorage = UniqueIdInternStorage
    { userUniqueIdInternStorage :: Map Text CInt
    , nextUserUniqueIdNumber    :: CInt
    }

uniqueIdInternStorageRef :: IORef UniqueIdInternStorage
uniqueIdInternStorageRef = unsafePerformIO (newIORef (UniqueIdInternStorage Map.empty 0))
{-# NOINLINE uniqueIdInternStorageRef #-}

internUniqueId :: UniqueId -> IO CInt
internUniqueId (SystemUniqueId n) = pure (10000 + fromIntegral n)
internUniqueId (UserUniqueId text) =
    atomicModifyIORef' uniqueIdInternStorageRef $ \storage ->
        case Map.lookup text (userUniqueIdInternStorage storage) of
            Just n ->
                (storage, n)

            Nothing ->
                let newNumber = nextUserUniqueIdNumber storage
                    newInternMap = Map.insert text newNumber (userUniqueIdInternStorage storage) in
                        (UniqueIdInternStorage newInternMap (newNumber + 1), newNumber)


