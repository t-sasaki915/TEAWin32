module TEAWin32.Application.WndProc
    ( HaskellWndProcCallbacks (..)
    , createHaskellWndProcCallbacks
    , freeHaskellWndProcCallbacks
    ) where

import           Foreign      (FunPtr, freeHaskellFunPtr)
import           Foreign.C    (CInt (..))
import           GHC.Stack    (HasCallStack)
import           TEAWin32.GUI (UniqueId, fromCUniqueId)

newtype HaskellWndProcCallbacks = HaskellWndProcCallbacks
    { onWindowDestroyCallback :: FunPtr OnWindowDestroyC
    }

data CallbackResult = Processed
                    | Ignored

type OnWindowDestroy = UniqueId -> IO CallbackResult
type OnWindowDestroyC = CInt -> IO CInt

foreign import ccall "wrapper"
    makeOnWindowDestroy :: OnWindowDestroyC -> IO (FunPtr OnWindowDestroyC)

wrapOnWindowDestroy :: OnWindowDestroy -> OnWindowDestroyC
wrapOnWindowDestroy f a = wrapCallbackResult <$> f (fromCUniqueId a)

onWindowDestroy :: HasCallStack => OnWindowDestroy
onWindowDestroy uniqueId = do
    putStrLn $ "DESTROY: " <> show uniqueId

    pure Processed

createHaskellWndProcCallbacks :: IO HaskellWndProcCallbacks
createHaskellWndProcCallbacks = do
    onWindowDestroyCallback' <- makeOnWindowDestroy (wrapOnWindowDestroy onWindowDestroy)

    pure $ HaskellWndProcCallbacks
        { onWindowDestroyCallback = onWindowDestroyCallback'
        }

freeHaskellWndProcCallbacks :: HaskellWndProcCallbacks -> IO ()
freeHaskellWndProcCallbacks callbacks =
    freeHaskellFunPtr (onWindowDestroyCallback callbacks)

wrapCallbackResult :: CallbackResult -> CInt
wrapCallbackResult Processed = 0
wrapCallbackResult Ignored   = 1
