{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.Application.Internal
    ( Model (..)
    , Msg (..)
    , SetWindowPosSchedule (..)
    , modelRef
    , updateFuncRef
    , viewFuncRef
    , isUpdateProgressing
    , scheduleSetWindowPos
    , flushWindowPosScheduleList
    , issueMsg
    , updateComponents
    ) where

import           Control.Applicative                      ((<|>))
import           Control.Concurrent                       (MVar, modifyMVar,
                                                           newMVar)
import           Control.Monad                            (foldM, unless, void,
                                                           when)
import           Control.Monad.State.Strict               (evalState)
import           Control.Monad.Writer.Strict              (execWriterT)
import           Data.Bits                                ((.|.))
import           Data.Data                                (Typeable, cast)
import           Data.Functor                             ((<&>))
import           Data.IntMap.Strict                       (IntMap)
import qualified Data.IntMap.Strict                       as IntMap
import           Data.IORef                               (IORef,
                                                           atomicModifyIORef',
                                                           newIORef, readIORef)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (fromMaybe, isJust,
                                                           isNothing)
import           GHC.IO                                   (bracket,
                                                           unsafePerformIO)
import           GHC.Stack                                (HasCallStack)
import qualified Graphics.Win32                           as Win32
import           TEAWin32.Exception                       (TEAWin32Error (..),
                                                           errorTEAWin32)
import           TEAWin32.GUI.Component                   (DSLState (..),
                                                           GUIComponent,
                                                           GUIComponents,
                                                           IsGUIComponent (..))
import           TEAWin32.GUI.Component.ComponentRegistry
import qualified TEAWin32.GUI.Component.Internal          as ComponentInternal
import qualified TEAWin32.GUI.Internal                    as GUIInternal
import           TEAWin32.Util                            (hwndToInt, intToHWND)

data Model = forall a. Typeable a => Model a
data Msg = forall a. (Typeable a, Eq a, Show a) => Msg a

instance Show Msg where
    show (Msg a) = show a

instance Eq Msg where
    (Msg a) == (Msg b) =
        case cast b of
            Just b' -> a == b'
            Nothing -> False

modelRef :: IORef Model
modelRef = unsafePerformIO (newIORef (errorTEAWin32 (InternalTEAWin32Error "TEA is not initialised.")))
{-# NOINLINE modelRef #-}

updateFuncRef :: IORef (Msg -> Model -> IO Model)
updateFuncRef = unsafePerformIO (newIORef (errorTEAWin32 (InternalTEAWin32Error "TEA is not initialised.")))
{-# NOINLINE updateFuncRef #-}

viewFuncRef :: IORef (Model -> GUIComponents)
viewFuncRef = unsafePerformIO (newIORef (errorTEAWin32 (InternalTEAWin32Error "TEA is not initialised.")))
{-# NOINLINE viewFuncRef #-}

isUpdateProgressingRef :: IORef Bool
isUpdateProgressingRef = unsafePerformIO (newIORef False)
{-# NOINLINE isUpdateProgressingRef #-}

isUpdateProgressing :: IO Bool
isUpdateProgressing = readIORef isUpdateProgressingRef

setWindowPosActionMapRef :: MVar (IntMap SetWindowPosAction)
setWindowPosActionMapRef = unsafePerformIO (newMVar IntMap.empty)
{-# NOINLINE setWindowPosActionMapRef #-}

data SetWindowPosAction = SetWindowPosAction
    { setWindowLoc       :: Maybe (Int, Int)
    , setWindowSize      :: Maybe (Int, Int)
    , bringWindowToFront :: Bool
    }
    deriving Show

data SetWindowPosSchedule = SetWindowLocation Int Int
                          | SetWindowSize Int Int
                          | BringWindowToFront

fromSetWindowPosSchedule :: SetWindowPosSchedule -> SetWindowPosAction
fromSetWindowPosSchedule (SetWindowLocation x y) =
    SetWindowPosAction { setWindowLoc = Just (x, y), setWindowSize = Nothing, bringWindowToFront = False }
fromSetWindowPosSchedule (SetWindowSize width height) =
    SetWindowPosAction { setWindowLoc = Nothing, setWindowSize = Just (width, height), bringWindowToFront = False }
fromSetWindowPosSchedule BringWindowToFront =
    SetWindowPosAction { setWindowLoc = Nothing, setWindowSize = Nothing, bringWindowToFront = True }

combineSetWindowPosAction :: SetWindowPosAction -> SetWindowPosAction -> SetWindowPosAction
combineSetWindowPosAction newAction oldAction = SetWindowPosAction
    { setWindowLoc       = setWindowLoc newAction <|> setWindowLoc oldAction
    , setWindowSize      = setWindowSize newAction <|> setWindowSize oldAction
    , bringWindowToFront = bringWindowToFront newAction || bringWindowToFront oldAction
    }

scheduleSetWindowPos :: SetWindowPosSchedule -> Win32.HWND -> IO ()
scheduleSetWindowPos newAct hwnd =
    let hwnd' = hwndToInt hwnd in
        modifyMVar setWindowPosActionMapRef $ \actionMap ->
            case IntMap.lookup hwnd' actionMap of
                Just currentAct ->
                    pure (IntMap.insert hwnd' (combineSetWindowPosAction (fromSetWindowPosSchedule newAct) currentAct) actionMap, ())

                Nothing ->
                    pure (IntMap.insert hwnd' (fromSetWindowPosSchedule newAct) actionMap, ())

applySetWindowPosSchedule :: Win32.HDWP -> (Int, SetWindowPosAction) -> IO Win32.HDWP
applySetWindowPosSchedule hdwp (hwnd, action) = do
    let flags = (if isJust (setWindowLoc action)  then 0                    else Win32.sWP_NOMOVE)
            .|. (if isJust (setWindowSize action) then 0                    else Win32.sWP_NOSIZE)
            .|. (if bringWindowToFront action     then Win32.sWP_SHOWWINDOW else Win32.sWP_NOZORDER)
            .|. Win32.sWP_NOACTIVATE

        (x, y)          = fromMaybe (0, 0) (setWindowLoc action)
        (width, height) = fromMaybe (0, 0) (setWindowSize action)

    Win32.c_DeferWindowPos hdwp (intToHWND hwnd) Win32.nullPtr x y width height flags

flushWindowPosScheduleList :: IO ()
flushWindowPosScheduleList = do
    setWindowPosActionMap <- modifyMVar setWindowPosActionMapRef $ \scheduleList ->
        pure (IntMap.empty, reverse $ IntMap.toList scheduleList)

    print setWindowPosActionMap

    unless (null setWindowPosActionMap) $
        void $ bracket
            (Win32.beginDeferWindowPos (length setWindowPosActionMap))
            (\hdwp -> unless (hdwp == Win32.nullPtr) $ Win32.endDeferWindowPos hdwp)
            (\hdwp -> foldM applySetWindowPosSchedule hdwp setWindowPosActionMap)

issueMsg :: HasCallStack => Msg -> IO ()
issueMsg msg = do
    atomicModifyIORef' isUpdateProgressingRef (const (True, ()))

    updateFunc   <- readIORef updateFuncRef
    currentModel <- readIORef modelRef
    newModel     <- updateFunc msg currentModel
    atomicModifyIORef' modelRef (const (newModel, ()))

    viewFunc <- readIORef viewFuncRef
    let newGUIComponents = evalState (execWriterT $ viewFunc newModel) (DSLState 0 [])

    updateComponents newGUIComponents Nothing

    flushWindowPosScheduleList

    atomicModifyIORef' isUpdateProgressingRef (const (False, ()))

updateComponents :: HasCallStack => [GUIComponent] -> Maybe Win32.HWND -> IO ()
updateComponents newGUIComponents maybeParent = do
    childrenWithUniqueId <- Map.fromList <$>
        case maybeParent of
            Just parent ->
                GUIInternal.withImmediateChildWindows parent $ mapM $ \child ->
                    getComponentRegistryEntryValue ComponentUniqueIdRegKey child >>= \uniqueId ->
                        pure (uniqueId, child)

            Nothing ->
                GUIInternal.withTopLevelWindows $ mapM $ \child ->
                    getComponentRegistryEntryValue ComponentUniqueIdRegKey child >>= \uniqueId ->
                        pure (uniqueId, child)

    let newGUIComponentsWithUniqueId = Map.fromList [ (getUniqueId comp, comp) | comp <- newGUIComponents ]

    _ <- flip Map.traverseWithKey childrenWithUniqueId $ \uniqueId child ->
        when (isNothing (Map.lookup uniqueId newGUIComponentsWithUniqueId)) $ do
            putStrLn $ "DELETE " <> show uniqueId
            ComponentInternal.destroyComponent child

    _ <- flip Map.traverseWithKey newGUIComponentsWithUniqueId $ \uniqueId newGUIComponent ->
        case Map.lookup uniqueId childrenWithUniqueId of
            Just currentHWND ->
                getComponentRegistryEntryValue ComponentTypeRegKey currentHWND <&> (== getComponentType newGUIComponent) >>= \case
                    True ->
                        putStrLn $ "UPDATE " <> show uniqueId

                    False -> do
                        putStrLn $ "RERENDER " <> show uniqueId
                        ComponentInternal.destroyComponent currentHWND >>
                            void (render newGUIComponent maybeParent)

            Nothing -> do
                putStrLn $ "RENDER " <> show uniqueId
                void (render newGUIComponent maybeParent)

    pure ()

