module Graphics.GUI.Component.Internal.Prop
    ( ManagedProp (..)
    , finalisePropValue
    , registerHWNDToPropMap
    , unregisterHWNDFromPropMap
    , setProp
    , getProp
    , removeProp
    , getProps
    , finaliseProps
    , finaliseAndUnregisterHWNDFromPropMap
    ) where

import           Control.Monad          (void, (>=>))
import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef)
import           Data.Map.Strict        (Map, (!))
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import           Foreign                (castPtr)
import qualified Framework.TEA.Internal as TEAInternal
import           GHC.IO                 (unsafePerformIO)
import           Graphics.Drawing       (Colour)
import           Graphics.GUI           (UniqueId)
import qualified Graphics.GUI.Foreign   as Win32
import qualified Graphics.Win32         as Win32

data ManagedProp = ComponentUniqueId     UniqueId
                 | ComponentType         Text
                 | ComponentFlag
                 | ComponentEventHandler TEAInternal.Msg
                 | ComponentGDIResource  Win32.HANDLE
                 | ComponentColour       Colour
                 deriving Show

finalisePropValue :: ManagedProp -> IO ()
finalisePropValue (ComponentUniqueId _)       = pure ()
finalisePropValue (ComponentType _)           = pure ()
finalisePropValue ComponentFlag               = pure ()
finalisePropValue (ComponentEventHandler _)   = pure ()
finalisePropValue (ComponentGDIResource hndl) = void $ Win32.c_DeleteObject (castPtr hndl)
finalisePropValue (ComponentColour _)         = pure ()

propMapRef :: IORef (Map Win32.HWND (Map Text ManagedProp))
propMapRef = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE propMapRef #-}

registerHWNDToPropMap :: Win32.HWND -> IO ()
registerHWNDToPropMap hwnd =
    atomicModifyIORef' propMapRef $ \propMap ->
        let newMap = Map.insert hwnd Map.empty propMap in
            (newMap, ())

unregisterHWNDFromPropMap :: Win32.HWND -> IO ()
unregisterHWNDFromPropMap hwnd =
    atomicModifyIORef' propMapRef $ \propMap ->
        let newMap = Map.delete hwnd propMap in
            (newMap, ())

setProp :: Win32.HWND -> Text -> ManagedProp -> IO ()
setProp hwnd propName propValue =
    atomicModifyIORef' propMapRef $ \propMap ->
        let newHWNDProps = Map.insert propName propValue (propMap ! hwnd)
            newMap = Map.insert hwnd newHWNDProps propMap in
                (newMap, ())

getProp :: Win32.HWND -> Text -> IO (Maybe ManagedProp)
getProp hwnd propName =
    Map.lookup propName <$> getProps hwnd

removeProp :: Win32.HWND -> Text -> IO ()
removeProp hwnd propName =
    atomicModifyIORef' propMapRef $ \propMap ->
        let newHWNDProps = Map.delete propName (propMap ! hwnd)
            newMap = Map.insert hwnd newHWNDProps propMap in
                (newMap, ())

getProps :: Win32.HWND -> IO (Map Text ManagedProp)
getProps hwnd =
    readIORef propMapRef >>= \propMap ->
        pure (propMap ! hwnd)

finaliseProps :: Win32.HWND -> IO ()
finaliseProps = getProps >=> mapM_ finalisePropValue

finaliseAndUnregisterHWNDFromPropMap :: Win32.HWND -> IO ()
finaliseAndUnregisterHWNDFromPropMap hwnd =
    finaliseProps hwnd >> unregisterHWNDFromPropMap hwnd
