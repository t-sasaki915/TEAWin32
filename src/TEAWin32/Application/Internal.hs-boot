{-# LANGUAGE ExistentialQuantification #-}

module TEAWin32.Application.Internal
    ( Msg (..)
    , SetWindowPosSchedule (..)
    , scheduleSetWindowPos
    , updateComponents
    ) where

import           Data.Data              (Typeable)
import           GHC.Stack              (HasCallStack)
import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI.Component (GUIComponent)

data Msg = forall a. (Typeable a, Eq a, Show a) => Msg a

instance Show Msg

data SetWindowPosSchedule = SetWindowLocation Int Int
                          | SetWindowSize Int Int
                          | BringWindowToFront

scheduleSetWindowPos :: SetWindowPosSchedule -> Win32.HWND -> IO ()

updateComponents :: HasCallStack => [GUIComponent] -> Maybe Win32.HWND -> IO ()
