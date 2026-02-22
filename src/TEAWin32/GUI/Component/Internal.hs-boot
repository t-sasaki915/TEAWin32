module TEAWin32.GUI.Component.Internal
    ( sortComponentsWithZIndex
    , resolveScalableValueForHWND
    , setComponentTitle
    , setComponentFont
    , useDefaultFont
    , destroyChildren
    , destroyComponent
    ) where

import           Data.Text              (Text)
import           GHC.Stack              (HasCallStack)
import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI           (Font, ScalableValue)
import           TEAWin32.GUI.Component (GUIComponent)

sortComponentsWithZIndex :: HasCallStack => [GUIComponent] -> Maybe Win32.HWND -> IO [GUIComponent]

resolveScalableValueForHWND :: HasCallStack => Win32.HWND -> ScalableValue -> IO Int

setComponentTitle :: Text -> Win32.HWND -> IO ()

setComponentFont :: HasCallStack => Font -> Win32.HWND -> IO ()

useDefaultFont :: HasCallStack => Win32.HWND -> IO ()

destroyChildren :: Win32.HWND -> IO ()

destroyComponent :: Win32.HWND -> IO ()
