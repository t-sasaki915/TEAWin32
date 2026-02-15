module TEAWin32.GUI.Component.Internal
    ( restoreComponentFromHWND
    , compareGUIComponents
    , setComponentTitle
    , setComponentPosition
    , setComponentSize
    , setComponentFont
    , useDefaultFont
    ) where

import           Data.Text              (Text)
import qualified Graphics.Win32         as Win32
import           TEAWin32.GUI           (Font)
import           TEAWin32.GUI.Component (GUIComponent)

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [(GUIComponent, GUIComponent)])

setComponentTitle :: Text -> Win32.HWND -> IO ()

setComponentPosition :: Int -> Int -> Win32.HWND -> IO ()

setComponentSize :: Int -> Int -> Win32.HWND -> IO ()

setComponentFont :: Font -> Win32.HWND -> IO ()

useDefaultFont :: Win32.HWND -> IO ()
