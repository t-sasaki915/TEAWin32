module Graphics.GUI.Component.Internal
    ( restoreComponentFromHWND
    , compareGUIComponents
    , setComponentTitle
    , setComponentPosition
    , setComponentSize
    , setComponentFont
    , useDefaultFont
    ) where

import           Data.Text              (Text)
import           Graphics.GUI           (Font)
import           Graphics.GUI.Component (GUIComponent)
import qualified Graphics.Win32         as Win32

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [(GUIComponent, GUIComponent)])

setComponentTitle :: Text -> Win32.HWND -> IO ()

setComponentPosition :: Int -> Int -> Win32.HWND -> IO ()

setComponentSize :: Int -> Int -> Win32.HWND -> IO ()

setComponentFont :: Font -> Win32.HWND -> IO ()

useDefaultFont :: Win32.HWND -> IO ()
