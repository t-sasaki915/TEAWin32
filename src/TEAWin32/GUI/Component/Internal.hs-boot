module TEAWin32.GUI.Component.Internal
    ( UpdateAction (..)
    , restoreComponentFromHWND
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

data UpdateAction = Render GUIComponent
                  | UpdateProperties GUIComponent GUIComponent
                  | Redraw GUIComponent
                  | Delete GUIComponent
                  | NoChange

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> [UpdateAction]

setComponentTitle :: Text -> Win32.HWND -> IO ()

setComponentPosition :: Int -> Int -> Win32.HWND -> IO ()

setComponentSize :: Int -> Int -> Win32.HWND -> IO ()

setComponentFont :: Font -> Win32.HWND -> IO ()

useDefaultFont :: Win32.HWND -> IO ()
