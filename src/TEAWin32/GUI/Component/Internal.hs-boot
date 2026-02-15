module TEAWin32.GUI.Component.Internal
    ( ComponentUpdateAction (..)
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

data ComponentUpdateAction = RenderComponent GUIComponent
                           | UpdateProperties GUIComponent GUIComponent
                           | RedrawComponent GUIComponent
                           | DeleteComponent GUIComponent
                           | NoComponentChange

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> [ComponentUpdateAction]

setComponentTitle :: Text -> Win32.HWND -> IO ()

setComponentPosition :: Int -> Int -> Win32.HWND -> IO ()

setComponentSize :: Int -> Int -> Win32.HWND -> IO ()

setComponentFont :: Font -> Win32.HWND -> IO ()

useDefaultFont :: Win32.HWND -> IO ()
