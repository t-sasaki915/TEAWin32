module TEAWin32.GUI.Component.Internal
    ( ComponentUpdateAction (..)
    , restoreComponentFromHWND
    , compareGUIComponents
    , sortComponentsWithZIndex
    , bringComponentToTop
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
                           | NoComponentChange GUIComponent

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> [ComponentUpdateAction]

sortComponentsWithZIndex :: [GUIComponent] -> Maybe Win32.HWND -> IO [GUIComponent]

bringComponentToTop :: Win32.HWND -> IO ()

setComponentTitle :: Text -> Win32.HWND -> IO ()

setComponentPosition :: Int -> Int -> Win32.HWND -> IO ()

setComponentSize :: Int -> Int -> Win32.HWND -> IO ()

setComponentFont :: Font -> Win32.HWND -> IO ()

useDefaultFont :: Win32.HWND -> IO ()
