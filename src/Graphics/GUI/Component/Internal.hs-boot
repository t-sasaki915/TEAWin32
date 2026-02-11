module Graphics.GUI.Component.Internal
    ( restoreComponentFromHWND
    , compareGUIComponents
    ) where

import           Graphics.GUI.Component (GUIComponent)
import qualified Graphics.Win32         as Win32

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [(GUIComponent, GUIComponent)])
