module Graphics.GUI.Component.Internal
    ( restoreComponentFromHWND
    , compareGUIComponents
    , setFlag
    , unsetFlag
    , setEventHandler
    , unregisterEventHandler
    ) where

import                          Data.Text              (Text)
import {-# SOURCE #-} qualified Framework.TEA.Internal as TEAInternal
import                          Graphics.GUI.Component (GUIComponent)
import                qualified Graphics.Win32         as Win32

restoreComponentFromHWND :: Win32.HWND -> IO GUIComponent

compareGUIComponents :: [GUIComponent] -> [GUIComponent] -> ([GUIComponent], [GUIComponent], [GUIComponent], [(GUIComponent, GUIComponent)])

setFlag :: Text -> Win32.HWND -> IO ()

unsetFlag :: Text -> Win32.HWND -> IO ()

setEventHandler :: Text -> TEAInternal.Msg -> Win32.HWND -> IO ()

unregisterEventHandler :: Text -> Win32.HWND -> IO ()
