module Graphics.GUI.Component.Internal.Attribute
    ( ComponentType (..)
    , EventType (..)
    , ComponentFlagKey (..)
    , ComponentAttribute (..)
    , addAttributeToHWND
    , updateAttributeOfHWND
    , removeAttributeFromHWND
    , isManagedByTEAWin32GUI
    ) where

import                          Data.Text              (Text)
import {-# SOURCE #-} qualified Framework.TEA.Internal as TEAInternal
import                          Graphics.Drawing       (Colour)
import                          Graphics.GUI           (Cursor, Font, Icon,
                                                        UniqueId, WindowStyle)
import                qualified Graphics.Win32         as Win32

data ComponentType = ComponentWindow
                   | ComponentButton

data EventType = ComponentClickEvent

data ComponentFlagKey = ComponentTitleSet
                      | ComponentSizeSet
                      | ComponentPositionSet
                      | ComponentFontSet
                      | ComponentOnClickSet
                      | ComponentChildrenSet
                      | ComponentBackgroundColourSet
                      | WindowIconSet
                      | WindowCursorSet
                      | WindowBackgroundColourSet

data ComponentAttribute = ComponentUniqueIdAttr         UniqueId
                        | ComponentTypeAttr             ComponentType
                        | ComponentFlagAttr             ComponentFlagKey
                        | ComponentEventHandlerAttr     EventType TEAInternal.Msg
                        | ComponentBackgroundColourAttr Colour
                        | ComponentFontAttr             Font
                        | ComponentTitleAttr            Text
                        | WindowClassNameAttr           Text
                        | WindowStyleAttr               WindowStyle
                        | WindowCursorAttr              Cursor
                        | WindowIconAttr                Icon

addAttributeToHWND :: Win32.HWND -> ComponentAttribute -> IO ()

updateAttributeOfHWND :: Win32.HWND -> ComponentAttribute -> IO ()

removeAttributeFromHWND :: Win32.HWND -> ComponentAttribute -> IO ()

isManagedByTEAWin32GUI :: Win32.HWND -> IO Bool
