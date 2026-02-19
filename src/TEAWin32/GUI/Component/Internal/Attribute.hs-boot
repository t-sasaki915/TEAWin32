module TEAWin32.GUI.Component.Internal.Attribute
    ( ComponentType (..)
    , EventType (..)
    , ComponentFlagKey (..)
    , ComponentAttribute (..)
    , addAttributeToHWND
    , updateAttributeOfHWND
    , removeAttributeFromHWND
    , isManagedByTEAWin32
    ) where

import                          Data.Text                     (Text)
import                qualified Graphics.Win32                as Win32
import {-# SOURCE #-} qualified TEAWin32.Application.Internal as ApplicationInternal
import                          TEAWin32.Drawing              (Colour)
import                          TEAWin32.GUI                  (Cursor, Font,
                                                               Icon,
                                                               ScalableValue,
                                                               UniqueId,
                                                               WindowStyle)

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
                        | ComponentCurrentDPIAttr       Int
                        | ComponentFlagAttr             ComponentFlagKey
                        | ComponentEventHandlerAttr     EventType ApplicationInternal.Msg
                        | ComponentBackgroundColourAttr Colour
                        | ComponentFontAttr             Font
                        | ComponentTitleAttr            Text
                        | ComponentSizeAttr             (ScalableValue, ScalableValue)
                        | ComponentPositionAttr         (ScalableValue, ScalableValue)
                        | WindowClassNameAttr           Text
                        | WindowStyleAttr               WindowStyle
                        | WindowCursorAttr              Cursor
                        | WindowIconAttr                Icon

addAttributeToHWND :: Win32.HWND -> ComponentAttribute -> IO ()

updateAttributeOfHWND :: Win32.HWND -> ComponentAttribute -> IO ()

removeAttributeFromHWND :: Win32.HWND -> ComponentAttribute -> IO ()

isManagedByTEAWin32 :: Win32.HWND -> IO Bool
