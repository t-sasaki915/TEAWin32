module TEAWin32.GUI.Component.Internal.Attribute
    ( EventType (..)
    , ComponentFlagKey (..)
    , ComponentAttribute (..)
    , addAttributeToHWND
    , updateAttributeOfHWND
    , removeAttributeFromHWND
    , isManagedByTEAWin32
    , isComponentWindow
    ) where

import                          Data.Text                     (Text)
import                          GHC.Stack                     (HasCallStack)
import                qualified Graphics.Win32                as Win32
import {-# SOURCE #-} qualified TEAWin32.Application.Internal as ApplicationInternal
import                          TEAWin32.Drawing              (Colour)
import                          TEAWin32.GUI                  (Cursor, Font,
                                                               Icon,
                                                               ScalableValue,
                                                               UniqueId,
                                                               WindowStyle)
import                          TEAWin32.GUI.Component        (ComponentType)

data EventType = ComponentClickEvent

data ComponentFlagKey = ComponentTitleSet
                      | ComponentSizeSet
                      | ComponentPositionSet
                      | ComponentFontSet
                      | ComponentOnClickSet
                      | ComponentChildrenSet
                      | ComponentBackgroundColourSet
                      | ComponentZIndexSet
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
                        | ComponentZIndexAttr           Int
                        | WindowClassNameAttr           Text
                        | WindowStyleAttr               WindowStyle
                        | WindowCursorAttr              Cursor
                        | WindowIconAttr                Icon

addAttributeToHWND :: HasCallStack => Win32.HWND -> ComponentAttribute -> IO ()

updateAttributeOfHWND :: HasCallStack => Win32.HWND -> ComponentAttribute -> IO ()

removeAttributeFromHWND :: HasCallStack => Win32.HWND -> ComponentAttribute -> IO ()

isManagedByTEAWin32 :: Win32.HWND -> IO Bool

isComponentWindow :: Win32.HWND -> IO Bool
