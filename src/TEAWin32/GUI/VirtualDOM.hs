module TEAWin32.GUI.VirtualDOM
    ( CCallRequest (..)
    , UpdatePosReq (..)
    , CreateWindowReq (..)
    , CreateButtonReq (..)
    ) where

import           Data.Text      (Text)
import qualified Graphics.Win32 as Win32
import           TEAWin32.GUI   (Cursor, Font, Icon)

data UpdatePosReq = UpdatePosReq
    { setLocation           :: Maybe (Int, Int)
    , setSize               :: Maybe (Int, Int)
    , bringComponentToFront :: Bool
    }

data CreateWindowReq = CreateWindowReq
    { newWindowClassName  :: Text
    , newWindowExStyles   :: Win32.DWORD
    , newWindowStyles     :: Win32.DWORD
    , newWindowParentHWND :: Maybe Win32.HWND
    }

newtype CreateButtonReq = CreateButtonReq
    { newButtonParentHWND :: Win32.HWND
    }

data CCallRequest = CreateWindowRequest CreateWindowReq
                  | CreateButtonRequest CreateButtonReq
                  | DestroyComponentRequest Win32.HWND
                  | UpdateTextRequest Win32.HWND Text
                  | UpdatePosRequest Win32.HWND UpdatePosReq
                  | UpdateFontRequest Win32.HWND Font
                  | UpdateIconRequest Win32.HWND Icon
                  | UpdateCursorRequest Win32.HWND Cursor
                  | InvalidateRectFullyRequest Win32.HWND

