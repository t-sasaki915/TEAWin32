module TEAWin32.GUI.VirtualDOM
    ( CCallRequest (..)
    , UpdatePosReq (..)
    , CreateWindowReq (..)
    , CreateButtonReq (..)
    , CachedIconType (..)
    , fromHaskellCachedIconType
    ) where

import           Data.Text                 (Text)
import           Foreign.C                 (CDouble, CInt)
import qualified Graphics.Win32            as Win32
import           TEAWin32.GUI              (Cursor, Font, Icon)
import qualified TEAWin32.Internal.Foreign as Win32

data UpdatePosReq = UpdatePosReq
    { newLocation           :: Maybe (Int, Int)
    , newSize               :: Maybe (Int, Int)
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

data CachedIconType = ResourceIcon | StockIcon

fromHaskellCachedIconType :: CachedIconType -> CInt

data CCallRequest = CreateWindowRequest        CreateWindowReq
                  | CreateButtonRequest        CreateButtonReq
                  | DestroyComponentRequest    Win32.HWND
                  | UpdateTextRequest          Win32.HWND Text
                  | UpdatePosRequest           Win32.HWND UpdatePosReq
                  | UpdateFontRequest          Win32.HWND Font
                  | UpdateIconRequest          Win32.HWND Icon
                  | UpdateCursorRequest        Win32.HWND Cursor
                  | InvalidateRectFullyRequest Win32.HWND
                  | CreateWindowRequest'       Win32.HWND Win32.LPCWSTR Win32.DWORD Win32.DWORD
                  | CreateButtonRequest'       Win32.HWND
                  | UpdateTextRequest'         Win32.HWND Win32.LPCWSTR
                  | UpdatePosRequest'          Win32.HWND Win32.BOOL Win32.BOOL Win32.BOOL (Maybe CInt) (Maybe CInt) (Maybe CInt) (Maybe CInt)
                  | UpdateFontRequest'         Win32.HWND Win32.LPCWSTR CInt CDouble CInt
                  | UpdateIconRequest'         Win32.HWND CachedIconType CDouble (Maybe Win32.SHSTOCKICONID) (Maybe Win32.LPCWSTR)
                  | UpdateCursorRequest'       Win32.HWND Win32.LPCWSTR
