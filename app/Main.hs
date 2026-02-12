{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens               (makeLenses, over, (^.))
import           Control.Monad              (when)
import qualified Data.Text                  as Text
import           Framework.TEA              (GUIComponents, runTEA)
import           Graphics.GUI.DSL
import           Prelude                    hiding (init)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)

{-
HANDLE CreateAndActivateContext(ULONG_PTR* pul)
{
    HANDLE hActCtx = NULL;
    ACTCTX act = { 0 };
    TCHAR szPath[MAX_PATH];
    HINSTANCE hInstance = LoadLibrary(L"SHLWAPI.DLL");
    GetModuleFileName(hInstance, szPath, ARRAYSIZE(szPath));
    act.cbSize = sizeof(act);
    act.dwFlags = ACTCTX_FLAG_RESOURCE_NAME_VALID;
    act.lpResourceName = MAKEINTRESOURCE(123);
    act.lpSource = szPath;
    hActCtx = CreateActCtx(&act);
    if (hActCtx != INVALID_HANDLE_VALUE)
        ActivateActCtx(hActCtx, pul);
    return hActCtx;
}

ULONG_PTR ul;
HANDLE hActCtx = CreateAndActivateContext(&ul);
-}

data Model = Model
    { _displayWidth  :: Int
    , _displayHeight :: Int
    , _clickedCount  :: Int
    } deriving Show

makeLenses ''Model

data Msg = ButtonClicked
         | ButtonClicked2
         deriving (Show, Eq)


init :: IO Model
init = do
    displayWidth'  <- getSystemMetrics sM_CXSCREEN
    displayHeight' <- getSystemMetrics sM_CYSCREEN

    pure $ Model
        { _displayWidth  = displayWidth'
        , _displayHeight = displayHeight'
        , _clickedCount  = 0
        }

update :: Msg -> Model -> IO Model
update ButtonClicked model =
    print model >>
        pure (over clickedCount (+1) model)
update ButtonClicked2 model =
    print model >>
        pure (over clickedCount (+2) model)

view :: Model -> GUIComponents
view model = do
    when (even (model ^. clickedCount)) $
        window "TEAWin32GUI-SubSubSub" "TEAWin32GUI-SubSubSub" Normal $ do
            windowTitle "Count is even!"
            windowIcon Exclamation
            windowSize (400, 10)
            windowPosition (0, 0)
            windowBackgroundColour (RGB 0 0 255)

    window "TEAWin32GUI-Main" "TEAWin32GUI-Main" Normal $ do
        windowTitle ("TEAWin32GUI - Click Count: " <> Text.show (model ^. clickedCount))
        windowIcon Application
        when (even (model ^. clickedCount)) $
            windowCursor IBeam
        windowSize (model ^. displayWidth, model ^. displayHeight)
        --windowPosition (0, 0)
        if even (model ^. clickedCount)
            then windowBackgroundColour (RGB 255 255 255)
            else windowBackgroundColour (RGB 100 100 100)
        windowChildren $ do
            button "TestButton" $ do
                buttonLabel "TEST BUTTON"
                buttonSize (100, 50)
                buttonPosition (0, 0)
                buttonClicked ButtonClicked

            button "TestButton4" $ do
                        buttonLabel ("Click Count 1: " <> Text.show (model ^. clickedCount))
                        buttonSize (150, 100)
                        buttonPosition (150, 100)

            window "TEAWin32GUI-Sub" "TEAWin32GUI-Sub" NormalChild $ do
                windowTitle "HELLO"
                windowIcon Exclamation
                windowCursor Arrow
                windowSize (model ^. displayWidth `div` 2, model ^. displayHeight `div` 2)
                windowPosition (100, 100)
                windowBackgroundColour (RGB 255 0 0)
                windowChildren $ do
                    button "TestButton2" $ do
                        buttonLabel ("Click Count 2: " <> Text.show (model ^. clickedCount))
                        buttonSize (150, 100)
                        buttonPosition (20, 50)

                    button "TestButton3" $ do
                        buttonLabel "!?"
                        buttonSize (50, 50)
                        buttonPosition (100, 150)
                        buttonClicked ButtonClicked2

                    window "TEAWin32GUI-Sub-Sub" "TEAWin32GUI-Sub-Sub" BorderlessChild $ do
                        windowTitle "GOOD MORNING"
                        windowIcon Application
                        windowCursor Wait
                        windowSize (50, 50)
                        windowPosition (0, 0)
                        windowBackgroundColour (RGB 0 255 0)

main :: IO ()
main = runTEA init update view
