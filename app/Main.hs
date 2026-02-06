{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens               (makeLenses, over, (^.))
import           Control.Monad              (when)
import           Data.Text                  (append)
import qualified Data.Text                  as Text
import           Framework.TEA              (GUIComponents, IsModel, IsMsg,
                                             runTEA)
import           Graphics.GUI.DSL
import           Prelude                    hiding (init)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)

data Model = Model
    { _displayWidth  :: Int
    , _displayHeight :: Int
    , _clickedCount  :: Int
    } deriving Show

makeLenses ''Model

data Msg = ButtonClicked deriving (Show, Eq)

instance IsModel Model
instance IsMsg Msg

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

view :: Model -> GUIComponents
view model = do
    when (even (model ^. clickedCount)) $
        window "TEAWin32GUI-SubSubSub" "TEAWin32GUI-SubSubSub" Normal $ do
            windowTitle "Count is even!"
            windowIcon Exclamation
            windowSize (400, 10)
            windowPosition (0, 0)

    window "TEAWin32GUI-Main" ("TEAWin32GUI-Main" <> Text.show (model ^. clickedCount)) Normal $ do
        windowTitle "TEAWin32GUI"
        windowIcon Application
        windowCursor IBeam
        windowSize (model ^. displayWidth, model ^. displayHeight)
        windowPosition (0, 0)
        windowBrush (SolidBrush 255 255 255)
        windowChildren $ do
            button "TestButton" $ do
                buttonLabel "TEST BUTTON"
                buttonSize (100, 50)
                buttonPosition (0, 0)
                buttonClicked ButtonClicked

            window "TEAWin32GUI-Sub" "TEAWin32GUI-Sub" NormalChild $ do
                windowTitle "HELLO"
                windowIcon Exclamation
                windowCursor Arrow
                windowSize (model ^. displayWidth `div` 2, model ^. displayHeight `div` 2)
                windowPosition (100, 100)
                windowBrush (SolidBrush 255 0 0)
                windowChildren $ do
                    button "TestButton2" $ do
                        buttonLabel ("Clicked: " `append` Text.show (model ^. clickedCount))
                        buttonSize (100, 100)
                        buttonPosition (20, 50)

                    button "TestButton3" $ do
                        buttonLabel "!?"
                        buttonSize (50, 50)
                        buttonPosition (100, 100)

                    window "TEAWin32GUI-Sub-Sub" "TEAWin32GUI-Sub-Sub" BorderlessChild $ do
                        windowTitle "GOOD MORNING"
                        windowIcon Application
                        windowCursor Wait
                        windowSize (50, 50)
                        windowPosition (0, 0)
                        windowBrush (SolidBrush 0 255 0)

main :: IO ()
main = runTEA init update view
