module Main (main) where

import           Control.Lens               (over, (^.))
import           Control.Monad              (when)
import qualified Data.Text                  as Text
import           Model
import           Prelude                    hiding (init)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)
import           TEAWin32.Application       (Settings (..), runTEA)
import           TEAWin32.GUI.DSL


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
    let isCountEven = even (model ^. clickedCount)

    when isCountEven $
        window_ "TEAWin32GUI-SubSubSub" "TEAWin32GUI-SubSubSub" Normal
            [title_ "Count is even!", icon_ Exclamation, size_ (400, 10), position_ (0, 0), backgroundColour_ (RGB 0 0 255)] noChildren

    window_ "TEAWin32GUI-Main" "TEAWin32GUI-Main" Normal
        ([ title_ ("TEAWin32GUI - Click Count: " <> Text.show (model ^. clickedCount))
        , icon_ Application
        , size_ (model ^. displayWidth, model ^. displayHeight)
        --, position_ (0, 0)
        , backgroundColour_ (if isCountEven then RGB 255 255 255 else RGB 100 100 100)
        ] ++ [cursor_ IBeam | isCountEven]) $ do
            button_ "TestButton" [title_ "TEST BUTTON", size_ (100, 50), position_ (0, 0), onClick_ ButtonClicked]

            button_ "TestButton4" [title_ ("Click Count 1: " <> Text.show (model ^. clickedCount)), size_ (150, 150), position_ (150, 100)]

            window_ "TEAWin32GUI-Sub" "TEAWin32GUI-Sub" NormalChild
                [ title_ "HELLO"
                , icon_ Exclamation
                , cursor_ Arrow
                , size_ (model ^. displayWidth `div` 2, model ^. displayHeight `div` 2)
                , position_ (100, 100)
                , backgroundColour_ (RGB 255 0 0)
                ] $ do
                    button_ "TestButton2" [title_ ("Click Count 2: " <> Text.show (model ^. clickedCount)), size_ (150, 100), position_ (20, 50)]

                    button_ "TestButton3" [title_ "!?", size_ (50, 50), position_ (100, 150), onClick_ ButtonClicked2, font_ SystemFont]

                    button_ "TestButton5"
                        [title_ "おはようございます", size_ (200, 50), position_ (300, 0), font_ (if isCountEven then Font "Meiryo" 24 else SystemFont)]

                    window_ "TEAWin32GUI-Sub-Sub" "TEAWin32GUI-Sub-Sub" BorderlessChild
                        [title_ "GOOD MORNING", icon_ Application, cursor_ Wait, size_ (50, 50), position_ (0, 0), backgroundColour_ (RGB 0 255 0)] noChildren

main :: IO ()
main =
    let settings = Settings
            { useVisualStyles = True
            }
        in runTEA settings init update view
