module Main (main) where

import           Control.Lens               (over, (^.))
import           Control.Monad              (when)
import qualified Data.Text                  as Text
import           Model
import           Prelude                    hiding (init)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)
import           TEAWin32.Application       (Settings (..), runTEA)
import           TEAWin32.Effect.MessageBox
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

update ButtonClicked2 model = do
    msgBoxResult <- showMessageBox defaultMessageBoxSettings
            { messageBoxTitle         = "TEAWin32"
            , messageBoxContent       = "!?!?!?"
            , messageBoxButtons       = MessageBoxButtonsYesNo
            , messageBoxIcon          = MessageBoxIconError
            , messageBoxDefaultButton = Just MessageBoxButton2
            , messageBoxTaskModal     = True
            }

    case msgBoxResult of
        MessageBoxResultYes ->
            pure (over clickedCount (+100) model)

        MessageBoxResultNo ->
            pure model

        _ ->
            error "!?"

view :: Model -> GUIComponents
view model = do
    let isCountEven = even (model ^. clickedCount)

    when isCountEven $
        window_ "TEAWin32-SubSubSub" "TEAWin32-SubSubSub" WindowStyleNormal
            [title_ "Count is even!", icon_ IconExclamation, size_ (400, 10), position_ (0, 0), backgroundColour_ (RGB 0 0 255)] noChildren

    window_ "TEAWin32-SubSubSubSubSub" "TEAWin32-SubSubSubSubSub" WindowStyleNormal
        [title_ "!?", icon_ IconExclamation, size_ (400, 200), position_ (200, 0)] $
            button_ "TestButton6" [title_ "!?!?", size_ (100, 50), position_ (0, 0), onClick_ ButtonClicked]

    window_ "TEAWin32-Main" "TEAWin32-Main" WindowStyleNormal
        ([ title_ ("TEAWin32 - Click Count: " <> Text.show (model ^. clickedCount))
        , icon_ IconApplication
        , size_ (fromIntegral (model ^. displayWidth), fromIntegral (model ^. displayHeight))
        , backgroundColour_ (if isCountEven then RGB 255 255 255 else RGB 100 100 100)
        ] ++ [cursor_ CursorIBeam | isCountEven]) $ do
            button_ "TestButton" [title_ "TEST BUTTON", size_ (100, 50), position_ (0, 0), onClick_ ButtonClicked]

            window_ "TEAWin32-Sub" "TEAWin32-Sub" WindowStyleNormalChild
                [ title_ "HELLO"
                , icon_ IconExclamation
                , cursor_ CursorArrow
                , size_ (fromIntegral (model ^. displayWidth `div` 2), fromIntegral (model ^. displayHeight `div` 2))
                , position_ (100, 100)
                , backgroundColour_ (RGB 255 0 0)
                ] $ do
                    button_ "TestButton2" [title_ ("Click Count 2: " <> Text.show (model ^. clickedCount)), size_ (150, 100), position_ (20, 50)]

                    button_ "TestButton3" [title_ "!?", size_ (50, 50), position_ (100, 150), onClick_ ButtonClicked2, font_ SystemFont]

                    button_ "TestButton5"
                        [title_ "おはようございます", size_ (200, 50), position_ (300, 0), font_ (if isCountEven then Font "Meiryo" 24 else SystemFont)]

                    window_ "TEAWin32-Sub-Sub" "TEAWin32-Sub-Sub" WindowStyleBorderlessChild
                        [title_ "GOOD MORNING", icon_ IconApplication, cursor_ CursorWait, size_ (50, 50), position_ (0, 0), backgroundColour_ (RGB 0 255 0)] noChildren

            window_ "TEAWin32-SubSubSubSub" "TEAWin32-SubSubSubSub" WindowStyleNormalChild
                [ title_ "HELLO 2"
                , icon_ IconExclamation
                , cursor_ CursorSizeNWSE
                , size_ (fromIntegral (model ^. displayWidth `div` 2), fromIntegral (model ^. displayHeight `div` 2))
                , position_ (200, 200)
                , backgroundColour_ (RGB 0 0 255)
                ] noChildren

            button_ "TestButton4" [title_ ("Click Count 1: " <> Text.show (model ^. clickedCount)), size_ (150, 150), position_ (150, 100), zIndex_ (-1)]

main :: IO ()
main =
    let settings = Settings
            { useVisualStyles = True
            }
        in runTEA settings init update view
