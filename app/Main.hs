module Main (main) where

import           Control.Monad              (when)
import qualified Data.Text                  as Text
import           Prelude                    hiding (init)
import           System.Win32               (sM_CXSCREEN, sM_CYSCREEN)
import           System.Win32.Info.Computer (getSystemMetrics)
import           TEAWin32

data Model = Model
    { displayWidth  :: Int
    , displayHeight :: Int
    , clickedCount  :: Int
    } deriving Show

data Msg = ButtonClicked
         | ButtonClicked2
         deriving (Show, Eq)

init :: IO Model
init = do
    displayWidth'  <- getSystemMetrics sM_CXSCREEN
    displayHeight' <- getSystemMetrics sM_CYSCREEN

    pure $ Model
        { displayWidth  = displayWidth'
        , displayHeight = displayHeight'
        , clickedCount  = 0
        }

update :: Msg -> Model -> IO Model
update ButtonClicked model =
    print model >>
        pure (model { clickedCount = clickedCount model + 1})

update ButtonClicked2 model = do
    {-msgBoxResult <- showMessageBox defaultMessageBoxSettings
            { messageBoxTitle         = "TEAWin32"
            , messageBoxContent       = "!?!?!?"
            , messageBoxButtons       = MessageBoxButtonsYesNo
            , messageBoxIcon          = MessageBoxIconError
            , messageBoxDefaultButton = Just MessageBoxButton2
            }

    case msgBoxResult of
        MessageBoxResultYes ->
            pure (over clickedCount (+100) model)

        MessageBoxResultNo ->
            error "NO"

        _ ->
            error "!?"-}
    pure model

view :: Model -> View
view model = do
    let isCountEven = even (clickedCount model)

    when isCountEven $
        window_ "TEAWin32-SubSubSub" WindowStyleNormal
            (do { title_ "Count is even!"; icon_ IconFolder; size_ (400, 10); pos_ (0, 0); bgColour_ (RGB 0 0 255) }) noChildren

    window_' "TEAWin32-SubSubSubSubSub" "TEAWin32-SubSubSubSubSub" WindowStyleNormal
        (do { title_ "!?"; icon_ IconDriveRAM; size_ (400, 200); pos_ (200, 0) }) $
            button_ (do { title_ "!?!?"; size_ (100, 50); pos_ (0, 0); onClick_ ButtonClicked })

    window_' "TEAWin32-Main" "TEAWin32-Main" WindowStyleNormal
        ( do
            title_ ("TEAWin32 - Click Count: " <> Text.show (clickedCount model))
            icon_ IconImageFiles
            size_ (fromIntegral (displayWidth model), fromIntegral (displayHeight model))
            bgColour_ (if isCountEven then RGB 255 255 255 else RGB 100 100 100)
            when isCountEven $
                cursor_ CursorIBeam
        ) $ do
            button_ (do { title_ "TEST BUTTON"; size_ (100, 50); pos_ (0, 0); onClick_ ButtonClicked })

            window_ "TEAWin32-Sub" WindowStyleNormalChild
                ( do
                    title_ "HELLO"
                    icon_ IconError
                    cursor_ CursorArrow
                    size_ (fromIntegral (displayWidth model `div` 2), fromIntegral (displayHeight model `div` 2))
                    pos_ (100, 100)
                    bgColour_ (RGB 255 0 0)
                ) $ do
                    button_ (do { title_ ("Click Count 2: " <> Text.show (clickedCount model)); size_ (150, 100); pos_ (20, 50) })

                    button_ (do { title_ "!?"; size_ (50, 50); pos_ (100, 150); onClick_ ButtonClicked2{-; font_ SystemFont-} })

                    button_
                        (do { title_ "おはようございます"; size_ (200, 50); pos_ (300, 0); font_ (if isCountEven then Font "Meiryo" 24 else Font "Meiryo" 9) })

                    window_ "TEAWin32-Sub-Sub" WindowStyleBorderlessChild
                        (do { title_ "GOOD MORNING"; cursor_ CursorWait; size_ (50, 50); pos_ (0, 0); bgColour_ (RGB 0 255 0) }) noChildren

            window_ "TEAWin32-SubSubSubSub" WindowStyleNormalChild
                ( do
                    title_ "HELLO 2"
                    icon_ IconFolder
                    cursor_ CursorSizeNWSE
                    size_ (fromIntegral (displayWidth model `div` 2), fromIntegral (displayHeight model `div` 2))
                    pos_ (200, 200)
                    bgColour_ (RGB 0 0 255)
                ) noChildren

            button_ (do { title_ ("Click Count 1: " <> Text.show (clickedCount model)); size_ (150, 150); pos_ (150, 100){-; zIndex_ (-1)-} })

main :: IO ()
main = runTEAWin32 defaultTEAWin32Settings init update view
