# TEAWin32
Type-safe, Elm-inspired native Win32 GUI framework for Haskell

## Features
- Type-safe Lucid-like DSL
- The Elm Architecture
- No extra DLL requirements
- Compatible with Windows 7 - Windows 11
- Optimised VirtualDOM
- Automated DPI scaling
- Full `text` support
- Minimum dependencies

## Example
![Sample programme working on Windows 7](https://raw.githubusercontent.com/t-sasaki915/TEAWin32/refs/heads/main/demo.png)

```haskell
module Main (main) where

import qualified Data.Text as Text
import           Prelude   hiding (init)
import           TEAWin32

newtype Model = Model
    { clickCount  :: Int
    }

data Msg = ButtonClicked
         deriving (Show, Eq)

init :: IO Model
init = pure $ Model { clickCount = 0 }

update :: Msg -> Model -> IO Model
update ButtonClicked model =
    pure (model { clickCount = clickCount model + 1})

view :: Model -> View
view model =
    window_ "MainWindow" WindowStyleNormal
        ( do
            title_ ("TestApp ClickCount: " <> Text.show (clickCount model))
            size_ (400, 400)
            bgColour_ (RGB 0 0 255)
            icon_ IconInternet
        ) $
            button_ (do { title_ "CLICK"; size_ (150, 50); pos_ (100, 100); font_ (Font "Meiryo" 10); onClick_ ButtonClicked })

main :: IO ()
main = runTEAWin32 defaultTEAWin32Settings init update view

```
