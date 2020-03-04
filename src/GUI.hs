import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn

createTextDisplay text = do
  textView <- textViewNew
  buffer <- textBufferNew Nothing
  textIter <- textBufferGetIterAtOffset buffer (-1)
  textBufferInsert buffer textIter text
  set textView [ textViewEditable := False
               , textViewBuffer   := buffer
              ]
  return textView

editTextDisplay textView text = do
  buffer <- textBufferNew Nothing
  textIter <- textBufferGetIterAtOffset buffer (-1)
  textBufferInsert buffer textIter text
  set textView [ textViewBuffer := buffer ]

main :: IO ()
main = do
  -- setup
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Calculator"
             , windowDefaultWidth  := 400
             , windowResizable     := False
             , windowDefaultHeight := 500 ]

  -- create layout
  grid <- gridNew
  gridSetRowHomogeneous grid True
  gridSetColumnHomogeneous grid True

  -- create display for solution
  solution <- entryNew
  set solution [ entryEditable := False
              , entryXalign   := 1 -- makes contents right-aligned
              , entryText     := "0" ]

  -- create display for the steps the solver takes
  steps <- createTextDisplay "Steps will show here\n when you input an equation"
  -- create entry for user input
  entry <- entryNew
  set entry [ entryEditable   := True
            , entryText       := "Input"]

  -- bind our displays to our grid layout
  let attach x y w h item = gridAttach grid item x y w h
  attach 0 0 5 1 solution
  mkBtn "="   >>= attach 4 3 1 1
  attach 0 1 5 2 steps
  attach 0 3 5 1 entry
  
  -- bind our grid to our display and display all of our widgets
  containerAdd window grid
  -- exits program when window is closed
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  widgetShowAll window
  mainGUI