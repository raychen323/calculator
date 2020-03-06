-- Referenced https://www.stackbuilders.com/tutorials/haskell/gui-application/ for button creation, basic setup, and signals
-- Referenced https://hackage.haskell.org/package/gtk3-0.14.6 for everything else

-- commenting is really extensive as many calls are to library functions so we over-explain a bit

import Control.Monad
import Control.Monad.IO.Class
import Graphics.UI.Gtk hiding (Action, backspace)
import Laws
import Presentable
import Simplify
import Parse
import Text.Megaparsec
import Text.Megaparsec.Char()
import DataTypes

-- Same function as in main for running our solver
-- Copy pasted here so our command line program is decoupled from our GUI
-- Applies calculation to our parser result
solve :: String -> Calculation
solve eq = calc where
  calc = case (parse parseDerive "" eq) of
        Left _ -> (Calc (Var "error, parsing failed") [])
        Right (Derive var output) -> calculate (laws var) (UnOp "derive" output)

-- function for making our equals button
mkBtn :: String -> Entry -> TextView -> Entry -> IO Button
mkBtn btnLabel entry steps solView = do
  -- create new button and set it's label
  btn <- buttonNew
  set btn [ buttonLabel := btnLabel ]
  -- when we click the button run the following
  _ <- btn `on` buttonActivated $ do
    -- get the user input
    input <- entryGetText entry
    -- plug it into our calculator
    let soln = solve input
    -- display the steps
    editTextDisplay steps (finalOutput(pretty(soln)))
    -- display the final answer
    entrySetText solView (finalSolution soln)
    -- reset the entry field to empty string
    entrySetText entry ""
  -- return the button we made
  return btn

-- function for building our text display for showing our steps
createTextDisplay :: String -> IO TextView
createTextDisplay text = do
  -- make a new textview
  textView <- textViewNew
  -- make a new buffer
  buffer <- textBufferNew Nothing
  -- create our iterator within our buffer
  textIter <- textBufferGetIterAtOffset buffer (-1)
  -- write to our buffer
  textBufferInsert buffer textIter text
  -- set display to show our buffer
  set textView [ textViewEditable := False
               , textViewBuffer   := buffer ]
  return textView

-- function for editing our text display for showing our steps
editTextDisplay :: TextView -> String -> IO()
editTextDisplay textView text = do
  -- create our buffer
  buffer <- textBufferNew Nothing
  -- create our iterator within our buffer
  textIter <- textBufferGetIterAtOffset buffer (-1)
  -- write to our buffer
  textBufferInsert buffer textIter text
  -- set display to show our buffer
  set textView [ textViewBuffer := buffer ]

main :: IO ()
main = do
  -- setup
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Calculator"
             , windowDefaultWidth  := 400
             , windowResizable     := True
             , windowDefaultHeight := 600 ]

  -- create layout
  grid <- gridNew
  gridSetRowHomogeneous grid True
  gridSetColumnHomogeneous grid True

  -- create display for solution
  solutionDisp <- entryNew
  set solutionDisp [ entryEditable := False]

  -- create display for the steps the solver takes
  steps <- createTextDisplay "Steps will show here when you input an equation"

  scroll <- scrolledWindowNew Nothing Nothing
  containerAdd scroll steps

  -- create entry for user input
  entry <- entryNew
  set entry [ entryEditable   := True
            , entryPlaceholderText := (Just "Input")]

  -- bind our displays to our grid layout
  let attach  x   y   w   h   item = gridAttach grid item x y w h
  attach      0   0   6   1   solutionDisp
  attach      0   1   6   19  scroll
  attach      0   20  5   1   entry
  mkBtn "=" entry steps solutionDisp  >>= attach 5 20 1 1
  -- bind our grid to our display and display all of our widgets
  containerAdd window grid
  widgetShowAll window
  -- exits program when window is closed
  _ <- window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  mainGUI