module ClientEditor where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk

startEditor :: IO ()
startEditor = do
  initGUI
  window <- windowNew
  set window [windowTitle := "KNote", containerBorderWidth := 10,
             windowDefaultWidth := 600, windowDefaultHeight := 400 ]

  vbox <- vBoxNew False 0

  containerAdd window vbox

  box     <- hBoxNew False 0
  button1 <- buttonNewWithLabel "Open"
  onClicked button1 (putStrLn "button clicked")
  boxPackStart box button1 PackNatural 0
  button2 <- buttonNewWithLabel "Read"
  onClicked button2 (putStrLn "button clicked")
  boxPackStart box button2 PackNatural 0
  button3 <- buttonNewWithLabel "Write"
  onClicked button3 (putStrLn "button clicked")
  boxPackStart box button3 PackNatural 0
  boxPackStart vbox box PackNatural 0
  sep1       <- hSeparatorNew
  boxPackStart vbox sep1 PackNatural 10


  tv <- textViewNew

  containerAdd vbox tv
  buf <- textViewGetBuffer tv

  onBufferChanged buf $ do 
    cn <- textBufferGetCharCount buf
    putStrLn (show cn)   

  widgetShowAll window 
  onDestroy window mainQuit
  mainGUI