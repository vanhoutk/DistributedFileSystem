module ClientEditor where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk

import APIs
import ClientAPI

startEditor :: AuthToken -> IO ()
startEditor token = do
  initGUI                                     -- Initialise the GUI
  window <- windowNew                         -- Create a new window
  set window [ windowTitle := "Text Editor"
             , containerBorderWidth := 10
             , windowDefaultWidth := 800
             , windowDefaultHeight := 600 
             ]

  vbox <- vBoxNew False 0                     {- Create a new vertical box (False = Not equal space for all children, 
                                                                                0 = Default spacing between children.) -}

  containerAdd window vbox

  buttonBox     <- hBoxNew False 2
  readButton <- buttonNewWithLabel "Read File"
  onClicked readButton (putStrLn "button clicked")
  boxPackStart buttonBox readButton PackNatural 0

  readWriteButton <- buttonNewWithLabel "Read/Write File"
  onClicked readWriteButton (putStrLn "button clicked")
  boxPackStart buttonBox readWriteButton PackNatural 0

  uploadButton <- buttonNewWithLabel "Upload File"
  onClicked uploadButton (putStrLn "button clicked")
  boxPackStart buttonBox uploadButton PackNatural 0

  listButton <- buttonNewWithLabel "List Files"
  onClicked listButton $ listButtonClicked token
  boxPackStart buttonBox listButton PackNatural 0


  boxPackStart vbox buttonBox PackNatural 0
  sep1       <- hSeparatorNew
  boxPackStart vbox sep1 PackNatural 10


  tv <- textViewNew

  containerAdd vbox tv
  buf <- textViewGetBuffer tv

  onBufferChanged buf $ do 
    cn <- textBufferGetCharCount buf
    putStrLn (show cn)   -- TODO - Remove this

  widgetShowAll window                          -- Show the Window
  onDestroy window mainQuit
  mainGUI                                       -- Enter the main gtk processing loop


listButtonClicked :: AuthToken -> IO()
listButtonClicked token = do
  runQuery token "listfiles" "fileName" "fileContents"