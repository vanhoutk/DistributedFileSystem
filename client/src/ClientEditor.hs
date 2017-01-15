module ClientEditor where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk

import APIs
import Cache
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

  menuBox           <- hBoxNew False 2
  newButton         <- buttonNewWithLabel "New File"
  readButton        <- buttonNewWithLabel "Read File"
  readWriteButton   <- buttonNewWithLabel "Read/Write File"
  uploadButton      <- buttonNewWithLabel "Upload File"
  listButton        <- buttonNewWithLabel "List Files"
  sep1              <- hSeparatorNew
  fileNameBox       <- hBoxNew False 2
  fileNameLabel     <- labelNew (Just "Filename: ")
  fileNameTextView  <- textViewNew
  sep2              <- hSeparatorNew
  fileContentsTV    <- textViewNew

  onClicked newButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    contentsBuffer <- textViewGetBuffer fileContentsTV
    textBufferSetText nameBuffer "" 
    textBufferSetText contentsBuffer ""
    widgetSetSensitivity uploadButton True
    widgetSetSensitivity fileNameTextView True
    widgetSetSensitivity fileContentsTV True

  onClicked readButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    contentsBuffer <- textViewGetBuffer fileContentsTV
    (File fileName fileContents) <- readButtonClicked token
    textBufferSetText nameBuffer fileName 
    textBufferSetText contentsBuffer fileContents
    widgetSetSensitivity uploadButton False
    widgetSetSensitivity fileNameTextView False
    widgetSetSensitivity fileContentsTV False

  onClicked readWriteButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    contentsBuffer <- textViewGetBuffer fileContentsTV
    (File fileName fileContents) <- readWriteButtonClicked token
    textBufferSetText nameBuffer fileName 
    textBufferSetText contentsBuffer fileContents
    widgetSetSensitivity uploadButton True
    widgetSetSensitivity fileNameTextView False
    widgetSetSensitivity fileContentsTV True

  onClicked uploadButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    nameBufferStart <- textBufferGetStartIter nameBuffer
    nameBufferEnd <- textBufferGetEndIter nameBuffer
    fileName <- textBufferGetText nameBuffer nameBufferStart nameBufferEnd False :: IO String

    contentsBuffer <- textViewGetBuffer fileContentsTV
    contentsBufferStart <- textBufferGetStartIter contentsBuffer
    contentsBufferEnd <- textBufferGetEndIter contentsBuffer
    fileContents <- textBufferGetText contentsBuffer contentsBufferStart contentsBufferEnd False :: IO String

    uploadToServerQuery token fileName fileContents


  onClicked listButton $ listButtonClicked token

  -- Menu Bar
  boxPackStart menuBox readButton PackNatural 0
  boxPackStart menuBox readWriteButton PackNatural 0
  boxPackStart menuBox uploadButton PackNatural 0
  boxPackStart menuBox listButton PackNatural 0
  boxPackStart vbox menuBox PackNatural 0
  
  boxPackStart vbox sep1 PackNatural 10

  boxPackStart fileNameBox fileNameLabel PackNatural 0
  boxPackStart fileNameBox fileNameTextView PackGrow 0

  boxPackStart vbox fileNameBox PackNatural 5

  boxPackStart vbox sep2 PackNatural 10
  
  containerAdd vbox fileContentsTV
  --buf <- textViewGetBuffer fileContentsTV

  --st <- readFile "./file1.txt"
  --textBufferSetText buf st

  --onBufferChanged buf $ do 
  --  cn <- textBufferGetCharCount buf
  --  putStrLn (show cn)   -- TODO - Remove this

  widgetShowAll window                          -- Show the Window
  onDestroy window quitClient
  mainGUI                                       -- Enter the main gtk processing loop


readButtonClicked :: AuthToken -> IO(File)
readButtonClicked token = do
  putStrLn "Please enter the name of the file you want to read:"
  fileName <- getLine
  file <- downloadReadWriteQ token fileName "read"
  case file of
    Nothing -> return (File "" "")
    Just file' -> return file' 

readWriteButtonClicked :: AuthToken -> IO(File)
readWriteButtonClicked token = do
  putStrLn "Please enter the name of the file you want to download:"
  fileName <- getLine
  file <- downloadReadWriteQ token fileName "write"
  case file of
    Nothing -> return (File "" "")
    Just file' -> return file' 

listButtonClicked :: AuthToken -> IO()
listButtonClicked token = do
  fileList <- fileListQuery token
  return ()

quitClient :: IO()
quitClient = do
  clearCache
  mainQuit