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

                     
  -- | Widget Declarations

  vbox              <- vBoxNew False 0  -- Create a new vertical box (False = Not equal space for all children, 0 = Default spacing between children.)
  menuBox           <- hBoxNew False 2
  newButton         <- buttonNewWithLabel "New File"
  menuSeparator1    <- vSeparatorNew
  readButton        <- buttonNewWithLabel "Read"
  readWriteButton   <- buttonNewWithLabel "Read/Write"
  uploadButton      <- buttonNewWithLabel "Upload"
  listButton        <- buttonNewWithLabel "List Files"
  menuSeparator2    <- vSeparatorNew
  transactionButton <- buttonNewWithLabel "Start Transaction"
  tReadWriteButton  <- buttonNewWithLabel "Read/Write"
  tUploadButton     <- buttonNewWithLabel "Upload"
  tCommitButton     <- buttonNewWithLabel "Commit"
  tAbortButton      <- buttonNewWithLabel "Abort"

  boxSeparator1     <- hSeparatorNew
  fileNameBox       <- hBoxNew False 2
  fileNameLabel     <- labelNew (Just "Filename: ")
  fileNameTextView  <- textViewNew
  boxSeparator2     <- hSeparatorNew
  fileContentsTV    <- textViewNew

  -- Disable all transaction buttons initially
  widgetSetSensitivity tReadWriteButton False
  widgetSetSensitivity tUploadButton    False
  widgetSetSensitivity tCommitButton    False
  widgetSetSensitivity tAbortButton     False

  -- | onClicked Functions

  onClicked newButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    contentsBuffer <- textViewGetBuffer fileContentsTV
    textBufferSetText nameBuffer "" 
    textBufferSetText contentsBuffer ""
    widgetSetSensitivity uploadButton     True
    widgetSetSensitivity fileNameTextView True
    widgetSetSensitivity fileContentsTV   True

  onClicked readButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    contentsBuffer <- textViewGetBuffer fileContentsTV
    (File fileName fileContents) <- readButtonClicked token
    textBufferSetText nameBuffer fileName 
    textBufferSetText contentsBuffer fileContents
    widgetSetSensitivity uploadButton     False
    widgetSetSensitivity fileNameTextView False
    widgetSetSensitivity fileContentsTV   False

  onClicked readWriteButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    contentsBuffer <- textViewGetBuffer fileContentsTV
    (File fileName fileContents) <- readWriteButtonClicked token
    textBufferSetText nameBuffer fileName 
    textBufferSetText contentsBuffer fileContents
    widgetSetSensitivity uploadButton     True
    widgetSetSensitivity fileNameTextView False
    widgetSetSensitivity fileContentsTV   True

  onClicked uploadButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    nameBufferStart <- textBufferGetStartIter nameBuffer
    nameBufferEnd <- textBufferGetEndIter nameBuffer
    fileName <- textBufferGetText nameBuffer nameBufferStart nameBufferEnd False :: IO String
    contentsBuffer <- textViewGetBuffer fileContentsTV
    contentsBufferStart <- textBufferGetStartIter contentsBuffer
    contentsBufferEnd <- textBufferGetEndIter contentsBuffer
    fileContents <- textBufferGetText contentsBuffer contentsBufferStart contentsBufferEnd False :: IO String
    if(fileName == "" || fileContents == "") then return () else uploadToServerQuery token fileName fileContents 

  onClicked listButton $ do
    listButtonClicked token

  onClicked transactionButton $ do
    startTQuery token
    widgetSetSensitivity readWriteButton  False
    widgetSetSensitivity uploadButton     False
    widgetSetSensitivity tReadWriteButton True
    widgetSetSensitivity tUploadButton    True
    widgetSetSensitivity tCommitButton    True
    widgetSetSensitivity tAbortButton     True

  onClicked tReadWriteButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    contentsBuffer <- textViewGetBuffer fileContentsTV
    (File fileName fileContents) <- readWriteTButtonClicked token
    if(fileName == "Failed") then do
      putStrLn $ "Download Failed - " ++ fileContents
    else do
      textBufferSetText nameBuffer fileName 
      textBufferSetText contentsBuffer fileContents

  onClicked tUploadButton $ do
    nameBuffer <- textViewGetBuffer fileNameTextView
    nameBufferStart <- textBufferGetStartIter nameBuffer
    nameBufferEnd <- textBufferGetEndIter nameBuffer
    fileName <- textBufferGetText nameBuffer nameBufferStart nameBufferEnd False :: IO String
    contentsBuffer <- textViewGetBuffer fileContentsTV
    contentsBufferStart <- textBufferGetStartIter contentsBuffer
    contentsBufferEnd <- textBufferGetEndIter contentsBuffer
    fileContents <- textBufferGetText contentsBuffer contentsBufferStart contentsBufferEnd False :: IO String
    if(fileName == "" || fileContents == "") then return () else uploadTQuery token fileName fileContents 

  onClicked tCommitButton $ do
    commitTQuery token
    widgetSetSensitivity readWriteButton  True
    widgetSetSensitivity uploadButton     True
    widgetSetSensitivity tReadWriteButton False
    widgetSetSensitivity tUploadButton    False
    widgetSetSensitivity tCommitButton    False
    widgetSetSensitivity tAbortButton     False

  onClicked tAbortButton $ do
    abortTQuery token
    widgetSetSensitivity readWriteButton  True
    widgetSetSensitivity uploadButton     True
    widgetSetSensitivity tReadWriteButton False
    widgetSetSensitivity tUploadButton    False
    widgetSetSensitivity tCommitButton    False
    widgetSetSensitivity tAbortButton     False

  -- | Layout

  -- Add vbox to the window
  containerAdd window vbox

  -- Menu Bar
  boxPackStart menuBox newButton PackNatural 0
  boxPackStart menuBox menuSeparator1 PackNatural 10
  boxPackStart menuBox readButton PackNatural 0
  boxPackStart menuBox readWriteButton PackNatural 0
  boxPackStart menuBox uploadButton PackNatural 0
  boxPackStart menuBox listButton PackNatural 0
  boxPackStart menuBox menuSeparator2 PackNatural 10
  boxPackStart menuBox transactionButton PackNatural 0
  boxPackStart menuBox tReadWriteButton PackNatural 0
  boxPackStart menuBox tUploadButton PackNatural 0
  boxPackStart menuBox tCommitButton PackNatural 0
  boxPackStart menuBox tAbortButton PackNatural 0

  -- Add to the main box
  boxPackStart vbox menuBox PackNatural 0
  boxPackStart vbox boxSeparator1 PackNatural 10

  -- File Name Bar
  boxPackStart fileNameBox fileNameLabel PackNatural 0
  boxPackStart fileNameBox fileNameTextView PackGrow 0

  -- Add to the main box
  boxPackStart vbox fileNameBox PackNatural 5
  boxPackStart vbox boxSeparator2 PackNatural 10
  
  -- File Contents Box
  containerAdd vbox fileContentsTV


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

readWriteTButtonClicked :: AuthToken -> IO(File)
readWriteTButtonClicked token = do
  putStrLn "Please enter the name of the file you want to download:"
  fileName <- getLine
  file <- downloadTQuery token fileName
  case file of
    Nothing -> return (File "Failed" "No file found.")
    Just file' -> return file' 

quitClient :: IO()
quitClient = do
  clearCache
  mainQuit