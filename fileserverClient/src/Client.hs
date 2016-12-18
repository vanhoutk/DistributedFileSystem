module Client where

import System.IO

import ClientAPI

startClient :: IO()
startClient = do
	setupCache
	clientLoop

clientLoop :: IO()
clientLoop = do
	putStrLn "Please select command: upload/list_files/download/close_client"
	user_input <- getLine
	case user_input of
		"upload" -> processUpload
		"list_files" -> do
			--putStrLn $ "listfiles" ++ "fileName" ++ "fileContents"
			runQuery "listfiles" "fileName" "fileContents"
			clientLoop
		"download" -> processDownload
		"close_client" -> do
			clearCache
		_ -> do
			putStrLn "Invalid commmand. Please try again."
			clientLoop

processUpload :: IO()
processUpload = do
	putStrLn "Please enter the name of the file to upload"
	fileName <- getLine
	putStrLn "Please enter the contents of the file to upload"
	contents <- getLine
	--putStrLn $ "upload" ++ fileName ++ contents
	runQuery "upload" fileName contents
	clientLoop

processDownload :: IO()
processDownload = do
	putStrLn "Please enter the name of the file you wish to download"
	fileName <- getLine
	--putStrLn $ "download" ++ fileName ++ "fileContents"
	runQuery "download" fileName "fileContents"
	clientLoop