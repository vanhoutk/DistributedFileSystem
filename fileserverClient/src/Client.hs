module Client where

import Control.Concurrent.STM
import Data.Map as M hiding (filter, map)
import System.IO

import ClientAPI

startClient :: IO()
startClient = do
	cacheSize <- atomically $ newTVar 0 
	cacheList <- atomically $ newTVar M.empty
	setupCache
	clientLoop cacheSize cacheList

clientLoop :: TVar Int -> CacheList -> IO()
clientLoop cacheSize cacheList = do
	putStrLn "Please select command: upload/list_files/download/close_client"
	user_input <- getLine
	case user_input of
		"upload" -> processUpload cacheSize cacheList
		"list_files" -> do
			runQuery "listfiles" "fileName" "fileContents" cacheSize cacheList
			clientLoop cacheSize cacheList
		"download" -> processDownload cacheSize cacheList
		"close_client" -> do
			clearCache
		_ -> do
			putStrLn "Invalid commmand. Please try again."
			clientLoop cacheSize cacheList

processUpload :: TVar Int -> CacheList -> IO()
processUpload cacheSize cacheList = do
	putStrLn "Please enter the name of the file to upload"
	fileName <- getLine
	putStrLn "Please enter the contents of the file to upload"
	contents <- getLine
	runQuery "upload" fileName contents cacheSize cacheList
	clientLoop cacheSize cacheList

processDownload :: TVar Int -> CacheList -> IO()
processDownload cacheSize cacheList = do
	putStrLn "Please enter the name of the file you wish to download"
	fileName <- getLine
	runQuery "download" fileName "fileContents" cacheSize cacheList
	clientLoop cacheSize cacheList