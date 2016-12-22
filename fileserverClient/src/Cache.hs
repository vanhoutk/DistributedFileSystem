module Cache 
    ( setupCache
    , storeNewFileInCache
    , getFileFromCache
    , removeFileFromCache
    , clearCache
    ) where

import Control.Monad
import Data.List
import Data.Time
import System.Directory
import System.Environment
import System.IO
import Control.Exception

import APIs


type FileSet = (Int, UTCTime, FilePath)

maxCacheSize :: Int
maxCacheSize = 30 -- Size is detemined by number of characters. TODO: Make this bigger

setupCache :: IO()
setupCache = do
  putStrLn "Initialising client-side cache..."
  createDirectoryIfMissing True ("temp/")
  putStrLn "Changing current directory..."
  setCurrentDirectory ("temp/")

storeNewFileInCache :: File -> IO()
storeNewFileInCache (File name contents) = do
  putStrLn $ "Storing file in cache: " ++ name
  writeFile name contents
  putStrLn "Updating cache..."
  updateCache

getFileFromCache :: String -> IO(File)
getFileFromCache fileName = do
  contents <- readFile fileName
  return (File fileName contents)

removeFileFromCache :: String -> IO()
removeFileFromCache fileName = do
  putStrLn $ "Removing file from cache: " ++ fileName
  removeFile fileName

clearCache :: IO()
clearCache = do
  putStrLn "Clearing client-side cache..."
  removeDirectoryRecursive("../temp/")

updateCache :: IO()
updateCache = do
  unsortedCache <- listCache "../temp/"
  let sortedCache = sortBy (\(_,a,_) (_,b,_) -> compare a b) unsortedCache
  print unsortedCache
  print sortedCache
  clearSpaceCache sortedCache

  where
    size = foldl (\acc (fileSize,_,_) -> fileSize + acc) 0 -- Calculates the total size of the list
    clearSpaceCache cache
      | size cache < maxCacheSize = return ()
      | otherwise = do
        let (_,_,filePath) = head cache
        let sizeOfCache = size cache
        print ("Size of cache: " ++ (show sizeOfCache) ++ " Deleting file: " ++ filePath)
        removeFileFromCache filePath
        clearSpaceCache $ tail cache
   
listCache:: FilePath -> IO [FileSet]
listCache path = do
  files <- listDirectory path
  fileSet <- mapM (fileinfo []) files
  return $ concat fileSet
  
  where
    fileinfo:: [FileSet] -> FilePath -> IO [FileSet]
    fileinfo a []   = return a
    fileinfo a filePath =  do 
      let p = path ++ filePath
      act <- getAccessTime p
      szt <- getFileSize p
      let sz = (read :: String -> Int) $ show szt
      isdir <- doesFileExist p 
      if isdir then  return $ (sz, act, p):a 
        else  return a