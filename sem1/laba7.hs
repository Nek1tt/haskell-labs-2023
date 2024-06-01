import System.FilePath (takeDirectory, takeFileName)
import System.Directory (listDirectory)
import System.IO
import Data.List
import qualified Data.ByteString as BS


--- Sanity check --- 

printFile :: FilePath -> IO ()
printFile fileToPrint = do 
    handle <- openFile fileToPrint ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle


--- Text check --- 

areEqualText :: FilePath -> FilePath -> IO (Bool)
areEqualText fileToCheckOne fileToCheckTwo = do
    handleOne <- openFile fileToCheckOne ReadMode
    handleTwo <- openFile fileToCheckTwo ReadMode
    contentsOne <- hGetContents handleOne
    contentsTwo <- hGetContents handleTwo
    hClose handleOne
    hClose handleTwo
    return (contentsOne == contentsTwo)

--- dos2unix --- 

dos2unix :: FilePath -> IO ()
dos2unix filePath = do
    contents <- readFile filePath
    writeFile filePath (replaceLineEndings contents)
        where
            replaceLineEndings :: String -> String
            replaceLineEndings [] = []
            replaceLineEndings ('\r':'\n':xs) = '\n' : replaceLineEndings xs
            replaceLineEndings (x:xs) = x : replaceLineEndings xs

unix2dos :: FilePath -> IO ()
unix2dos filePath = do
    contents <- readFile filePath
    writeFile filePath (replaceLineEndings contents)
        where
            replaceLineEndings :: String -> String
            replaceLineEndings [] = []
            replaceLineEndings ('\n' : xs) = '\r' : '\n' : replaceLineEndings xs
            replaceLineEndings (x:xs) = x : replaceLineEndings xs


--- Binary check | ByteString

areEqualBin :: FilePath -> FilePath -> IO (Bool)
areEqualBin binFileToCheckOne binFileToCheckTwo = do
    handle1 <- openBinaryFile binFileToCheckOne ReadMode
    handle2 <- openBinaryFile binFileToCheckTwo ReadMode
    file1 <- BS.hGetContents handle1
    file2 <- BS.hGetContents handle2
    hClose handle1
    hClose handle2
    return (file1 == file2)


--- No vimmers?

fileIsBeingEdited :: FilePath -> IO (Bool)
fileIsBeingEdited filePath = do
    let name = takeFileName filePath
    files <- listDirectory (takeDirectory filePath)
    return (name ++ ".sw" `elem` files)
    






    