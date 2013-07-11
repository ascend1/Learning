import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.IO.Error
import System.Directory
import Data.List

dispatch :: [(String, [String] -> IO())]
dispatch = [("add", add),
            ("view", view),
            ("del", del)
           ]

main = catchIOError doWork eHandler

doWork = do
    (cmd:args) <- getArgs
    let (Just action) = lookup cmd dispatch
    action args

eHandler :: IOError -> IO()
eHandler e 
    | isDoesNotExistError e = 
        case ioeGetFileName e of Just path -> putStrLn $ path ++ " doesn't exist."
                                 Nothing -> putStrLn "File doesn't exist."
    | otherwise = ioError e

add :: [String] -> IO()
add [] = putStrLn "Nothing to add."
add [item] = appendFile "todo.txt" (item ++ "\n")

view :: [String] -> IO()
view [] = do
    content <- readFile "todo.txt"
    putStr $ (unlines . zipWith (\n line -> show n ++ " - " ++ line) [0..] . lines) content

del :: [String] -> IO()
del [] = putStrLn "Nothing to delete."
del [itemId] = do
    content <- B.readFile "todo.txt"
    (tempName, tempHandle) <- openTempFile "." "temp"
    let
        index = read itemId
        items = B.lines content
        newItems = delete (items !! index) items
    B.hPutStr tempHandle $ B.unlines newItems
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
