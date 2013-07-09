import System.Environment
import System.IO
import System.Directory
import Data.List

dispatch :: [(String, [String] -> IO())]
dispatch = [("add", add),
            ("view", view),
            ("del", del)
           ]

main = do
    (cmd:args) <- getArgs
    let (Just action) = lookup cmd dispatch
    action args

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
    content <- readFile "todo.txt"
    (tempName, tempHandle) <- openTempFile "." "temp"
    let
        index = read itemId
        items = lines content
        newItems = delete (items !! index) items
    hPutStr tempHandle $ unlines newItems
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
