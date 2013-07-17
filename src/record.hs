import qualified Person as P
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import System.IO
import System.IO.Error
import System.Directory
import Data.List

dispatch = [("add", add),
            ("view", view),
            ("del", del),
            ("search", search)
           ]

main = catchIOError doWork eHandler

doWork = do
    (x:xs) <- getArgs
    let (Just action) = lookup x dispatch
    action xs

eHandler e
    | isDoesNotExistError e = 
        case ioeGetFileName e of Just path -> putStrLn $ path ++ " does not exist."
                                 Nothing -> putStrLn "File does not exist."
    | otherwise = ioError e

add :: [String] -> IO()
add [] = putStrLn "Nothing to add"
add x
    | length x < 2 = putStrLn "Must be first name + last name."
    | otherwise = let p = P.Person (x !! 0) (x !! 1) in
                    putStrLn $ "firstName=" ++ P.getFirstName p ++ ", lastName=" ++ P.getLastName p

view :: [String] -> IO()
view [] = do
    putStrLn "Not supported yet."

del :: [String] -> IO()
del [] = do
    putStrLn "Not supported yet."

search :: [String] -> IO()
search [] = putStrLn "No argument for search, stop."
search (x:xs)
    | length xs /= 1 = putStrLn "Cannot understand what to search, stop."
    | x == "f" = putStrLn $ show $ countF (xs !! 0) "record.txt"
    | x == "l" = putStrLn $ show $ countL (xs !! 0) "record.txt"
    | otherwise = putStrLn "Unknow argument for search, stop."

countF :: String -> String -> Int
countF _ _ = 0

countL :: String -> String -> Int
countL _ _ = 0
