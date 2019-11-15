module Main where

import           Coroutine

program :: Trampoline IO ()
program = do
    lift $ putStr "Hello "
    lift $ putStr "the "
    pause
    lift $ putStrLn "coroutine!"

main :: IO ()
main = do
    Left cont <- bounce program
    putStr "wonderful "
    run cont
