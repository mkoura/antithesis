module Main where
import System.Environment (getArgs)
import Adversary (adversary, toString)

main :: IO ()
main = do
    args <- getArgs
    msg <- adversary args
    putStrLn $ toString msg

