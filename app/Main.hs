module Main (main) where

import Lib
import System.Environment
import System.IO
import qualified System.Exit as Exit

main :: IO ()
main = do
        args <- getArgs
        progName <- getProgName
        putStrLn  $ "Running: "++ progName

        if null args
            then do
                    putStrLn "Usage: brainfuckhask <file.bf>"
                    Exit.exitSuccess
            else do 
                    putStrLn $ "Running program "++ head args
                    runFile $ head args
