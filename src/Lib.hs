module Lib
    ( 
        someFunc,
        runFile
    ) where

import Parser
import Evaluator (runProgram)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

runFile :: String -> IO ()
runFile s = do
    contents <- readFile s
    runProgram $ runParser program contents