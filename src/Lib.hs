module Lib
    ( 
        someFunc,
        runFile,
        runPrompt
    ) where

import Parser
import Evaluator (runProgram)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

runFile :: String -> IO ()
runFile s = do
    contents <- readFile s
    runProgram $ runParser program contents

runPrompt :: IO ()
runPrompt = do
    putStr "> "
    line <- getLine
    if line == "" then
        return ()
    else do
        runProgram $ runParser program (trim line)
        runPrompt