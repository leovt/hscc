module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (die, exitSuccess)
import System.IO (appendFile)
import System.Process (readProcess)
import Control.Monad (when)

import Lexer (lexer)
import Parser (parser)
import CLI(getOptions, Options(..))

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    let fullCommand = unwords (prog : args)  -- combine program name + args into a single string
    appendFile "hscc.log" (fullCommand ++ "\n")  -- append to the log file
    
    options <- getOptions

    content <- readProcess "gcc" ["-E", "-P", inputFile options] ""
    
    tokens <- case lexer content of 
        Left errormsg -> die errormsg
        Right tokens -> return tokens

    when (lexOnly options) $ do
        mapM_ print tokens
        exitSuccess

    case parser tokens of
        Left errormsg -> die errormsg
        Right ast -> print ast
        
