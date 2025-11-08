module Main where

import System.Environment (getArgs, getProgName)
import System.FilePath (replaceExtension)
import System.Exit (die, exitSuccess)
import System.IO (appendFile)
import System.Process (readProcess, callProcess)
import Control.Monad (when)

import Lexer (lexer)
import Parser (parser, Program)
import AsmAst (translateProgram, emitProgram)
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

    ast <- case parser tokens of
        Left errormsg -> die errormsg
        Right ast -> return ast

    when (parseOnly options) $ do
        print ast
        exitSuccess

    let asmast = (translateProgram ast)

    when (codegenOnly options) $ do
        print asmast
        exitSuccess

    let asmoutput = replaceExtension (inputFile options) ".s"

    writeFile asmoutput (unlines (emitProgram asmast))

    let executable = case (outputFile options) of
            Just filename -> filename
            Nothing -> replaceExtension (inputFile options) ""

    callProcess "gcc" [asmoutput, "-o", executable]

