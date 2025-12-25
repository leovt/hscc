module Main where

import System.Environment (getArgs, getProgName)
import System.FilePath (replaceExtension)
import System.Exit (die, exitSuccess)
import System.Process (readProcess, callProcess)
import Control.Monad (when)
import Control.Exception (evaluate)
import Control.DeepSeq (deepseq)

import Lexer (lexer)
import Parser (parser)
import Validate (validate)
import AsmAst (translateTACtoASM, emitProgram)
import CLI(getOptions, Options(..))
import TAC

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

    (validated_ast, nextID) <- case validate ast of
        Left errormsg -> die errormsg
        Right (ast, nextID) -> return (ast, nextID)

    evaluate (show validated_ast `deepseq` ())

    when (validateOnly options) $ do
        print validated_ast
        exitSuccess



    let tac = TAC.translate validated_ast nextID

    when (tackyOnly options) $ do
        print tac
        exitSuccess

    let asmast = translateTACtoASM tac

    when (codegenOnly options) $ do
        print asmast
        exitSuccess

    let output = unlines (emitProgram asmast)
    evaluate (output `deepseq` ())

    let asmoutput = replaceExtension (inputFile options) ".s"

    writeFile asmoutput output

    let executable = case outputFile options of
            Just filename -> filename
            Nothing -> replaceExtension (inputFile options) ""

    callProcess "gcc" [asmoutput, "-o", executable]

