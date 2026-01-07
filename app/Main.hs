module Main where

import AsmAst (emitProgram, translateTACtoASM)
import CLI (Options (..), getOptions)
import Control.Monad (when)
import Lexer (lexer)
import Parser (parser)
import PrettyASM ()
import PrettyAst
import PrettyTAC ()
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Environment (getArgs, getProgName)
import System.Exit (die, exitSuccess)
import System.FilePath (replaceExtension)
import System.Process (callProcess, readProcess)
import TAC
import Validate (validate)

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  let fullCommand = unwords (prog : args) -- combine program name + args into a single string
  appendFile "hscc.log" (fullCommand ++ "\n") -- append to the log file
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
    let doc = pretty ast
    putStrLn (renderString (layoutPretty defaultLayoutOptions doc))
    exitSuccess

  (validated_ast, nextID, symbolTable) <- case validate ast of
    Left errormsg -> die errormsg
    Right (ast, nextID, symbolTable) -> return (ast, nextID, symbolTable)

  when (validateOnly options) $ do
    let doc = pretty validated_ast
    putStrLn (renderString (layoutPretty defaultLayoutOptions doc))
    let doc = pretty symbolTable
    putStrLn (renderString (layoutPretty defaultLayoutOptions doc))
    exitSuccess

  let tac = TAC.translate validated_ast symbolTable nextID

  when (tackyOnly options) $ do
    let doc = pretty tac
    putStrLn (renderString (layoutPretty defaultLayoutOptions doc))
    exitSuccess

  let asmast = translateTACtoASM tac

  when (codegenOnly options) $ do
    let doc = pretty asmast
    putStrLn (renderString (layoutPretty defaultLayoutOptions doc))
    exitSuccess

  let output = unlines (emitProgram asmast)

  let asmoutput = replaceExtension (inputFile options) ".s"

  writeFile asmoutput output

  let executable = case outputFile options of
        Just filename -> filename
        Nothing -> replaceExtension (inputFile options) ""

  let objectfile = case outputFile options of
        Just filename -> filename
        Nothing -> replaceExtension (inputFile options) ".o"

  callProcess "gcc" $
    if noLink options
      then [asmoutput, "-c", "-o", objectfile]
      else [asmoutput, "-o", executable]