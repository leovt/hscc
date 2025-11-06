-- File: Main.hs
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (appendFile)
import System.Process (readProcess)
import Lexer (lexer)

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    let fullCommand = unwords (prog : args)  -- combine program name + args into a single string
    appendFile "hscc.log" (fullCommand ++ "\n")  -- append to the log file
    
    case args of
        [input, "--lex"] -> clex input
        ["--lex", input] -> clex input


clex :: String -> IO ()
clex filename = do
    content <- readProcess "gcc" ["-E", "-P", filename] ""
    case lexer content of 
        Left errormsg -> die errormsg
        Right tokens -> mapM_ print tokens




