-- File: Main.hs
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (appendFile, readFile)

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    let fullCommand = unwords (prog : args)  -- combine program name + args into a single string
    appendFile "hscc.log" (fullCommand ++ "\n")  -- append to the log file
    
    case args of
        [input, "-o", output] -> do
            putStrLn $ "Input file: " ++ input
            putStrLn $ "Output file: " ++ output
        [input, "--lex"] -> clex input
        ["--lex", input] -> clex input
        _ -> die "Usage: mycompiler <input.c> -o <output.s>"

clex :: String -> IO ()
clex filename = do
    content <- readFile filename
    case lexer content of 
        Left error -> die error
        Right tokens -> putStrLn $ "Lex Success!"

data Token
    = TokInt Int      -- integer literal
    | TokIdent String -- identifier
    | TokPlus         -- '+'
    | TokMinus        -- '-'
    | TokEOF          -- end of input
    deriving (Show, Eq)

lexer :: String -> Either String [Token]
lexer [] = Right [TokEOF]  -- end of input
lexer (c:cs) = Left $ "Unexpected character: " ++ [c]

