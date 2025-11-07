module CLI where

import Options.Applicative

data Options = Options
    { inputFile   :: FilePath
    , outputFile  :: Maybe FilePath
    , lexOnly     :: Bool
    , parseOnly   :: Bool
    }
    deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
    <$> strArgument
        ( metavar "INPUT"
       <> help "Input C source file" )
    <*> optional (strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> help "Output file" ))
    <*> switch
        ( long "lex"
       <> help "Run lexer only" )
    <*> switch
        ( long "parse"
       <> help "Run parser (after lexing)" )

getOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )