module CLI where

import Options.Applicative

data Options = Options
  { inputFile :: FilePath,
    outputFile :: Maybe FilePath,
    lexOnly :: Bool,
    parseOnly :: Bool,
    validateOnly :: Bool,
    tackyOnly :: Bool,
    codegenOnly :: Bool,
    noLink :: Bool
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument
      ( metavar "INPUT"
          <> help "Input C source file"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "Output file"
          )
      )
    <*> switch
      ( long "lex"
          <> help "Run lexer only"
      )
    <*> switch
      ( long "parse"
          <> help "Stop after parser"
      )
    <*> switch
      ( long "validate"
          <> help "Stop after validation"
      )
    <*> switch
      ( long "tacky"
          <> help "Stop after TAC generation"
      )
    <*> switch
      ( long "codegen"
          <> help "Stop after codegen"
      )
    <*> switch
      ( short 'c'
          <> help "Compile and assemble, but do not link."
      )

getOptions :: IO Options
getOptions = execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "compile INPUT"
            <> header "hscc"
        )