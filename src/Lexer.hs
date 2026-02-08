module Lexer
  ( lexer,
    Token (..),
    Position (..),
    Span (..),
    LocatedToken,
    IntSuffix (..),
  )
where

import Data.List (mapAccumL)

data IntSuffix = NoSuffix | LSuffix | USuffix | LUSuffix deriving (Show, Eq)

data Token
  = TokInt Integer IntSuffix
  | TokFloat Double
  | TokIdent String
  | TokKeyInt
  | TokKeyLong
  | TokKeySigned
  | TokKeyUnsigned
  | TokKeyDouble
  | TokKeyVoid
  | TokKeyReturn
  | TokKeyGoto
  | TokKeyFor
  | TokKeyWhile
  | TokKeyDo
  | TokKeyBreak
  | TokKeyContinue
  | TokKeySwitch
  | TokKeyCase
  | TokKeyDefault
  | TokKeyExtern
  | TokKeyStatic
  | TokKeyIf
  | TokKeyElse
  | TokOpenParen
  | TokCloseParen
  | TokOpenBrace
  | TokCloseBrace
  | TokSemicolon
  | TokTilde
  | TokMinus
  | TokDblMinus
  | TokPlus
  | TokDblPlus
  | TokAsterisk
  | TokSlash
  | TokPercent
  | TokAmpersand
  | TokPipe
  | TokCircumflex
  | TokLess
  | TokDblLess
  | TokGreater
  | TokDblGreater
  | TokBang
  | TokBangEqual
  | TokDblAmp
  | TokDblPipe
  | TokEqual
  | TokDblEqual
  | TokLessEqual
  | TokGreaterEqual
  | TokPlusEqual
  | TokMinusEqual
  | TokAsteriskEqual
  | TokSlashEqual
  | TokPercentEqual
  | TokAmpersandEqual
  | TokPipeEqual
  | TokCircumflexEqual
  | TokDblLessEqual
  | TokDblGreaterEqual
  | TokQuestion
  | TokColon
  | TokComma
  deriving (Show, Eq)

data Position = Position {line :: Int, column :: Int}
  deriving (Eq, Ord)

data Span = Span {start :: Position, end :: Position}
  deriving (Eq, Show)

instance Show Position where
  show (Position l c) = show l ++ ":" ++ show c

type LocatedChar = (Char, Position)

type LocatedToken = (Token, Span)

enumerateSourcePositions :: String -> [LocatedChar]
-- mapAccumL :: step_function -> initial_accumulator -> input_list -> (final_accumulator, output_list)
-- snd :: (final_accumulator, output_list) -> output_list
enumerateSourcePositions = snd . mapAccumL step (1, 1)
  where
    step :: (Int, Int) -> Char -> ((Int, Int), LocatedChar)
    -- step :: pre_acumulator -> input_element -> (post_accumulator, output_element)
    -- the accumulator is a tuple (line, column) for the next character to be emitted
    step (line, col) '\n' = ((line + 1, 1), ('\n', Position line col))
    step (line, col) c = ((line, col + 1), (c, Position line col))

{- HLINT ignore "Use camelCase" -}
data LexerState
  = LS_Start
  | LS_Ident Position String
  | LS_Integer Position String
  | LS_IntSuffix Position String String
  | LS_Decimal Position String
  | LS_DecimalDigits Position String
  | LS_Exponent Position String
  | LS_ExponentSign Position String
  | LS_ExponentDigits Position String
  | LS_Punctuation Position String

lexer :: String -> Either String [LocatedToken]
lexer = fmap reverse . snd . foldl step (LS_Start, Right []) . enumerateSourcePositions . add_eof
  where
    step :: (LexerState, Either String [LocatedToken]) -> LocatedChar -> (LexerState, Either String [LocatedToken])
    step (state, Left x) _ = (state, Left x)
    step (LS_Start, Right tokens) (c, pos)
      | c `elem` whitespace = (LS_Start, Right tokens)
      | c `elem` digits = (LS_Integer pos [c], Right tokens)
      | c == '.' = (LS_Decimal pos [c], Right tokens)
      | c `elem` id_start = (LS_Ident pos [c], Right tokens)
      | c `elem` punctuation = (LS_Punctuation pos [c], Right tokens)
      | c == '(' = emitToken tokens (TokOpenParen, Span pos pos)
      | c == ')' = emitToken tokens (TokCloseParen, Span pos pos)
      | c == '{' = emitToken tokens (TokOpenBrace, Span pos pos)
      | c == '}' = emitToken tokens (TokCloseBrace, Span pos pos)
      | c == ';' = emitToken tokens (TokSemicolon, Span pos pos)
      | otherwise = lexerError ("Unexpected " ++ [c])
    step (LS_Integer start n, Right tokens) (c, pos)
      | c `elem` digits = (LS_Integer start (n ++ [c]), Right tokens)
      | c `elem` int_suffix = (LS_IntSuffix start n [c], Right tokens)
      | c == '.' = (LS_DecimalDigits start (n ++ [c]), Right tokens)
      | c == 'e' || c == 'E' = (LS_Exponent start (n ++ [c]), Right tokens)
      | c `elem` id_continue = lexerError ("Unexpected in Integer " ++ [c])
      | otherwise = emitTokenAndConsume tokens (TokInt (read n) NoSuffix, Span start pos) (c, pos)
    step (LS_IntSuffix start n suffix, Right tokens) (c, pos)
      | c `elem` int_suffix = (LS_IntSuffix start n (suffix ++ [c]), Right tokens)
      | c == '.' = lexerError "Illegal '.' after integer suffix"
      | otherwise = case intSuffix suffix of
          Left err -> lexerError err
          Right s -> emitTokenAndConsume tokens (TokInt (read n) s, Span start pos) (c, pos)
    step (LS_Decimal start part, Right tokens) (c, pos)
      | c `elem` digits = (LS_DecimalDigits start (part ++ [c]), Right tokens)
      | c == 'e' || c == 'E' = lexerError $ "No digits in mantissa, write `0.` or `.0` in `" ++ part ++ [c] ++ "`"
      | c == '.' || c `elem` id_continue = lexerError ("Unexpected in Float " ++ [c])
      | otherwise = emitTokenAndConsume tokens (floatToken part, Span start pos) (c, pos)
    step (LS_DecimalDigits start part, Right tokens) (c, pos)
      | c `elem` digits = (LS_DecimalDigits start (part ++ [c]), Right tokens)
      | c == 'e' || c == 'E' = (LS_Exponent start (if last part == '.' then part ++ ['0', c] else part ++ [c]), Right tokens)
      | c == '.' || c `elem` id_continue = lexerError ("Unexpected in Float " ++ [c])
      | otherwise = emitTokenAndConsume tokens (floatToken part, Span start pos) (c, pos)
    step (LS_Exponent start part, Right tokens) (c, _)
      | c `elem` digits = (LS_ExponentDigits start (part ++ [c]), Right tokens)
      | c == '+' || c == '-' = (LS_ExponentSign start (part ++ [c]), Right tokens)
      | otherwise = lexerError "expected digits in exponent"
    step (LS_ExponentSign start part, Right tokens) (c, _)
      | c `elem` digits = (LS_ExponentDigits start (part ++ [c]), Right tokens)
      | otherwise = lexerError "expected digits in exponent"
    step (LS_ExponentDigits start part, Right tokens) (c, pos)
      | c `elem` digits = (LS_ExponentDigits start (part ++ [c]), Right tokens)
      | c `elem` id_continue = lexerError ("Unexpected in Float " ++ [c])
      | c == '.' = lexerError "Illegal '.' after float literal"
      | otherwise = emitTokenAndConsume tokens (floatToken part, Span start pos) (c, pos)
    step (LS_Ident start ident, Right tokens) (c, pos)
      | c `elem` id_continue = (LS_Ident start (ident ++ [c]), Right tokens)
      | otherwise = emitTokenAndConsume tokens (map_keyword ident, Span start pos) (c, pos)
    step (LS_Punctuation start punct, Right tokens) (c, pos) = case punctuationToken (punct ++ [c]) of
      Just _ -> (LS_Punctuation start (punct ++ [c]), Right tokens)
      Nothing -> case punctuationToken punct of
        Just token -> emitTokenAndConsume tokens (token, Span start pos) (c, pos)
        Nothing -> lexerError ("punctuationToken: unexpected punctuation: " ++ show punct)

    lexerError :: String -> (LexerState, Either String [LocatedToken])
    lexerError msg = (LS_Start, Left msg)

    emitToken :: [LocatedToken] -> LocatedToken -> (LexerState, Either String [LocatedToken])
    emitToken tokens token = (LS_Start, Right (token : tokens))

    {- HLINT ignore "Eta reduce" -}
    {- this function emits a token and then calls step again with the same character in order to process it.
       The character passed as the last argument is not part of the emitted token. -}
    emitTokenAndConsume :: [LocatedToken] -> LocatedToken -> (Char, Position) -> (LexerState, Either String [LocatedToken])
    emitTokenAndConsume tokens token locChar = step (emitToken tokens token) locChar

    floatToken :: String -> Token
    floatToken s = case reads (fix s) of
      [(double, "")] -> TokFloat double
      _ -> error $ "floatToken: DFA produced invalid literal `" ++ s ++ "`"
      where
        fix :: String -> String
        fix s@('.' : _) = '0' : s
        fix s | last s == '.' = s ++ "0"
        fix s = s

    map_keyword "int" = TokKeyInt
    map_keyword "long" = TokKeyLong
    map_keyword "signed" = TokKeySigned
    map_keyword "unsigned" = TokKeyUnsigned
    map_keyword "double" = TokKeyDouble
    map_keyword "void" = TokKeyVoid
    map_keyword "return" = TokKeyReturn
    map_keyword "goto" = TokKeyGoto
    map_keyword "for" = TokKeyFor
    map_keyword "while" = TokKeyWhile
    map_keyword "do" = TokKeyDo
    map_keyword "break" = TokKeyBreak
    map_keyword "continue" = TokKeyContinue
    map_keyword "switch" = TokKeySwitch
    map_keyword "case" = TokKeyCase
    map_keyword "default" = TokKeyDefault
    map_keyword "extern" = TokKeyExtern
    map_keyword "static" = TokKeyStatic
    map_keyword "if" = TokKeyIf
    map_keyword "else" = TokKeyElse
    map_keyword ident = TokIdent ident

    add_eof source = source ++ "\0"

    whitespace = " \t\n\r\f\v\0"
    id_start = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    digits = "0123456789"
    int_suffix = "lLuU"
    id_continue = id_start ++ digits
    punctuation = "-~+*/%&|^<>!=?:,"

    punctuationToken "-" = Just TokMinus
    punctuationToken "--" = Just TokDblMinus
    punctuationToken "~" = Just TokTilde
    punctuationToken "+" = Just TokPlus
    punctuationToken "++" = Just TokDblPlus
    punctuationToken "*" = Just TokAsterisk
    punctuationToken "/" = Just TokSlash
    punctuationToken "%" = Just TokPercent
    punctuationToken "&" = Just TokAmpersand
    punctuationToken "|" = Just TokPipe
    punctuationToken "^" = Just TokCircumflex
    punctuationToken "<" = Just TokLess
    punctuationToken "<<" = Just TokDblLess
    punctuationToken ">" = Just TokGreater
    punctuationToken ">>" = Just TokDblGreater
    punctuationToken "!" = Just TokBang
    punctuationToken "!=" = Just TokBangEqual
    punctuationToken "&&" = Just TokDblAmp
    punctuationToken "||" = Just TokDblPipe
    punctuationToken "=" = Just TokEqual
    punctuationToken "==" = Just TokDblEqual
    punctuationToken "<=" = Just TokLessEqual
    punctuationToken ">=" = Just TokGreaterEqual
    punctuationToken "+=" = Just TokPlusEqual
    punctuationToken "-=" = Just TokMinusEqual
    punctuationToken "*=" = Just TokAsteriskEqual
    punctuationToken "/=" = Just TokSlashEqual
    punctuationToken "%=" = Just TokPercentEqual
    punctuationToken "&=" = Just TokAmpersandEqual
    punctuationToken "|=" = Just TokPipeEqual
    punctuationToken "^=" = Just TokCircumflexEqual
    punctuationToken "<<=" = Just TokDblLessEqual
    punctuationToken ">>=" = Just TokDblGreaterEqual
    punctuationToken "?" = Just TokQuestion
    punctuationToken ":" = Just TokColon
    punctuationToken "," = Just TokComma
    punctuationToken _ = Nothing

    intSuffix :: String -> Either String IntSuffix
    intSuffix "l" = Right LSuffix
    intSuffix "L" = Right LSuffix
    intSuffix "u" = Right USuffix
    intSuffix "U" = Right USuffix
    intSuffix "lu" = Right LUSuffix
    intSuffix "lU" = Right LUSuffix
    intSuffix "Lu" = Right LUSuffix
    intSuffix "LU" = Right LUSuffix
    intSuffix "ul" = Right LUSuffix
    intSuffix "uL" = Right LUSuffix
    intSuffix "Ul" = Right LUSuffix
    intSuffix "UL" = Right LUSuffix
    intSuffix "" = Right NoSuffix
    intSuffix s = Left ("Invalid integer suffix: " ++ s)