module Lexer
    ( lexer
    , Token(..)
    , Position(..)
    , Span(..)
    , LocatedToken
    ) where

import Data.List (mapAccumL)

data Token
    = TokInt Int      -- integer literal
    | TokIdent String -- identifier
    | TokKeyInt
    | TokKeyVoid
    | TokKeyReturn
    | TokOpenParen  -- (
    | TokCloseParen -- )
    | TokOpenBrace  -- {
    | TokCloseBrace -- }
    | TokSemicolon  -- ;

    deriving (Show, Eq)

data Position = Position {line::Int, column::Int}
    deriving (Eq, Ord)
data Span = Span { start :: Position, end :: Position }
    deriving (Eq, Show)

instance Show Position where
    show (Position l c) = show l ++ ":" ++ show c

type LocatedChar = (Char, Position)
type LocatedToken = (Token, Span)

enum_src :: String -> [LocatedChar]
-- mapAccumL :: step_function -> initial_accumulator -> input_list -> (final_accumulator, output_list)
-- snd :: (final_accumulator, output_list) -> output_list
enum_src = snd . mapAccumL step (1,1)  
    where
        step :: (Int, Int) -> Char -> ((Int, Int), LocatedChar) 
        -- step :: pre_acumulator -> input_element -> (post_accumulator, output_element)
        -- the accumulator is a tuple (line, column) for the next character to be emitted
        step (line, col) '\n' = ((line+1, 1), ('\n', Position line col))
        step (line, col) c = ((line, col+1), (c, Position line col))

data LexerState 
    = LS_Start
    | LS_Ident Position String
    | LS_Integer Position Int 

lexer :: String -> Either String [LocatedToken]
lexer = fmap reverse . snd . foldl step (LS_Start, Right []) . enum_src . add_eof
    where
        step :: (LexerState, Either String [LocatedToken]) -> LocatedChar -> (LexerState, Either String [LocatedToken])
        step (state, Left x) _ = (state, Left x)
        step (LS_Start, Right tokens) (c, pos)
            | c `elem` whitespace  = (LS_Start, Right tokens)
            | c `elem` digits      = (LS_Integer pos (read [c]), Right tokens)
            | c `elem` id_start    = (LS_Ident pos [c], Right tokens)
            | c == '('             = (LS_Start, Right ((TokOpenParen,  Span pos pos):tokens)) 
            | c == ')'             = (LS_Start, Right ((TokCloseParen, Span pos pos):tokens)) 
            | c == '{'             = (LS_Start, Right ((TokOpenBrace,  Span pos pos):tokens)) 
            | c == '}'             = (LS_Start, Right ((TokCloseBrace, Span pos pos):tokens)) 
            | c == ';'             = (LS_Start, Right ((TokSemicolon,  Span pos pos):tokens)) 
            | otherwise            = (LS_Start, Left ("Unexpected " ++ [c]))
        step (LS_Integer start n, Right tokens) (c, pos)
            | c `elem` digits      = (LS_Integer start (10*n+(read [c])), Right tokens)
            | c `elem` id_continue = (LS_Integer start n, Left ("Unexpected in Integer " ++ [c]))
            | otherwise            = step (LS_Start, Right ((TokInt n, Span start pos):tokens)) (c, pos)
        step (LS_Ident start ident, Right tokens) (c, pos)
            | c `elem` id_continue = (LS_Ident start (ident++[c]), Right tokens)
            | otherwise            = step (LS_Start, Right ((map_keyword ident, Span start pos):tokens)) (c, pos)
        
        map_keyword "int" = TokKeyInt
        map_keyword "void" = TokKeyVoid
        map_keyword "return" = TokKeyReturn
        map_keyword ident = TokIdent ident



        add_eof source = source ++ "\0"

        whitespace = " \t\n\r\f\v\0"
        id_start = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
        digits = "0123456789"
        id_continue = id_start ++ digits