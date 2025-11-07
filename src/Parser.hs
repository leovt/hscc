module Parser
    ( parser
    ) where

import Lexer (LocatedToken)

parser :: [LocatedToken] -> Either String ()
parser tokens = Left "Parser not implemented yet"

