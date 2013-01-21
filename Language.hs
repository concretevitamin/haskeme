-- | Syntax of Scheme, defined for Parsec.

module Language ( whiteSpace
                , integer
                , identifier
                , parens
                ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

-- | TODO:
--      1. Add support for block comments.
--      2. Add other / fix fields
schemeDef :: LanguageDef st
schemeDef =
    emptyDef { T.commentLine        = ";"
             , T.nestedComments     = True
             , T.caseSensitive      = False

             , T.identStart         = letter <|> specialSym
             , T.identLetter        = alphaNum <|> specialSym

             , T.reservedNames = ["[", "]", "{", "}", "|", "#", "\\"] -- TODO: ?
             , T.reservedOpNames = ["[", "]", "{", "}", "|"]
             }

schemeLexer :: T.TokenParser st
schemeLexer = T.makeTokenParser schemeDef

specialSym = oneOf "#!$%&|*+-/:<=>?@^_~"

-- Lexeme parsers -------------------------------------------------------------
whiteSpace = T.whiteSpace schemeLexer
integer = T.integer schemeLexer
identifier = T.identifier schemeLexer
parens = T.parens schemeLexer
