module Text.Haithon.Tokens where

import Control.Monad.State as S
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as T


identifier = T.identifier pyTokenParser
reserved = T.reserved pyTokenParser
operator = T.operator pyTokenParser
reservedOp = T.reservedOp pyTokenParser
charLiteral = T.charLiteral pyTokenParser
stringLiteral = T.stringLiteral pyTokenParser
natural = T.natural pyTokenParser
integer = T.integer pyTokenParser
float = T.float pyTokenParser
naturalOrFloat = T.naturalOrFloat pyTokenParser
decimal = T.decimal pyTokenParser
hexadecimal = T.hexadecimal pyTokenParser
octal = T.octal pyTokenParser
symbol = T.symbol pyTokenParser
lexeme = T.lexeme pyTokenParser
whiteSpace = T.whiteSpace pyTokenParser
parens = T.parens pyTokenParser
braces = T.braces pyTokenParser
angles = T.angles pyTokenParser
brackets = T.brackets pyTokenParser
squares = T.squares pyTokenParser
semi = T.semi pyTokenParser
comma = T.comma pyTokenParser
colon = T.colon pyTokenParser
dot = T.dot pyTokenParser
semiSep = T.semiSep pyTokenParser
semiSep1 = T.semiSep1 pyTokenParser
commaSep = T.commaSep pyTokenParser
commaSep1 = T.commaSep1 pyTokenParser


pyTokenParser = makeTokenParser pythonDef

pythonDef :: GenLanguageDef String st (S.State SourcePos)
pythonDef = LanguageDef
            { commentStart = ""
            , commentEnd = ""
            , commentLine = "#"
            , nestedComments = True
            , identStart = letter <|> char '_'
            , identLetter = alphaNum <|> char '_'
            , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
            , opLetter = oneOf  ":!#$%&*+./<=>?@\\^|-~"
            , reservedOpNames = []
            , reservedNames = [ "False"
                              , "class"
                              , "finally"
                              , "is"
                              , "return"
                              , "None"
                              , "continue"
                              , "for"
                              , "lambda"
                              , "try"
                              , "True"
                              , "def"
                              , "from"
                              , "nonlocal"
                              , "while"
                              , "and"
                              , "del"
                              , "global"
                              , "not"
                              , "with"
                              , "as"
                              , "elif"
                              , "if"
                              , "or"
                              , "yield"
                              , "assert"
                              , "else"
                              , "import"
                              , "pass"
                              , "break"
                              , "except"
                              , "in"
                              , "raise"
                              ]
            , caseSensitive = True
            }

