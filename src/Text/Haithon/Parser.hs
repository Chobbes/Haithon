{- Copyright (C) 2015 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE FlexibleContexts #-}

module Text.Haithon.Parser where

import Text.Haithon.Expressions
import Text.Haithon.ParserTypes
import Text.Haithon.Tokens
import Text.Parsec
import Text.Parsec.Indent


-- | Parser is based loosely off of:
--
--     https://docs.python.org/3/reference/grammar.html
pythonParser :: PyParser ()
pythonParser = undefined  -- Currently undefined.


-- | Python statements.
data PyStmt = PyAssign String PyExpr -- Python assignments are more complicated than this.
            | PyPass
            | PyBreak
            | PyContinue
            | PyReturn PyExpr
            | PyIf PyExpr [PyStmt]
            deriving (Show, Eq)


pyNextLine :: PyParser ()
pyNextLine = many1 (lexeme newline) >> return ()


pySimpleStmtLine :: PyParser [PyStmt]
pySimpleStmtLine = do s <- flip sepBy1 (many1 $ reservedOp ";")
                           $ choice [ pyAssign
                                    , pyBreak
                                    , pyContinue
                                    , pyReturn
                                    ]
                      pyNextLine <|> eof
                      return s


pyAssign :: PyParser PyStmt
pyAssign = do assign <- identifier
              reservedOp "="
              value <- pyExpr
              return $ PyAssign assign value


pyBreak :: PyParser PyStmt
pyBreak = reserved "break" >> return PyBreak


pyContinue :: PyParser PyStmt
pyContinue = reserved "continue" >> return PyContinue


pyReturn :: PyParser PyStmt
pyReturn = do reserved "return"
              expr <- pyExpr
              return $ PyReturn expr


pyIf :: PyParser PyStmt
pyIf = pyBlock PyIf pyIfTest pySuite


pyIfTest :: PyParser PyExpr
pyIfTest = do reserved "if"
              value <- pyExpr
              reservedOp ":"
              return value


pySuite :: PyParser [PyStmt]
pySuite = try pySimpleStmtLine
          <|> do pyNextLine
                 s <- block pySimpleStmtLine
                 return $ concat s


pyBlock f a b = withPos $ do
  resA <- a
  resB <- indented >> pySuite
  return $ f resA resB
