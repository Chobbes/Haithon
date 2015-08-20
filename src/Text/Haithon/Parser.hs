{- Copyright (C) 2014 Calvin Beck

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

module Text.Haithon.Parser where

import Text.Haithon.Expressions
import Text.Haithon.ParserTypes
import Text.Haithon.Tokens
import Text.Parsec


-- | Parser is based loosely off of:
--
--     https://docs.python.org/3/reference/grammar.html
pythonParser :: PyParser ()
pythonParser = undefined  -- Currently undefined.


-- | Simple Python statements.
data PySimpleStmt = PyAssign String PyExpr -- Python assignments are more complicated than this.
                  | PyPass
                  | PyBreak
                  | PyContinue
                  | PyReturn PyExpr
                  deriving (Show)


pySimpleStmtLine :: PyParser [PySimpleStmt]
pySimpleStmtLine = flip sepBy1 (reservedOp ";")
                   $ choice [ pyAssign
                            , pyBreak
                            , pyContinue
                            , pyReturn
                            ]


pyAssign :: PyParser PySimpleStmt
pyAssign = do assign <- identifier
              reservedOp "="
              value <- pyExpr
              return $ PyAssign assign value


pyBreak :: PyParser PySimpleStmt
pyBreak = reserved "break" >> return PyBreak


pyContinue :: PyParser PySimpleStmt
pyContinue = reserved "continue" >> return PyContinue


pyReturn :: PyParser PySimpleStmt
pyReturn = do reserved "return"
              expr <- pyExpr
              return $ PyReturn expr


