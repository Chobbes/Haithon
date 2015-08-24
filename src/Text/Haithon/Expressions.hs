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

module Text.Haithon.Expressions where

import Text.Haithon.ParserTypes
import Text.Haithon.Tokens
import Text.Parsec hiding (State)
import Text.Parsec.Expr
import Control.Monad.State as S


-- | Python expressions.
data PyExpr = PyPow PyExpr PyExpr
            | PyNeg PyExpr
            | PyPos PyExpr
            | PyMult PyExpr PyExpr
            | PyDiv PyExpr PyExpr
            | PyPlus PyExpr PyExpr
            | PySub PyExpr PyExpr
            | PyInt Integer
            | PyDouble Double
            | PyString String
            | PyIdentifier String
            deriving (Show, Eq)


pyExpr :: PyParser PyExpr
pyExpr = buildExpressionParser table term


term :: PyParser PyExpr
term = parens pyExpr
       <|> fmap (either PyInt PyDouble) naturalOrFloat
       <|> liftM PyString stringLiteral
       <|> liftM PyIdentifier identifier


-- | https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Expr.html
-- is pretty much exactly what we need here.
table :: OperatorTable String st (S.State SourcePos) PyExpr
table = [ [binary "**" PyPow AssocLeft]
        , [prefix "-" PyNeg, prefix "+" PyPos]
        , [binary "*" PyMult AssocLeft, binary "/" PyDiv AssocLeft]
        , [binary "+" PyPlus AssocLeft, binary "-" PySub AssocLeft]
        ]


binary  name fun assoc = Infix (do { reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do { reservedOp name; return fun })
postfix name fun       = Postfix (do { reservedOp name; return fun })
