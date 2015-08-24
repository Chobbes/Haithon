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

module Statements where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.Haithon.Parser
import Text.Haithon.Expressions
import Text.Parsec
import Text.Parsec.Indent


statementTests = testGroup "Statements" [ ifSimple
                                        , ifBlock
                                        ]

ifSimple = testCase "If statement followed by simple statements." test
  where test = (runIndent "" $ runParserT pyIf () "" "if blah: x = 2 + 3; x = 7") @?= (Right (PyIf (PyIdentifier "blah") [ PyAssign "x" (PyPlus (PyInt 2) (PyInt 3))
                                                                                                                      , PyAssign "x" (PyInt 7)]))

ifBlock = testCase "If statement followed by a block." test
  where test = (runIndent "" $ runParserT pyIf () "" "if blah:\n x = 2 + 3\n x = 7") @?= (Right (PyIf (PyIdentifier "blah") [ PyAssign "x" (PyPlus (PyInt 2) (PyInt 3))
                                                                                                                      , PyAssign "x" (PyInt 7)]))
        
