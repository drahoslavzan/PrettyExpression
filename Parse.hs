---------------------------------------------------------------------
-- File:        Parse.hs
-- Author:      Drahoslav Zan
-- Date:        Jun 01 2012
-- Project:     Symbolic Expression Formating in ASCII (SEFA)
---------------------------------------------------------------------
-- Copyright (C) 2012 Drahoslav Zan
--
-- This file is part of SEFA.
--
-- SEFA is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- SEFA is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with SEFA. If not, see <http:--www.gnu.org/licenses/>.
---------------------------------------------------------------------
-- vim: set nowrap sw=2 ts=2


module Parse
( parseExp
) where

import Expression

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

lexer = Token.makeTokenParser languageDef

languageDef = emptyDef { Token.reservedOpNames = ["+", "-", "*", "/", "^"] }

identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer

expression :: Parser Exp
expression = buildExpressionParser ops term
	where
		ops =
			[ [Prefix (reservedOp "-" >> return Minus)          ]
			, [Infix  (reservedOp "^" >> return Pow)   AssocLeft]
			, [Infix  (reservedOp "*" >> return Mul)   AssocLeft]
			, [Infix  (reservedOp "/" >> return Frac)  AssocLeft]
			, [Infix  (reservedOp "+" >> return Add)   AssocLeft]
			, [Infix  (reservedOp "-" >> return Sub)   AssocLeft]
			]

term = parens expression
	<|> liftM Id identifier
	<|> liftM Const integer

expr :: Parser Exp
expr = whiteSpace >> expression

parseExp :: String -> Either String Exp
parseExp str =
	case parse expr "" str of
		Left e  -> Left $ show e
		Right r -> Right r

