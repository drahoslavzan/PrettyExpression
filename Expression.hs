---------------------------------------------------------------------
-- File:        Expression.hs
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


module Expression
( Exp(..)
, test
) where

data Exp
	= Add Exp Exp
	| Sub Exp Exp
	| Mul Exp Exp
	| Frac Exp Exp
	| Pow Exp Exp
	| Minus Exp
	| Func String Exp
	| Comma Exp Exp 	-- Func with more params
	| Id String
	| Const Integer
	deriving (Eq)

test = Add (Frac (Add (Pow (Id "x") (Const 2))
	(Add (Pow (Add (Mul (Const 3) (Id "x")) (Const 7))
	(Pow (Frac (Const 3) (Func "cos" (Id "v"))) (Id "x"))) (Const 1)))
	(Pow (Const 72) (Pow (Sub (Const 333) (Id "u")) (Const 2))))
	(Frac (Func "sin2" (Mul (Minus (Pow (Minus (Const 3)) (Pow (Const 4) (Id "u"))))
	(Pow (Const (2)) (Const 2)))) (Const 10))

