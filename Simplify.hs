---------------------------------------------------------------------
-- File:        Simplify.hs
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


module Simplify
( simplify
) where

import Expression

-- Compute operation on constants
simplify :: Exp -> Exp
simplify e = if f e == e then remake e else simplify (f e)
	where
		f (Add (Const l) (Const r)) = Const (l + r)
		f (Sub (Const l) (Const r)) = Const (l - r)
		f (Mul (Const l) (Const r)) = Const (l * r)
		f (Mul (Frac (Const u) d) (Const r)) = Frac (Const (u * r)) (f d)
		f (Mul (Const l) (Frac (Const u) d)) = Frac (Const (l * u)) (f d)
		f a@(Frac (Const u) (Const d)) = if mod u d == 0 then Const (div u d) else a
		f (Pow (Const b) (Const e)) = Const (b^e)
		f (Minus (Const c)) = Const (negate c)

		f (Add l r)   = Add (f l) (f r)
		f (Sub l r)   = Sub (f l) (f r)
		f (Mul l r)   = Mul (f l) (f r)
		f (Frac u d)  = Frac (f u) (f d)
		f (Pow b e)   = Pow (f b) (f e)
		f (Minus e)   = Minus (f e)
		f (Func id e) = Func id (f e)
		f (Comma l r) = Comma (f l) (f r)
		f a@(Id _)    = a
		f a@(Const _) = a

		remake (Add l r)   = Add (remake l) (remake r)
		remake (Sub l r)   = Sub (remake l) (remake r)
		remake (Mul l r)   = Mul (remake l) (remake r)
		remake (Frac u d)  = Frac (remake u) (remake d)
		remake (Pow b e)   = Pow (remake b) (remake e)
		remake (Minus e)   = Minus (remake e)
		remake (Func id e) = Func id (remake e)
		remake (Comma l r) = Comma (remake l) (remake r)
		remake a@(Id _)    = a
		remake a@(Const c) = if c < 0 then Minus (Const $ negate c) else a

