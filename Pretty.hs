---------------------------------------------------------------------
-- File:        Pretty.hs
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


module Pretty
(
) where

import Expression

instance Show Exp
	where show = showExp

type Bind = (Int, [String])

space = " "
slash = "-"

pad n = concat $ replicate n space
bar n = concat $ replicate n slash

-- monus
(|-) :: (Num a, Ord a) => a -> a -> a
(|-) a b = if a - b < 0 then 0 else a - b

hbind :: Int -> Bind -> Bind -> Bind
hbind n (i, l) (j, r) = (n, f)
	where
		f = map (\(a, b) -> a ++ b) $ zip ll rr

		ll = mkrect (j |- i) ((length r - j) |- (length l - i)) l
		rr = mkrect (i |- j) ((length l - i) |- (length r - j)) r

		mkrect m n s = replicate m (plh s) ++ s ++ replicate n (plh s)

		plh s = pad $ length $ head s

vbind :: Int -> Bind -> Bind -> Bind
vbind n il@(i, l) jr@(j, r) = (n, f)
	where
		f = ll ++ rr

		ll = mkrect (j |- i) ((width jr - j) |- (width il - i)) l
		rr = mkrect (i |- j) ((width il - i) |- (width jr - j)) r

		mkrect m n s = map ((++) (pad m) . (flip (++)) (pad n)) s

-- vertical left
(!<) :: Bind -> Bind -> Bind
(!<) (i, u) (j, d) = vbind 0 (0, u) (0, d)

-- vertical center
(!:) :: Bind -> Bind -> Bind
(!:) uu@(_, u) dd@(_, d) =
			let
				wu = width uu
				wd = width dd
			in if wu < wd
			then vbind 0 (0, u) (div (wd - wu) 2, d)
			else vbind 0 (div (wu - wd) 2, u) (0, d)

-- horizontal
(-+) :: Bind -> Bind -> Bind
(-+) ll@(i, _) rr@(j, _) = hbind (max i j) ll rr

-- horizontal up
(-^) :: Bind -> Bind -> Bind
(-^) (i, l) rr@(_, r) = hbind (i + length r) (0, l) (up rr)

-- horizontal center
(-:) :: Bind -> Bind -> Bind
(-:) ll@(i, l) rr@(j, r) =
			let
				wl = height ll
				wr = height rr
			in if wl < wr
			then hbind (max i j) ll (div (height rr) 2, r)
			else hbind (max i j) (div (height ll) 2, l) rr

-- horizontal down
(-.) :: Bind -> Bind -> Bind
(-.) (i, l) rr@(_, r) = hbind i (pred $ length l, l) rr

up :: Bind -> Bind
up bb@(i, b) = (height bb, b ++ [pad $ width bb])

width :: Bind -> Int
width (_, b) = length $ head b

height :: Bind -> Int
height (_, b) = length b

-- Check if need for parenthesis to ensure priority
solid :: Exp -> Bool
solid (Id _)            = True
solid (Const _)         = True
solid (Func _ _)        = True
solid (Minus (Id _))    = True
solid (Minus (Const _)) = True
solid (Minus (Pow _ _)) = True
solid (Minus e)         = False
solid _                 = False

showExp :: Exp -> String
showExp e = unpack $ out e
	where
		out (Add l r)   = out l -+ op " + "  -+ out r
		out (Sub l r)   = out l -+ op " - "  -+ out r
		out (Comma l r) = out l -. op ", "   -+ out r

		out (Mul l@(Pow _ _) r@(Pow _ _)) = out l -: op " " -: out r
		out (Mul l@(Pow _ _) r)           = out l -: op " " -: sbracket r
		out (Mul l r@(Pow _ _))           = sbracket l -: op " " -: out r
		out (Mul l r)                     = sbracket l -: op " " -: sbracket r

		out (Pow b@(Minus _) e) = bracket b -^ sbracket e
		out (Pow b@(Pow _ _) e) = out b -^ sbracket e
		out (Pow b e)           = sbracket b -^ sbracket e

		out (Frac u d) = repack (height (out u)) $
				let
					wu = width (out u)
					wd = width (out d)
				in if wu < wd
				then out u !: op (bar $ wd) !< out d
				else out u !< op (bar $ wu) !: out d

		out (Id id)     = op id
		out (Const i)   = op $ show i
		out (Func id e) = op (id ++ " ") -: sbracket e
		out (Minus e)   = op "-" -: sbracket e

		bo i 1 = op "("
		bo i h = (i, ("/" : (replicate (h - 2) "|")) ++ ["\\"])
		bc i 1 = op ")"
		bc i h = (i, ("\\" : (replicate (h - 2) "|")) ++ ["/"])

		op o = (0, [o])

		bracket e  = bo (fe e) (he e) -+ out e -+ bc (fe e) (he e)
		sbracket e = if solid e then out e else bracket e

		fe e = fst $ out e
		he e = height $ out e

		repack n (_, b) = (n, b)
		unpack (_, l)   = concatMap ((flip (++)) "\n") (init l) ++ (last l)


