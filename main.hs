---------------------------------------------------------------------
-- File:        main.hs
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


import Expression
import Parse
import Simplify
import Pretty

import System.Environment
import System.Exit

die :: String -> IO()
die err = do
	putStrLn $ "ERROR: " ++ err
	exitWith (ExitFailure 1)

main = do
	args <- getArgs

	if length args /= 1
	then die "Exactly 1 expression needed"
	else putStrLn $ show $ simplify $ f $ parseExp $ head args

f :: Either String Exp -> Exp
f (Left err) = error $ "ERROR: " ++ err
f (Right e)  = e

