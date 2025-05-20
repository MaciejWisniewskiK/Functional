module Syntax where

import qualified Data.Map as Map

data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name
type Name = String

newtype Prog = Prog {progDefs :: [Def]}

type DefMap = Map.Map Name Def