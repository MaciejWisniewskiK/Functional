module Syntax where

import qualified Data.Map as Map

type Name = String
data Def = Def { defMatches :: [Match] }
data Match = Match
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  ::Expr
    }

infixl 9 :$
data Expr
    = Var Name
    | Con Name
    | Expr :$ Expr
data Pat = PVar Name | PApp Name [Pat]

newtype Prog = Prog {progDefs :: [Def]}

type DefMap = Map.Map Name Def