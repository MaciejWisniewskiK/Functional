module Reductor where

import Syntax
import qualified Data.Map as Map

extractLeftMost :: Expr -> (Expr, [Expr])
extractLeftMost expr = go expr []
  where
    go :: Expr -> [Expr] -> (Expr, [Expr])
    go (e1 :$ e2) args = go e1 (e2 : args)
    go e1 args        = (e1, args)

subst :: [(Name, Expr)] -> Expr -> Expr
subst subst_list (Var x) = case lookup x subst_list of
    Just e  -> e
    Nothing -> Var x
subst subst_list (e1 :$ e2) = subst subst_list e1 :$ subst subst_list e2

rstep :: DefMap -> Expr -> Maybe Expr
rstep defmap expr =
    case expr of
        -- One variable expression, either a pattern or stop
        Var name ->
            case Map.lookup name defmap of
                Just (Def _ [] rhs) -> Just rhs
                _                    -> Nothing
        -- Longer expression
        _ ->
            let (leftMost, args) = extractLeftMost expr
            in case leftMost of
                Var name ->
                    case Map.lookup name defmap of
                        Just (Def _ args2 rhs)
                          | length args2 <= length args ->
                              let (matchedArgs, restArgs) = splitAt (length args2) args -- take the first n arguments
                                  subst_list = zip args2 matchedArgs                    -- create a substitution list
                                  replaced = subst subst_list rhs                       -- replace the n arguments with the rhs
                                  resultExpr = foldl (:$) replaced restArgs             -- fold into an expression
                              in Just resultExpr
                        _ -> recursion
                _ -> recursion
  where
    -- If the leftmost expression is not a pattern or doesn't have enough arguments, try recursion
    recursion =
        case expr of
            e1 :$ e2 ->
                case rstep defmap e1 of
                    Just e1' -> Just (e1' :$ e2)            -- try reducing e1
                    Nothing  -> case rstep defmap e2 of
                        Just e2' -> Just (e1 :$ e2')        -- try reducing e2
                        Nothing  -> Nothing
            _ -> Nothing

rpath :: DefMap -> Expr -> [Expr]
rpath defMap expr = acc (Just expr)
    where
        acc :: Maybe Expr -> [Expr]
        acc Nothing = []
        acc (Just expr) = expr : acc (rstep defMap expr)