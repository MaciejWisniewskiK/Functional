module Reductor where

import Syntax
import qualified Data.Map as Map

rstep :: DefMap -> Expr -> Maybe Expr
-- If the expression is a variable, we check if it is a combinator with no arguments.
rstep defMap (Var name) = 
    case Map.lookup name defMap of
        Just (Def _ [] rhs) -> Just rhs
        _ -> Nothing

-- If the leftmost expression is just one name, we check if it is a combinator.
rstep defMap (Var name :$ exp2) = 
    case Map.lookup name defMap of
        Just (Def _ args rhs) -> tryApplyExpr args exp2 rhs
        _ -> Nothing

-- If the leftmost expression is a nested expression, we try to recursively reduce it.
-- If we can't reduce it, we try to recursively reduce the right side.
rstep defMap (exp1 :$ exp2) = 
    case rstep defMap exp1 of
        Just exp1' -> Just (exp1' :$ exp2)
        Nothing -> 
            case rstep defMap exp2 of
                Just exp2' -> Just (exp1 :$ exp2')
                Nothing -> Nothing

-- Try to replace all occurences of all arguments with their substitutions.
-- Returns nothing if there are not enough arguments.
tryApplyExpr :: [Pat] -> Expr -> Expr -> Maybe Expr
tryApplyExpr args exp2 rhs = case getFirstNExpressions (length args) exp2 of
    Nothing -> Nothing
    Just subs -> Just (applyExpr args subs rhs)

-- Replace all occurences of all arguments with their substitutions.
applyExpr :: [Pat] -> [Expr] -> Expr -> Expr
applyExpr [] [] rhs = rhs
applyExpr (arg:args) (sub:subs) rhs = applyExpr args subs (applyOneArg arg sub rhs)

-- Replace all occurences of an argument with substitution.
applyOneArg :: Pat -> Expr -> Expr -> Expr
applyOneArg pat sub rhs = go rhs
    where
        go :: Expr -> Expr
        go (Var name) = case name == pat of
            True -> sub
            False -> Var name
        go (e1 :$ e2) = go e1 :$ go e2

-- Gets the first N expressions from the expression and returns them as a list.
-- Returns Nothing if there are not enough expressions.
getFirstNExpressions :: Int -> Expr -> Maybe [Expr]
getFirstNExpressions 0 _ = Just []
getFirstNExpressions 1 (Var name) = Just [Var name]
getFirstNExpressions n (Var name) = Nothing
getFirstNExpressions n (x :$ xs) = 
    case getFirstNExpressions (n - 1) xs of
        Nothing -> Nothing
        Just xs' -> Just (x : xs')


rpath :: DefMap -> Expr -> [Expr]
rpath defMap expr = acc (Just expr)
    where
        acc :: Maybe Expr -> [Expr]
        acc Nothing = []
        acc (Just expr) = expr : acc (rstep defMap expr)