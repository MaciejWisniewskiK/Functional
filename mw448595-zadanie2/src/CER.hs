module CER where

import qualified Data.Map as Map
import Parser
import Syntax
import Reductor
import Syntax

instance Show Expr where
    showsPrec _ (Var name) = showString name
    showsPrec prec (exp1 :$ exp2) = showParen (prec > appPrec) (showsPrec appPrec exp1 . showChar ' ' . showsPrec (appPrec + 1) exp2)
        where appPrec = 10

instance Show Def where
    showsPrec _ (Def name args rhs) = 
        showString name .
        showChar ' ' .
        showString (unwords args) .
        showString " = " .
        shows rhs

instance Show Prog where
    showsPrec _ (Prog defs) = showString (unlines (map show defs))

runString :: String -> IO ()
runString str = do
    let prog = fromHsString str
    print prog
    putStrLn "------------------------------------------------------------"
    let defMap = buildDefMap prog
    case Map.lookup "main" defMap of
        Just (Def _ args expr) -> do
            printpath (rpath defMap expr)
        Nothing -> error "Main combinator not found"

printpath :: [Expr] -> IO ()
printpath [] = return ()
printpath (expr:rest) = do
    print expr
    printpath rest