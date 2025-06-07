module CER where

import qualified Data.Map as Map
import qualified Data.List as List
import Parser
import Syntax
import Reductor

instance Show Context where
  showsPrec _ Top        = showString "{ • }"

  showsPrec p (L k e)    = showsPrec p k
                          . showString "{ "
                          . showsPrec p e
                          . showString " • }"

  showsPrec p (R e k)    = showsPrec p k
                          . showString "{ • "
                          . showsPrec p e
                          . showString " }"

instance Show Pat where
  showsPrec _ (PVar n)     = showString n
  showsPrec _ (PApp c [])   = showString c
  showsPrec _ (PApp c ps)  =
        showChar '('
      . showString c
      . showChar ' '
      . showString (unwords (map show ps))
      . showChar ')'

instance Show Expr where
  showsPrec _ (Var n)      = showString n
  showsPrec _ (Con n)      = showString n
  showsPrec p (e1 :$ e2)   =
        showParen (p > appP) (showsPrec appP e1 . showChar ' ' . showsPrec (appP+1) e2)
    where appP = 10

instance Show Match where
  showsPrec _ (Match n ps rhs) =
        showString n
      . ( if null ps
            then id
            else showChar ' ' . showString (unwords (map show ps)) )
      . showString " = "
      . shows rhs

instance Show Def where
  showsPrec p (Def ms) =
        foldr (.) id
              (List.intersperse (showChar '\n') (map (showsPrec p) ms))

instance Show Prog where
    showsPrec _ (Prog defs) = showString (unlines (map show defs))


runString :: String -> IO ()
runString str = do
  let prog   = fromHsString str
  print prog
  putStrLn "------------------------------------------------------------"
  let defMap = buildDefMap prog
  case Map.lookup "main" defMap of
    Just (Def (Match _ _ expr : _)) ->
      printpath (rpath defMap expr)
    _ ->
      error "Main combinator not found"

showsMarked :: Context -> Expr -> ShowS
showsMarked Top        e = showChar '{' . shows e . showChar '}'
showsMarked (L k r)    e = showsMarked k e . showChar ' ' . shows r
showsMarked (R l k)    e = shows l . showChar ' ' . showsMarked k e

printpath :: [(Context,Expr)] -> IO ()
printpath = mapM_ (\(k,e) -> putStrLn (showsMarked k e ""))