module Parser where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import qualified Data.Map as Map
import Syntax

fromHsString :: String -> Prog
fromHsString str =
    case parseModule str of
        ParseOk hsModule        -> Prog (fromHsModule hsModule)
        ParseFailed _ _         -> error "Parse error"

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult parseResult =
    case parseResult of
        ParseOk hsModule        -> fromHsModule hsModule
        ParseFailed _ _         -> error "Parse error"

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ declarations) = concatMap declToListOfDef declarations
    where
        -- Returns a list of one Def if the declaration is to be included or an empty list if it is to be ignored.
        declToListOfDef :: HsDecl -> [Def]
        declToListOfDef (HsFunBind (HsMatch _ name args rhs _ : _)) = [Def (nameToStr name) (map patToStr args) (rhsToExpr rhs)] -- Functions (Combinators with arguments)
        declToListOfDef (HsPatBind _ pat rhs _) = [Def (patToStr pat) [] (rhsToExpr rhs)] -- Patterns (Combinators without arguments)
        declToListOfDef _ = [] -- Ignore other

        hsExpToExpr :: HsExp -> Expr
        hsExpToExpr (HsApp e1 e2)     = hsExpToExpr e1 :$ hsExpToExpr e2
        hsExpToExpr (HsVar  n)        = Var (qNameToStr n)
        hsExpToExpr (HsCon  n)        = Var (qNameToStr n)
        hsExpToExpr (HsParen e)       = hsExpToExpr e
        hsExpToExpr _ = error "Unsupported expression"

        rhsToExpr :: HsRhs -> Expr
        rhsToExpr (HsUnGuardedRhs e) = hsExpToExpr e
        rhsToExpr _ = error "Unsupported right-hand side"

        patToStr :: HsPat -> String
        patToStr (HsPVar name)      = nameToStr name

        qNameToStr :: HsQName -> String
        qNameToStr (UnQual name)    = nameToStr name
        qNameToStr _ = error "Unsupported name"

        nameToStr :: HsName -> String
        nameToStr (HsIdent str) = str
        nameToStr other       = error "Unsupported name"

buildDefMap :: Prog -> DefMap
buildDefMap (Prog defs) = Map.fromList (getPairList defs)
    where
        getPairList :: [Def] -> [(Name, Def)]
        getPairList [] = []
        getPairList ((Def name args rhs) : rest) = (name, Def name args rhs) : getPairList rest