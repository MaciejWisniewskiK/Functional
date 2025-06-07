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
        -- f ps = rhs
        declToListOfDef (HsFunBind ms@(m:_)) =
            [Def { defMatches = map hsMatchToMatch ms }]
            where
                hsMatchToMatch (HsMatch _ n ps rhs _) =
                    Match { matchName = nameToStr n
                            , matchPats = map hsPatToPat ps
                            , matchRhs  = rhsToExpr rhs }
        
        -- x = rhs
        declToListOfDef (HsPatBind _ (HsPVar n) rhs _) =
            [Def { defMatches =
                    [ Match { matchName = nameToStr n
                            , matchPats = []
                            , matchRhs  = rhsToExpr rhs } ] }]

        declToListOfDef _ = []           -- ignore everything else


        hsExpToExpr :: HsExp -> Expr
        hsExpToExpr (HsApp e1 e2)     = hsExpToExpr e1 :$ hsExpToExpr e2
        hsExpToExpr (HsVar  n)        = Var (qNameToStr n)
        hsExpToExpr (HsCon  n)        = Con (qNameToStr n)
        hsExpToExpr (HsParen e)       = hsExpToExpr e
        hsExpToExpr _ = error "Unsupported expression"

        rhsToExpr :: HsRhs -> Expr
        rhsToExpr (HsUnGuardedRhs e) = hsExpToExpr e
        rhsToExpr _ = error "Unsupported right-hand side"

        hsPatToPat :: HsPat -> Pat
        hsPatToPat (HsPVar  n)     = PVar (nameToStr n)
        hsPatToPat (HsPApp qn ps)  = PApp (qNameToStr qn) (map hsPatToPat ps)
        hsPatToPat (HsPParen p)    = hsPatToPat p
        hsPatToPat _               = error "Unsupported pattern"

        qNameToStr :: HsQName -> String
        qNameToStr (UnQual name)    = nameToStr name
        qNameToStr _ = error "Unsupported name"

        nameToStr :: HsName -> String
        nameToStr (HsIdent str) = str
        nameToStr other       = error "Unsupported name"

buildDefMap :: Prog -> DefMap
buildDefMap (Prog defs) = Map.fromList (map toPair defs)
  where
    toPair d@(Def (m:_)) = (matchName m, d)
