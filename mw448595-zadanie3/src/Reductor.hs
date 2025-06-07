module Reductor where

import Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad (zipWithM, when)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Applicative ( (<|>) )

-- | Decompose an application chain into its left-most head and the list of
--   arguments in *left-to-right* order.
extractLeftMost :: Expr -> (Expr, [Expr])
extractLeftMost = go []
  where
    go acc (e1 :$ e2) = go (e2 : acc) e1   -- prepend, O(1)
    go acc h          = (h, acc)           -- ‘acc’ is already in order

-- | Substitute variables inside an expression using an association list.
subst :: [(Name, Expr)] -> Expr -> Expr
subst env (Var n)      = maybe (Var n) id (lookup n env)
subst _   c@(Con _)    = c
subst env (e1 :$ e2)   = subst env e1 :$ subst env e2

-- | Pattern matching utilities.
matchPat :: Pat -> Expr -> Maybe [(Name, Expr)]
matchPat (PVar n) ex = Just [(n, ex)]
matchPat (PApp con ps) ex = do
  let (hd, args) = extractLeftMost ex
  case hd of
    Con c | c == con && length ps == length args ->
      fmap concat (zipWithM matchPat ps args)
    _ -> Nothing

matchPatterns :: [Pat] -> [Expr] -> Maybe [(Name, Expr)]
matchPatterns ps es
  | length ps /= length es = Nothing
  | otherwise              = fmap concat (zipWithM matchPat ps es)

-- | Try to apply one definition clause.
reduceHead :: Def -> [Expr] -> Maybe Expr
reduceHead (Def matches) args = listToMaybe . mapMaybe tryMatch $ matches
  where
    tryMatch (Match _ ps rhs) = do
      sub <- matchPatterns ps (take (length ps) args)
      let replaced = subst sub rhs
      pure (foldl (:$) replaced (drop (length ps) args))

-- | One outer‑most, left‑most reduction step.
rstep :: DefMap -> Expr -> Maybe Expr
rstep dmap expr =
  case extractLeftMost expr of
    (Var v, args) ->
      -- try to fire a rule for the head;
      -- if that fails, keep searching inside the term
      (Map.lookup v dmap >>= (`reduceHead` args))
        <|> descend expr
    _ -> descend expr
  where
    descend (e1 :$ e2) =
          (:$ e2) <$> rstep dmap e1
      <|> (e1  :$) <$> rstep dmap e2
    descend _ = Nothing

-- | Zipper context kept in the state (not needed for the basic reducer
--   below, but stored for future extensions).
data Context = Top | L Context Expr | R Expr Context

plug :: Context -> Expr -> Expr
plug Top e       = e
plug (L c r) e   = plug c (e :$ r)
plug (R l c) e   = plug c (l :$ e)

-- | State carried along the reduction.
data RedState = RS
  { current :: Expr      -- ^ current focus
  , ctxt    :: Context   -- ^ context zipper
  , fuel    :: Int       -- ^ steps remaining
  , hist    :: [(Context, Expr)]    -- ^ reduction history (reversed)
  }

type RedM = State RedState ()

record :: RedM
record = do
  st <- get
  modify (\s -> s { hist = (ctxt st, current st) : hist s })

-- | a single  outer-most, left-most step with context
rstepCtx :: DefMap -> Context -> Expr -> Maybe (Context,Expr)
rstepCtx dmap k e =
  case extractLeftMost e of
    (Var v, args) ->
        ((,)
          k
        <$> (Map.lookup v dmap >>= (`reduceHead` args)))  <|> desc
    _ -> desc
  where
    desc = case e of
      l :$ r ->   rstepCtx dmap (L k r) l
              <|> rstepCtx dmap (R l k) r
      _      -> Nothing

loop :: DefMap -> RedM
loop dmap = do
  record
  st <- get
  let term = plug (ctxt st) (current st)
  when (fuel st > 0) $
    case rstepCtx dmap Top term of
      Just (k', e') -> do
        modify $ \s -> s { current = e'
                         , ctxt    = k'
                         , fuel    = fuel s - 1 }
        loop dmap
      Nothing -> return ()

-- | Produce the full reduction path, limited to 10 steps to avoid
--   non‑terminating computations.
rpath :: DefMap -> Expr -> [(Context,Expr)]
rpath dmap e = reverse . hist $ execState (loop dmap) initSt
  where
    initSt = RS { current = e, ctxt = Top, fuel = 10, hist = [] }
