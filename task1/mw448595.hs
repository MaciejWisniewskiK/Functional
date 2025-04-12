-- Maciej Wiśniewski
-- mw448595

data Expr = S | K | I | B 
          | Expr :$ Expr 
          | X | Z | V Int  
          deriving (Show, Read)

infixl 9 :$

prettyExpr :: Expr -> String
prettyExpr = pretty 0
    where
        pretty :: Int -> Expr -> String
        pretty _ S       = "S"
        pretty _ K       = "K"
        pretty _ I       = "I"
        pretty _ B       = "B"
        pretty _ X       = "x"
        pretty _ Z       = "z"
        pretty _ (V n)   = "v" ++ show n
        pretty p (e1 :$ e2) =
            let s = pretty 0 e1 ++ " " ++ pretty 1 e2
            in if p > 0 then "(" ++ s ++ ")" else s

rstep :: Expr -> Expr
rstep (S :$ x :$ y :$ z) = x :$ z :$ (y :$ z)
rstep (K :$ x :$ y) = x
rstep (I :$ x) = x
rstep (B :$ x :$ y :$ z) = x :$ (y :$ z)
rstep (f :$ x) = rstep f :$ rstep x
rstep x = x


rpath :: Expr -> [Expr]
rpath x = take 30 (iterate rstep x)

printPath :: Expr -> IO ()
printPath x = putStrLn (unlines (map prettyExpr (rpath x)))

--test1 = S :$ K :$ K :$ X
--twoB = S :$ B :$ I
--threeB = S :$ B :$ (S :$ B :$ I)
--test3 = threeB :$ X :$ Z
--omega = ((S :$ I) :$ I) :$ ((S :$ I) :$ I)
--kio = K :$ I :$ omega
--add = B :$ S :$ (B :$ B)

--main = do
--    printPath (S :$ K :$ K :$ X)
--    printPath (add :$ twoB :$ threeB :$ X :$ Z)
