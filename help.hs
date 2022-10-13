import qualified Data.Map as M
--
-- type Power = Int
-- type Coefficient = Int
-- data Exp var = Exp var Power deriving (Show, Eq, Ord)
-- data Term var = Term Coefficient (Exp var) deriving Show
--
-- combine :: Ord a => [Term a] -> M.Map (Exp a) Coefficient
-- combine = M.fromListWith (+) . map toTuple
--   where toTuple (Term coef exp) = (exp, coef)
--
-- simplify :: Ord a => [Term a] -> [Term a]
-- simplify = map fromTuple . M.assocs . combine
--   where fromTuple (exp, coef) = Term coef exp
--
-- ziping :: [(Int,Char,Int)] -> Char -> Int -> [(Int, Char, Int)]
--
-- --ziping a b c = [(sum [x | (x,y,z) <- a, y/=b, z/=c], b, c)]
--
-- ziping a b c = [(x,y,z) | (x,y,z) <- a, y/=b && z/=c]

sorting :: [(Int,Char,Int)] -> [((Char,Int),[Int])]  --M.Map (Char,Int) [Int]
sorting assocs = M.toList (M.fromListWith (\n1 n2 -> [sum(n1 ++ n2)]) [((b,c), [a]) | (a,b,c) <- assocs])
