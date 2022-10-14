import Data.Char ( isDigit, isAlpha )
import qualified Data.Map as M

polynomialCleaner :: String -> String
polynomialCleaner [] = []
polynomialCleaner (x:xs)
    | x == ' ' = polynomialCleaner xs
    |x == '*' = polynomialCleaner xs
    | otherwise = x : polynomialCleaner xs

polynomialSimplifier :: String -> [String]
polynomialSimplifier [] = []
polynomialSimplifier (x:xs)
    |x == '+' = polynomialSimplifier xs
    |otherwise = (x : takeWhile (\x -> (x /='+') && (x /='-')) xs) : polynomialSimplifier (dropWhile (\x -> (x /='+') && (x /='-')) xs)

polynomialOrganizer :: String -> [String]
polynomialOrganizer xs = polynomialSimplifier (polynomialCleaner xs)

maybeTail :: [Char] -> String
maybeTail [] = "1"
maybeTail xs = tail xs

internalRepresentation :: [String] -> [(Int,Char ,Int)]
internalRepresentation xs
    |null xs = []
    |all isDigit (head xs) = (read (head xs), ' ', 0) : internalRepresentation (tail xs)
    |head (head xs) == '0' = internalRepresentation (drop 1 xs)
    |isAlpha (head (head xs)) = (1, head (dropWhile isDigit (head xs)), exponent) : internalRepresentation (drop 1 xs)
    |isAlpha ((head xs)!!1) && head (head xs) == '-' = [(-1, (head xs)!!1, exponent)]
    |head (head xs) == '-' = (-read (takeWhile isDigit (pos_mon)) :: Int, head (dropWhile isDigit (pos_mon)), exponent) : internalRepresentation (drop 1 xs)
    |otherwise = (read (takeWhile isDigit (head xs)) :: Int, head (dropWhile isDigit (head xs)), exponent) : internalRepresentation (drop 1 xs)
    where pos_mon = tail (head xs)
          exponent  = read (maybeTail (dropWhile (/= '^') (head xs))) :: Int

sorting :: [(Int,Char,Int)] -> [((Char,Int),[Int])]  -- takes the list with all variables (some repeated) and groups them without repetition
sorting assocs = M.toList (M.fromListWith (\n1 n2 -> [sum(n1 ++ n2)]) [((b,c), [a]) | (a,b,c) <- assocs]) -- [(1,'y',1),(2,'y',1)] becomes [(('y',1),[3])], etc

simply :: [((Char,Int),[Int])] -> [((Char,Int),Int)] -- changes count number from list to normal int
simply xs = [((a,b),c) | ((a,b),[c]) <- xs]

moreSimple :: [((Char,Int),Int)] -> [(Int, Char, Int)] -- changes representation of tuples
moreSimple xs = [(a,b,c) | ((b,c),a) <- xs]

tplToString :: [(Int, Char, Int)] -> [String] -- joins tuple into understandable list of strings
tplToString xs = [(show a) ++ "*" ++ [b] ++ "^" ++ (show c) | (a,b,c) <- xs, a > 1, c > 1] ++ [(show a) ++ "*" ++ [b] | (a,b,c) <- xs, a > 1, c == 1] ++ [[b] | (a,b,c) <- xs, a == 1, c == 1] ++ [[b] ++ "^" ++ (show c) | (a,b,c) <- xs, a == 1, c > 1] ++ [show a | (a,b,c) <- xs, b == ' ']

joiner :: [String] -> String -- joins list of strings into one string
joiner xs = foldr (\a b-> a ++ if b=="" then b else if (head b)=='-' then " - " ++ (drop 1 b) else " + " ++ b) "" xs

derive :: [(Int, Char, Int)] -> Char -> [(Int, Char, Int)]
derive xs car = [(a*c,b,c-1) | (a,b,c) <- xs, c > 1, b == car] ++ [(a,' ',0) | (a,b,c) <- xs, c == 1, b == car] 

normalize :: String -> String -- main function to run option a
normalize poly = joiner (tplToString (moreSimple (simply (sorting (internalRepresentation (polynomialOrganizer poly))))))

add :: String -> String -> String -- main function to run option b
add poly1 poly2 = joiner (tplToString (moreSimple (simply (sorting (internalRepresentation (polynomialOrganizer poly1 ++ polynomialOrganizer poly2))))))

-- multiply :: String -> String -> String
-- multiply poly1 poly2 =

derivative :: String -> Char -> String -- main function to run option d
derivative poly car = joiner (tplToString (moreSimple (simply (sorting (derive (moreSimple (simply (sorting (internalRepresentation (polynomialOrganizer poly))))) car)))))
