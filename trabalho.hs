{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
import Data.Char ( isDigit, isAlpha, isLetter )
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

exponentProcessor :: String -> [(Char, Int)]
exponentProcessor [] = []
exponentProcessor (x:xs)
    |  not (null xs) && isDigit (head xs) = (x, read (takeWhile isDigit xs)) : exponentProcessor (dropWhile isDigit xs)
    | otherwise = (x, 1) : exponentProcessor xs

internalRepresentation :: String -> (Int,[(Char,Int)])
internalRepresentation xs
    | all isDigit xs = (read xs, [])
    | head xs == '-' && all isDigit (tail xs) = (-read (tail xs), [])
    | all isLetter xs = (1, [(char,1)| char <- xs])
    | head xs == '-' && all isLetter (tail xs) = (-1, [(char,1)| char <- tail xs])
    | head xs == '-' && not (isDigit (xs !! 1)) = (-1, [tuples | tuples <- exponentProcessor (filter (/='^') (tail xs))])
    | head xs == '-' = (negDigit, [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit(tail xs)))])
    |not (isDigit (head xs)) = (1, [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    |otherwise = (digit, [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    where
        digit = read (takeWhile isDigit xs) :: Int
        negDigit = - read (takeWhile isDigit (tail xs)) :: Int

sorting :: [(Int,[(Char,Int)])] -> [([(Char,Int)],[Int])]  -- takes the list with all variables (some repeated) and groups them without repetition
sorting assocs = M.toList (M.fromListWith (\n1 n2 -> [sum(n1 ++ n2)]) [((b), [a]) | (a,b) <- assocs]) -- [(1,'y',1),(2,'y',1)] becomes [(('y',1),[3])], etc

simply :: [([(Char,Int)],[Int])] -> [([(Char,Int)],Int)] -- changes count number from list to normal int
simply xs = [(a,b) | ((a,[b])) <- xs]

simplifyVariables :: [(Char,Int)] -> [String] -- simplifies internal representation of variables e.g [('x',2),('y',1)] = ["x^2","y"]
simplifyVariables xs = [[a] ++ "^" ++ (show b) | (a,b) <- xs, b > 1] ++ [[a] | (a,b) <- xs, b == 1]

joinVariables :: [String] -> String -- simplifies internal representation of variables e.g ["x^2","y^1"] = "x^2*y"
joinVariables xs = foldr (\a b -> a ++ if b == "" then b else "*" ++ b) "" xs

simpVar :: [(Char,Int)] -> String -- from [('x',2),('y',1)] to "x^2*y"
simpVar xs = joinVariables (simplifyVariables xs)

tplToString :: [([(Char,Int)],Int)] -> [String]
tplToString xs = [(show b) ++ "*" ++ (simpVar a) | (a,b) <- xs, b /= 1] ++ [simpVar a | (a,b) <- xs, b == 1]

joiner :: [String] -> String -- joins list of strings into one string
joiner xs = foldr (\a b-> a ++ if b=="" then b else if (head b)=='-' then " - " ++ (drop 1 b) else " + " ++ b) "" xs

iR :: String -> String -> String -- Internal Representation of the polynomial e.g (2,[(x,2),(y,1)]) = x^2y
iR xs rs = joiner (tplToString (simply (sorting [internalRepresentation x | x <- polynomialOrganizer (xs ++ "+" ++ rs)])))

normalize :: String -> String -- main function to run option a (normalize polynomial)
normalize poly = joiner (tplToString (simply (sorting [internalRepresentation x | x <- polynomialOrganizer poly])))

add :: String -> String -> String -- main function to run option b (add 2 polynomials)
add poly1 poly2 = joiner (tplToString (simply (sorting [internalRepresentation x | x <- polynomialOrganizer (poly1 ++ "+" ++ poly2)])))

{-
moreSimple :: [((Char,Int),Int)] -> [(Int, Char, Int)] -- changes representation of tuples
moreSimple xs = [(a,b,c) | ((b,c),a) <- xs]

tplToString :: [(Int, Char, Int)] -> [String] -- joins tuple into understandable list of strings
tplToString xs = [(show a) ++ "*" ++ [b] ++ "^" ++ (show c) | (a,b,c) <- xs, a > 1, c > 1] ++ [(show a) ++ "*" ++ [b] | (a,b,c) <- xs, a > 1, c == 1] ++ [[b] | (a,b,c) <- xs, a == 1, c == 1] ++ [[b] ++ "^" ++ (show c) | (a,b,c) <- xs, a == 1, c > 1] ++ [show a | (a,b,c) <- xs, b == ' ']

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
-}
