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
    | head xs == '-' && not (isDigit (xs !! 2)) = (-1, [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    | head xs == '-' = (-digit, [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    |not (isDigit (head xs)) = (1, [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    |otherwise = (digit, [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    where
        digit = read (takeWhile isDigit xs) :: Int
        negDigit = - read (tail (takeWhile isDigit xs)) :: Int

iR :: String -> [(Int,[(Char,Int)])] -- Internal Representation of the polynomial e.g (2,[(x,2),(y,1)]) = x^2y
iR xs = [internalRepresentation x | x <- polynomialOrganizer xs]
{-
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
-}