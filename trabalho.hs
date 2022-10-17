{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
import Data.Char ( isDigit, isAlpha, isLetter )
import qualified Data.Map as M
import Distribution.Verbosity (normal)
import Control.Monad (when)
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

maybeTail :: [Char] -> String -- in case list is empty, return "1", otherwise return tail of list
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
sorting assocs = M.toList (M.fromListWith (\n1 n2 -> [sum(n1 ++ n2)]) [((b), [a]) | (a,b) <- assocs]) -- [(1,('y',1)),(2,('y',1))] becomes [(('y',1),[3])], etc


simply :: [([(Char,Int)],[Int])] -> [([(Char,Int)],Int)] -- changes count number from list to normal int
simply xs = [(a,b) | ((a,[b])) <- xs]

simplifyVariables :: [(Char,Int)] -> [String] -- simplifies internal representation of variables e.g [('x',2),('y',1)] = ["x^2","y"]
simplifyVariables xs = [[a] ++ "^" ++ (show b) | (a,b) <- xs, b > 1] ++ [[a] | (a,b) <- xs, b == 1]

joinVariables :: [String] -> String -- simplifies internal representation of variables e.g ["x^2","y"] = "x^2*y"
joinVariables xs = foldr (\a b -> a ++ if b == "" then b else "*" ++ b) "" xs

simpVar :: [(Char,Int)] -> String -- from [('x',2),('y',1)] to "x^2*y"
simpVar xs = joinVariables (simplifyVariables xs)

tplToString :: [([(Char,Int)],Int)] -> [String] -- [([(y,1)],1)]
tplToString xs = [(show b) ++ "*" ++ (simpVar a) | (a,b) <- xs, b /= 1, a /= []] ++ [simpVar a | (a,b) <- xs, b == 1, a /= []] ++ [(show b) | (a,b) <- xs, a == []]

joiner :: [String] -> String -- joins list of strings into one string
joiner xs = foldr (\a b-> a ++ if b=="" then b else if (head b)=='-' then " - " ++ (drop 1 b) else " + " ++ b) "" xs

iR :: String -> [String] -- Internal Representation of the polynomial e.g (2,[(x,2),(y,1)]) = x^2y
iR xs = tplToString (simply (sorting [internalRepresentation x | x <- polynomialOrganizer xs]))

normalize :: String -> String -- main function to run option a (normalize polynomial)
normalize poly = joiner (tplToString (simply (sorting [internalRepresentation x | x <- polynomialOrganizer poly])))

add :: String -> String -> String -- main function to run option b (add 2 polynomials)
add poly1 poly2 = normalize (poly1 ++ "+" ++ poly2)

multiplyVars :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)] -- multiplies variables of a 2 monomials
multiplyVars x y = x ++ y

multiplyOne :: (Int,[(Char,Int)]) -> (Int,[(Char,Int)]) -> (Int,[(Char,Int)]) -- multiplies two monomials by themselves
multiplyOne (a,b) (c,d) = (a*c, multiplyVars b d)

multiply :: [(Int,[(Char,Int)])] -> [(Int,[(Char,Int)])] -> [(Int,[(Char,Int)])] -- multiplies 2 polynomials by one another
multiply [] _ = []
multiply (x:xs) ys = [multiplyOne x y | y<- ys] ++ multiply xs ys

multiplication :: String -> String -> String -- main function to run option c
multiplication poly1 poly2 = joiner(tplToString (simply (sorting (multiply [internalRepresentation x | x <- polynomialOrganizer poly1] [internalRepresentation x | x <- polynomialOrganizer poly2]))))

reducer :: [(Char,Int)] -> Char -> [(Char,Int)] -- reduces exponent of variable to be derived e.g [('y',1),('x',2)] 'x' = [('y',1),('x',1)]
reducer xs vari = [(a,b) | (a,b) <- xs, a /= vari] ++ [(a,b-1) | (a,b) <- xs, a==vari, b>1]

maybeHead :: [Int] -> Int -- in case list is empty, return "1", otherwise return head of list
maybeHead [] = 0
maybeHead xs = head xs

coeficient :: [(Char,Int)] -> Char -> Int -- finds coeficient of variable to be derived e.g [('y',1),('x',2)] 'x' = 2
coeficient xs vari = maybeHead [b | (a,b) <- xs, a==vari]

changer :: [(Int,[(Char,Int)])] -> Char -> [(Int,[(Char,Int)])] -- changes internal tuples to be derived by the variable chosen
changer xs vari = [(a*(coeficient b vari),reducer b vari) | (a,b) <- xs, a*(coeficient b vari)/=0] -- e.g [(1,[('y',1),('x',1)]),(2,[('x',1),('y',2)])] 'y' = [(1,[('x',1)]),(4,[('x',1),('y',1)])]

derivative :: String -> Char -> String -- main function to run option d
derivative poly vari | joiner (tplToString (simply (sorting (changer [internalRepresentation x | x <- polynomialOrganizer poly] vari)))) /= "" = joiner (tplToString (simply (sorting (changer [internalRepresentation x | x <- polynomialOrganizer poly] vari))))
                     | otherwise = "0"
