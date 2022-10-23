import Data.Char ( isDigit, isAlpha, isLetter )
import qualified Data.Map as M

polynomialCleaner :: String -> String -- removes spaces and '*' from strings e.g "2*x*y + 3" = "2xy+3"
polynomialCleaner [] = []
polynomialCleaner (x:xs)
    | x == ' ' = polynomialCleaner xs
    |x == '*' = polynomialCleaner xs
    | otherwise = x : polynomialCleaner xs

polynomialSimplifier :: String -> [String] -- separates string into list of strings, by '+' or '-' e.g "2xy+3" = ["2xy","3"]
polynomialSimplifier [] = []
polynomialSimplifier (x:xs)
    |x == '+' = polynomialSimplifier xs
    |otherwise = (x : takeWhile (\x -> (x /='+') && (x /='-')) xs) : polynomialSimplifier (dropWhile (\x -> (x /='+') && (x /='-')) xs)

polynomialOrganizer :: String -> [String] -- converts string to list of strings divided by '+' or '-' e.g "2*x*y + 3" = ["2xy","3"]
polynomialOrganizer xs = polynomialSimplifier (polynomialCleaner xs)

maybeTail :: [Char] -> String -- in case list is empty, return "1", otherwise return tail of list e.g ['1', '2', '3'] = "3"
maybeTail [] = "1"
maybeTail xs = tail xs

exponentProcessor :: String -> [(Char, Int)] -- converts string to list of tuples, where first element is letter and second is exponent e.g "2xy" = [('x',1),('y',1)]
exponentProcessor [] = []
exponentProcessor (x:xs)
    |  not (null xs) && isDigit (head xs) = (x, read (takeWhile isDigit xs)) : exponentProcessor (dropWhile isDigit xs)
    | otherwise = (x, 1) : exponentProcessor xs

internalRepresentation :: String -> (Int,[(Char,Int)]) -- converts string to internal representation e.g "2xy" = (2,[('x',1),('y',1)])
internalRepresentation xs
    | all isDigit xs = (read xs, [])
    | head xs == '-' && all isDigit (tail xs) = (-read (tail xs), [])
    | all isLetter xs = (1, simplifyexponents [(char,1)| char <- xs])
    | head xs == '-' && all isLetter (tail xs) = (-1, simplifyexponents [(char,1)| char <- tail xs])
    | head xs == '-' && not (isDigit (xs !! 1)) = (-1, simplifyexponents [tuples | tuples <- exponentProcessor (filter (/='^') (tail xs))])
    | head xs == '-' = (negDigit, simplifyexponents[tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit(tail xs)))])
    |not (isDigit (head xs)) = (1, simplifyexponents [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    |otherwise = (digit, simplifyexponents [tuples | tuples <- exponentProcessor (filter (/='^') (dropWhile isDigit xs))])
    where
        digit = read (takeWhile isDigit xs) :: Int
        negDigit = - read (takeWhile isDigit (tail xs)) :: Int


polynomialGrade :: ([(Char, Int)], Int) -> Int -- returns the grade of a polynomial e.g (3, [('x', 2), ('y', 1)]) = 2
polynomialGrade ([], _) = 0
polynomialGrade (xs, _) = maximum [snd x | x <- xs]

polynomialGreaterThan :: ([(Char, Int)], Int) -> ([(Char, Int)], Int) -> Bool -- returns true if grade of first polynomial is greater than grade of second e.g (2, [('x', 3), ('y', 1)]) > (3, [('x', 2), ('y', 1)]) = True
polynomialGreaterThan xs ys = polynomialGrade xs > polynomialGrade ys

polynomialSorter :: [([(Char, Int)], Int)] -> [([(Char, Int)], Int)] -- sorts a list of polynomials by grade
polynomialSorter [] = []
polynomialSorter (x:xs) = polynomialSorter [y | y <- xs, polynomialGreaterThan y x] ++ [x] ++ polynomialSorter [y | y <- xs, not (polynomialGreaterThan y x)]

sorting :: [(Int,[(Char,Int)])] -> [([(Char,Int)],[Int])]  -- takes the list with all variables (some repeated) and groups them without repetition e.g [(1,('y',1)),(2,('y',1))] = [(('y',1),[3])]
sorting assocs = M.toList (M.fromListWith (\n1 n2 -> [sum(n1 ++ n2)]) [((b), [a]) | (a,b) <- assocs])

simply :: [([(Char,Int)],[Int])] -> [([(Char,Int)],Int)] -- converts second item in tuple form list of ints to normal int e.g [([(y,1)],[1])] = [([(y,1)],1)]
simply xs = [(a,b) | ((a,[b])) <- xs]

simplifyVariables :: [(Char,Int)] -> [String] -- simplifies internal representation of variables e.g [('x',2),('y',1)] = ["x^2","y"]
simplifyVariables xs = [[a] ++ "^" ++ (show b) | (a,b) <- xs, b > 1] ++ [[a] | (a,b) <- xs, b == 1]

joinVariables :: [String] -> String -- simplifies internal representation of variables e.g ["x^2","y"] = "x^2*y"
joinVariables xs = foldr (\a b -> a ++ if b == "" then b else "*" ++ b) "" xs

simpVar :: [(Char,Int)] -> String -- from [('x',2),('y',1)] to "x^2*y"
simpVar xs = joinVariables (simplifyVariables xs)

tplToString :: [([(Char,Int)],Int)] -> [String] -- converts list of monomials from internal representation to understandable string e.g [([('y',1)],1),([('x',1)],2)] = ["2*x","y"]
tplToString xs = [(show b) ++ "*" ++ (simpVar a) | (a,b) <- xs, b /= 1, a /= []] ++ [simpVar a | (a,b) <- xs, b == 1, a /= []] ++ [(show b) | (a,b) <- xs, a == []]

joiner :: [String] -> String -- joins list of strings into one string e.g ["2*x","y"] = "2*x + y"
joiner xs = foldr (\a b-> a ++ if b=="" then b else if (head b)=='-' then " - " ++ (drop 1 b) else " + " ++ b) "" xs

simplifyexponents :: [(Char,Int)] -> [(Char,Int)] -- simplifies exponents of variables e.g [('x',2),('x',1)] = [('x',3)]
simplifyexponents xs = [(a, sum b) | (a,b) <- M.toList (M.fromListWith (\n1 n2 -> n1 ++ n2) [(a,[b]) | (a,b) <- xs])]

multiplyVars :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)] -- multiplies variables of 2 monomials e.g [('x',2)] [('x',1)] = [('x',2),('x',1)]
multiplyVars x y = x ++ y

multiplyOne :: (Int,[(Char,Int)]) -> (Int,[(Char,Int)]) -> (Int,[(Char,Int)]) -- multiplies two monomials by themselves
multiplyOne (a,b) (c,d) = (a*c, simplifyexponents(multiplyVars b d))

multiply :: [(Int,[(Char,Int)])] -> [(Int,[(Char,Int)])] -> [(Int,[(Char,Int)])] -- multiplies two polynomials by one another
multiply [] _ = []
multiply (x:xs) ys = [multiplyOne x y | y<- ys] ++ multiply xs ys

reducer :: [(Char,Int)] -> Char -> [(Char,Int)] -- reduces exponent of variable to be derived e.g [('y',1),('x',2)] 'x' = [('y',1),('x',1)]
reducer xs vari = [(a,b) | (a,b) <- xs, a /= vari] ++ [(a,b-1) | (a,b) <- xs, a==vari, b>1]

maybeHead :: [Int] -> Int -- in case list is empty, return 0, otherwise return head of list e.g [1, 2, 3] = 1
maybeHead [] = 0
maybeHead xs = head xs

exponentt :: [(Char,Int)] -> Char -> Int -- finds exponent of variable to be derived e.g [('y',1),('x',2)] 'x' = 2
exponentt xs vari = maybeHead [b | (a,b) <- xs, a==vari]

changer :: [(Int,[(Char,Int)])] -> Char -> [(Int,[(Char,Int)])] -- changes internal tuples to be derived by the variable chosen e.g [(1,[('y',1),('x',1)]),(2,[('x',1),('y',2)])] 'y' = [(1,[('x',1)]),(4,[('x',1),('y',1)])]
changer xs vari = [(a*(exponentt b vari),reducer b vari) | (a,b) <- xs, a*(exponentt b vari)/=0]

normalize :: String -> String -- main function to run option a (normalize polynomial)
normalize poly = joiner (tplToString (polynomialSorter (simply (sorting ([internalRepresentation x | x <- polynomialOrganizer poly, head x /= '0'])))))

norm :: String -> String -- main function to run option a (normalize polynomial)
norm poly = joiner (tplToString (polynomialSorter (simply (sorting ([internalRepresentation x | x <- polynomialOrganizer poly, head x /= '0'])))))

add :: String -> String -> String -- main function to run option b (add 2 polynomials)
add poly1 poly2 = normalize (poly1 ++ "+" ++ poly2)

multiplication :: String -> String -> String -- main function to run option c (multiply 2 polynomials)
multiplication poly1 poly2 = joiner (tplToString (polynomialSorter (simply (sorting (multiply [internalRepresentation x | x <- polynomialOrganizer poly1, head x /= '0'] [internalRepresentation x | x <- polynomialOrganizer poly2, head x /= '0'])))))

derivative :: String -> Char -> String -- main function to run option d (derive polynomial)
derivative poly vari | joiner (tplToString (polynomialSorter(simply (sorting (changer [internalRepresentation x | x <- polynomialOrganizer poly, head x /= '0'] vari))))) /= "" = joiner (tplToString (polynomialSorter(simply (sorting (changer [internalRepresentation x | x <- polynomialOrganizer poly, head x /= '0'] vari)))))
                     | otherwise = "0"

main :: IO() -- main menu to choose what option you want to run
main = do
            putStrLn "What do you want to do? Normalize (1), Add (2), Multiply (3), Differentiate (4) or Exit (5)?"
            str <- getLine
            let option = read str :: Int
            if option == 1
               then do putStrLn "What is the polynomial you want to normalize?"
                       poly <- getLine
                       putStrLn("\n" ++(normalize poly))
                       putStrLn "\n"
               else return ()
            if option == 2
               then do putStrLn "What is the first polynomial you want to add?"
                       poly1 <- getLine
                       putStrLn "What is the second polynomial you want to add?"
                       poly2 <- getLine
                       putStrLn("\n" ++(add poly1 poly2))
                       putStrLn "\n"
               else return ()
            if option == 3
               then do putStrLn "What is the first polynomial you want to multiply?"
                       poly1 <- getLine
                       putStrLn "What is the second polynomial you want to multiply?"
                       poly2 <- getLine
                       putStrLn("\n" ++(multiplication poly1 poly2))
                       putStrLn "\n"
               else return ()
            if option == 4
               then do putStrLn "What is the polynomial you want to differentiate?"
                       poly <- getLine
                       putStrLn "What is the variable you want to differentiate by?"
                       vari <- getLine
                       putStrLn("\n" ++(derivative poly (head vari)))
                       putStrLn "\n"
                else return ()
            if option == 5
               then do putStrLn "Goodbye!"
            else main
