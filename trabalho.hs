import Data.Char ( isDigit, isAlpha )
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

internalRepresentation :: [String] -> [(Int,Char ,Int)]
internalRepresentation xs
    |null xs = []
    |all isDigit (head xs) = (read (head xs), ' ', 0) : internalRepresentation (tail xs)
    |head (head xs) == '-' = (-read (takeWhile isDigit (pos_mon)) :: Int, head (dropWhile isDigit (pos_mon)), exponent) : internalRepresentation (drop 1 xs)
    |otherwise = (read (takeWhile isDigit (head xs)) :: Int, head (dropWhile isDigit (head xs)), exponent) : internalRepresentation (drop 1 xs)
    where pos_mon = tail (head xs)
          exponent  = read (tail (dropWhile (/= '^') (head xs))) :: Int 

iR :: String -> [(Int,Char ,Int)]
iR xs = internalRepresentation (polynomialOrganizer xs)
