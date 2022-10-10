doubleMe x = x + x  

doubleUs x y = doubleMe x + doubleMe y


removeNonUppercase :: [Char] -> [Char]
removeNonUppercase string = [ character | character <- string, character `elem` ['A'..'Z'] ]

dobleSmallNumber x = if x > 100
                        then x
                        else x * 2

factorial :: Integer -> Integer 
factorial n = product [1..n]


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height 
    | bmi <= 18.5 = "something"
    | bmi <= 25.0 = "something else"
    | otherwise = "else clause"
    where bmi = weight / height ^ 2    


maximum' :: (Ord a) => [a] -> a 
maximum' [] = error "Cant calculate maximum on an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Ord a, Num a) => a -> i -> [i]
replicate' times element
    | times <= 0 = []
    | otherwise = element: replicate' (times - 1) element


take' :: (Ord a, Num a) => a -> [b] -> [b]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' n [x]    = n == x
elem' n (x:xs) = elem' n xs