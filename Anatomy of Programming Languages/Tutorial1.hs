module Tutorial1 where
import Data.Char

absolute :: Int -> Int
absolute x = if x < 0 then -x else x

nested_if1 x = if ( absolute x <= 10)
               then x
               else error "Only numbers between [-10, 10] allowed"

nested_if2 x = if ( (if x < 0 then -x else x) <= 10)
               then x
               else error "Only numbers between [-10, 10] allowed"


data Geometry = Point Int Int
              | Circle Int Int Int
              | Rectangle Int Int Int Int
              | Composite [Geometry]


x = Point 2 3
y = Circle 3 5 3
z = Rectangle 1 2 3 4
a = Composite [x, y, z]


tl :: [a] -> [a] 
tl [] = []
tl (x:xs) = xs

{- 
   given a number returns the corresponding number in the fibonacci sequence 
   0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ...
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mapList :: (a -> b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : (mapList f xs)

charToNumber :: [Char] -> [Int]
charToNumber []     = []
charToNumber (x:xs) = ord x : charToNumber xs

zipList :: [a] -> [b] -> [(a, b)]
zipList _ []          = []
zipList [] _          = []
zipList (x:xs) (y:ys) = (x,y) : zipList xs ys

zipSum :: [Int] -> [Int] -> [Int]
zipSum _ [] = []
zipSum [] _ = []
zipSum xs ys = mapList (\ (x,y) -> x + y) (zipList xs ys)

