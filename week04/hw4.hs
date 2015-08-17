fun1 :: [Integer] -> Integer
fun1 = foldr (\x y -> if even x then (x - 2) * y else y) 1

fun2:: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
            deriving Show

foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs
    where
        height Leaf = -1
        height (Node h _ _ _) = h
        count Leaf = 0
        count (Node _ l _ r) = 1 + count l + count r
        insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node h l d r)
            | height l > height r = Node h l d $ insert x r
            | count l > count r   = Node h l d (insert x r)
            | otherwise           = let newl = insert x l
                                        newHeight = (1 + (height newl))
                                        in Node newHeight newl d r

xor :: [Bool] -> Bool
xor = foldr (\x accum -> if x then not accum else accum) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x accum -> f x : accum) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a bs = foldr (\b g x -> g (f x b)) id bs a

sSundDelete :: Int -> [Int]
sSundDelete n = [i+j+2*i*j|i<-[1..n], j<-[i..n]]

sieveSundaram :: Int -> [Int]
sieveSundaram n = let del = sSundDelete n in
     [2*x+1 | x <- [1..n], not (x `elem` del)]
