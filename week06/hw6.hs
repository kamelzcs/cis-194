import Data.List (intersperse)
import Stream

fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

nats :: Stream Integer
nats = Stream [0..]

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

{-http://codereview.stackexchange.com/questions/66700/implementing-function-that-maps-over-stream-}


