-- Exercise 01

-- Return the list of digits of the input integer
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = [1]
