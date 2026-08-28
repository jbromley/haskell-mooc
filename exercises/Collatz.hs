module Collatz where

-- One step of the Collatz sequence
step :: Integer -> Integer
step x =
  if even x
    then down
    else up
  where
    down = div x 2
    up = 3 * x + 1

-- collatz x counts how many steps it takes the Collatz sequence to reach 1
-- when starting from x.
collatz :: Integer -> Integer
collatz 1 = 0
collatz x = 1 + collatz (step x)

-- longest finds the number with the longest Collatz sequence for initial
-- values between 0 and upperBound.
longest :: Integer -> Integer
longest upperBound = longest' 0 0 upperBound

-- helper function for longest
longest' :: Integer -> Integer -> Integer -> Integer
longest' number _ 0 = number
longest' number maxLength n =
  if len > maxLength
    then longest' n len (n - 1)
    else longest' number maxLength (n - 1)
  where
    len = collatz n
