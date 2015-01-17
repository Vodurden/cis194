module HW07 where

import System.Random

-- == Excercise 1 ==
-- Write the fibonacci function (easy-but-inefficient style)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- Now use fib to construct the infinite list of fibs
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- == Excercise 2 ==
-- Write a more efficient infinite fibs list
--
-- Extra question - why is tail ok here: Because we're in an infinite stream so it'll never error.
--
-- Also: This is awesome! But trying to evaluate it in your head and keeping the thunks straight will give you
-- a headache (or at least it gives me a headache!).
fibs2 :: [Integer]
fibs2 = [0,1] ++ (zipWith (+) (fibs2) (tail fibs2))

-- == Excercise 3 ==
-- Write a function to convert a Stream to an infinite list
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x rest) = x:streamToList rest

-- == Excercise 4 ==
-- Define show for Stream limited to n elements
instance Show a => Show (Stream a) where
  show = show . (take 30) . streamToList

-- == Excercise 5 ==
-- Define some tools for working with streams

-- Generate a stream containing infinitely many copies of the given element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x rest) = Cons (f x) (streamMap f rest)

-- Builds a stream from a seed and unfolding rule
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = (Cons x (streamFromSeed f (f x)))

-- == Excercise 6 ==
-- Now we have some stream tools, let's create a few.
nats :: Stream Integer
nats = streamFromSeed (+1) 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ~(Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

-- == Excercise 7 ==
-- Random numbers: Write a function that produces an infinite pseudo-random sequence
randomList :: (Random a, RandomGen g) => g -> [a]
randomList generator = value : (randomList nextG)
                       where (value, nextG) = random generator

-- == Excercise 8 ==
-- Write a function that produces infinite pseudo-random sequence of ints of length n from a given seed
randomInts :: Int -> [Int]
randomInts n = take n $ randomList (mkStdGen 0)

-- == Excercise 9 ==
-- Profiling: Given a function minMax find the min and max of a
-- pseudo-random sequence of 1 million Ints. Then print them out from main.
-- Then profile this function and see how long it takes!
--
-- $ ./HW07.exe +RTS -s
-- Stack space overflow: current size 8388608 bytes.
-- Use `+RTS -Ksize -RTS' to increase it.
--      180,732,368 bytes allocated in the heap
--      212,192,212 bytes copied during GC
--       54,255,296 bytes maximum residency (8 sample(s))
--        1,421,980 bytes maximum slop
--              111 MB total memory in use (0 MB lost due to fragmentation)
--
--                                     Tot time (elapsed)  Avg pause  Max pause
--   Gen  0       306 colls,     0 par    0.12s    0.13s     0.0004s    0.0215s
--   Gen  1         8 colls,     0 par    0.14s    0.16s     0.0194s    0.0690s
--
--   INIT    time    0.00s  (  0.00s elapsed)
--   MUT     time    0.05s  (  0.03s elapsed)
--   GC      time    0.27s  (  0.29s elapsed)
--   EXIT    time    0.00s  (  0.01s elapsed)
--   Total   time    0.31s  (  0.33s elapsed)
--
--   %GC     time      85.0%  (87.5% elapsed)
--
--   Alloc rate    3,861,777,980 bytes per MUT second
--
--   Productivity  15.0% of total user, 14.2% of total elapsed
--
-- The above tells us we're using we're using 111 MB total memory with the below version of minMax!
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = Just (minimum xs, maximum xs)

-- == Excercise 10 ==
-- Lets see if we can do better: Write a minMax that doesn't
-- need to traverse the list twice for minimum and maximum
flattenMaybe :: (Maybe a, Maybe a) -> Maybe (a, a)
flattenMaybe (Nothing, _) = Nothing
flattenMaybe (_, Nothing) = Nothing
flattenMaybe (Just x, Just y) = Just (x, y)

-- $ ./HW07.exe +RTS -s
-- Stack space overflow: current size 8388608 bytes.
-- Use `+RTS -Ksize -RTS' to increase it.
--       83,354,088 bytes allocated in the heap
--       72,297,552 bytes copied during GC
--       15,282,892 bytes maximum residency (6 sample(s))
--           93,344 bytes maximum slop
--               53 MB total memory in use (0 MB lost due to fragmentation)
--
--                                     Tot time (elapsed)  Avg pause  Max pause
--   Gen  0       118 colls,     0 par    0.08s    0.08s     0.0007s    0.0165s
--   Gen  1         6 colls,     0 par    0.03s    0.04s     0.0073s    0.0173s
--
--   INIT    time    0.00s  (  0.00s elapsed)
--   MUT     time    0.00s  (  0.02s elapsed)
--   GC      time    0.11s  (  0.12s elapsed)
--   EXIT    time    0.00s  (  0.01s elapsed)
--   Total   time    0.12s  (  0.15s elapsed)
--
--   %GC     time      87.5%  (82.3% elapsed)
--
--   Alloc rate    0 bytes per MUT second
--
--   Productivity  12.5% of total user, 10.4% of total elapsed
--
-- Compared to the above we're only using 53 MB! A half space reduction is about what we expect
-- so it looks like this minMax' is a lot more space efficient! (Shame it's so much more complicated
-- to define)
minMax' :: [Int] -> Maybe (Int, Int)
minMax' = flattenMaybe . foldr go (Nothing, Nothing)
          where go :: Int -> (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int)
                go x (Just mini, Just maxi) = (Just (min mini x), Just (max maxi x))
                go x (Just mini, Nothing) = (Just (min mini x), Just x)
                go x (Nothing, Just maxi) = (Just x, Just (max maxi x))
                go x (Nothing, Nothing) = (Just x, Just x)

main :: IO ()
main = do
  let d = randomInts 1000000
  putStrLn (show (minMax' d))

-- == Excercise 11 (Optional) ==
-- The matrix is defined in row-first order such that
--
-- (a, b, c, d) = [a, b]
--                [c, d]
data Matrix = Matrix (Integer, Integer, Integer, Integer)
              deriving (Show)

instance Num Matrix where
  (*) (Matrix (a, b, c, d)) (Matrix (e, f, g, h)) = Matrix (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)
  (+) (Matrix (a, b, c, d)) (Matrix (e, f, g, h)) = Matrix (a+e, b+f, c+g, d+h)
  abs (Matrix (a, b, c, d)) = (Matrix (abs a, abs b, abs c, abs d))
  signum (Matrix (a, b, c, d)) = (Matrix (signum a, signum b, signum c, signum d))
  fromInteger i = Matrix (i, i, i, i)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n | Matrix (_, b, _, _) <- Matrix (1, 1, 1, 0) ^ n = b
