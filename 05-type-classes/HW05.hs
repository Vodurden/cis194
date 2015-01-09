module HW05 where

import Data.List
import Data.Maybe    ( fromJust, listToMaybe )
import Data.Char

import Ring
import Parser

-- == Excercise 1 ==
-- Write some tests
integerDefinitionsWork :: Bool
integerDefinitionsWork =
  (parse "3" == Just (3 :: Integer, ""))
  && (parseRing "1 + 2 * 5" == Just (11 :: Integer))
  && (addId == (0 :: Integer))

-- == Excercise 2 ==
-- Define Ring and Parsable instances over Mod5 Integer rings
data Mod5 = MkMod Integer
          deriving (Show, Eq)

instance Parsable Mod5 where
  parse str
    | Just (i, rest) <- intPart = Just (MkMod (i `mod` 5), rest)
    | otherwise = Nothing
    where intPart = listToMaybe $ (reads str :: [(Integer, String)])

instance Ring Mod5 where
  addId = MkMod 0
  addInv (MkMod x) = MkMod ((-x) `mod` 5)
  mulId = MkMod 1
  add (MkMod x) (MkMod y) = MkMod ((x + y) `mod` 5)
  mul (MkMod x) (MkMod y) = MkMod ((x * y) `mod` 5)

-- Don't forget the tests!
mod5DefinitionsWork :: Bool
mod5DefinitionsWork =
  (parse "3" == Just (MkMod 3, ""))
  && (parse "5" == Just (MkMod 0, ""))
  && (parse "14" == Just (MkMod 4, ""))
  && (parseRing "5 + 10" == Just (MkMod 0))
  && (parseRing "2 * 6" == Just (MkMod 2))
  && (parseRing "5 + 10 * 2" == Just (MkMod 0))
  && (add (MkMod 2) addId == MkMod 2)
  && (add (MkMod 2) (addInv (MkMod 2)) == addId)
  && (mul (MkMod 2) mulId == MkMod 2)

-- == Excercise 3 ==
-- Matrix arithmetic forms a ring. Write a Mat2x2 datatype with Ring and Parsable instances
-- Parser should read something like [[1,2],[3,4]] with no possibility of spaces
--
-- Mat2x2 is specified row major - each pair corresponds to a row with each
-- individual integer corresponding to the column.
data Mat2x2 = Mat2x2 ((Integer, Integer), (Integer, Integer))
              deriving (Show, Eq)

-- Basic pattern-match based parsing. If this was for-reals I think it would be time to
-- learn monadic parser-combinators and/or parsec
instance Parsable Mat2x2 where
  parse ('[':'[':x1:',':y1:']':'[':x2:',':y2:']':']':rest)
    | areDigits = Just (matrix, rest)
    | otherwise = Nothing
    where
      makeInt = toInteger . digitToInt
      areDigits = all isHexDigit [x1, y1, x2, y2]
      matrix = Mat2x2 ((makeInt x1, makeInt y1), (makeInt x2, makeInt y2))
  parse _ = Nothing

-- Simple row/column wise add/mul definition
instance Ring Mat2x2 where
  addId = Mat2x2 ((0, 0), (0, 0))
  addInv (Mat2x2 ((x1, y1), (x2, y2))) = (Mat2x2 ((-x1, -y1), (-x2, -y2)))
  mulId = Mat2x2 ((1, 1), (1, 1))
  add (Mat2x2 ((ax1, ay1), (ax2, ay2))) (Mat2x2 ((bx1, by1), (bx2, by2))) =
    Mat2x2 (((ax1 + bx1), (ay1 + by1)), ((ax2 + bx2), (ay2 + by2)))
  mul (Mat2x2 ((ax1, ay1), (ax2, ay2))) (Mat2x2 ((bx1, by1), (bx2, by2))) =
    Mat2x2 (((ax1 * bx1), (ay1 * by1)), ((ax2 * bx2), (ay2 * by2)))

mat2x2DefinitionsWork :: Bool
mat2x2DefinitionsWork =
  (parse "[[1,2][3,4]]" == Just (Mat2x2 ((1,2),(3,4)), ""))
  && (parseRing "[[1,2][3,4]] + [[2,4][6,8]]" == Just (Mat2x2 ((3, 6),(9, 12))))
  && (parseRing "[[1,2][3,4]] * [[2,4][6,8]]" == Just (Mat2x2 ((2, 8),(18, 32))))
  && (add (Mat2x2 ((1, 2), (3, 4))) addId == Mat2x2 ((1,2), (3, 4)))
  && (add (Mat2x2 ((1, 2), (3, 4))) (addInv (Mat2x2 ((1, 2), (3, 4)))) == addId)
  && (mul (Mat2x2 ((1, 2), (3, 4))) mulId == Mat2x2 ((1,2), (3, 4)))

-- == Excercise 4 ==
-- Define Boolean arithmetic with Parsable and Ring
instance Parsable Bool where
  parse = listToMaybe . reads

-- If we think of False of 0 and True as 1 we can define a ring that is equivalent
-- to the Mod1 ring defined by the set {0, 1}. Addition is accomplished by xor which causes
-- True + True (1+1) to wrap to False.
instance Ring Bool where
  addId = False
  addInv p = p
  mulId = True
  add True p = not p
  add False p = p
  mul = (&&)

boolDefinitionsWork :: Bool
boolDefinitionsWork =
  (parse "True" == Just (True, ""))
  && (parse "False" == Just (False, ""))
  && (parseRing "True + False" == Just True)
  && (parseRing "True * False" == Just False)
  && (add (addId :: Bool) (addId :: Bool) == addId :: Bool)
  && (add True addId == True)
  && (add False addId == False)
  && (mul True mulId == True)
  && (mul False mulId == False)
  && (add True (addInv True) == addId)
  && (add False (addInv False) == addId)

-- == Excercise 5 ==
-- Write distribute that distributes any use of multiplication over addition on a ring.
-- Handle both left-distribution and right-distribution
--
-- i.e if we have 2 * (5 + 6) change it to (5*2 + 6*2)
distribute :: RingExpr a -> RingExpr a
distribute (AddInv x) = AddInv (distribute x)
distribute (Mul mulVal (Add x y)) = Add (Mul mulVal x) (Mul mulVal y)
distribute (Mul (Add x y) mulVal) = Add (Mul mulVal x) (Mul mulVal y)
distribute (Add x y) = Add (distribute x) (distribute y)
distribute x = x

distributeWorksHelper :: (RingExpr Bool -> RingExpr Bool) -> Bool
distributeWorksHelper distributeFn =
  let boolRing = parseRing "True * (False + True)" :: Maybe (RingExpr Bool)
      distributedBoolRing = parseRing "(True * False) + (True * True)" :: Maybe (RingExpr Bool)
      rightBoolRing = parseRing "(False + True) * False" :: Maybe (RingExpr Bool)
      distributedRightBoolRing = parseRing "(False * False) + (False * True)" :: Maybe (RingExpr Bool)
  in (distributeFn $ fromJust boolRing) == fromJust distributedBoolRing
     && (distributeFn $ fromJust rightBoolRing) == fromJust distributedRightBoolRing

distributeWorks :: Bool
distributeWorks = distributeWorksHelper distribute

-- == Excercise 6 ==
-- Write squashMulId that detects if you multiply by mulId and removes the multiplication.
-- TODO: Consider if we can do this without having to restrict to Eq
--       (Maybe test by multiplying x by y and seeing if it's still x? - still needs Eq though!)
squashMulId :: (Ring a, Eq a) => RingExpr a -> RingExpr a
squashMulId (AddInv x) = AddInv (squashMulId x)
squashMulId (Mul (MulId) y) = y
squashMulId (Mul x (MulId)) = x
squashMulId (Mul (Lit x) y) | x == mulId = y
squashMulId (Mul x (Lit y)) | y == mulId = x
squashMulId (Add x y) = Add (squashMulId x) (squashMulId y)
squashMulId x = x

squashMulIdWorksHelper :: (RingExpr Integer -> RingExpr Integer) -> Bool
squashMulIdWorksHelper squashMulIdFn =
  let intRing = fromJust $ (parseRing "1 + (5 * 1)" :: Maybe (RingExpr Integer))
      squashedIntRing = fromJust $ (parseRing "1 + 5" :: Maybe (RingExpr Integer))
  in squashMulIdFn intRing == squashedIntRing

squashMulIdWorks :: Bool
squashMulIdWorks = squashMulIdWorksHelper squashMulId

-- == Excercise 7 ==
-- Generalise distribute and squashMulId so they only need to concentrate on the parts they modify.
--
-- Idea 1: Implement a foldRingExpr function that takes a method for each data constructor.
--         Major issue: Still have to provide id functions to non-changed data constructors
--
-- Idea 2: A set of "transform" functions that can be composed together. Combine transformMul and transformAdd
--         Issue: squash and distribute only affect add and mul respectively. Doesn't feel general enough.
--         Issue: Additional transform functions are going to have a lot of repetition
--
-- Idea 3: Implement arbitrary transformations using the Mapping to a Maybe with a value of
--         'nothing' representing 'do the default behaviour'. This seems like the best way to go.
transform :: (RingExpr a -> Maybe (RingExpr a)) -> RingExpr a -> RingExpr a
transform f e | (Just result) <- f e = result
transform f (AddInv x) = AddInv (transform f x)
transform f (Mul x y) = Mul (transform f x) (transform f y)
transform f (Add x y) = Add (transform f x) (transform f y)
transform _ x = x

-- Updated distribute using idea 3
distribute' :: RingExpr a -> RingExpr a
distribute' = transform distributeHelper
  where distributeHelper :: RingExpr a -> Maybe (RingExpr a)
        distributeHelper (Mul mulVal (Add x y)) = Just $ Add (Mul mulVal x) (Mul mulVal y)
        distributeHelper (Mul (Add x y) mulVal) = Just $ Add (Mul mulVal x) (Mul mulVal y)
        distributeHelper _ = Nothing

distribute'Works :: Bool
distribute'Works = distributeWorksHelper distribute'

-- Updated squashMulId using idea 3
squashMulId' :: (Ring a, Eq a) => RingExpr a -> RingExpr a
squashMulId' = transform squashMulIdHelper
  where squashMulIdHelper :: (Ring a, Eq a) => RingExpr a -> Maybe (RingExpr a)
        squashMulIdHelper (Mul (MulId) y) = Just y
        squashMulIdHelper (Mul x (MulId)) = Just x
        squashMulIdHelper (Mul (Lit x) y) | x == mulId = Just y
        squashMulIdHelper (Mul x (Lit y)) | y == mulId = Just x
        squashMulIdHelper _ = Nothing

squashMulId'Works :: Bool
squashMulId'Works = squashMulIdWorksHelper squashMulId'
