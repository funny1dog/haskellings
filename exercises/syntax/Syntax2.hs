import Test.Tasty
import Test.Tasty.HUnit

{-

- Nested If-Statements can be annoying to write. So Haskell has a few different
  ways to deal with having several condition branches.

- The first of these are "guards".

function :: InputType -> OutputType
function x
  | {condition :: Bool} = {possible output :: OutputType}
  | {condition :: Bool} = {possible output :: OutputType}
  | otherwise           = {catch all case  :: OutputType}

- Each case has its own line, starting with a vertical bar.
- Then you'll list the condition (which should be a Bool expression).
- The condition is followed by the "equals" sign, and then the return value for that case.
- Instead of "else" for the catch-all case, use the keyword "otherwise".
- Like the two branches of an If-Statement, all branches must have the same output type!

example :: [Int] -> Int
example list1
  | head list1 == 4 = 0
  | head (tail list1) == 4 = 1
  | otherwise = 2

- Observe that the "equals" sign goes on each individual line, NOT after the
  initial function name and arguments!

- Note: The cases are evaluated in the order they are listed. 
-}

-- TODO:

-- Rewrite 'countTrue', except this it takes 3 inputs, so use guards!
countTrue :: Bool -> Bool -> Bool -> Int
-- countTrue a b c = if a then (if b then (if c then 3 else 2) else (if c then 2 else 1)) else (if b then (if c then 1 else 0) else (if c then 1 else 0))
countTrue a b c 
  | a = if b then (if c then 3 else 2) else (if c then 2 else 1)
  | b = if c then 1 else 0
  | c = 1
  | otherwise = 0

-- Return a string representation of the (positive) input number,
-- from 0 = "Zero" up through 5 = "Five".
-- If it's larger than 5, return "Too many!"
numberString :: Word -> String
numberString a
  | a == 0 = "Zero"
  | a == 1 = "One"
  | a == 2 = "Two"
  | a == 3 = "Three"
  | a == 4 = "Four"
  | a == 5 = "Five"
  | otherwise = "Too many!"

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax2" $
  [ testCase "countTrue 1" $ countTrue True False False @?= 1
  , testCase "countTrue 2" $ countTrue False False False @?= 0
  , testCase "countTrue 3" $ countTrue True False True @?= 2
  , testCase "countTrue 4" $ countTrue True True True @?= 3
  , testCase "numberString 1" $ numberString 1 @?= "One"
  , testCase "numberString 2" $ numberString 4 @?= "Four"
  , testCase "numberString 3" $ numberString 6 @?= "Too many!"
  ]
