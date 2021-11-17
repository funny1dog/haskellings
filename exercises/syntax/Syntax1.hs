import Test.Tasty
import Test.Tasty.HUnit

{-

- "If" statements are different in Haskell from most other languages
  in that they *must* have an "else" branch.

- The general syntax looks like:
  if {Boolean} then {true branch} else {false branch}

- An If-Statement is an expression, and so it must have a type. In
  order for it to have a type, both branches must exist and they
  must be expressions of the *same type*.

- You can use 1 line or multiple lines for an "if" statement.

-}

-- Here's an example.
-- Note: "mod" is a useful function giving us the modulus of an integer.
--       Also, we can compare basic types using "==", like in other languages.
doubleIfOdd :: Int -> Int
doubleIfOdd x = if mod x 2 == 1
  then 2 * x
  else x

-- There is no "elif" like in Python. To add additional branches, make another
-- "if" statement in the "else" branch.
multiplicationFunc :: Int -> Int
multiplicationFunc x 
  | mod x 3 == 0 = x * 3
  | mod x 3 == 1 = x * 6
  | otherwise = x * 8

-- TODO: Fill in these functions!
-- Of the two inputs, return how many are "True"
countTrue :: Bool -> Bool -> Int
countTrue b1 b2 
  | b1 = if b2 then 2 else 1
  | b2 = 1
  | otherwise = 0

-- What is the type signature of this function?
evalInput :: Int -> [Int] -> Double
evalInput x myList 
  | x == 0 = 1.0
  | head myList == 0 = 2.5
  | otherwise = 3.5

-- The following will not work, since the "else" branch has a different type.
-- TODO: Try uncommenting this when you're finished and check that it
--       doesn't compile.
-- badIf x = if x == 3 then 45 else "Hello"

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax1" $
  [ testCase "countTrue 1" $ countTrue True False @?= 1
  , testCase "countTrue 2" $ countTrue False False @?= 0
  , testCase "countTrue 3" $ countTrue False True @?= 1
  , testCase "countTrue 4" $ countTrue True True @?= 2
  , testCase "evalInput 1" $ evalInput 0 [1, 2] @?= 1.0
  , testCase "evalInput 2" $ evalInput 1 [0, 2] @?= 2.5
  , testCase "evalInput 3" $ evalInput 5 [3, 2] @?= 3.5
  ]
