import Test.Tasty
import Test.Tasty.HUnit

{-

- When pattern matching on the lists, we can use the structure of the list
  to specify different branches. This includes specifying individual elements
  of the list, limiting its length, or even using the cons operator (:) to
  state that there are "at least" a certain number of items in the list.

evalList :: [Int] -> Int
evalList [] = ...              -- Only evaluates empty list
evalList [x] = ...             -- Evaluates any list with a single element
evalList (1 : 2 : _) = ...     -- List must start with 1 and 2
evalList (x : y : z : _) = ... -- Any list with at least three elements
evalList xs = ...              -- Matches any list

- We can also run pattern matching in the middle of a function by using a "case" statement.
  Each pattern is followed by an arrow, and then the expression. As always, every expression
  must result in the same type.

- The following is equivalent to above:

evalList :: [Int] -> Int
evalList mylist = case myList of
  [] -> ...
  [x] -> ...
  (1 : 2 : _) -> ...
  (x : y : z : _) -> ...
  xs -> ...

-}

-- TODO: Fill in this function on a boolean and a list.
-- If the boolean is set as True, then we care about the
-- number of elements in the list:
-- 0 -> return 0
-- 1 -> return 1 (up through 3 elements)
-- If there are at least 4 elements, return 4.
--
-- If the boolean is set as False, then we care about the
-- sum of the first several elements(up through 3). But if there are at least 4 elements,
-- you can simply return 10.
evalList :: Bool -> [Int] -> Int
evalList True [] = 0
evalList True [a] = 1
evalList True [a, b] = 2
evalList True [a, b, c] = 3 
evalList True a = 4
evalList False [] = 0
evalList False [a] = a
evalList False [a, b] = a + b
evalList False [a, b, c] = a + b + c
evalList False a = 10

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax4" $
  [ testCase "evalList 1" $ evalList True [] @?= 0
  , testCase "evalList 2" $ evalList True [5] @?= 1
  , testCase "evalList 3" $ evalList True [6, 7] @?= 2
  , testCase "evalList 4" $ evalList True [14, 15, 16, 18, 19, 20] @?= 4
  , testCase "evalList 5" $ evalList False [] @?= 0
  , testCase "evalList 6" $ evalList False [5] @?= 5
  , testCase "evalList 7" $ evalList False [6, 7] @?= 13
  , testCase "evalList 8" $ evalList False [14, 15, 16, 18, 19, 20] @?= 10
  ]
