secondAndThird' :: [Int] -> Either String (Int, Int)
secondAndThird' (_:b:c:_) = (b, c)
secondAndThird' a = "Only " ++ show (length a) ++ " element(s) in the list"