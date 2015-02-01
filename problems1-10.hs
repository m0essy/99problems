module Problem1 (problem1, problem2, problem3, problem4, problem5, problem6) where 

problem1 :: [a] -> a
problem1 = head . reverse 

problem2 :: [a] -> a
problem2 = last . init 

problem3 :: [a] -> Int -> a
problem3 (x:_) 1 = x
problem3 (x:xs) count = problem3 xs (count - 1) 

problem4 :: [a] -> Int
problem4 list = loop list 0
  where
    loop :: [a] -> Int -> Int
    loop [] n = n
    loop (x:xs) n = loop xs (n + 1)

-- reverse
problem5 :: [a] -> [a]
problem5 [] = []
problem5 a = [last(a)] ++ problem5(init(a))
             
-- is pallindrome
problem6 :: (Eq a) => [a] -> Bool
problem6 [] = True
problem6 list = loop list (reverse list)
  where
    loop :: (Eq a) => [a] -> [a] -> Bool
    loop [] [] = True
    loop (a:as) (b:bs) = if(a == b) then
                 loop as bs
               else
                 False
