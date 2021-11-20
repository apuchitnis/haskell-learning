-- 1
last' :: [a] -> a
last' [] = error "trying to get last of an empty list"
last' [x] = x
last' (_ : xs) = last' xs

last'' :: [a] -> a
last'' xs = (head .reverse) xs

-- 2
lastButOne :: [a] -> a
lastButOne [] = error "last but of one an empty list"
lastButOne [x] = error "last but one of a list with one element"
lastButOne [x,_] = x
lastButOne (x:xs) = lastButOne xs

lastButOne' :: [a] -> a
lastButOne' =  (head . reverse . init)

lastButOne'' :: [a] -> a
lastButOne'' = (head . tail . reverse)

-- 3
elementAt :: [a] -> Int -> a
elementAt xs k = xs !! k

elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "index out of bounds"
elementAt' (x:_) 1 = x
elementAt' (_:xs) k
  | k < 1 = error "index out of bounds"
  | otherwise = elementAt' xs (k-1)

elementAt'' :: [a] -> Int -> a
elementAt'' (x:_) 1 = x
elementAt'' (_:xs) i = elementAt'' xs (i-1)
elementAt'' _ _ = error "index out of bounds"

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' xs = length xs

myLength'' :: [a] -> Int
myLength'' list = myLength_acc list 0
                  where
                    myLength_acc [] n = n
                    myLength_acc (_:xs) n = myLength_acc xs (n+1)

myLength''' :: [a] -> Int
myLength''' list = sum [1 | x <- list]

myLength'''' :: [a] -> Int
myLength''''  = foldl (\acc _ -> acc + 1) 0

-- 5
myReverse :: [a] -> [a]
myReverse list = reverse list

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' [x] = [x]
myReverse' (x:xs) = myReverse' xs ++ [x]

myReverse'' :: [a] -> [a]
myReverse'' list = tail list ++ myReverse'' (init list)

myReverse''' :: [a] -> [a]
myReverse''' list = myReverse_acc list []
                    where
                      myReverse_acc [] reversed = reversed
                      myReverse_acc (x:xs) reversed = myReverse_acc xs (x:reversed)

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome list@(x:xs) = x == last list && (isPalindrome . middle) list
                    where
                      middle list = (init . tail) list

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = all (==True) $ zipWith (==) xs (reverse xs)

-- 7
data NestedList a = Elem a | List [NestedList a]
testData = List[Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten(x) ++ myFlatten(List xs)

-- 8
testList = "aaaabccaadeeee"
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == (head xs) = compress xs
  | otherwise      = x : compress xs

compress' :: Eq a => [a] -> [a]
compress' = foldr removeDuplicates []
  where removeDuplicates x [] = [x]
        removeDuplicates x acc
          | x == head acc = acc
          | otherwise = x : acc

compress'' :: Eq a => [a] -> [a]
compress'' (x:xs) = x : (compress'' . dropWhile (==x) $ xs)

compress''' :: Eq a => [a] -> [a]
compress''' xs = foldr (\x acc -> if (x == head acc) then acc else (x:acc) ) [last xs] xs

-- 9
pack' :: Eq a => [a] -> [[a]]
pack' xs = foldr func [[last xs]] xs
           where
             func x (y:ys) = if (x==head y) then ((x:y):ys) else ([x]:y:ys)

pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = (x : first) : pack'' (rest)
                where (first, rest) = span (==x) xs

pack''' :: Eq a => [a] -> [[a]]
pack''' [x] = [[x]]
pack''' (x:xs) = if (x == (head.head $ result))
                 then (x:(head result)):(tail result)
                 else [x] : result
  where result = pack''' xs

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode x = map (\y -> (length y, head y)) (pack''' x)

encode' :: Eq a => [a] -> [(Int, a)]
encode' x = [(length y, head y) | y <- pack''' x]

encode'' :: Eq a => [a] -> [(Int, a)]
encode'' xs = zip (map length packed) (map head packed)
              where packed = pack'' xs
