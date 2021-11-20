-- 11
data ListItem a = Multiple Int a | Single a deriving (Show)

pack''' :: Eq a => [a] -> [[a]]
pack''' [x] = [[x]]
pack''' (x:xs) = if (x == (head.head $ result))
                 then (x:(head result)):(tail result)
                 else [x] : result
  where result = pack''' xs

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map extract packedList
                    where
                      packedList = pack''' xs
                      extract [x] = Single x
                      extract list@(x:xs) =  Multiple (length list) x

-- 12
decodeModified :: [(ListItem a)] -> [a]
decodeModified  = concatMap decode'
  where
    decode' (Single value) = [value]
    decode' (Multiple count value) = replicate count value

-- 13
encodeDirect :: [a] ->  [(ListItem a)]
encodeDirect [] = []
encodeDirect [x] = process [x]
encodeDirect list@(x:xs) =
  where
    a = takeWhile (== x) list
    b = dropWhile (== x) list
    result = (process a) : encodeDirect b
    process :: [a] -> (ListItem a)
    process [x] = Single x
    process list@(x:xs) = Multiple (length list) x
  
