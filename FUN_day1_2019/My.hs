mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | x >= 0 = False

myAbs :: Int -> Int
myAbs x
    | x < 0 = (-x)
    | otherwise = x

myMin :: Int -> Int -> Int
myMin x y
    | x < y = x
    | otherwise = y

myMax :: Int -> Int -> Int
myMax x y
    | x < y = y
    | otherwise = x

myTuple :: a -> b -> (a, b)
myTuple = \ a -> \ b -> (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple = \ a -> \ b -> \c -> (a, b, c)

myFst :: (a, b) -> a
myFst = \ (a,b) -> a

mySnd :: (a, b) -> b
mySnd = \ (a,b) -> b

mySwap :: (a, b) -> (b, a)
mySwap = \ (a,b) -> (b, a)

myHead :: [a] -> a
myHead list = case list of
    [] -> error "Empty list"
    (x:xs) -> x

myTail :: [a] -> [a]
myTail list = case list of
    [] -> error "Empty list"
    (_:xs) -> xs

myLength :: [a] -> Int
myLength list = case list of
    [] -> 0
    (_:xs) -> 1 + myLength xs

myNth :: [a] -> Int -> a
myNth list a
    | a < 0 = error "Negative number"
    | a >= myLength (list) = error "List to small"
    | a == 0 = myHead list
    | otherwise = myNth (myTail list) (a-1)

myTake :: Int -> [a] -> [a]
myTake n list
    | n < 0 = error "Negative number"
    | n >= myLength (list) = list
    | n == 0 = []
    | otherwise = myHead list : myTake (n-1) (myTail list)

myDrop :: Int -> [a] -> [a]
myDrop n list
    | n < 0 = error "Negative number"
    | n >= myLength (list) = []
    | n == 0 = list
    | otherwise = myDrop (n-1) (myTail list)

myAppend :: [a] -> [a] -> [a]
myAppend list1 list2
    | myLength list1 /= 0 = myHead list1 : myAppend (myTail list1) (list2)
    | myLength list2 /= 0 = myHead list2 : myAppend list1 (myTail list2)
    | otherwise = []

myReverse :: [a] -> [a]
myReverse list
    | myLength list > 0 = myNth (list) (myLength list - 1) : myReverse (myTake (myLength list - 1) (list))
    | otherwise = []

myInit :: [a] -> [a]
myInit list
    | myLength list == 0 = error "Empty list"
    | otherwise = (myTake (myLength list - 1) (list))

myLast :: [a] -> a
myLast list
    | myLength list == 0 = error "Empty list"
    | otherwise = myNth list (myLength list - 1)

myZip :: [a] ->  [b] -> [(a, b)]
myZip list1 list2
    | myLength list1 == 0 = []
    | myLength list2 == 0 = []
    | otherwise = (myHead list1, myHead list2) : myZip (myTail list1) (myTail list2)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip list_tuples
    | myLength list_tuples == 0 = ([],[])
    | otherwise = case list_tuples of
    ((i,j):xs) -> (i : myFst (myUnzip xs) , j : mySnd (myUnzip xs))

myMap :: (a -> b) -> [a] -> [b]
myMap fnc list
    | myLength list == 0 = []
    | otherwise = case list of
    (x:xs) -> fnc x : myMap fnc xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter fnc (x:xs)
    | myLength xs == 0 = []
    | fnc x == True = x : myFilter fnc xs
    | otherwise = myFilter fnc xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ ele [] = ele
myFoldl fnc ele (x:xs)
    | myLength xs < 0 = ele
    | otherwise = myFoldl fnc (fnc ele x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ ele [] = ele
myFoldr fnc ele list
    | myLength list < 0 = ele
    | otherwise = myFoldr fnc (fnc (myNth list (myLength list - 1)) ele ) (myInit list)

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan _ [] = ([], [])
mySpan fnc (x:xs)
    | fnc x == True = (x : myFst (mySpan fnc xs), mySnd (mySpan fnc xs))
    | otherwise = (myFst (mySpan fnc xs), x: mySnd (mySpan fnc xs))