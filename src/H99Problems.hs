module H99Problems where
    -- Problem 1. Find the last element of a list.
    myLast:: [a] -> a
    myLast [x] = x
    myLast (x:xs) = myLast xs
    myLast [] = error "Empty list"

    -- Problem 2. Find the last but one element of a list.
    myButLast:: [a] -> a
    myButLast [x,_] = x
    myButLast (x:xs) = myButLast xs
    myButLast [] = error "Empty list"

    -- Problem 3. Find the K'th element of a list.
    elementAt:: [a] -> Int -> a
    elementAt list num = list !! (num - 1)

    -- Problem 4. Find the number of elements of a list.
    myLength:: [a] -> Int
    myLength = foldr (\x acc -> 1 + acc) 0

    -- Problem 5. Reverse a list.
    -- My solution
    myReverse:: [a] -> [a]
    myReverse = foldr (\x acc -> acc ++ [x]) []
    -- Solution from the book
    myReversePrime:: [a] -> [a]
    myReversePrime [] = []
    myReversePrime (x:xs) = myReverse xs ++ [x]

    -- Problem 6. Find out whether a list is a palindrome.
    -- My solution
    isPalindrome :: Eq a => [a] -> Bool
    isPalindrome xs = xs == myReverse xs
    -- Solution from the book
    isPalindromePrime :: Eq a => [a] -> Bool
    isPalindromePrime xs = xs == myReversePrime xs

    -- Problem 7. Flatten a nested list structure.
    data NestedList a = Elem a | List [NestedList a]
    flatten :: NestedList a -> [a]
    flatten (Elem a) = [a]
    flatten (List xs) = foldr (\x acc -> flatten x ++ acc) [] xs

    -- Problem 8. Eliminate consecutive duplicates of list elements.
    compress :: Eq a => [a] -> [a]
    compress [] = []
    compress [x] = [x]
    compress (x:xs) = if x == head xs then compress xs else x : compress xs

    -- Problem 9. Pack consecutive duplicates of list elements into sublists.
    -- My solution
    pack :: Eq a => [a] ->[[a]]
    pack [] = [[]]
    pack [x] = [[x]]
    pack (x:xs) = if x == head xs then [x,head xs] : pack (drop 1 xs) else [x] : pack xs
    -- Solution from the book
    packPrime :: Eq a => [a] ->[[a]]
    packPrime [] = []
    packPrime (x:xs) = (x:takeWhile (==x) xs) : packPrime (dropWhile (==x) xs)

    -- Problem 10. Run-length encoding of a list.
    -- My solution
    encodeHelper :: Eq a => [[a]] -> [(Int,a)]
    encodeHelper [] = []
    encodeHelper (x:xs) = [(myLength x, head x)] ++ encodeHelper xs

    encode :: Eq a => [a] -> [(Int,a)]
    encode [] = []
    encode xs = encodeHelper $ packPrime xs

    -- Solution from the book
    encodePrime :: Eq a => [a] -> [(Int,a)]
    encodePrime = map (\x -> (length x, head x)) . pack

    -- Problem 11. Modified run-length encoding.
    data MultipleSingleton a = Single a | Multiple Int a
                            deriving (Show)

    encodeModified :: Eq a => [a] -> [MultipleSingleton a]
    encodeModified [] = []
    encodeModified lists = map (\(x,y) -> if x == 1 then Single y else Multiple x y) $ encode lists

    -- Problem 12. Decode a run-length encoded list.
    decodeModified :: [MultipleSingleton a] -> [a]
    decodeModified [] = []
    decodeModified (Single x:xs) = [x] ++ decodeModified xs
    decodeModified (Multiple num x:xs) = replicate num x ++ decodeModified xs

    -- Problem 13. Run-length encoding of a list (direct solution).
    encodeDirect :: Eq a => [a] -> [MultipleSingleton a]
    encodeDirect [] = []
    encodeDirect (x:xs) = if length (takeWhile (==x) xs) == 1 then Single x : encodeDirect xs else Multiple (length (takeWhile (==x) xs)) x : encodeDirect (dropWhile (==x) xs)

    -- Problem 14. Duplicate the elements of a list.
    dupli :: [a] -> [a]
    dupli = foldr (\x acc -> replicate 2 x ++ acc) []

    -- Problem 15. Replicate the elements of a list a given number of times.
    repli :: [a] -> Int -> [a]
    repli list num = foldr (\x acc -> replicate num x ++ acc) [] list

    -- Problem 16. Drop every N'th element from a list.
    dropEvery :: [a] -> Int -> [a]
    dropEvery lists num = fst $ foldr (\x (lists,i) -> (if i `mod` num == 0 then lists else x:lists, i-1)) ([], length lists) lists

    -- Problem 17. Split a list into two parts; the length of the first part is given.
    splitPrime :: [a] -> Int -> ([a],[a])
    splitPrime list num = tripleToPair $ foldr (\x (firstlist,secondList,i) -> (if i <= num then x:firstlist else firstlist, if i > num then x:secondList else secondList, i-1)) ([],[], length list) list

    tripleToPair :: (a,b,c) -> (a,b)
    tripleToPair (x,y,z) = (x,y)




    