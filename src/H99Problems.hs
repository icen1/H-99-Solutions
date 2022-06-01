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
    isPalindrome:: Eq a => [a] -> Bool
    isPalindrome xs = xs == myReverse xs 
    -- Solution from the book
    isPalindromePrime:: Eq a => [a] -> Bool
    isPalindromePrime xs = xs == myReversePrime xs

    -- Problem 7. Flatten a nested list structure.
    data NestedList a = Elem a | List [NestedList a]
    flatten:: NestedList a -> [a]
    flatten (Elem a) = [a]
    flatten (List xs) = foldr (\x acc -> flatten x ++ acc) [] xs

    