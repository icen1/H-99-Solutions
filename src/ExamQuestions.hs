module ExamQuestions where
    import Data.Maybe
    import Data.Char (toUpper, ord)
    import Data.List
------------------------------ Question 4 --------------------------------------    
    -- Tape data structure
    data Tape a = Tape
                {
                    left :: [a],
                    focus :: a,
                    right :: [a]
                }
                deriving (Show)

    -- Singleton tape
    singletonTape :: a -> Tape a
    singletonTape a = Tape [] a []

    -- Moving the focus left
    moveLeft :: Tape a -> Tape a
    moveLeft (Tape [] x rs) = Tape [] x rs
    moveLeft (Tape (x:xs) y ys) = Tape xs x (y:ys)

    -- Changes a list to a tape
    fromList :: Int -> [a] -> Tape a
    fromList num list = Tape left focus right
                    where
                        cutList = splitAt num list
                        templeft = fst cutList
                        right = snd cutList
                        removeTail list = take (length list-1) list
                        left = removeTail templeft
                        focus = last left

    instance Foldable Tape where
        foldr f acc (Tape left focus right) = foldr f (f focus foldedRs) $ reverse left
                                            where
                                                foldedRs = foldr f acc right

    makeTape :: [a] -> a -> [a] -> Tape a
    makeTape = Tape

    moveRight :: Tape a -> Tape a
    moveRight (Tape left focus []) = Tape left focus []
    moveRight (Tape [] focus (x:xs)) = Tape [focus] x xs
    moveRight (Tape left focus (right:rights)) = Tape (focus:left) right rights

    toLeft :: Tape a -> Tape a
    toLeft (Tape left focus right) = Tape [] (last left) (left ++ focus:right)

    splitAtTape :: (a -> Bool) -> Tape a -> ([a], [a])
    splitAtTape f t = splitAt' start
                    where
                        start = toLeft t
                        splitAt' t = case right t of
                                        [] ->
                                            if f (focus t)
                                            then (reverse (left t), [])
                                            else (reverse (left t) ++ [focus t],[])
                                        _ ->
                                            if f (focus t)
                                            then (reverse $ left t, right t)
                                            else splitAt' (moveRight t)

------------------------------- Question 2--------------------------------------

    isPrime :: Integer -> Bool
    isPrime num = null [x | x <- [2..num-1], num `mod` x == 0]

    primes :: [Integer]
    primes = [x | x <- [2..], isPrime x]

    toPrime :: Char -> Integer
    toPrime c = primes !! (ord(toUpper c) - ord 'A')

    score :: String -> Integer
    score str = product $ map toPrime str

    isAnagram :: String -> String -> Bool
    isAnagram str1 str2 = score str1 == score str2

    anagrams :: [String] -> [(String,[String])]
    anagrams list = [(x,filter (isAnagram x) (list \\ [x])) | x <- list]

    isSubgram :: String -> String -> Bool
    isSubgram str1 str2 = (score str1 `mod` score str2 == 0)

    subgrams :: [String] -> [(String,[String])]
    subgrams list = [(x,filter (isSubgram x) (list \\ [x])) | x <- list]

------------------------------- Question 3 -------------------------------------
    class Default a where
        def :: a

    instance Default (Maybe a) where
        def = Nothing

    instance (Default e, Default f) => Default (e,f) where
        def = (def,def)

    --instance Default (Either c d) where
        {-
        Not possible to implement this as we don't know whether either/both/any
        of the values has the instance of Default. Even if they both had instances
        of Default, we can't know which one to use.
        -}
------------------------------ Question 6 --------------------------------------
    doFizzBuzz :: Int -> String
    doFizzBuzz num
            |   (num `mod` 15) == 0   = "FizzBuzz"
            |   num `mod` 3 == 0        = "Fizz"
            |   num `mod `5 == 0        = "Buzz"
            |   otherwise               = show num

    fizzBuzz :: [String]
    fizzBuzz = map doFizzBuzz [1..]

    takeOutput :: IO () -> IO String
    takeOutput io = pure "Hello\nWorld"

    intercept :: (String -> String) -> IO () -> IO ()
    intercept f io = takeOutput io >>= mapM_ (putStrLn . f) . lines

            