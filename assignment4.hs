import System.Environment


mylist = putStrLn "I may not work"



maxList :: [Integer] -> Integer
maxlist[] = putStrLn "Please input atleast 1 element"
maxList(x) = maximum x




deletex ::Integer -> [Integer] -> [Integer]

deletex n[] = []


deletex n(x:xs) 
      | n-1 == 0   =
                        deletex n(xs)
      | otherwise = 
                        do
                        x : deletex (n-1)(xs)
delete :: Int -> [a] -> [a]

delete _ [] = []
delete n xs = take (n-1) xs ++ delete n (drop n xs)


isort :: [Int] -> [Int]
isort [] = []
isort [x] = [x]
isort (x:xs) = insert (isort xs)
  where insert [] = [x]
        insert (y:ys)
          | x < y = x : y : ys
          | otherwise = y : insert ys

double :: [Integer]->[Integer]
double [] = []
double [x] = [2*x]
double (x:y:ys) = 2*x:y:double ys


single:: [Integer] -> [[Integer]]
single [] = []
single[x] = [[x]]
single (x:xs) = [[x]]++single(xs)

rotate :: Int -> [a] -> [a]
rotate 0 x = x
rotate times (x) = rotate (times - 1) (last(x) : init(x))
                      