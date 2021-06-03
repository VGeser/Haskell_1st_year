import Data.List

min1 :: Integer -> Integer -> Integer
min1 x y  =  if x <= y then x else y
min2 :: [Integer] -> Integer
min2 [x]     =  x
min2 (x:xs)  =  min1 x (min2 xs)
num1 :: [Integer] -> Integer
num1 b = (min2 b)-1

min2' :: [Integer] -> Integer
min2' = foldl1 min1
num1' :: [Integer] -> Integer
num1' b = (min2' b)-1

max1 :: Integer -> Integer -> Integer
max1 x y  =  if x >= y then x else y
max2 :: [Integer] -> Integer
max2 [x]     =  x
max2 (x:xs)  =  max1 x (max2 xs)
num2 :: [Integer] -> Integer
num2 b = (max2 b)+1

max2' :: [Integer] -> Integer
max2' = foldl1 max1
num2' :: [Integer] -> Integer
num2' b = (max2' b)+1

newlist :: [Integer] -> [Integer] -> [Integer]
work :: [Integer] -> [Integer] -> [Integer] -> [Integer]
newlist c d = work c d []
work c d e = if not(null c) && not(null d)
             then if (head c) < (head d)
                  then work (tail c) d ((head c):e)
                  else work c (tail d) ((head d):e)
             else if null c
                  then (reverse e) ++ d
                  else (reverse e) ++ c
--только для чисел, потому что для произвольного типа а не получается. по-видимому, это monomorphism restriction
--если тип заменить на Char, то работает

rep :: Integer -> [Integer] -> Integer
count :: Integer -> [Integer] -> Integer -> Integer
rep el list = count el list 0
count x list n = if not(null list)
                 then if (head list) == x 
                      then count x (tail list) n+1
                      else count x (tail list) n
                 else n

findM :: [Integer] -> Integer
findor :: Integer -> Integer -> [Integer] -> [Integer] ->Integer
checker :: Integer -> [Integer] -> Integer -> Integer
checker elem spis num = if not(null spis)
                        then if (head spis) == elem 
                             then checker elem (tail spis) (num+1)
                             else checker elem (tail spis) num
                        else num
findM z =  findor 0 0 z z
findor up m list orig = if not(null list) 
                          then if up == 0
                               then findor 1 (head list) (tail list) orig
                               else if (head list) == m
                                    then findor (up+1) m (tail list) orig
                                    else findor (up-1) m (tail list) orig
                          else if fromIntegral(checker m orig 0)>((fromIntegral(length orig))/ 2)
                               then m
                               else error "No major"
--алгоритм Бойера-Мура с проверкой

freq :: [Integer] -> [(Integer,Integer)]
freq' :: [Integer] -> [(Integer,Integer)]
frcnt :: [Integer] -> Integer -> [(Integer,Integer)] -> Integer -> [(Integer,Integer)]
freq list = freq' (sort list)
freq' list = frcnt list (head list) [] 0
frcnt from elem dest entr = if not(null from)
                            then if (head from) == elem 
                                 then frcnt (tail from) elem dest (entr+1)
                                 else frcnt (tail from) (head from) ((elem,entr):dest) 1
                            else reverse ((elem,entr):dest) 

--выводит список кортежей (элемент, вхождения)









