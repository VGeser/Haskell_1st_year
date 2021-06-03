import Data.List

--гистограмма
bar :: [Integer] -> [(Integer,Integer)]
bar1 :: Integer -> Integer -> [Integer] -> [(Integer,Integer)]-> [(Integer,Integer)]
bar list = bar1 0 0 (sort list) []
bar1 elem freq inl outl = if (null inl) 
                        then reverse ((elem,freq):outl) 
                        else if (head inl)==elem 
                             then bar1 elem (freq+1) (tail inl) outl
                             else bar1 (elem+1) 0 inl ((elem,freq):outl)  
{- тесты
выводит список кортежей (элемент,вхождения)
1) let b = [3,0,2,7,3,5,3,5]
   bar b
2) let b = [0,1,2,3,4,5,6,7,8,9,10]
   bar b
3) let b = [1,23,1,1,12,3,22,1,1,13,3,3,1,3,3,13,1]
   bar b
-}

--евклидова норма вектора
sqmake :: [Integer] -> [Integer]-> [Integer]
sqmake list outl = if (null list)
                   then outl
                   else sqmake (tail list) ((head list)^2 : outl)
summator :: [Integer] -> Integer
summator list = abs(foldl (+) 0 list)
norm :: [Integer] -> Double
norm list = sqrt(fromIntegral(summator (sqmake list [])))
{- тесты
1) norm [-1,1,2]
2) norm [3, 2, 6, 7, 11]
3) norm [0, -1, 2, -10, -3, -6, -7, 9, -8, 5]
4) norm [523386,99,5,38040,35,215,-684,-1,0,1,236548,-7,44,-844]
-}

--reverse
myrev :: [a]->[a]
myrev a1 = foldl (\acc x -> x:acc) [] a1 
{- тесты
1) myrev ["so","long","and","goodnight"]
2) myrev [1,2,3,4,5,6,7,8,9,10]
3) myrev [3.25,0.0,20.12,1.7,8.55,16.923]
-}

--дубликаты
dupl :: [Integer] -> [Integer]
dfunc :: [Integer] -> Integer -> Int ->[Integer]-> [Integer]
dupl list = dfunc list (head list) 0 [head list]
dfunc inlist elem freq outlist = if (null inlist) 
                            then reverse outlist
                            else if (head inlist)== elem 
                                 then dfunc (tail inlist) elem (freq+1) outlist
                                 else dfunc (tail inlist) (head inlist) 1 ((head inlist):outlist)
{- тесты
1) dupl [1,2,3,3,3,3,3,3,4,5,6,6]
2) dupl [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
3) dupl [0,0,1,1,2,2,7,7,7,18,93]
-}


