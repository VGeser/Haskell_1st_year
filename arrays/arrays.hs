import Data.Array

arI :: Array (Int,Int) Int
arI = array ((1,1),(2,3)) [((1,1),8), ((1,2),11), ((1,3),4),
                           ((2,1),3), ((2,2),2),  ((2,3),4)]
-- (+) elems foldl
summator :: [Int] -> Int
summator list = foldl (+) 0 list
sumel2 :: Array (Int,Int) Int -> Int 
sumel2 arr = summator (elems arr)
--для матрицы, когда два индекса
sumel1 :: Array Int Int -> Int 
sumel1 arr = summator (elems arr)
--для обычного массива, когда 1 индекс
{-
тесты:
1) sumel2 arI
2) sumel1 (array (0,5) [(0,1),(1,-1),(2,2),(3,-2),(4,3),(5,-3)])
-}

--(+) indices
--аналогично для матрицы с цифрой 2 и для обычного с 1
indexator2 :: [(Int,Int)] -> Array (Int,Int) Int -> Integer -> Integer
indexator2 ind arr s = if (null ind) 
                          then s 
                       else indexator2 (tail ind) arr (s + toInteger(arr! (head ind)))
sumi2 :: Array (Int,Int) Int -> Integer
sumi2 arr = indexator2 (indices arr) arr 0 
indexator1 :: [Int] -> Array (Int) Int -> Integer -> Integer
indexator1 ind arr s = if (null ind) 
                          then s 
                       else indexator1 (tail ind) arr (s + toInteger(arr! (head ind)))
sumi1 :: Array (Int) Int -> Integer
sumi1 arr = indexator1 (indices arr) arr 0 
{-
 тесты:
1) sumi2 arI
2) sumi1 (array (0,5) [(0,1),(1,-1),(2,2),(3,-2),(4,3),(5,-3)])
-}

--сумма индексов
maker :: [(Int,Int)] -> Array (Int,Int) Int -> [((Int,Int),Int)] -> [((Int,Int),Int)]
maker ind arr res = if (null ind)
                    then reverse res
                    else if (fst (head ind)+ snd (head ind) == arr! (head ind))
                         then maker (tail ind) arr ((head ind, arr! (head ind)):res)
                         else maker (tail ind) arr res
sumind :: Array (Int,Int) Int -> [((Int,Int),Int)]
sumind arr = maker (indices arr) arr []
{-
тесты:
1)sumind arI
2)b=array ((1,1),(3,3)) [((1,1),2),((1,2),4),((1,3),6),((2,1),7),((2,2),9),((2,3),5),((3,1),4),((3,2),1),((3,3),4)]
sumind b
-}

--77
changer :: [(Int,Int)] -> Array (Int,Int) Int -> Array (Int,Int) Int
changer ind arr = if (null ind)
                    then arr
                    else if (fst (head ind)+ snd (head ind) == arr! (head ind))
                         then changer (tail ind) (arr // [((head ind),arr! (head ind)),((head ind),77)])
                         else changer (tail ind) arr
callch :: Array (Int,Int) Int -> Array (Int,Int) Int
callch arr = changer (indices arr) arr
{-
тесты:
1)callch arI
2)b=array ((1,1),(3,3)) [((1,1),2),((1,2),4),((1,3),6),((2,1),7),((2,2),9),((2,3),5),((3,1),4),((3,2),1),((3,3),4)]
callch b
-}

--ломаная
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt((x2-x1)**2 + (y2-y1)**2)
line:: Array Int (Double,Double) -> Double -> Int-> Double
line arr len num = if (num+1)<= snd(bounds arr)
                   then line arr (dist (arr!num) (arr!(num+1))) (num+1)
                   else len
caline :: Array Int (Double,Double) -> Double
caline arr = line arr 0 (fst (bounds arr))
{-
тесты:
1)b= listArray (1,5) ([((-5.23),4.2),(2.456,13.234),(11241,211.2),((-1),0),(8.3,3.8)]::[(Double,Double)])
caline b
2)b= listArray (1,3) ([(25.7004,40.3735),(73.156,(-66.1171)),(92.2154,115.0466)]::[(Double,Double)])
caline b
-}

--усреднение
sooth :: Array Int Double -> Int -> Array Int Double
sooth arr num | num == fst (bounds arr) = sooth (arr // [(num,arr!num),(num,(arr!num + arr!(num+1))/2)]) (num+1)
              | num == snd (bounds arr) = arr // [(num,arr!num),(num,(arr!num + arr!(num-1))/2)]
              | otherwise = sooth (arr // [(num,arr!num),(num,(arr!(num-1)+arr!num+arr!(num+1))/3)]) (num+1)
med :: Array Int Double -> Array Int Double
med arr = sooth arr (fst (bounds arr)) 
{-
тесты:
1)a = listArray (1,10) ([1.3,1.2,1.9,1.25,1.4,0.1,1.2,1.32,1.23,1.5]::[Double])
med a
2)a = listArray (1,5) ([110.762,41.641,52.643,93.053,97.773]::[Double])
med a
-}

