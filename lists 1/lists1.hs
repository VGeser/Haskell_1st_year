distr:: (Double,Double) -> (Double,Double) -> Double
distr (x1,y1) (x2,y2) = sqrt((x2-x1)**2 + (y2-y1)**2)

dist :: [(Double,Double)] -> Double
dist a = distc 0 2 a (distr (head a) (head (tail a))) (length a)

distc :: Int -> Int -> [(Double,Double)] -> Double -> Int -> Double
distc num1 num2 a curmin len = if (num1 /= len)
                                  then (if num2 <= (len-1) 
                                           then distc num1 (num2+1) a (if curmin <(distr (a !! num1)(a !! num2)) then curmin else (distr (a !! num1)(a !! num2))) len
                                        else distc (num1+1) (num1+2) a curmin len)
                               else curmin

piflist = [(a,b,c) | a <- [1..], b <- [1..a], c <- [1..b], (c^2)+(b^2)==(a^2)]

nod :: Integer -> Integer -> Integer
nod x 0 = 0
nod x 1 = 1
nod x y | (rem x y == 0) = y
        | (rem y x == 0) = x
        | (x>y) = nod (rem x y) y 
        | otherwise = nod x (rem y x)


piflist1 = [(a,b,c) | a <- [1..], b <- [1..a], c <- [1..b], (c^2)+(b^2)==(a^2), (nod a b == 1) && (nod b c == 1)]

sumd :: Integer -> Integer
sumd' :: Integer -> Integer -> Integer -> Integer
sumd 0 = 0
sumd 1 = 1
sumd x = sumd' 0 1 x
sumd' s n x = if n < x 
                 then (if rem x n == 0 then sumd' (s+n) (n+1) x else sumd' s (n+1) x)
              else s


perfect = [a | a <- [1..], sumd a == a]