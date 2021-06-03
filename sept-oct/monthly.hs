fabs:: Integer -> Integer
fabs x | (x>=0) = x
       | (x<0) = -x

sg' x | x == 0 = 1
      | x > 0 = 0
      | otherwise = error "Not positive arg"
 
inc :: Integer -> Integer
inc x | (x >=0) = x+1
      | otherwise = error "Not positive arg"

dec:: Integer -> Integer
dec x | x > 0 = x - 1
      | x == 0 = 0
      | otherwise = error "Not positive arg"

pls:: Integer->Integer->Integer
pls x 0 = x
pls x 1 = inc x
pls x y = pls (inc x) (dec y)

mns:: Integer->Integer->Integer
mns x 0 = x
mns x 1 = dec x
mns x y = if (y<=x) then mns (dec x) (dec y) else 0

mlt:: Integer->Integer->Integer
mlt x 1 = x
mlt x 0 = 0
mlt x y = pls x (mlt x (dec y))

mymax:: Integer->Integer->Integer
mymax x y | (x<y) = y
          | otherwise = x

mymin:: Integer->Integer->Integer
mymin x y | (x<y) = x
          | otherwise = y

divis:: Integer->Integer->Integer
divi':: Integer->Integer->Integer->Integer
divis m 1 = m
divis m 0 = 0
divis m k |(m==k) = 1
          |(m/=k) = divi' m k 1

divi' m k n= if (mns m k)>=k then divi' (mns m k) k (inc n) else n

rem1:: Integer->Integer->Integer
rem1 m 1 = 0
rem1 m 0 = 0
rem1 m k |(m==k) = 0
         |(m/=k) = if (mns m k)>=k then rem1 (mns m k) k else (mns m k)

proc:: Integer -> Integer -> Bool
proc':: Integer->Integer -> Integer -> Bool
proc x 0 = False
proc x y |(x==y) = True
         |(x/=y) = proc' x y 1 
proc' x y t |(t<=y && x>(mlt t y))= proc' x y (inc t)
            |(t<=y && x<(mlt t y))= False
            |(t<=y && x==(mlt t y)) = True
            |(t>y) = False

divsum:: Integer -> Integer -> Integer
divsum' :: Integer -> Integer -> Integer -> Integer -> Integer
divsum x 0 = 0
divsum x 1 = x
divsum x y = divsum' 1 0 x y
divsum' i a x y = if (i<x) 
                     then divsum' (inc i) (pls a (sg' (mns(mlt i y) x) )) x y
                  else a

remsum:: Integer -> Integer -> Integer
remsum x 0 = 0
remsum x 1 = 0
remsum x y = mns x (mlt y (divsum x y)) 

procsum:: Integer -> Integer -> Bool
procsum x 0 = False
procsum x 1 = True
procsum x y = if (remsum x y == 0) then True else False

div1:: Integer -> Integer -> Integer
div2:: Integer -> Integer -> Integer -> Integer
div1 x 1 = x
div1 x 0 = 0
div1 x y = if (x==y) then 1 else div2 x y 1
div2 x y n |(x>0 && y>0) = if (mns x y)>=y then div2 (mns x y) y (inc n) else n
           |(x<0 && y<0) = if negate(mlt (fabs(y)) n)>=x then div2 x y (inc n) else n
           |(x>0 && y<0) = if negate(mlt (fabs(y)) n)<=x then div2 x y (dec n) else n
           |(x<0 && y>0) = if negate(mlt (fabs(y)) n)>=x then div2 x y (dec n) else n

rem2:: Integer -> Integer -> Integer
rem2 x 0 = 0
rem2 x 1 = 1
rem2 x y |(x==y) = 0
         |(x/=y) = mns (fabs(mymax x (div1 x y))) (fabs(mymin x (div1 x y)))

nd:: Integer -> Integer
nd':: Integer -> Integer -> Integer -> Integer
nd 1 = 1
nd 0 = 0
nd x | (x>0) = nd' x 1 1
     | (x<0) = nd' x (-1) 1
nd' x xi n | (x>0) = if xi<x then nd' x (inc xi) (if (rem2 x xi)==0 then (inc n) else n) else n
           | (x<0) = if xi>x then nd' x (negate(inc (fabs(xi)))) (if (rem2 x xi)==0 then (inc n) else n) else n

sumd:: Integer -> Integer
sumd':: Integer -> Integer -> Integer -> Integer
sumd 0 = 0
sumd 1 = 1
sumd x | (x>0) = sumd' x 1 0
       | (x<0) = sumd' x (-1) 0
sumd' x xi s | (x>0) = if xi<x then sumd' x (inc xi) (if (rem2 x xi)==0 then (pls xi s) else s) else s
             | (x<0) = if xi>x then sumd' x (negate(inc (fabs(xi)))) (if (rem2 x xi)==0 then (pls (fabs(xi)) (fabs(s))) else s) else s

prime:: Integer -> Bool
prime1 :: Integer -> Integer -> Bool
prime 0 = False
prime 1 = False
prime x = prime1 x 2
prime1 x xi = if (proc x xi)
                 then False
              else (if xi<x then prime1 x (inc x) else True)

pnd:: Integer -> Integer
pnd':: Integer -> Integer -> Integer -> Integer
pnd 1 = 0
pnd 0 = 0
pnd x = pnd' x 2 0
pnd' x xi n = if xi<x then pnd' x (inc xi) (if (prime n) then (inc n) else n) else n

nod :: Integer -> Integer -> Integer
nod x 0 = 0
nod x 1 = 1
nod x y | (rem1 x y == 0) = y
        | (rem1 y x == 0) = x
        | (x>y) = nod (rem x y) y 
        | otherwise = nod x (rem1 y x)

nok :: Integer -> Integer -> Integer
nok x 0 = 0
nok x 1 = 1
nok x y = divis (mlt x y) (nod x y)  



