fabs :: Double -> Double
fabs x | (x<0) = (-x)
       | (x>=0) = x

func :: Double -> Double
func x = x^2 - 2**x 

solver :: Double -> Double -> Double -> Double
solver a b eps |fabs (func ((a+b)/2)) < eps = (a+b)/2
               |func (((a+b)/2) * func a) < 0 = solver a ((a+b)/2) eps
               |func (((a+b)/2) * func b) < 0 = solver ((a+b)/2) b eps

arctan :: Double -> Double -> Double
arctan x m | (fabs x <= 1) = sum [((-1)**(n-1))*(x**(2*n-1))/(2*n-1)| n <- [1..m]]
           | (fabs x >1) = error "Don't do this"

fpi :: Double -> Double
fpi n = sum [ (((-1)**k)/(4**k))*(2/(4*k+1)+2/(4*k+2)+1/(4*k+3)) | k <- [0..n]]

fibon :: Integer -> Integer
fibon 0 = 0
fibon 1 = 1
fibon n = fibon (n-1) + fibon (n-2)

-- с точностью 
arctane :: Double -> Double -> Double -> Double
arctane x eps n = if (((-1)**(n-1))*(x**(2*n-1))/(2*n-1)) < eps
                    then arctan x n 
                    else arctane x eps (n+1)

fpie :: Double -> Double -> Double
fpie k eps = if (((-1)**k)/(4**k))*(2/(4*k+1)+2/(4*k+2)+1/(4*k+3)) < eps
               then fpi k
               else fpie (k+1) eps

-- actane, fpie выдают ошибку переполнения стека. как по-другому сделать, я не знаю

