fabs :: Double -> Double
fabs x | (x<0) = (-x)
       | (x>=0) = x
-- считаем сумму до n
f :: Double -> Double -> Double -> Double
f b1 n q | (fabs q/=1) = ( b1*( 1 - q**(n)))/(1-q)
         | (fabs q==1) = n*b1
         | ((b1==0) || (n==0) || (q==0)) = error "Don't do this"
-- с точностью  
lim :: Double -> Double -> Double -> Double -> Double
lim b1 n q e = if (fabs q < 1) then (if (f b1 n q - f b1 (n-1) q) < e then f b1 n q 
                                                                     else lim b1 (n+1) q e)
                               else (if (f b1 n q - f b1 (n-1) q) > e then f b1 n q 
                                                                      else lim b1 (n+1) q e)
