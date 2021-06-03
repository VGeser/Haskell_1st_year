import Data.Maybe
import Control.Applicative
import Data.Either
import Control.Monad.Writer
import Control.Monad.State

--1
data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Read)
dTree :: ATree Double
dTree = ABranch (ABranch (ALeaf 21) 2.2 (ABranch (ABranch (ALeaf 62) 43 (ALeaf 66)) 22 (ALeaf 46))) 1.1 (ABranch (ALeaf 34) 3.3 (ALeaf 35))

appl x y = (*) <$> x <*> y

multipl :: ATree Double -> Maybe Double
multipl (ALeaf t) = if (t==0) then Nothing else Just t
multipl (ABranch l t r) = if isJust(multipl l) && isJust(multipl r)
                             then appl (appl (multipl l)(multipl r)) (Just t)
                             else Nothing
--multipl dTree

my_div :: Double -> ATree Double -> Maybe Double
my_div x t = (/) <$> (Just x) <*> (multipl t)

--my_div 1 dTree
--my_div 1.0e13 dTree

--2
idTree :: ATree (Int,Double)
idTree = ABranch(ABranch(ALeaf (4,21))(2,2.2)(ABranch(ABranch(ALeaf (100,62))(7,0)(ALeaf (200,66)))(5,22)(ALeaf (203,46))))(1,1.1)(ABranch(ALeaf (301,34))(3,3.3)(ALeaf (307,0)))

multE :: ATree (Int,Double) -> Either String Double
multE (ALeaf (id,n)) = if (n==0) then Left ("id "++(show id)++" is flawed") else Right n
multE (ABranch l (id,n) r) = if isLeft(multE l) then (multE l)
                                                else if isLeft (multE r) then (multE r)
                                                                         else appl (appl (multE l)(multE r)) (Right n)

divE :: Double -> ATree (Int,Double) -> Either String Double
divE x t = (/) <$> (Right x) <*> (multE t)

--divE 12 idTree

--3
summator::ATree (Int,Double)->Double-> Writer [String] Double
summator (ALeaf (id,n)) s = do
                              tell ["Processing "++ show id]
                              return (s+n)
summator (ABranch l (id,n) r) s= do
                                   tell ["Processing "++ show id]
                                   (+) <$> (summator l s) <*> (summator r n)                                   
call_sum a = do
                let p = runWriter (summator a 0)
                mapM_ putStrLn $ snd $ p
                putStrLn $ show $ fst $ p

--call_sum idTree

--4
numTree :: Int -> ATree Double -> ATree (Int,Double)
numTree id (ALeaf leaf)= (ALeaf (id+1,leaf))
numTree id (ABranch l t r) = ABranch (numTree (id+1) l) ((id+1),t) (numTree (id+2) r)

call_num :: ATree Double -> ATree (Int,Double)
call_num a = numTree 0 a

--вызов: call_num dTree

{-
у меня не получается сделать для state, пишет
 Couldn't match expected type `ATree (Int, Double)'
                  with actual type `ATree (Int, Double) -> ATree (Int, Double)'
    * Probable cause: `q' is applied to too few arguments

во втором варианте сопоставления

enumS :: Int -> ATree Double -> State (ATree (Int,Double)) Int
enumS n (ALeaf b) = do
                       let m = n+1
                       m <- get
                       let p = (ALeaf ((n+1),b))
                       put p
                       return (n+1)
enumS n (ABranch l b r) = do
                            let m = n+1
                            m <- get
                            let q = execState (enumS (n+1) l)
                            let q1 = execState(enumS (n+1) r)
                            let p = (ABranch (q) ((n+1),b) (q1))
                            put p
                            return (n+1)
-}


