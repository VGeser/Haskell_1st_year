import System.IO
import Text.Regex.Posix

data Person = Person {name :: String,
                      age :: Int,
                      weight :: Double} deriving (Eq,Show,Read)

personer :: String->Int->Double->Person
personer a b c = Person {name = a, age = b, weight = c}

emptyp :: Person
emptyp = Person {name = "", age = 0, weight =0}

resolve :: String -> Person -> Person
resolve str inp
        | (str =~ "(^age = {1})" :: Bool) == True = personer (name inp) (read (drop 6 str):: Int) (weight inp)
        | (str =~ "(^name = {1})" :: Bool) == True = personer (drop 7 str) (age inp) (weight inp)
        | (str =~ "(^weight = {1})" :: Bool) == True = personer (name inp) (age inp) (read (drop 9 str) :: Double)
        | otherwise = inp

call :: [String] -> Int -> Person -> Person
call strs n pin = if n<3
                     then call (tail strs) (n+1) (resolve (head strs) pin)
                     else pin

main = do
         file <- readFile "input1.txt"
         let fields = lines file
         let outp = call fields 0 emptyp
         writeFile "output1.txt" (show outp)
         return()