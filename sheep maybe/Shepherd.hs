import Barans
import Control.Monad
import Data.Maybe

--большинство решений взято из лекции
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing _  = Nothing
comb (Just x) f = f x

ded :: Sheep -> Maybe Sheep
ded s = (Just s) `comb` mother
                 `comb` father

--ded "i8"
--ded "i10"
--ded "i11"
--ded "i6"

praded :: Sheep -> Maybe Sheep 
praded s = (Just s) `comb` mother
                    `comb` father
                    `comb` father

roditel :: Sheep -> [Sheep]
roditel s = (maybeToList (mother s)) ++ (maybeToList (father s))

--лучше способа, чем в лекции не удалось придумать

grandparents :: Sheep -> [Sheep]
grandparents s = (roditel s) >>= roditel

sirota :: Sheep -> Bool
sirota x = isNothing (father x `mplus` mother x)

selected_barans = ["i3", "i5", "i6", "i9", "i12"]

--без guard и do

call :: String -> Bool
call x = elem x selected_barans

dad_select :: Sheep -> Maybe Sheep
dad_select s = if (maybe False call (father s) ) == False
                  then Nothing 
                  else (father s)
--dad_select "i6"
--dad_select "i11"
--dad_select "i2"

relative_select :: Sheep -> Maybe Sheep
relative_select s = if (maybe False call (father s)) == False 
                       then (father s) >>= relative_select 
                       else (father s)
--relative_select "i8"
--relative_select "i12"