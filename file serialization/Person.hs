module Person (Person,p1) where
data Person = Person {name :: String,
                      age :: Int,
                      weight :: Double} deriving (Eq,Show,Read)
p1 :: Person 
p1 = Person {name = "Dumbledore", age = 115, weight = 80.20}

