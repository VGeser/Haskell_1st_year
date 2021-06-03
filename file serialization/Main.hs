import Person
import System.IO

main = do
	writeFile "output.txt" (show p1)
        x <- readFile "output.txt"
        let p2 = read x 
        if p2==p1 then appendFile "output.txt" "\n success" else return()
       
        