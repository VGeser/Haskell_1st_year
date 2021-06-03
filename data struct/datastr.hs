--дерево
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read)

--дерево из документа
b:: Tree Integer
b = (Node 1 (Node 2 (Node 21 EmptyTree EmptyTree) (Node 22 (Node 43 (Node 62 EmptyTree EmptyTree) (Node 66 EmptyTree EmptyTree)) (Node 46 EmptyTree EmptyTree))) (Node 3 (Node 34 EmptyTree EmptyTree) (Node 35 EmptyTree EmptyTree)))

--функции для задания из консоли   
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)| x == a = Node x left right
                                | x < a = Node a (treeInsert x left) right
                                | x > a = Node a left (treeInsert x right)
{-
задание из консоли
1)let d = [2,6,9,1,3,5,8,11,4,10,7]
let dTree = foldr treeInsert EmptyTree d
2) let c = [15.4,49.328,(-2.57),(75.94),43.888,17.996,58.003,52.1]
let cTree = foldr treeInsert EmptyTree c
-}

--сумма вершин
summator :: Tree Integer -> Integer -> Integer
summator (Node a left right) s | (left == EmptyTree && right == EmptyTree) = (s+a)
                               | (left == EmptyTree) = summator right (s+a)
                               | (right == EmptyTree) = summator left (s+a)
                               | otherwise = summator left (summator right (s+a))
sum1 :: Tree Integer -> Integer
sum1 a = summator a 0 

{- 
тесты
1) sum1 dTree
2) sum1 b
-}

--четные
even1 :: Tree Integer -> [Integer] -> [Integer]
even1 (Node a left right) list | (left == EmptyTree && right == EmptyTree) = if (rem a 2)==0 then (a:list) else list
                               | (left == EmptyTree) = if (rem a 2)==0 then even1 right (a:list) else even1 right list
                               | (right == EmptyTree) = if (rem a 2)==0 then even1 left (a:list) else even1 left list
                               | otherwise = even1 left (even1 right (if (rem a 2)==0 then (a:list) else list))
calleven :: Tree Integer -> [Integer]
calleven a = even1 a []

{-
тесты
1)calleven b
2)calleven dTree
--для Double нужно менять тип в самой функции
-}

--соседние
gettuple :: [Tree y] -> (y,y)
gettuple tr = (head (map(\(Node x l r) -> x)tr), last (map(\(Node x l r) -> x)tr))
sister :: Tree x -> [(x,x)] -> [(x,x)]
sister (Node a left right) list | (left == EmptyTree || right == EmptyTree) = list
                                | otherwise = sister left (sister right ((gettuple [left,right]):list))
callsis :: Tree x -> [(x,x)]
callsis a = sister a []
{-
тесты 
1) callsis b
2) callsis dTree
3) callsis cTree
-}

--поддеревья
subtree :: Tree aa -> [Tree aa]-> [Tree aa]
subtree (Node a left right) list| (left == EmptyTree && right == EmptyTree) = list
                                | (left == EmptyTree) = subtree right ((Node a left right):list)
                                | (right == EmptyTree) = subtree left ((Node a left right):list)
                                | otherwise = subtree left (subtree right ((Node a left right):list))
callsbtr :: Tree Integer -> [Tree Integer]
callsbtr a = subtree a []
{-
тесты
1) callsbtr b
2) callsbtr dTree
3) callsbtr cTree
-}

--подобные
isAlike :: (Tree a, Tree a) -> Bool
isAlike ((Node aa al ar),(Node b bl br)) |(bl == ar && br==al)||(bl == al && ar==br) = True
                                         | otherwise = False
--подобное b
bal:: Tree Integer
bal = (Node 1 (Node 3 (Node 34 EmptyTree EmptyTree) (Node 35 EmptyTree EmptyTree))(Node 2 (Node 21 EmptyTree EmptyTree) (Node 22 (Node 43 (Node 62 EmptyTree EmptyTree) (Node 66 EmptyTree EmptyTree)) (Node 46 EmptyTree EmptyTree))))

--неподобное
bn:: Tree Integer
bn = (Node 1 (Node 2 (Node 21 EmptyTree EmptyTree) (Node 22 (Node 43 (Node 62 EmptyTree EmptyTree) (Node 66 EmptyTree EmptyTree)) (Node 46 EmptyTree EmptyTree))) (Node 3 (Node 39 EmptyTree EmptyTree) (Node 45 EmptyTree EmptyTree)))

{-
тесты 
1) isAlike (b,bal)
2) isAlike (b,bn)
-}

--eq
instance Eq (Tree a) where
         x == y = isAlike (x,y)
                                       
