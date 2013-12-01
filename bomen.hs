data IntTree = IntEmpty | IntNode (IntTree) (Integer) (IntTree)

instance Show IntTree where
    show IntEmpty = "E"
    show (IntNode t1 i t2) =  "(" ++ show t1 ++ " " ++ show i ++ " " ++ show t2 ++ ")"

mapIntTree :: (Integer -> Integer) -> IntTree -> IntTree
mapIntTree _ IntEmpty = IntEmpty
mapIntTree f (IntNode t1 i t2) = IntNode (mapIntTree f t1) (f i) (mapIntTree f t2)

intTree2list :: IntTree -> [Integer]
intTree2list IntEmpty = []
intTree2list (IntNode t1 i t2) = intTree2list t1 ++ (i : (intTree2list t2))

data Tree t = Empty | Node (Tree t) (t) (Tree t)

instance Eq t => Eq (Tree t) where
    Empty == Empty = True
    Empty == Node t1 a t2 = False
    Node t1 a t2 == Empty = False
    Node t1 a1 t2 == Node t3 a2 t4 | tree2list (Node t1 a1 t2) == tree2list (Node t3 a2 t4) = True
                                   | otherwise = False

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node t1 a t2) = Node (mapTree f t1) (f a) (mapTree f t2)

tree2list :: Tree a -> [a]
tree2list Empty = []
tree2list (Node t1 a t2) = tree2list t1 ++ (a : (tree2list t2))

--Niet volgens de opgave
foldTree :: (a -> b -> b) -> b -> Tree a -> b 
foldTree f ne Empty = ne
foldTree f ne (Node t1 a t2) = f a (foldTree f (foldTree f ne t1) t2)

--Wel volgens de opgave
vouwTree :: (a -> b -> b) -> b -> Tree a -> b 
vouwTree f ne Empty = ne
vouwTree f ne (Node t1 a t2) = vouw f ne $ tree2list (Node t1 a t2)
--Uit de opgave
vouw :: (a -> b -> b) -> b -> [a] -> b  
vouw f ne [] = ne
vouw f ne (x:xs) = f x (vouw f ne xs)



