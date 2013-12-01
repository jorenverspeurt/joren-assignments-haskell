som :: [Int] -> Int
som [] = 0
som (x:xs) = x+(som xs)

mijn_product :: [Int] -> Int
mijn_product [] = 1
mijn_product (x:xs) = x*(mijn_product xs)

vouw :: (b->a->b) -> b -> [a] -> b
vouw f b [] = b
vouw f b (a:as) = vouw f (f b a) as

ontkoppel :: [(a,b)] -> ([a],[b])
ontkoppel [(a,b)] = ([a],[b])
ontkoppel (x:xs) = ontkoppel_h (x:xs) ([],[])

ontkoppel_h :: [(a,b)] -> ([a],[b]) -> ([a],[b])
ontkoppel_h [] t = t
ontkoppel_h ((x1,x2):xs) (as,bs) = ontkoppel_h xs (as++[x1],bs++[x2])

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (r:rs) = transpose_h (r:rs) []

transpose_h :: [[a]] -> [[a]] -> [[a]]
transpose_h [] l = l
transpose_h (x:xs) [] = transpose_h xs [ [a] | a<-x]
transpose_h (x:xs) (y:ys) = transpose_h xs [ ((y:ys)!!i)++[(x!!i)] | i <- [0..((length x)-1)]]
