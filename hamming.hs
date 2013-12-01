--samenvoegen :: Ord a => [a]->[a]->[a]
samenvoegen :: [Int]->[Int]->[Int]
samenvoegen [] l = l
samenvoegen l [] = l
samenvoegen (x:xs) (y:ys) | x<y = (x : samenvoegen xs (y:ys))
                          | x>y = (y : samenvoegen (x:xs) ys)
                          | True = (x : samenvoegen xs ys)

hamming :: [Int]
hamming = 1:(samenvoegen (samenvoegen [2*a|a<-hamming] [3*a|a<-hamming]) [5*a|a<-hamming])

