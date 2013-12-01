enen :: [Int]
enen = 1 : enen

machten :: [Int]
machten = 1 : (zipWith (*) (map (2*) enen) machten)

rij :: [Int]
rij = 1:2:3:(zipWith3 (\a1 a2 a3 -> (a1*a3)+a2) rij (tail rij) (tail $ tail rij))

fibs :: [Float]
fibs = 1:1:[fibs!!(i-2)+fibs!!(i-1) | i<-[2..]]

