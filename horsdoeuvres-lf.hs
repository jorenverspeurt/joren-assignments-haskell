-- Laatste element van een lijst
laatste::[Int]->Int
laatste l = last l
-- Herhaal n keer het getal x in een lijst
herhaal::Int->Int->[Int]
herhaal n x = take n (repeat x)
-- Metalijst -> Platte lijst
lineariseer::[[Int]]->[Int]
lineariseer a = concat a
-- Lijst van getallen tussen 2 gegeven getallen
bereik::Int->Int->[Int]
bereik a b = scanl (+) 1 (take (b-a-1) (repeat a))
-- Verwijder veelvouden van een getal uit een lijst
verwijderVeelvouden::Int->[Int]->[Int]
verwijderVeelvouden a b = filter (\x -> not $ (mod x a)==0) b

-- Werkt niet
vvalt::Int->[Int]->[Int]
vvalt a b = filter (not . 0==(flip . mod ($) a)) b
