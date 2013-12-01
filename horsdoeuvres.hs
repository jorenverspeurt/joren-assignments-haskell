-- Laatste element van een lijst
laatste::[Int]->Int
laatste [a] = a
laatste (a:tail) = laatste tail

-- Herhaal n keer het getal x in een lijst
herhaal::Int->Int->[Int]
herhaal 1 x = [x]
herhaal n x = (x:(herhaal (n-1) x))

-- Metalijst -> Platte lijst
lineariseer::[[Int]]->[Int]
lineariseer [l] = l
lineariseer (la:lb:tail) = lineariseer ((append la lb):tail)
append::[Int]->[Int]->[Int]
append [] l = l
append (h:t) l = (h:(append t l))

-- Lijst van getallen tussen 2 gegeven getallen
bereik::Int->Int->[Int]
bereik a b = if a==b then [b]
             else if a<b then (a:(bereik (a+1) b))
             else bereik b a
-- Verwijder veelvouden van een getal uit een lijst
verwijderVeelvouden::Int->[Int]->[Int]
verwijderVeelvouden a [] = []
verwijderVeelvouden a (b:t) = if (mod b a)==0 then verwijderVeelvouden a t
                            else b:(verwijderVeelvouden a t)
