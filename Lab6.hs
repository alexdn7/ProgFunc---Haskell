data Fruct
          = Mar String Bool
           | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                    Portocala "Sanguinello" 10,
                    Portocala "Valencia" 22,
                    Mar "Golden Delicious" True,
                    Portocala "Sanguinello" 15,
                    Portocala "Moro" 12,
                    Portocala "Tarocco" 3,
                    Portocala "Moro" 12,
                    Portocala "Valencia" 2,
                    Mar "Golden Delicious" False,
                    Mar "Golden" False,
                    Mar "Golden" True]


{- a) Scrieti o functie

    ePortocalaDeSicilia :: Fruct -> Bool
    ePortocalaDeSicilia = undefined

    care indică dacă un fruct este o portocală de Sicilia sau nu. Soiurile de portocale din Sicilia sunt Tarocco, Moro si Sanguinello. De exemplu, -}

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _  _ ) = False
ePortocalaDeSicilia (Portocala tip _) = tip `elem` ["Tarocco", "Moro", "Sanguinello"] 

-- Exemple teste

test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12) --  True
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True) -- False


{- b) Scrieti o functie care calculează numărul total de felii ale portocalelor de Sicilia dintr-o listă de fructe. -}

nrFelii :: Fruct -> Int
nrFelii (Mar _ _) = 0
nrFelii (Portocala _ nrf) = nrf

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (h: t) 
                         | ePortocalaDeSicilia h = nrFelii h + nrFeliiSicilia t
                         | otherwise = nrFeliiSicilia t 

-- Test
test_nrFeliiSicilia = nrFeliiSicilia listaFructe -- 52


-- 3. Scrieti o functie care calcuelază numărul de mere care au viermi dintr-o lista de fructe. 

viermi :: Fruct -> Bool
viermi (Portocala _  _) = False
viermi (Mar _ v) = v

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (h : t)
                                | viermi h = 1 + nrMereViermi t
                                | otherwise = nrMereViermi t


-- Test
test_nrMereViermi = nrMereViermi listaFructe -- 2


-- Exc 2

type NumeA = String

type Rasa = String

data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

-- a) Scrieti o functie care întoarce "Meow!" pentru pisică si"Woof!" pentru câine.

vorbeste :: Animal -> String
vorbeste (Caine _ _) = "Woof!"
vorbeste (Pisica _) = "Meow!"


{- b) Vă reamintiti tipul de date predefinit Maybe

    data Maybe a = Nothing | Just a

Scrieti o functie care întoarce rasa unui câine dat ca parametru sau Nothing dacă parametrul este o
pisică.-}

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ rs) = Just rs


{- Exercitiul 3
    
    Se dau urmatoarele tipuri de date ce reprezintă matrici cu linii de lungimi diferite: -}

data Linie = L [Int]
    deriving Show

data Matrice = M [Linie]
    deriving Show


-- a) Scrieti o functie care verifica daca suma elementelor de pe fiecare linie este egala  cu o valoare n. Rezolvati cerinta folosind foldr.

-- va uma
