-- Lab5
{- 1. Calculati suma pătratelor elementelor impare dintr-o listă dată ca parametru.-}

sumap :: [Int] -> Int
sumap l = foldr (+) 0 (map (^2) (filter (odd) l))


{- 2. Scrieti o functie care verifică faptul că toate elementele dintr-o listă sunt True, folosind foldr.-}

suntAdev :: [Bool] -> Bool
suntAdev l = foldr (&&) True l 


{-3. Scrieti o functie care verifică dacă toate elementele dintr-o listă de numere
întregi satisfac o proprietate dată ca parametru. -}

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies fct l = foldr (&&) True (map fct l)


{- 4. Scrieti o functie care verifică dacă există elemente într-o listă de numere
întregi care satisfac o proprietate dată ca parametru. -}

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies fct l = foldr (||) False (map fct l)

-- se face || intre valoare si False, astfel,  daca cel putin o valoare este True, vom avea True


{- 5. Redefiniti functiile map si filter folosind foldr. Le puteti numi mapFoldr si filterFoldr. -}

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr fct l = foldr (\h t -> fct h : t) [] l

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr fct l = foldr (\h t -> if fct h then h : t else t) [] l


{- 6. Folosind functia foldl, definiti functia listToInt care transformă o lista de cifre (un număr foarte mare stocat sub formă de listă) în numărul intreg
asociat. Se presupune ca lista de intrare este dată corect. -}

listToInt :: [Int] -> Int
listToInt l = foldl (\h t -> h * 10 + t) 0 l 


-- 7.

-- (a) Scrieti o functie care elimină un caracter din sir de caractere.

rmChar :: Char -> String -> String
rmChar ch sir = foldr (\h t -> if h == ch then t else h : t) [] sir


{- (b) Scrieti o functie recursivă care elimină toate caracterele din al doilea
argument care se găsesc în primul argument, folosind rmChar. -}

rmCharsRec :: String -> String -> String
rmCharsRec [] sir = sir
rmCharsRec (h:t) "" = ""
rmCharsRec (h:t) sir = rmCharsRec t (rmChar h sir)


{- (c) Scrieti o functie echivalentă cu cea de la (b) care foloseste rmChar si foldr în locul recursiei. -}

rmCharsFold :: String -> String -> String
rmCharsFold sir1 sir2 = foldr rmChar sir2 sir1
