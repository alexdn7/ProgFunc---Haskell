-- Lab2

--1. Să se scrie o functie poly2 care are patru argumente de tip Double, a, b, c, x si calculează a*xˆ2 + b*x+c.
-- Scrieti si signatura funct, iei (poly : ceva).

poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a * x ^ 2 + b * x + c

{- 2. Să se scrie o functie eeny care întoarce “eeny” pentru input par si “meeny” pentru input impar.
   Hint: puteti folosi functia even -}

eeny :: Integer -> String
eeny x = if (even x)
            then "eeny"
         else "meeny"

{- 3. Să se scrie o functie fizzbuzz care întoarce “Fizz” pentru numerele divizibile cu 3, “Buzz” pentru
numerele divizibile cu 5 si “FizzBuzz” pentru numerele divizibile cu ambele. 
Pentru orice alt număr se întoarce sirul vid. Pentru a calcula modulo a două numere puteti folosi functia mod. Să se scrie această functie în 2 moduri: folosind if si folosind gărzi (conditii).-}

fizzbuzz :: Integer -> String
fizzbuzz x = if (x `mod` 3 == 0 && x `mod` 5 == 0)
                then "FizzBuz"
             else if (x `mod` 3 == 0)
                then "Fizz"
             else if (x `mod` 5 == 0)
                then "Buzz"
             else  ""

-- acum cu garzi
fizzbuzzg :: Integer -> String
fizzbuzzg x
           | x `mod` 15 == 0 = "FizzBuz"
           | x `mod` 3 == 0 = "Fizz"
           | x `mod` 5 == 0 = "Buzz"
           | otherwise = ""


{- 4. Numerele tribonacci sunt definite de ecuattia
Tn = 1 dacă n = 1
     1 dacă n = 2
     2 dacă n = 3
     Tn−1 + Tn−2 + Tn−3 dacă n > 3
Să se implementeze functia tribonacci atât cu cazuri cât si ecuational -}

tribonacci :: Integer -> Integer
tribonacci x 
            | x == 1 = 1
            | x == 2 = 1
            | x == 3 = 2
            | otherwise = tribonacci(x - 1) + tribonacci(x - 2) + tribonacci(x - 3)

tribonaccie :: Integer -> Integer
tribonaccie 1 = 1
tribonaccie 2 = 1
tribonaccie 3 = 2
tribonaccie x = tribonaccie(x - 1) + tribonaccie(x - 2) + tribonaccie(x - 3)

{- 5. Să se scrie o functie care calculează coeficientii binomiali, folosind recursivitate. Acestia sunt determinati folosind urmatoarele ecuatii.
B(n,k) = B(n-1,k) + B(n-1,k-1)
B(n,0) = 1
B(0,k) = 0 -}

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 1
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

{- 6. Să se implementeze următoarele functii folosind liste:
a) verifL - verifică dacă lungimea unei liste date ca parametru este pară -}

verifL :: [Int] -> Bool
verifL = even.length

{- b) takefinal - pentru o listă dată ca parametru si un număr n, întoarce lista cu ultimele n elemente.
Dacă lista are mai putin de n elemente, se intoarce lista nemodificată. -}

takefinal :: [Int] -> Int -> [Int]
takefinal l n
              | n > length l = l
              | otherwise = drop (length l - n) l


{-7. Exercitii: să se scrie urmatoarele functii folosind recursivitate:

a) myreplicate - pentru un întreg n si o valoare v întoarce lista de lungime n ce are doar elemente egale cu v. Să se scrie si prototipul functiei.
b) sumImp - pentru o listă de numere întregi, calculează suma valorilor impare. Să se scrie si prototipul functiei.
c) totalLen - pentru o listă de siruri de caractere, calculează suma lungimilor sirurilor care încep cu caracterul ‘A’. -}


myreplicate :: Int -> Int -> [Int]
myreplicate 0 v = []
myreplicate n v = [v] ++ myreplicate (n - 1) v

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h : t) 
              | odd h = h + sumImp t
              | otherwise = sumImp t

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h : t) 
                | h !! 0 == 'A' = length h + totalLen t
                | otherwise = totalLen t
