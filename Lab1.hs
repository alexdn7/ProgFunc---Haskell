{- 6. Să se scrie următoarele functii:
a) functie cu 2 parametri care calculeaza suma pătratelor celor două numere;
b) functie cu un parametru ce întoarce mesajul “par” dacă parametrul este par si “impar” altfel;
c) functie care calculează factorialul unui număr;
d) functie care verifică dacă un primul parametru este mai mare decât dublul celui de-al doilea.
parametru. -}

-- 6 a)
sumaPatrate :: Integer -> Integer -> Integer
sumaPatrate a b = a * a + b * b

-- b)
paritate :: Integer -> [Char]
paritate x = if (even x == True)
                then "par"
             else "impar"

-- c)
factoriall :: Integer -> Integer
factoriall 0 = 1
factoriall n = n * factoriall (n - 1)

-- d)
functie :: Integer -> Integer -> Bool
functie a b = if (a > b * 2)
                then True
              else False
