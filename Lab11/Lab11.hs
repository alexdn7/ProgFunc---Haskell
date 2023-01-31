data List a = Nil     
    | Cons a (List a)
    deriving (Eq, Show)

--1.  Să se scrie instante Functor si Applicative pentru tipul de date List.

instance Functor List where
    fmap func Nil = Nil
    fmap func (Cons a list) = Cons (func a) (fmap func list)
    
append :: List a -> List a -> List a
append a Nil = a
append Nil a = a
append (Cons a l) b = Cons a (append l b)
    
instance Applicative List where
        pure l = Cons l Nil
        f <*> Nil = Nil
        Nil <*> f = Nil
        Cons f fs <*> Cons x xs = Cons (f x) (append (fmap f xs) (fs <*> xs))

-- 2.
data Cow = Cow {
         name :: String, 
         age :: Int, 
         weight :: Int} 
         deriving (Eq, Show)
    

noEmpty :: String -> Maybe String 
noEmpty "" = Nothing
noEmpty sir = Just sir 
             

noNegative :: Int -> Maybe Int
noNegative x
                    | x < 0 = Nothing
                    | otherwise = Just x 


cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString sir varsta greutate  
                                                        | (noEmpty sir == Just sir) && (noNegative varsta == Just varsta) && (noNegative greutate == Just greutate) = Just (Cow {name = sir, age = varsta, weight = greutate})
                                                        | otherwise  = Nothing 

cowFromString2 :: String -> Int -> Int -> Maybe Cow
cowFromString2 sir varsta greutate = fmap Cow (noEmpty sir) <*> (noNegative varsta) <*> (noNegative greutate)
 

--3 
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

-- 3 a)

validateLength :: Int -> String -> Maybe String
validateLength x sir = if(length sir < x) 
                            then Just sir
                        else
                            Nothing

-- 3 b)
mkName :: String -> Maybe Name
mkName sir = if(validateLength 25 sir /= Nothing)
                    then Just(Name sir)
            else
                Nothing

mkAddress :: String -> Maybe Address
mkAddress sir = if(validateLength 100 sir /= Nothing)
                    then Just(Address sir)
                else
                    Nothing

-- 3 c)

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = if(mkName nume /= Nothing && mkAddress adresa /= Nothing)
                            then Just (Person (Name nume) (Address adresa))
                       else 
                            Nothing

-- 3 d)

mkPerson1 :: String -> String -> Maybe Person
mkPerson1 nume adresa = Person <$> (mkName nume) <*> (mkAddress adresa)

mkName1 :: String -> Maybe Name
mkName1 nume = Name <$> (validateLength 25 nume)

mkAddress1 :: String -> Maybe Address
mkAddress1 adresa = Address <$> (validateLength 100 adresa)

