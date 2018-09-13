data Natural = Cero | Suc Natural deriving (Eq, Show)

mayorQue::Natural->Natural->Bool
mayorQue Cero Cero = False
mayorQue Cero _ = False
mayorQue _ Cero = True
mayorQue (Suc n) (Suc m) = mayorQue n m

menorQue::Natural->Natural->Bool
menorQue Cero Cero = False
menorQue Cero _ = True
menorQue _ Cero = False
menorQue (Suc n) (Suc m) = menorQue n m

igual::Natural->Natural->Bool
igual Cero Cero = True
igual Cero _ = False
igual _ Cero = False
igual (Suc n) (Suc m) = igual n m

data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales
--Dadas dos listas regresa la concatenación de ambas
concate :: [Natural] -> [Natural] -> [Natural]
concate [] [] = []
concate [] x = x
concate x [] = x
concate l (x:xs) = concate (l++[x]) xs

--Dada una lista, regresa la reversa de la lista
reversa :: [Natural] -> [Natural]
reversa [] = []
reversa (x:xs) = reversa (xs) ++ [x]

-- Tipo de dato indice
type Indice = Int

-- Tipo de dato
data PL = Top |Bot | Var Indice
    |Oneg PL
    |Oand PL PL 
    |Oor PL PL
    |Oimp PL PL deriving (Eq, Show)

--conj :: PL -> [PL]

numConj :: PL -> Int
numConj x = case x of  
    Top -> 0
    Bot -> 0
    Var y -> 0
    Oneg y -> numConj y
    Oand a b-> (numConj a) + (numConj b) + 1
    Oor a b-> (numConj a) + (numConj b) 
    Oimp a b -> (numConj a) + (numConj b) 

