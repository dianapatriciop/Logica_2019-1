data Natural = Cero | Suc Natural deriving (Eq, Show)
--Dados dos naturales nos dice si el primero es mayor que el segundo.
mayorQue::Natural->Natural->Bool
mayorQue Cero Cero = False
mayorQue Cero _ = False
mayorQue _ Cero = True
mayorQue (Suc n) (Suc m) = mayorQue n m

--Dados dos naturales nos dice si el primero es menor que el segundo.
menorQue::Natural->Natural->Bool
menorQue Cero Cero = False
menorQue Cero _ = True
menorQue _ Cero = False
menorQue (Suc n) (Suc m) = menorQue n m

--Dados dos naturales nos dice si son iguales.
igual::Natural->Natural->Bool
igual Cero Cero = True
igual Cero _ = False
igual _ Cero = False
igual (Suc n) (Suc m) = igual n m

--Consideremos la siguiente definición de las listas de naturales.
data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales

--Dadas dos listas de naturales regresar la concatenación de ambas.
concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
concate Nil Nil = Nil
concate x Nil = x
concate Nil x = x
concate (Cons n ns) (Cons x xs) = case ns of
                                    Nil -> Cons n (Cons x xs)
                                    otherwise -> case xs of
                                                    Nil -> Cons n (concate ns (Cons x Nil))
                                                    otherwise -> Cons n (concate ns (Cons x xs))

--Dada una lista regresar la reversa de dicha lista.
reversa :: ListaDeNaturales -> ListaDeNaturales
reversa Nil = Nil
reversa q = q

-- Tipo de dato indice
type Indice = Int

-- Tipo de dato
data PL = Top |Bot | Var Indice
    |Oneg PL
    |Oand PL PL 
    |Oor PL PL
    |Oimp PL PL deriving (Eq, Show)

--Dada una fórmula regresar una lista con las conjunciones de dicha fórmula
conj :: PL -> [PL]
conj Top = []
conj Bot = []
conj (Var indice) = []
conj (Oneg pl) = conj pl
conj (Oor pl1 pl2) = (conj pl1)++ (conj pl2)
conj (Oimp pl1 pl2) = (conj pl1) ++ (conj pl2)
conj (Oand pl1 pl2) = [Oand pl1 pl2]++(conj pl1) ++ (conj pl2)

--Dada una fórmula regresar el número de conjunciones que tiene dicha formula
numConj :: PL -> Int
numConj x = case x of  
    Top -> 0
    Bot -> 0
    Var y -> 0
    Oneg y -> numConj y
    Oand a b-> (numConj a) + (numConj b) + 1
    Oor a b-> (numConj a) + (numConj b) 
    Oimp a b -> (numConj a) + (numConj b) 


--Semántica
--Consideremos los tipos de datos Valuación y Modelo de la forma
type Valuacion = Indice -> Bool
type Modelo = [Indice]

--Dada una fórmula proposicional, evalúar la fórmula de acuerdo a un modelo
satMod :: Modelo-> PL -> Bool
satMod m phi= case phi of 
    Bot -> False        
    Top -> True         
    Var x -> x `elem` m   
    Oimp alpha beta -> not(satMod m alpha) || (satMod m beta)      
    Oor alpha beta -> (satMod m alpha) || (satMod m beta)      
    Oand alpha beta -> (satMod m alpha) && (satMod m beta)      
    Oneg alpha -> not(satMod m alpha)    

--satPL :: Valuacion ->PL ->Bool 

--Formas normales

--función auxiliar para saber si una fórmua es literal
esLiteral :: PL -> Bool
esLiteral x = case x of
    Bot -> False 
    Top -> False
    Var y -> True  --es clausula si es Literal = <Variable> | Oneg <Variable>
    Oimp alpha beta -> False
    Oor alpha beta -> False
    Oand alpha beta -> False
    Oneg  alpha -> esLiteral alpha


--Dada una fórmula nos indica si es una clausula.
esClausula :: PL ->Bool
esClausula x = case x of
    Bot -> True 
    Top -> False
    Var y -> True  --es clausula si es Literal = <Variable> | Oneg <Variable>
    Oimp alpha beta -> False
    Oor alpha beta -> (esLiteral alpha) && (esClausula beta) 
    Oand alpha beta -> (False)
    Oneg alpha -> esLiteral alpha


--Dada una fórmula nos indica si esta en forma normal de conjunción.
esCNF :: PL ->Bool
esCNF x = case x of
    Bot -> esClausula Bot
    Top -> esClausula Top
    Var y -> esClausula (Var y)
    Oimp alpha beta-> False
    Oor alpha beta -> False
    Oand alpha beta -> (esClausula alpha) && (esCNF beta)
    Oneg alpha -> esClausula alpha

--Dada una fórmula nos indica si es un término.
esTermino :: PL ->Bool
esTermino x = case x of
    Bot -> False
    Top -> True
    Var y -> esLiteral (Var y)
    Oimp alpha beta-> False
    Oor alpha beta -> False
    Oand alpha beta -> (esLiteral alpha) && (esTermino beta)
    Oneg alpha -> esLiteral alpha



--Dada una fórmula nos indica si esta en forma normal de disyunción.
esDNF :: PL ->Bool
esDNF x = case x of
    Bot -> esTermino Bot
    Top -> esTermino Top
    Var y -> esTermino (Var y)
    Oimp alpha beta-> False
    Oor alpha beta -> (esTermino alpha) && (esDNF beta)
    Oand alpha beta -> False
    Oneg alpha -> esTermino alpha




--Dada una fórmula en NNF y sin implicaciones, dar su CNF, tal que sean logicamente equivalentes
--toCNF :: PL ->PL
